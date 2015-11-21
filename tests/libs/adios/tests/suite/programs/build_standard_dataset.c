/*
 * build_standard_dataset.c
 *
 *  Created on: Sep 26, 2014
 *      Author: David A. Boyuka II
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include <mpi.h>
#include <adios.h>
#include <adios_types.h>

// Declaration of all datasets available from this program

typedef void (*dataset_builder_fn)(const char *filename_prefix, const char *transform_name);

typedef struct {
	const char *name;
	const char *desc;
	dataset_builder_fn builder_fn;
} dataset_info_t;

// Forward declaration of dataset builder functions
static void build_dataset_1(const char *filename_prefix, const char *transform_name);
static void build_dataset_2(const char *filename_prefix, const char *transform_name);
static void build_dataset_3(const char *filename_prefix, const char *transform_name);
static void build_dataset_unevenpg(const char *filename_prefix, const char *transform_name);
static void build_dataset_particle(const char *filename_prefix, const char *transform_name);

static const dataset_info_t DATASETS[] = {
    { .name = "DS-1D", .builder_fn = build_dataset_1,
      .desc = "A simple, small dataset with a 1D variable 'temp' consisting of 1 PG and 1 timestep" },

    { .name = "DS-2D", .builder_fn = build_dataset_2,
      .desc = "A simple, small dataset with a 2D (8x8) variable 'temp' decomposed in to 2x8 slices across 2 timesteps (8 PGs total)" },

    { .name = "DS-3D", .builder_fn = build_dataset_3,
      .desc = "A simple, small dataset with a 3D (4x4x4) variable 'temp' decomposed in to 2x2x2 blocks across 2 timesteps (16 PGs total)" },

    { .name = "DS-particle", .builder_fn = build_dataset_particle,
      .desc = "A dataset simulating particle data, with a 2D (3x64) 1 variable 'temp' decomposed in 3x32 slices across 2 timesteps (4 PGs total). "
    		  "A given X coordinate represents a 'variable', as 'variables' in particle datasets are often packed as tuples." },
    { .name = "DS-unevenpg", .builder_fn = build_dataset_unevenpg,
      .desc = "A simple, small dataset with a 2D (8x8) variable 'temp' decomposed in to 2x8 slices in the first timestep, and 8x2 slices in the second timestep (8 PGs total)" },
};
static const int NUM_DATASETS = sizeof(DATASETS)/sizeof(DATASETS[0]);

// Dataset description/writing code

typedef struct {
	const char *group_name;
	unsigned int buffer_size_mb;
	const char *write_transport_method;
	int ndim;
	int nvar;
	const char **varnames;
	const enum ADIOS_DATATYPES *vartypes;
} dataset_xml_spec_t;

// Imported from adios_internal.c
extern const char * adios_type_to_string_int(int type);                    // converts enum ADIOS_DATATYPES to string
extern uint64_t adios_get_type_size(enum ADIOS_DATATYPES type, void *var); // returns the size in bytes of a given enum ADIOS_DATATYPES

static void build_dimension_var_list(int ndim, const char *dimvar_base, char *outbuf) {
	int i;
	for (i = 0; i < ndim; i++)
		outbuf += sprintf(outbuf, i == 0 ? "%s%d" : ",%s%d", dimvar_base, i);
}

static void produce_xml(
		FILE *outfile,
		const dataset_xml_spec_t *xml_spec,
		const char *transform_name)
{
	static const char *HEADER_XML =
	"<?xml version=\"1.0\"?>\n"
	"<adios-config host-language=\"C\">\n"
	"	<adios-group name=\"%s\" coordination-communicator=\"comm\">\n";

	static const char *DIMVAR_XML =
	"		<var name=\"N%d\" type=\"integer\"/>\n"
	"		<var name=\"D%d\" type=\"integer\"/>\n"
	"		<var name=\"O%d\" type=\"integer\"/>\n";

	static const char *GLOBALBOUNDS_HEADER_XML =
	"		<global-bounds dimensions=\"%s\" offsets=\"%s\">\n";

	static const char *VAR_XML =
	"			<var name=\"%s\" type=\"%s\" dimensions=\"%s\" transform=\"%s\"/>\n";

	static const char *GLOBALBOUNDS_FOOTER_XML =
	"		</global-bounds>\n";

	static const char *FOOTER_XML =
	"	</adios-group>\n"
	"	<method group=\"%s\" method=\"%s\"/>\n"
	"	<buffer size-MB=\"%u\" allocate-time=\"now\"/>\n"
	"</adios-config>\n";

	// Actual function begins
	int i;
	char dimvar_list_buf1[256]; // for storing D0,D1,D2,...,Dn
	char dimvar_list_buf2[256]; // for storing D0,D1,D2,...,Dn

	fprintf(outfile, HEADER_XML, xml_spec->group_name);
	for (i = 0; i < xml_spec->ndim; i++)
		fprintf(outfile, DIMVAR_XML, i, i, i);

	build_dimension_var_list(xml_spec->ndim, "N", dimvar_list_buf1);
	build_dimension_var_list(xml_spec->ndim, "O", dimvar_list_buf2);
	fprintf(outfile, GLOBALBOUNDS_HEADER_XML, dimvar_list_buf1, dimvar_list_buf2);

	for (i = 0; i < xml_spec->nvar; ++i) {
		build_dimension_var_list(xml_spec->ndim, "D", dimvar_list_buf1);
		fprintf(outfile, VAR_XML, xml_spec->varnames[i], adios_type_to_string_int(xml_spec->vartypes[i]), dimvar_list_buf1, transform_name);
	}

	fprintf(outfile, GLOBALBOUNDS_FOOTER_XML);
	fprintf(outfile, FOOTER_XML, xml_spec->group_name, xml_spec->write_transport_method, xml_spec->buffer_size_mb);
}

static void write_adios_dimension_scalars(int64_t fd, const char *dimvar_basename, int ndim, const uint64_t *dims) {
	int i;
	char dimvar_name[32];
	for (i = 0; i < ndim; ++i) {
		sprintf(dimvar_name, "%s%d", dimvar_basename, i);
		adios_write(fd, dimvar_name, (void*)dims);
		++dims;
	}
}

typedef struct {
	int num_ts;
	int num_pgs_per_ts;
	const uint64_t *global_dims;
} dataset_global_spec_t;

typedef struct {
	const uint64_t *pg_dim;
	const uint64_t *pg_offset;
	const void **vardata;
} dataset_pg_spec_t;

static uint64_t dim_prod(int ndim, const uint64_t *dims) {
	uint64_t pg_gridsize = 1;
	while (ndim--)
		pg_gridsize *= *dims++;
	return pg_gridsize;
}

static uint64_t compute_groupsize(uint64_t base_groupsize, const dataset_xml_spec_t *xml_spec, const dataset_pg_spec_t *pg) {
	int var, dim;

	// Compute the number of points contained in this PG
	uint64_t pg_gridsize = dim_prod(xml_spec->ndim, pg->pg_dim);

	// Compute the sum of the datatype sizes across all variables defined in this PG
	uint64_t total_var_datatypes_size = 0;
	for (var = 0; var < xml_spec->nvar; ++var)
		total_var_datatypes_size += adios_get_type_size(xml_spec->vartypes[var], NULL);

	// The final group size is the product of the number of points and the number of bytes per point, plus the base groupsize
	return base_groupsize + pg_gridsize * total_var_datatypes_size;
}

extern void adios_pin_timestep(uint32_t ts); // Not in the standard header, but accessible
static void build_dataset_from_specs(
		const char *filename_prefix,
		const char *transform_name,
		const dataset_xml_spec_t *xml_spec,
		const dataset_global_spec_t *global_spec,
		int num_ts, int num_pgs_per_ts,
		dataset_pg_spec_t pg_specs[num_ts][num_pgs_per_ts]) // Not const because C has an corner case here (http://c-faq.com/ansi/constmismatch.html)
{
	int var;
	char xml_filename[strlen(filename_prefix) + strlen(".xml") + 1];
	char bp_filename[strlen(filename_prefix) + strlen(".bp") + 1];
	int timestep, pg_in_timestep;
	char dimvar[32];

	// Construct the XML and BP filenames
	sprintf(xml_filename, "%s.xml", filename_prefix);
	sprintf(bp_filename, "%s.bp", filename_prefix);

	// Write out the XML file
	FILE *xml_out = fopen(xml_filename, "w");
	assert(xml_out);
	produce_xml(xml_out, xml_spec, transform_name);
	fclose(xml_out);

	// Write out the BP file
	adios_init(xml_filename, MPI_COMM_WORLD);

	// Compute the groupsize contribution of the dimension scalars
	const uint64_t base_groupsize = xml_spec->ndim * 3 * 4; // *3 for 3 scalars (N, D, O) *4 for sizeof(adios_integer) (not sure how what function in the User API to call to get this programatically

	// For each timestep, for each PG in that timestep, write out all variables using the provided vardata buffers
	int64_t adios_file;
	for (timestep = 0; timestep < global_spec->num_ts; ++timestep) {
		for (pg_in_timestep = 0; pg_in_timestep < global_spec->num_pgs_per_ts; ++pg_in_timestep) {
			// (Re-)open the file in write or append mode, depending on whether or not this is the first PG written
			const int is_first_pg = (timestep == 0 && pg_in_timestep == 0);
			adios_open(&adios_file, xml_spec->group_name, bp_filename, is_first_pg ? "w" : "a", MPI_COMM_WORLD);

			// Pin the timestep to allow multiple adios_open/adios_close cycles to write
			// to the same timestep (this simulates a parallel file write with fewer core)
			adios_pin_timestep(timestep + 1); // +1 because we want the timesteps to be 1-based

			const dataset_pg_spec_t *pg_spec = &pg_specs[timestep][pg_in_timestep];

			// Compute the group size
			uint64_t groupsize = compute_groupsize(base_groupsize, xml_spec, pg_spec);
			uint64_t out_groupsize;
			adios_group_size(adios_file, groupsize, &out_groupsize);

			write_adios_dimension_scalars(adios_file, "N", xml_spec->ndim, global_spec->global_dims);
			write_adios_dimension_scalars(adios_file, "D", xml_spec->ndim, pg_spec->pg_dim);
			write_adios_dimension_scalars(adios_file, "O", xml_spec->ndim, pg_spec->pg_offset);

			// Write each variable
			for (var = 0; var < xml_spec->nvar; ++var) {
				adios_write(adios_file, xml_spec->varnames[var], (void*)pg_spec->vardata[var]); // (void*) to get rid of compiler complaining about constness
			}

			// Close the file to commit it
			adios_close(adios_file);
		}
	}
}

// NOTE: varblocks_by_var is actually a 1D array varblocks_by_var[var] of 3D "arrays" varblockdata[ts][pg][point_in_pg] of type xml_spec->vartypes[var] (however, points per PG varies)
static void collect_varblocks_by_pg(
		const dataset_xml_spec_t *xml_spec,
		const dataset_global_spec_t *global_spec,
		int num_ts, int num_pgs_per_ts, int ndim, int nvar,
		const uint64_t pg_dims[num_ts][num_pgs_per_ts][ndim],
		const void **varblocks_by_var,
		const void *out_varblocks_by_pg[num_ts][num_pgs_per_ts][nvar])
{
	int ts, pg, var;

	// Some maths
	const uint64_t varblocks_per_ts = nvar * num_pgs_per_ts;

	// Cache the datatype size for each variable, and create a data pointer that we can advance
	int var_typesizes[nvar];
	const char *varblock_datas[nvar];
	for (var = 0; var < nvar; ++var) {
		var_typesizes[var] = adios_get_type_size(xml_spec->vartypes[var], NULL);
		varblock_datas[var] = (const char *)varblocks_by_var[var];
	}

	// Iterate over all varblocks (var in pg in timestep), get a pointer to the data buffer for that
	// varblock, and assign it to the proper place in the output array of PG data buffers
	for (ts = 0; ts < num_ts; ++ts) {
		for (pg = 0; pg < num_pgs_per_ts; ++pg) {
			// Compute the points per varblock in this PG
			const uint64_t points_per_varblock = dim_prod(ndim, pg_dims[ts][pg]);

			for (var = 0; var < nvar; ++var) {
				const int var_typesize = var_typesizes[var];
				const uint64_t varblock_size = points_per_varblock * var_typesize;

				// Get the data for this varblock, and advance the pointer by one varblock
				const char *data_for_varblock = varblock_datas[var];
				varblock_datas[var] += varblock_size;

				// Finally, assign the pointer
				out_varblocks_by_pg[ts][pg][var] = data_for_varblock;
			}
		}
	}
}

// NOTE: pg_dims and pg_offsets are really a 2D arrays pd_dims[pg][dim] of dimension lengths
// NOTE: pg_datas is really a 2D array pg_datas[pg][var] of varblock buffers
// NOTE: pg_specs is really a 1D array of pg_specs[pg] of dataset_pg_spec_t's
static void collect_pg_specs(
		int num_ts, int num_pgs_per_ts, int ndim, int nvar,
		const uint64_t pg_dims[num_ts][num_pgs_per_ts][ndim],
		const uint64_t pg_offsets[num_ts][num_pgs_per_ts][ndim],
		const void *pg_datas[num_ts][num_pgs_per_ts][nvar],
		dataset_pg_spec_t pg_specs[num_ts][num_pgs_per_ts]) {
	int ts, pg;
	for (ts = 0; ts < num_ts; ++ts) {
		for (pg = 0; pg < num_pgs_per_ts; ++pg) {
			pg_specs[ts][pg].pg_dim = pg_dims[ts][pg];
			pg_specs[ts][pg].pg_offset = pg_offsets[ts][pg];
			pg_specs[ts][pg].vardata = pg_datas[ts][pg];
		}
	}
}

static void build_dataset_from_varblocks_by_var(
		const char *filename_prefix,
		const char *transform_name,
		const dataset_xml_spec_t *xml_spec,
		const dataset_global_spec_t *global_spec,
		int num_ts, int num_pgs_per_ts, int ndim, int nvar,
		const uint64_t pg_dims[num_ts][num_pgs_per_ts][ndim],
		const uint64_t pg_offsets[num_ts][num_pgs_per_ts][ndim],
		const void *varblocks_by_var[nvar])
{
	const uint64_t num_pgs = num_ts * num_pgs_per_ts;

	// Array for repackaging global per-variable data in per-PG data
	const void *varblocks_by_pg[num_ts][num_pgs_per_ts][nvar];
	// Array for repackaging pieces of per-PG inforamtion into an array of per-PG specification structs
	dataset_pg_spec_t pg_specs[num_ts][num_pgs_per_ts];

	collect_varblocks_by_pg(
			xml_spec, global_spec,
			num_ts, num_pgs_per_ts, ndim, nvar,
			pg_dims, varblocks_by_var, varblocks_by_pg);

	collect_pg_specs(
			num_ts, num_pgs_per_ts, ndim, nvar,
			pg_dims, pg_offsets, varblocks_by_pg, pg_specs);

	build_dataset_from_specs(
			filename_prefix, transform_name, xml_spec, global_spec,
			num_ts, num_pgs_per_ts, pg_specs);
}







static void build_dataset_1(const char *filename_prefix, const char *transform_name) {
	// Basic dataset information
	// NOTE: we have to use an anonymous enum here to define these constants, since
	// C is picky and doesn't consider a static const int "const enough" to use
	// as an array length (e.g., if these were static const ints, it would not compile)
	enum {
		NUM_DIMS = 1,
		NUM_TS = 1,
		NUM_PGS_PER_TS = 1,
		NUM_VARS = 1,
		NUM_PGS = NUM_TS * NUM_PGS_PER_TS,
	};

	// Variable names/types
	static const char *VARNAMES[NUM_VARS]					= { "temp"     };
	static const enum ADIOS_DATATYPES VARTYPES[NUM_VARS]	= { adios_real };

	// Global and PG dimensions/offsets
	static const uint64_t GLOBAL_DIMS                         [NUM_DIMS] = { 16 };
	static const uint64_t PG_DIMS	  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = { { { 16 } } };
	static const uint64_t PG_OFFSETS  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = { { { 0 } } };

	// Variable data (we can use [TS][PG][16] here because every PG is the same size, 16)
	static const float TEMP_DATA[NUM_TS][NUM_PGS_PER_TS][16] = {
		// Timestep 1
		// PG 0 in timestep 1
		//  -1.00000000     -1.00003052     2.00000000     2.00006104
			-0x1.000000p+0, -0x1.000200p+0, 0x1.000000p+1, 0x1.000200p+1,
		//  2.00012207     -30.00000000    -30.00048828    -30.00097656
			0x1.000400p+1, -0x1.e00000p+4, -0x1.e00200p+4, -0x1.e00400p+4,
		//  -30.00146484    50.00000000    50.00097656    50.00195312
			-0x1.e00600p+4, 0x1.900000p+5, 0x1.900200p+5, 0x1.900400p+5,
		//  50.00292969    50.00390625    50.00488281    50.00585938
			0x1.900600p+5, 0x1.900800p+5, 0x1.900a00p+5, 0x1.900c00p+5,
	};

	static const void *VARBLOCKS_BY_VAR[NUM_VARS] = {
		TEMP_DATA,
	};

	// Now, collect all this information into specification structs
	// File specification
	static const dataset_xml_spec_t XML_SPEC = {
		.group_name = "S3D",
		.buffer_size_mb = 128,
		.write_transport_method = "MPI",
		.ndim = NUM_DIMS,
		.nvar = NUM_VARS,
		.varnames = VARNAMES,
		.vartypes = VARTYPES,
	};

	// Global space specification
	static const dataset_global_spec_t GLOBAL_SPEC = {
		.num_ts = NUM_TS,
		.num_pgs_per_ts = NUM_PGS_PER_TS,
		.global_dims = GLOBAL_DIMS,
	};

	// Finally, invoke the dataset builder with this information
	build_dataset_from_varblocks_by_var(
			filename_prefix, transform_name, &XML_SPEC, &GLOBAL_SPEC,
			NUM_TS, NUM_PGS_PER_TS, NUM_DIMS, NUM_VARS,
			PG_DIMS, PG_OFFSETS, (const void **)VARBLOCKS_BY_VAR);
}

// Variable data used in datasets 2 and 3
static const float TEMP_DATA_FOR_DS2_AND_DS3[] = {
	// Timestep 1
	-0x1.122f92p-5, 0x1.51e224p-2, -0x1.619b00p-1, 0x1.b0a05ap-2, -0x1.a66e56p-3, 0x1.0021acp-2, -0x1.45eb00p-1, 0x1.ba2cb6p-1,
	-0x1.34e536p-2, 0x1.985a90p-6, -0x1.75c0a6p-2, 0x1.87e004p-1, -0x1.457d2ap-2, 0x1.15f8c4p-3, -0x1.b53a84p-4, 0x1.8576bep-1,
	-0x1.4e8798p-4, 0x1.1a16eep-1, -0x1.211a0ep-1, 0x1.7c8df4p-1, -0x1.f6a944p-1, 0x1.c03452p-3, -0x1.d14d2ap-2, 0x1.098d06p-1,
	-0x1.25ce96p-1, 0x1.1c473ep-1, -0x1.750404p-1, 0x1.41fbc4p-1, -0x1.979e1cp-1, 0x1.2adcccp-1, -0x1.195c60p-2, 0x1.a8c114p-1,
	-0x1.d3cddep-1, 0x1.ee4930p-1, -0x1.022282p-2, 0x1.eb4ba0p-4, -0x1.b9681ap-3, 0x1.c6fc40p-1, -0x1.f7962ap-1, 0x1.08cca2p-1,
	-0x1.d3bf16p-1, 0x1.64ecfap-2, -0x1.21594cp-2, 0x1.d9f6a6p-3, -0x1.efe95cp-2, 0x1.8ea7eep-2, -0x1.fbf468p-1, 0x1.21c5a2p-1,
	-0x1.e16ae6p-1, 0x1.1d0e76p-1, -0x1.3ca72ap-2, 0x1.d8142ap-1, -0x1.8d1b8ap-1, 0x1.86fa2ap-1, -0x1.c3425ep-2, 0x1.65d43ep-2,
	-0x1.4682d0p-2, 0x1.5a94cap-3, -0x1.f4e5e4p-1, 0x1.d6fc1cp-4, -0x1.818200p-1, 0x1.032826p-2, -0x1.e3a098p-1, 0x1.554fdep-1,
	// Timestep 2
	-0x1.554fdep-1, 0x1.e3a098p-1, -0x1.032826p-2, 0x1.818200p-1, -0x1.d6fc1cp-4, 0x1.f4e5e4p-1, -0x1.5a94cap-3, 0x1.4682d0p-2,
	-0x1.65d43ep-2, 0x1.c3425ep-2, -0x1.86fa2ap-1, 0x1.8d1b8ap-1, -0x1.d8142ap-1, 0x1.3ca72ap-2, -0x1.1d0e76p-1, 0x1.e16ae6p-1,
	-0x1.21c5a2p-1, 0x1.fbf468p-1, -0x1.8ea7eep-2, 0x1.efe95cp-2, -0x1.d9f6a6p-3, 0x1.21594cp-2, -0x1.64ecfap-2, 0x1.d3bf16p-1,
	-0x1.08cca2p-1, 0x1.f7962ap-1, -0x1.c6fc40p-1, 0x1.b9681ap-3, -0x1.eb4ba0p-4, 0x1.022282p-2, -0x1.ee4930p-1, 0x1.d3cddep-1,
	-0x1.a8c114p-1, 0x1.195c60p-2, -0x1.2adcccp-1, 0x1.979e1cp-1, -0x1.41fbc4p-1, 0x1.750404p-1, -0x1.1c473ep-1, 0x1.25ce96p-1,
	-0x1.098d06p-1, 0x1.d14d2ap-2, -0x1.c03452p-3, 0x1.f6a944p-1, -0x1.7c8df4p-1, 0x1.211a0ep-1, -0x1.1a16eep-1, 0x1.4e8798p-4,
	-0x1.8576bep-1, 0x1.b53a84p-4, -0x1.15f8c4p-3, 0x1.457d2ap-2, -0x1.87e004p-1, 0x1.75c0a6p-2, -0x1.985a90p-6, 0x1.34e536p-2,
	-0x1.ba2cb6p-1, 0x1.45eb00p-1, -0x1.0021acp-2, 0x1.a66e56p-3, -0x1.b0a05ap-2, 0x1.619b00p-1, -0x1.51e224p-2, 0x1.122f92p-5,
};

static void build_dataset_2(const char *filename_prefix, const char *transform_name) {
	// Basic dataset information
	// NOTE: we have to use an anonymous enum here to define these constants, since
	// C is picky and doesn't consider a static const int "const enough" to use
	// as an array length (e.g., if these were static const ints, it would not compile)
	enum {
		NUM_DIMS = 2,
		NUM_TS = 2,
		NUM_PGS_PER_TS = 4,
		NUM_VARS = 1,
		NUM_PGS = NUM_TS * NUM_PGS_PER_TS,
	};

	// Variable names/types
	static const char *VARNAMES[NUM_VARS]					= { "temp"     };
	static const enum ADIOS_DATATYPES VARTYPES[NUM_VARS]	= { adios_real };

	// Global and PG dimensions/offsets
	static const uint64_t GLOBAL_DIMS                         [NUM_DIMS] = { 8, 8 };
	static const uint64_t PG_DIMS	  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = {
		{ { 2, 8 }, { 2, 8 }, { 2, 8 }, { 2, 8 }, }, // Timestep 1
		{ { 2, 8 }, { 2, 8 }, { 2, 8 }, { 2, 8 }, }, // Timestep 2
	};
	static const uint64_t PG_OFFSETS  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = {
		{ { 0, 0 }, { 2, 0 }, { 4, 0 }, { 6, 0 }, }, // Timestep 1
		{ { 0, 0 }, { 2, 0 }, { 4, 0 }, { 6, 0 }, }, // Timestep 2
	};

	static const void *VARBLOCKS_BY_VAR[NUM_VARS] = {
		TEMP_DATA_FOR_DS2_AND_DS3,
	};

	// Now, collect all this information into specification structs
	// File specification
	static const dataset_xml_spec_t XML_SPEC = {
		.group_name = "S3D",
		.buffer_size_mb = 128,
		.write_transport_method = "MPI",
		.ndim = NUM_DIMS,
		.nvar = NUM_VARS,
		.varnames = VARNAMES,
		.vartypes = VARTYPES,
	};

	// Global space specification
	static const dataset_global_spec_t GLOBAL_SPEC = {
		.num_ts = NUM_TS,
		.num_pgs_per_ts = NUM_PGS_PER_TS,
		.global_dims = GLOBAL_DIMS,
	};

	// Finally, invoke the dataset builder with this information
	build_dataset_from_varblocks_by_var(
			filename_prefix, transform_name, &XML_SPEC, &GLOBAL_SPEC,
			NUM_TS, NUM_PGS_PER_TS, NUM_DIMS, NUM_VARS,
			PG_DIMS, PG_OFFSETS, (const void **)VARBLOCKS_BY_VAR);
}

static void build_dataset_3(const char *filename_prefix, const char *transform_name) {
	// Basic dataset information
	// NOTE: we have to use an anonymous enum here to define these constants, since
	// C is picky and doesn't consider a static const int "const enough" to use
	// as an array length (e.g., if these were static const ints, it would not compile)
	enum {
		NUM_DIMS = 3,
		NUM_TS = 2,
		NUM_PGS_PER_TS = 8,
		NUM_VARS = 1,
		NUM_PGS = NUM_TS * NUM_PGS_PER_TS,
	};

	// Variable names/types
	static const char *VARNAMES[NUM_VARS]					= { "temp"     };
	static const enum ADIOS_DATATYPES VARTYPES[NUM_VARS]	= { adios_real };

	// Global and PG dimensions/offsets
	static const uint64_t GLOBAL_DIMS                         [NUM_DIMS] = { 4, 4, 4 };
	static const uint64_t PG_DIMS	  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = {
		{ { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, }, // Timestep 1
		{ { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, { 2, 2, 2 }, }, // Timestep 2
	};
	static const uint64_t PG_OFFSETS  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = {
		{ { 0, 0, 0 }, { 0, 0, 2 }, { 0, 2, 0 }, { 0, 2, 2 }, { 2, 0, 0 }, { 2, 0, 2 }, { 2, 2, 0 }, { 2, 2, 2 }, }, // Timestep 1
		{ { 0, 0, 0 }, { 0, 0, 2 }, { 0, 2, 0 }, { 0, 2, 2 }, { 2, 0, 0 }, { 2, 0, 2 }, { 2, 2, 0 }, { 2, 2, 2 }, }, // Timestep 2
	};

	static const void *VARBLOCKS_BY_VAR[NUM_VARS] = {
		TEMP_DATA_FOR_DS2_AND_DS3,
	};

	// Now, collect all this information into specification structs
	// File specification
	static const dataset_xml_spec_t XML_SPEC = {
		.group_name = "S3D",
		.buffer_size_mb = 128,
		.write_transport_method = "MPI",
		.ndim = NUM_DIMS,
		.nvar = NUM_VARS,
		.varnames = VARNAMES,
		.vartypes = VARTYPES,
	};

	// Global space specification
	static const dataset_global_spec_t GLOBAL_SPEC = {
		.num_ts = NUM_TS,
		.num_pgs_per_ts = NUM_PGS_PER_TS,
		.global_dims = GLOBAL_DIMS,
	};

	// Finally, invoke the dataset builder with this information
	build_dataset_from_varblocks_by_var(
			filename_prefix, transform_name, &XML_SPEC, &GLOBAL_SPEC,
			NUM_TS, NUM_PGS_PER_TS, NUM_DIMS, NUM_VARS,
			PG_DIMS, PG_OFFSETS, (const void **)VARBLOCKS_BY_VAR);
}

static void build_dataset_particle(const char *filename_prefix, const char *transform_name) {
	// Basic dataset information
	// NOTE: we have to use an anonymous enum here to define these constants, since
	// C is picky and doesn't consider a static const int "const enough" to use
	// as an array length (e.g., if these were static const ints, it would not compile)
	enum {
		NUM_DIMS = 2,
		NUM_TS = 2,
		NUM_PGS_PER_TS = 2,
		NUM_VARS = 1,
		NUM_PGS = NUM_TS * NUM_PGS_PER_TS,
	};

	// Variable names/types
	static const char *VARNAMES[NUM_VARS]					= { "temp"     };
	static const enum ADIOS_DATATYPES VARTYPES[NUM_VARS]	= { adios_real };

	// Global and PG dimensions/offsets
	static const uint64_t GLOBAL_DIMS                         [NUM_DIMS] = { 3, 64 };
	static const uint64_t PG_DIMS	  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = { 
		{ { 3, 32 }, { 3, 32 } },
		{ { 3, 32 }, { 3, 32 } }
	};
	static const uint64_t PG_OFFSETS  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = {
		{ { 0, 0 }, { 0, 32 }, },
		{ { 0, 0 }, { 0, 32 }, },
	};

	// Variable data (we can use [TS][PG][96] here because every PG is the same size, 96)
	static const float TEMP_DATA[NUM_TS][NUM_PGS_PER_TS][96] = {
		// Timestep 1
		// PG 0 in timestep 1
                -10.033470,   10.329965,   -10.690636,   10.422486,   -10.206265,   10.250129,   -10.636559,   10.863623,
                -10.301656,   10.024924,   -10.364993,   10.765381,   -10.317861,   10.135729,   -10.106746,   10.760672,
                -10.081673,   10.550956,   -10.564651,   10.743271,   -10.981760,   10.218850,   -10.454396,   10.518654,
                -10.573842,   10.555231,   -10.728546,   10.628874,   -10.796128,   10.583715,   -10.274767,   10.829598,
                -10.913680,   10.965403,   -10.252085,   10.119946,   -10.215530,   10.888643,   -10.983567,   10.517186,
                -10.913568,   10.348560,   -10.282567,   10.231427,   -10.484288,   10.389313,   -10.992099,   10.565961,
                -10.940269,   10.556751,   -10.309232,   10.922029,   -10.775600,   10.763627,   -10.440682,   10.349442,
                -10.318858,   10.169230,   -10.978316,   10.114986,   -10.752945,   10.253083,   -10.944585,   10.666625,
                -100.033470,  100.329964,  -100.690636,  100.422485,  -100.206268,  100.250130,  -100.636559,  100.863625,
                -100.301659,  100.024925,  -100.364990,  100.765381,  -100.317863,  100.135727,  -100.106743,  100.760674,
                -100.081673,  100.550957,  -100.564651,  100.743271,  -100.981758,  100.218849,  -100.454399,  100.518654,
                -100.573845,  100.555229,  -100.728546,  100.628876,  -100.796127,  100.583717,  -100.274765,  100.829597,
        // PG 1 in timestep 1
                -100.913681,  100.965401,  -100.252083,  100.119942,  -100.215530,  100.888641,  -100.983566,  100.517189,
                -100.913567,  100.348557,  -100.282570,  100.231430,  -100.484291,  100.389313,  -100.992096,  100.565964,
                -100.940269,  100.556747,  -100.309235,  100.922028,  -100.775604,  100.763626,  -100.440681,  100.349442,
                -100.318855,  100.169228,  -100.978317,  100.114990,  -100.752945,  100.253082,  -100.944588,  100.666626,
                -1000.033447, 1000.329956, -1000.690613, 1000.422485, -1000.206238, 1000.250122, -1000.636536, 1000.863647,
                -1000.301636, 1000.024902, -1000.364990, 1000.765381, -1000.317871, 1000.135742, -1000.106750, 1000.760681,
                -1000.081665, 1000.550964, -1000.564636, 1000.743286, -1000.981750, 1000.218872, -1000.454407, 1000.518677,
                -1000.573853, 1000.555237, -1000.728577, 1000.628845, -1000.796143, 1000.583740, -1000.274780, 1000.829590,
                -1000.913696, 1000.965393, -1000.252075, 1000.119934, -1000.215515, 1000.888672, -1000.983582, 1000.517212,
                -1000.913574, 1000.348572, -1000.282593, 1000.231445, -1000.484314, 1000.389282, -1000.992126, 1000.565979,
                -1000.940247, 1000.556763, -1000.309204, 1000.922058, -1000.775574, 1000.763611, -1000.440674, 1000.349426,
                -1000.318848, 1000.169250, -1000.978333, 1000.114990, -1000.752930, 1000.253113, -1000.944580, 1000.666626,
		
        // Timestep 2
		// PG 0 in timestep 2
                -10.218485,   10.196670,   -10.786571,   10.434015,   -10.085313,   10.770138,   -10.951201,   10.998879,
                -10.118698,   10.233769,   -10.230308,   10.602986,   -10.623081,   10.222406,   -10.168947,   10.563350,
                -10.779158,   10.478179,   -10.485379,   10.554758,   -10.241806,   10.926062,   -10.904201,   10.560665,
                -10.095291,   10.882517,   -10.675652,   10.848236,   -10.135600,   10.620235,   -10.514861,   10.354085,
                -10.816905,   10.301431,   -10.788100,   10.902218,   -10.071568,   10.739302,   -10.901097,   10.190267,
                -10.973071,   10.131405,   -10.793253,   10.596152,   -10.353811,   10.962200,   -10.159503,   10.132969,
                -10.440379,   10.644882,   -10.687727,   10.682186,   -10.570944,   10.591928,   -10.242850,   10.666235,
                -10.474444,   10.918502,   -10.514470,   10.610044,   -10.538737,   10.029331,   -10.964128,   10.355642,
                -100.218483,  100.196671,  -100.786568,  100.434013,  -100.085312,  100.770134,  -100.951202,  100.998878,
                -100.118698,  100.233772,  -100.230309,  100.602989,  -100.623085,  100.222404,  -100.168945,  100.563347,
                -100.779160,  100.478180,  -100.485382,  100.554756,  -100.241806,  100.926064,  -100.904198,  100.560661,
                -100.095291,  100.882515,  -100.675652,  100.848236,  -100.135597,  100.620239,  -100.514862,  100.354088,
        // PG 1 in timestep 2
                -100.816902,  100.301430,  -100.788101,  100.902214,  -100.071571,  100.739304,  -100.901100,  100.190269,
                -100.973068,  100.131409,  -100.793251,  100.596153,  -100.353813,  100.962204,  -100.159500,  100.132965,
                -100.440376,  100.644882,  -100.687729,  100.682182,  -100.570946,  100.591927,  -100.242851,  100.666237,
                -100.474442,  100.918503,  -100.514473,  100.610046,  -100.538734,  100.029327,  -100.964127,  100.355644,
                -1000.218506, 1000.196655, -1000.786560, 1000.434021, -1000.085327, 1000.770142, -1000.951172, 1000.998901,
                -1000.118713, 1000.233765, -1000.230286, 1000.602966, -1000.623108, 1000.222412, -1000.168945, 1000.563354,
                -1000.779175, 1000.478149, -1000.485352, 1000.554749, -1000.241821, 1000.926086, -1000.904175, 1000.560669,
                -1000.095276, 1000.882507, -1000.675659, 1000.848206, -1000.135620, 1000.620239, -1000.514832, 1000.354065,
                -1000.816895, 1000.301453, -1000.788086, 1000.902222, -1000.071594, 1000.739319, -1000.901123, 1000.190247,
                -1000.973083, 1000.131409, -1000.793274, 1000.596130, -1000.353821, 1000.962219, -1000.159485, 1000.132996,
                -1000.440369, 1000.644897, -1000.687744, 1000.682190, -1000.570923, 1000.591919, -1000.242859, 1000.666260,
                -1000.474426, 1000.918518, -1000.514465, 1000.610046, -1000.538757, 1000.029358, -1000.964111, 1000.355652,
	};

	static const void *VARBLOCKS_BY_VAR[NUM_VARS] = {
		TEMP_DATA
	};

	// Now, collect all this information into specification structs
	// File specification
	static const dataset_xml_spec_t XML_SPEC = {
		.group_name = "S3D",
		.buffer_size_mb = 128,
		.write_transport_method = "MPI",
		.ndim = NUM_DIMS,
		.nvar = NUM_VARS,
		.varnames = VARNAMES,
		.vartypes = VARTYPES,
	};

	// Global space specification
	static const dataset_global_spec_t GLOBAL_SPEC = {
		.num_ts = NUM_TS,
		.num_pgs_per_ts = NUM_PGS_PER_TS,
		.global_dims = GLOBAL_DIMS,
	};

	// Finally, invoke the dataset builder with this information
	build_dataset_from_varblocks_by_var(
			filename_prefix, transform_name, &XML_SPEC, &GLOBAL_SPEC,
			NUM_TS, NUM_PGS_PER_TS, NUM_DIMS, NUM_VARS,
			PG_DIMS, PG_OFFSETS, (const void **)VARBLOCKS_BY_VAR);
}

static void build_dataset_unevenpg(const char *filename_prefix, const char *transform_name) {
        // Copied with only pg dim and offset  modification from ds2
	// Basic dataset information
	// NOTE: we have to use an anonymous enum here to define these constants, since
	// C is picky and doesn't consider a static const int "const enough" to use
	// as an array length (e.g., if these were static const ints, it would not compile)
	enum {
		NUM_DIMS = 2,
		NUM_TS = 2,
		NUM_PGS_PER_TS = 4,
		NUM_VARS = 1,
		NUM_PGS = NUM_TS * NUM_PGS_PER_TS,
	};

	// Variable names/types
	static const char *VARNAMES[NUM_VARS]					= { "temp"     };
	static const enum ADIOS_DATATYPES VARTYPES[NUM_VARS]	= { adios_real };

	// Global and PG dimensions/offsets
	static const uint64_t GLOBAL_DIMS                         [NUM_DIMS] = { 8, 8 };
	static const uint64_t PG_DIMS	  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = {
		{ { 2, 8 }, { 2, 8 }, { 2, 8 }, { 2, 8 }, }, // Timestep 1
		{ { 8, 2 }, { 8, 2 }, { 8, 2 }, { 8, 2 }, }, // Timestep 2
	};
	static const uint64_t PG_OFFSETS  [NUM_TS][NUM_PGS_PER_TS][NUM_DIMS] = {
		{ { 0, 0 }, { 2, 0 }, { 4, 0 }, { 6, 0 }, }, // Timestep 1
		{ { 0, 0 }, { 0, 2 }, { 0, 4 }, { 0, 6 }, }, // Timestep 2
	};

	static const void *VARBLOCKS_BY_VAR[NUM_VARS] = {
		TEMP_DATA_FOR_DS2_AND_DS3,
	};

	// Now, collect all this information into specification structs
	// File specification
	static const dataset_xml_spec_t XML_SPEC = {
		.group_name = "S3D",
		.buffer_size_mb = 128,
		.write_transport_method = "MPI",
		.ndim = NUM_DIMS,
		.nvar = NUM_VARS,
		.varnames = VARNAMES,
		.vartypes = VARTYPES,
	};

	// Global space specification
	static const dataset_global_spec_t GLOBAL_SPEC = {
		.num_ts = NUM_TS,
		.num_pgs_per_ts = NUM_PGS_PER_TS,
		.global_dims = GLOBAL_DIMS,
	};

	// Finally, invoke the dataset builder with this information
	build_dataset_from_varblocks_by_var(
			filename_prefix, transform_name, &XML_SPEC, &GLOBAL_SPEC,
			NUM_TS, NUM_PGS_PER_TS, NUM_DIMS, NUM_VARS,
			PG_DIMS, PG_OFFSETS, (const void **)VARBLOCKS_BY_VAR);
}






static void usage_and_exit() {
	int i;
	fprintf(stderr, "Usage: build_standard_dataset <dataset-id> <filename-prefix> [<transform-type>]\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  dataset-id: the ID of a standard dataset packaged in this executable:\n");
	fprintf(stderr, "    - 'DS1': 1 variable, 2D array of floats\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  filename-prefix: the filename prefix for the XML and BP files to be generated.\n");
	fprintf(stderr, "    - Example: filename-prefix = 'some/path/myfile':\n");
	fprintf(stderr, "            -> produces some/path/myfile.xml, some/path/myfile.bp:\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  transform-type: the data transform to apply (default: none). May include\n");
	fprintf(stderr, "                  transform parameters, just like in an ADIOS XML file.");
	fprintf(stderr, "\n");
	fprintf(stderr, "Available datasets:\n");
	for (i = 0; i < NUM_DATASETS; ++i)
		fprintf(stderr, "- \"%s\": %s\n", DATASETS[i].name, DATASETS[i].desc);
	exit(1);
}

int main(int argc, char **argv) {
	if (argc < 3 || argc > 4)
		usage_and_exit();

	const char *dataset_id = argv[1];
	const char *path = argv[2];
	const char *transform_name = (argc >= 4) ? argv[3] : "none";

	int i;
	const dataset_info_t *dataset = NULL;
	for (i = 0; i < NUM_DATASETS; ++i)
		if (strcasecmp(dataset_id, DATASETS[i].name) == 0)
			dataset = &DATASETS[i];

	if (dataset == NULL) {
		fprintf(stderr, "Error: '%s' does not name a dataset packaged in this executable\n");
		usage_and_exit();
	}

	MPI_Init(&argc, &argv);

	// Invoke the builder function for the selected dataset
	(*dataset->builder_fn)(path, transform_name);

	MPI_Finalize();
	return 0;
}
