/************************************************************

  This example shows how to read and write a complex
  compound datatype to an attribute.  The program first
  writes complex compound structures to an attribute with a
  dataspace of DIM0, then closes the file.  Next, it reopens
  the file, reads back selected fields in the structure, and
  outputs them to the screen.

  Unlike the other datatype examples, in this example we
  save to the file using native datatypes to simplify the
  type definitions here.  To save using standard types you
  must manually calculate the sizes and offsets of compound
  types as shown in h5ex_t_cmpd.c, and convert enumerated
  values as shown in h5ex_t_enum.c.

  The datatype defined here consists of a compound
  containing a variable-length list of compound types, as
  well as a variable-length string, enumeration, double
  array, object reference and region reference.  The nested
  compound type contains an int, variable-length string and
  two doubles.

  This file is intended for use with HDF5 Library version 1.8

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_cpxcmpdatt.h5"
#define DATASET         "DS1"
#define ATTRIBUTE       "A1"
#define DIM0            2

typedef struct {
    int     serial_no;
    char    *location;
    double  temperature;
    double  pressure;
} sensor_t;                                 /* Nested compound type */

typedef enum {
    RED,
    GREEN,
    BLUE
} color_t  ;                                /* Enumerated type */

typedef struct {
    hvl_t               sensors;
    char                *name;
    color_t             color;
    double              location[3];
    hobj_ref_t          group;
    hdset_reg_ref_t     surveyed_areas;
} vehicle_t;                                /* Main compound type */

typedef struct {
    hvl_t       sensors;
    char        *name;
} rvehicle_t;                               /* Read type */

int
main (void)
{
    hid_t       file, vehicletype, colortype, sensortype, sensorstype, loctype,
                strtype, rvehicletype, rsensortype, rsensorstype, space, dset,
                group, attr;
                                            /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0},
                adims[1] = {3},
                adims2[2] = {32, 32},
                start[2] = {8, 26},
                count[2] = {4, 3},
                coords[3][2] = { {3, 2},
                                 {3, 3},
                                 {4, 4} };
    vehicle_t   wdata[2];                   /* Write buffer */
    rvehicle_t  *rdata;                     /* Read buffer */
    color_t     val;
    sensor_t    *ptr;
    double      wdata2[32][32];
    int         ndims,
                i, j;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create dataset to use for region references.
     */
    for (i=0; i<32; i++)
        for (j=0; j<32; j++)
            wdata2[i][j]= 70. + 0.1 * (i - 16.) + 0.1 * (j - 16.);
    space = H5Screate_simple (2, adims2, NULL);
    dset = H5Dcreate (file, "Ambient_Temperature", H5T_NATIVE_DOUBLE, space,
                H5P_DEFAULT);
    status = H5Dwrite (dset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                wdata2[0]);
    status = H5Dclose (dset);

    /*
     * Create groups to use for object references.
     */
    group = H5Gcreate (file, "Land_Vehicles", H5P_DEFAULT);
    status = H5Gclose (group);
    group = H5Gcreate (file, "Air_Vehicles", H5P_DEFAULT);
    status = H5Gclose (group);

    /*
     * Initialize variable-length compound in the first data element.
     */
    wdata[0].sensors.len = 4;
    ptr = (sensor_t *) malloc (wdata[0].sensors.len * sizeof (sensor_t));
    ptr[0].serial_no = 1153;
    ptr[0].location = "Exterior (static)";
    ptr[0].temperature = 53.23;
    ptr[0].pressure = 24.57;
    ptr[1].serial_no = 1184;
    ptr[1].location = "Intake";
    ptr[1].temperature = 55.12;
    ptr[1].pressure = 22.95;
    ptr[2].serial_no = 1027;
    ptr[2].location = "Intake manifold";
    ptr[2].temperature = 103.55;
    ptr[2].pressure = 31.23;
    ptr[3].serial_no = 1313;
    ptr[3].location = "Exhaust manifold";
    ptr[3].temperature = 1252.89;
    ptr[3].pressure = 84.11;
    wdata[0].sensors.p = (void *) ptr;

    /*
     * Initialize other fields in the first data element.
     */
    wdata[0].name = "Airplane";
    wdata[0].color = GREEN;
    wdata[0].location[0] = -103234.21;
    wdata[0].location[1] = 422638.78;
    wdata[0].location[2] = 5996.43;
    status = H5Rcreate (&wdata[0].group, file, "Air_Vehicles", H5R_OBJECT, -1);
    status = H5Sselect_elements (space, H5S_SELECT_SET, 3, coords[0]);
    status = H5Rcreate (&wdata[0].surveyed_areas, file, "Ambient_Temperature",
                H5R_DATASET_REGION, space);

    /*
     * Initialize variable-length compound in the second data element.
     */
    wdata[1].sensors.len = 1;
    ptr = (sensor_t *) malloc (wdata[1].sensors.len * sizeof (sensor_t));
    ptr[0].serial_no = 3244;
    ptr[0].location = "Roof";
    ptr[0].temperature = 83.82;
    ptr[0].pressure = 29.92;
    wdata[1].sensors.p = (void *) ptr;

    /*
     * Initialize other fields in the second data element.
     */
    wdata[1].name = "Automobile";
    wdata[1].color = RED;
    wdata[1].location[0] = 326734.36;
    wdata[1].location[1] = 221568.23;
    wdata[1].location[2] = 432.36;
    status = H5Rcreate (&wdata[1].group, file, "Land_Vehicles", H5R_OBJECT, -1);
    status = H5Sselect_hyperslab (space, H5S_SELECT_SET, start, NULL, count,
                NULL);
    status = H5Rcreate (&wdata[1].surveyed_areas, file, "Ambient_Temperature",
                H5R_DATASET_REGION, space);

    status = H5Sclose (space);

    /*
     * Create variable-length string datatype.
     */
    strtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (strtype, H5T_VARIABLE);

    /*
     * Create the nested compound datatype.
     */
    sensortype = H5Tcreate (H5T_COMPOUND, sizeof (sensor_t));
    status = H5Tinsert (sensortype, "Serial number",
                HOFFSET (sensor_t, serial_no), H5T_NATIVE_INT);
    status = H5Tinsert (sensortype, "Location", HOFFSET (sensor_t, location),
                strtype);
    status = H5Tinsert (sensortype, "Temperature (F)",
                HOFFSET (sensor_t, temperature), H5T_NATIVE_DOUBLE);
    status = H5Tinsert (sensortype, "Pressure (inHg)",
                HOFFSET (sensor_t, pressure), H5T_NATIVE_DOUBLE);

    /*
     * Create the variable-length datatype.
     */
    sensorstype = H5Tvlen_create (sensortype);

    /*
     * Create the enumerated datatype.
     */
    colortype = H5Tenum_create (H5T_NATIVE_INT);
    val = (color_t) RED;
    status = H5Tenum_insert (colortype, "Red", &val);
    val = (color_t) GREEN;
    status = H5Tenum_insert (colortype, "Green", &val);
    val = (color_t) BLUE;
    status = H5Tenum_insert (colortype, "Blue", &val);

    /*
     * Create the array datatype.
     */
    loctype = H5Tarray_create (H5T_NATIVE_DOUBLE, 1, adims, NULL);

    /*
     * Create the main compound datatype.
     */
    vehicletype = H5Tcreate (H5T_COMPOUND, sizeof (vehicle_t));
    status = H5Tinsert (vehicletype, "Sensors", HOFFSET (vehicle_t, sensors),
                sensorstype);
    status = H5Tinsert (vehicletype, "Name", HOFFSET (vehicle_t, name),
                strtype);
    status = H5Tinsert (vehicletype, "Color", HOFFSET (vehicle_t, color),
                colortype);
    status = H5Tinsert (vehicletype, "Location", HOFFSET (vehicle_t, location),
                loctype);
    status = H5Tinsert (vehicletype, "Group", HOFFSET (vehicle_t, group),
                H5T_STD_REF_OBJ);
    status = H5Tinsert (vehicletype, "Surveyed areas",
                HOFFSET (vehicle_t, surveyed_areas), H5T_STD_REF_DSETREG);

    /*
     * Create dataset with a scalar dataspace. to serve as the parent
     * for the attribute.
     */
    space = H5Screate (H5S_SCALAR);
    dset = H5Dcreate (file, DATASET, H5T_STD_I32LE, space, H5P_DEFAULT);
    status = H5Sclose (space);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the attribute and write the compound data to it.
     */
    attr = H5Acreate (dset, ATTRIBUTE, vehicletype, space, H5P_DEFAULT);
    status = H5Awrite (attr, vehicletype, wdata);

    /*
     * Close and release resources.  Note that we cannot use
     * H5Dvlen_reclaim as it would attempt to free() the string
     * constants used to initialize the name fields in wdata.  We must
     * therefore manually free() only the data previously allocated
     * through malloc().
     */
    for (i=0; i<dims[0]; i++)
        free (wdata[i].sensors.p);
    status = H5Aclose (attr);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (strtype);
    status = H5Tclose (sensortype);
    status = H5Tclose (sensorstype);
    status = H5Tclose (colortype);
    status = H5Tclose (loctype);
    status = H5Tclose (vehicletype);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.  Here we assume
     * the attribute has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().  We will only read back the variable length strings.
     */

    /*
     * Open file, dataset, and attribute.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);
    attr = H5Aopen_name (dset, ATTRIBUTE);

    /*
     * Create variable-length string datatype.
     */
    strtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (strtype, H5T_VARIABLE);

    /*
     * Create the nested compound datatype for reading.  Even though it
     * has only one field, it must still be defined as a compound type
     * so the library can match the correct field in the file type.
     * This matching is done by name.  However, we do not need to
     * define a structure for the read buffer as we can simply treat it
     * as a char *.
     */
    rsensortype = H5Tcreate (H5T_COMPOUND, sizeof (char *));
    status = H5Tinsert (rsensortype, "Location", 0, strtype);

    /*
     * Create the variable-length datatype for reading.
     */
    rsensorstype = H5Tvlen_create (rsensortype);

    /*
     * Create the main compound datatype for reading.
     */
    rvehicletype = H5Tcreate (H5T_COMPOUND, sizeof (rvehicle_t));
    status = H5Tinsert (rvehicletype, "Sensors", HOFFSET (rvehicle_t, sensors),
                rsensorstype);
    status = H5Tinsert (rvehicletype, "Name", HOFFSET (rvehicle_t, name),
                strtype);

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Aget_space (attr);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    rdata = (rvehicle_t *) malloc (dims[0] * sizeof (rvehicle_t));

    /*
     * Read the data.
     */
    status = H5Aread (attr, rvehicletype, rdata);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<dims[0]; i++) {
        printf ("%s[%d]:\n", ATTRIBUTE, i);
        printf ("   Vehicle name :\n      %s\n", rdata[i].name);
        printf ("   Sensor locations :\n");
        for (j=0; j<rdata[i].sensors.len; j++)
            printf ("      %s\n", ( (char **) rdata[i].sensors.p )[j] );
    }

    /*
     * Close and release resources.  H5Dvlen_reclaim will automatically
     * traverse the structure and free any vlen data (including
     * strings).
     */
    status = H5Dvlen_reclaim (rvehicletype, space, H5P_DEFAULT, rdata);
    free (rdata);
    status = H5Aclose (attr);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (strtype);
    status = H5Tclose (rsensortype);
    status = H5Tclose (rsensorstype);
    status = H5Tclose (rvehicletype);
    status = H5Fclose (file);

    return 0;
}
