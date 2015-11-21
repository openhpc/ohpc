/************************************************************

  This example shows how to read and write compound
  datatypes to a dataset.  The program first writes
  compound structures to a dataset with a dataspace of DIM0,
  then closes the file.  Next, it reopens the file, reads
  back the data, and outputs it to the screen.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define FILE            "h5ex_t_cmpd.h5"
#define DATASET         "DS1"
#define DIM0            4

typedef struct {
    int     serial_no;
    char    *location;
    double  temperature;
    double  pressure;
} sensor_t;                                 /* Compound type */

int
main (void)
{
    hid_t       file, filetype, memtype, strtype, space, dset;
                                            /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0};
    sensor_t    wdata[DIM0],                /* Write buffer */
                *rdata;                     /* Read buffer */
    int         ndims,
                i;

    /*
     * Initialize data.
     */
    wdata[0].serial_no = 1153;
    wdata[0].location = "Exterior (static)";
    wdata[0].temperature = 53.23;
    wdata[0].pressure = 24.57;
    wdata[1].serial_no = 1184;
    wdata[1].location = "Intake";
    wdata[1].temperature = 55.12;
    wdata[1].pressure = 22.95;
    wdata[2].serial_no = 1027;
    wdata[2].location = "Intake manifold";
    wdata[2].temperature = 103.55;
    wdata[2].pressure = 31.23;
    wdata[3].serial_no = 1313;
    wdata[3].location = "Exhaust manifold";
    wdata[3].temperature = 1252.89;
    wdata[3].pressure = 84.11;

    /*
     * Create a new file using the default properties.
     */
    file = H5Fcreate (FILE, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create variable-length string datatype.
     */
    strtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (strtype, H5T_VARIABLE);

    /*
     * Create the compound datatype for memory.
     */
    memtype = H5Tcreate (H5T_COMPOUND, sizeof (sensor_t));
    status = H5Tinsert (memtype, "Serial number",
                HOFFSET (sensor_t, serial_no), H5T_NATIVE_INT);
    status = H5Tinsert (memtype, "Location", HOFFSET (sensor_t, location),
                strtype);
    status = H5Tinsert (memtype, "Temperature (F)",
                HOFFSET (sensor_t, temperature), H5T_NATIVE_DOUBLE);
    status = H5Tinsert (memtype, "Pressure (inHg)",
                HOFFSET (sensor_t, pressure), H5T_NATIVE_DOUBLE);

    /*
     * Create the compound datatype for the file.  Because the standard
     * types we are using for the file may have different sizes than
     * the corresponding native types, we must manually calculate the
     * offset of each member.
     */
    filetype = H5Tcreate (H5T_COMPOUND, 8 + sizeof (hvl_t) + 8 + 8);
    status = H5Tinsert (filetype, "Serial number", 0, H5T_STD_I64BE);
    status = H5Tinsert (filetype, "Location", 8, strtype);
    status = H5Tinsert (filetype, "Temperature (F)", 8 + sizeof (hvl_t),
                H5T_IEEE_F64BE);
    status = H5Tinsert (filetype, "Pressure (inHg)", 8 + sizeof (hvl_t) + 8,
                H5T_IEEE_F64BE);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Create the dataset and write the compound data to it.
     */
    dset = H5Dcreate (file, DATASET, filetype, space, H5P_DEFAULT);
    status = H5Dwrite (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, wdata);

    /*
     * Close and release resources.
     */
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (filetype);
    status = H5Fclose (file);


    /*
     * Now we begin the read section of this example.  Here we assume
     * the dataset has the same name and rank, but can have any size.
     * Therefore we must allocate a new array to read in data using
     * malloc().  For simplicity, we do not rebuild memtype.
     */

    /*
     * Open file and dataset.
     */
    file = H5Fopen (FILE, H5F_ACC_RDONLY, H5P_DEFAULT);
    dset = H5Dopen (file, DATASET);

    /*
     * Get dataspace and allocate memory for read buffer.
     */
    space = H5Dget_space (dset);
    ndims = H5Sget_simple_extent_dims (space, dims, NULL);
    rdata = (sensor_t *) malloc (dims[0] * sizeof (sensor_t));

    /*
     * Read the data.
     */
    status = H5Dread (dset, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<dims[0]; i++) {
        printf ("%s[%d]:\n", DATASET, i);
        printf ("Serial number   : %d\n", rdata[i].serial_no);
        printf ("Location        : %s\n", rdata[i].location);
        printf ("Temperature (F) : %f\n", rdata[i].temperature);
        printf ("Pressure (inHg) : %f\n\n", rdata[i].pressure);
    }

    /*
     * Close and release resources.  H5Dvlen_reclaim will automatically
     * traverse the structure and free any vlen data (strings in this
     * case).
     */
    status = H5Dvlen_reclaim (memtype, space, H5P_DEFAULT, rdata);
    free (rdata);
    status = H5Dclose (dset);
    status = H5Sclose (space);
    status = H5Tclose (memtype);
    status = H5Tclose (strtype);
    status = H5Fclose (file);

    return 0;
}
