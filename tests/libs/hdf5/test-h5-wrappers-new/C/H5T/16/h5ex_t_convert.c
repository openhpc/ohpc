/************************************************************

  This example shows how to convert between different
  datatypes in memory.  The program converts DIM0 elements
  of compound type sourcetype to desttype, then outputs the
  converted data to the screen.  A background buffer is used
  to fill in the elements of desttype that are not in
  sourcetype.

  This file is intended for use with HDF5 Library version 1.6

 ************************************************************/

#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>

#define DIM0            4

typedef struct {
    double  temperature;
    double  pressure;
} reading_t;                                /* Source type */

typedef struct {
    int     serial_no;
    char    *location;
    double  temperature;
    double  pressure;
} sensor_t;                                 /* Destination type */

int
main (void)
{
    hid_t       sourcetype, desttype, strtype, space;
                                            /* Handles */
    herr_t      status;
    hsize_t     dims[1] = {DIM0};
    reading_t   *reading;                   /* Conversion buffer */
    sensor_t    *sensor,                    /* Conversion buffer */
                bkgrd[DIM0];                /* Background buffer */
    int         i;

    /*
     * Allocate memory for conversion buffer.  We will allocate space
     * for it to hold DIM0 elements of the destination type, as the
     * type conversion is performed in place.  Of course, if the
     * destination type were smaller than the source type, we would
     * allocate space to hold DIM0 elements of the source type.
     */
    reading = (reading_t *) malloc (DIM0 * sizeof (sensor_t));

    /*
     * Assign the allocated space to a pointer of the destination type,
     * to allow the buffer to be accessed correctly after the
     * conversion has taken place.
     */
    sensor = (sensor_t *) reading;

    /*
     * Initialize data.
     */
    bkgrd[0].serial_no = 1153;
    bkgrd[0].location = "Exterior (static)";
    bkgrd[0].temperature = 53.23;
    bkgrd[0].pressure = 24.57;
    bkgrd[1].serial_no = 1184;
    bkgrd[1].location = "Intake";
    bkgrd[1].temperature = 55.12;
    bkgrd[1].pressure = 22.95;
    bkgrd[2].serial_no = 1027;
    bkgrd[2].location = "Intake manifold";
    bkgrd[2].temperature = 103.55;
    bkgrd[2].pressure = 31.23;
    bkgrd[3].serial_no = 1313;
    bkgrd[3].location = "Exhaust manifold";
    bkgrd[3].temperature = 1252.89;
    bkgrd[3].pressure = 84.11;

    reading[0].temperature = 54.84;
    reading[0].pressure = 24.76;
    reading[1].temperature = 56.63;
    reading[1].pressure = 23.10;
    reading[2].temperature = 102.69;
    reading[2].pressure = 30.97;
    reading[3].temperature = 1238.27;
    reading[3].pressure = 82.15;

    /*
     * Create variable-length string datatype.
     */
    strtype = H5Tcopy (H5T_C_S1);
    status = H5Tset_size (strtype, H5T_VARIABLE);

    /*
     * Create the compound datatype for memory.
     */
    sourcetype = H5Tcreate (H5T_COMPOUND, sizeof (reading_t));
    status = H5Tinsert (sourcetype, "Temperature (F)",
                HOFFSET (reading_t, temperature), H5T_NATIVE_DOUBLE);
    status = H5Tinsert (sourcetype, "Pressure (inHg)",
                HOFFSET (reading_t, pressure), H5T_NATIVE_DOUBLE);

    desttype = H5Tcreate (H5T_COMPOUND, sizeof (sensor_t));
    status = H5Tinsert (desttype, "Serial number",
                HOFFSET (sensor_t, serial_no), H5T_NATIVE_INT);
    status = H5Tinsert (desttype, "Location", HOFFSET (sensor_t, location),
                strtype);
    status = H5Tinsert (desttype, "Temperature (F)",
                HOFFSET (sensor_t, temperature), H5T_NATIVE_DOUBLE);
    status = H5Tinsert (desttype, "Pressure (inHg)",
                HOFFSET (sensor_t, pressure), H5T_NATIVE_DOUBLE);

    /*
     * Create dataspace.  Setting maximum size to NULL sets the maximum
     * size to be the current size.
     */
    space = H5Screate_simple (1, dims, NULL);

    /*
     * Convert the buffer in reading from sourcetype to desttype.
     * After this conversion we will use sensor to access the buffer,
     * as the buffer now matches its type.
     */
    status = H5Tconvert (sourcetype, desttype, DIM0, reading, bkgrd,
                H5P_DEFAULT);

    /*
     * Output the data to the screen.
     */
    for (i=0; i<DIM0; i++) {
        printf ("sensor[%d]:\n", i);
        printf ("Serial number   : %d\n", sensor[i].serial_no);
        printf ("Location        : %s\n", sensor[i].location);
        printf ("Temperature (F) : %f\n", sensor[i].temperature);
        printf ("Pressure (inHg) : %f\n\n", sensor[i].pressure);
    }

    /*
     * Close and release resources.  In this case H5Tconvert preserves
     * the memory locations of the variable-length strings in
     * "location", so we do not need to free those strings as they were
     * initialized as string constants.
     */
    free (sensor);
    status = H5Sclose (space);
    status = H5Tclose (sourcetype);
    status = H5Tclose (desttype);
    status = H5Tclose (strtype);

    return 0;
}
