/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

import hdf.hdf5lib.H5;
import hdf.hdf5lib.HDF5Constants;

/**
 * <p>
 * Title: HDF Native Package (Java) Example
 * </p>
 * <p>
 * Description: this example shows how to select a subset using the
 * "HDF Native Package (Java)". The example creates an integer dataset, and read
 * subset of the dataset:
 *
 * <pre>
 *     "/" (root)
 *             2D 32-bit integer 20x10
 * </pre>
 *
 * The whole 20x10 data set is
 *
 * <pre>
 * 1000, 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009
 * 1100, 1101, 1102, 1103, 1104, 1105, 1106, 1107, 1108, 1109
 * 1200, 1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208, 1209
 * 1300, 1301, 1302, 1303, 1304, 1305, 1306, 1307, 1308, 1309
 * 1400, 1401, 1402, 1403, 1404, 1405, 1406, 1407, 1408, 1409
 * 1500, 1501, 1502, 1503, 1504, 1505, 1506, 1507, 1508, 1509
 * 1600, 1601, 1602, 1603, 1604, 1605, 1606, 1607, 1608, 1609
 * 1700, 1701, 1702, 1703, 1704, 1705, 1706, 1707, 1708, 1709
 * 1800, 1801, 1802, 1803, 1804, 1805, 1806, 1807, 1808, 1809
 * 1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909
 * 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009
 * 2100, 2101, 2102, 2103, 2104, 2105, 2106, 2107, 2108, 2109
 * 2200, 2201, 2202, 2203, 2204, 2205, 2206, 2207, 2208, 2209
 * 2300, 2301, 2302, 2303, 2304, 2305, 2306, 2307, 2308, 2309
 * 2400, 2401, 2402, 2403, 2404, 2405, 2406, 2407, 2408, 2409
 * 2500, 2501, 2502, 2503, 2504, 2505, 2506, 2507, 2508, 2509
 * 2600, 2601, 2602, 2603, 2604, 2605, 2606, 2607, 2608, 2609
 * 2700, 2701, 2702, 2703, 2704, 2705, 2706, 2707, 2708, 2709
 * 2800, 2801, 2802, 2803, 2804, 2805, 2806, 2807, 2808, 2809
 * 2900, 2901, 2902, 2903, 2904, 2905, 2906, 2907, 2908, 2909
 * </pre>
 *
 * Subset: start=(4, 2), size=(5, 3) and stride=(3, 2). The subset values are:
 *
 * <pre>
 * 1402,1404,1406
 * 1702,1704,1706
 * 2002,2004,2006
 * 2302,2304,2306
 * 2602,2604,2606
 * </pre>
 *
 * </p>
 *
 * @author Peter X. Cao
 * @version 2.4
 */
public class HDF5SubsetSelect {
    private static String fname  = "HDF5SubsetSelect.h5";
    private static String dsname  = "2D 32-bit integer 20x10";
    private static long[] dims2D = { 20, 10 };

    public static void main(String args[]) throws Exception {
        long file_id = -1;
        long dataset_id = -1;
        long filespace_id = -1;
        long memspace_id = -1;

        // create the file and add groups and dataset into the file
        createFile();

        // Open file using the default properties.
        try {
            file_id = H5.H5Fopen(fname, HDF5Constants.H5F_ACC_RDWR, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Open dataset using the default properties.
        try {
            if (file_id >= 0)
                dataset_id = H5.H5Dopen(file_id, dsname, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Allocate array of pointers to two-dimensional arrays (the
        // elements of the dataset.
        int[][] dataRead = new int[5][3];

        // Define and select the hyperslab to use for reading.
        try {
            if (dataset_id >= 0) {
                filespace_id = H5.H5Dget_space(dataset_id);

                long[] start = { 4, 2 };
                long[] stride = { 3, 2 };
                long[] count = { 5, 3 };
                long[] block = null;

                if (filespace_id >= 0) {
                    H5.H5Sselect_hyperslab(filespace_id, HDF5Constants.H5S_SELECT_SET,
                            start, stride, count, block);

                    memspace_id = H5.H5Screate_simple(2, count, null);
                    // Read the data using the previously defined hyperslab.
                    if ((dataset_id >= 0) && (filespace_id >= 0) && (memspace_id >= 0))
                        H5.H5Dread(dataset_id, HDF5Constants.H5T_NATIVE_INT,
                                memspace_id, filespace_id, HDF5Constants.H5P_DEFAULT,
                                dataRead);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // print out the data values
        System.out.println("\n\nSubset Data Values");
        for (int i = 0; i < 5; i++) {
            System.out.print("\n" + dataRead[i][0]);
            for (int j = 1; j < 3; j++) {
                System.out.print("," + dataRead[i][j]);
            }
        }

        // Close the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        try {
            if (filespace_id >= 0)
                H5.H5Sclose(filespace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * create the file and add groups ans dataset into the file, which is the
     * same as javaExample.H5DatasetCreate
     *
     * @see javaExample.HDF5DatasetCreate
     * @throws Exception
     */
    private static void createFile() throws Exception {
        long file_id = -1;
        long dataspace_id = -1;
        long dataset_id = -1;

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(fname, HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the dataset.
        try {
            dataspace_id = H5.H5Screate_simple(2, dims2D, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the dataset.
        try {
            if ((file_id >= 0) && (dataspace_id >= 0))
                dataset_id = H5.H5Dcreate(file_id, dsname,
                        HDF5Constants.H5T_STD_I32LE, dataspace_id,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the data space.
        try {
            if (dataspace_id >= 0)
                H5.H5Sclose(dataspace_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // set the data values
        int[] dataIn = new int[20 * 10];
        for (int i = 0; i < 20; i++) {
            for (int j = 0; j < 10; j++) {
                dataIn[i * 10 + j] = 1000 + i * 100 + j;
            }
        }

        // Write the data to the dataset.
        try {
            if (dataset_id >= 0)
                H5.H5Dwrite(dataset_id, HDF5Constants.H5T_NATIVE_INT,
                        HDF5Constants.H5S_ALL, HDF5Constants.H5S_ALL,
                        HDF5Constants.H5P_DEFAULT, dataIn);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // End access to the dataset and release resources used by it.
        try {
            if (dataset_id >= 0)
                H5.H5Dclose(dataset_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the file.
        try {
            if (file_id >= 0)
                H5.H5Fclose(file_id);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

}
