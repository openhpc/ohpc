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
 * Description: this example shows how to create HDF5 datasets using the
 * "HDF Native Package (Java)". The example created the group structure and
 * datasets:
 *
 * <pre>
 *     "/" (root)
 *         integer arrays
 *             2D 32-bit integer 20x10
 *             3D 16-bit integer 20x10x5
 *         float arrays
 *             2D 64-bit double 20x10
 *             3D 32-bit float  20x10x5
 * </pre>
 *
 * </p>
 */
public class HDF5DatasetCreate {
    private static String fname  = "HDF5DatasetCreate.h5";
    private static long[] dims2D = { 20, 10 };
    private static long[] dims3D = { 20, 10, 5 };

    public static void main(String args[]) throws Exception {
        long file_id = -1;
        long group_id1 = -1;
        long group_id2 = -1;
        long dataspace_id1 = -1;
        long dataspace_id2 = -1;
        long dataset_id = -1;

        // Create a new file using default properties.
        try {
            file_id = H5.H5Fcreate(fname, HDF5Constants.H5F_ACC_TRUNC,
                    HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
        }
        catch (Exception e) {
            e.printStackTrace();
            System.err.println("Failed to create file:" + fname);
            return;
        }

        // Create a group in the file.
        try {
            if (file_id >= 0) {
                group_id1 = H5.H5Gcreate(file_id, "g1",
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                group_id2 = H5.H5Gcreate(file_id, "g2",
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the  2D dataset.
        try {
            dataspace_id1 = H5.H5Screate_simple(2, dims2D, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Create the data space for the  3D dataset.
        try {
            dataspace_id2 = H5.H5Screate_simple(3, dims3D, null);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 32-bit (4 bytes) integer dataset of 20 by 10
        try {
            if ((group_id1 >= 0) && (dataspace_id1 >= 0)) {
                dataset_id = H5.H5Dcreate(group_id1, "2D 32-bit integer 20x10",
                        HDF5Constants.H5T_NATIVE_INT32, dataspace_id1,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                if (dataset_id >= 0)
                    H5.H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 8-bit (1 byte) unsigned integer dataset of 20 by 10 by 5
        try {
            if ((group_id1 >= 0) && (dataspace_id2 >= 0)) {
                dataset_id = H5.H5Dcreate(group_id1, "3D 8-bit unsigned integer 20x10x5",
                        HDF5Constants.H5T_NATIVE_INT8, dataspace_id2,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                if (dataset_id >= 0)
                    H5.H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 2D 64-bit (8 bytes) double dataset of 20 by 10
        try {
            if ((group_id2 >= 0) && (dataspace_id1 >= 0)) {
                dataset_id = H5.H5Dcreate(group_id2, "2D 64-bit double 20x10",
                        HDF5Constants.H5T_NATIVE_DOUBLE, dataspace_id1,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                if (dataset_id >= 0)
                    H5.H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // create 3D 32-bit (4 bytes) float dataset of 20 by 10 by 5
        try {
            if ((group_id2 >= 0) && (dataspace_id2 >= 0)) {
                dataset_id = H5.H5Dcreate(group_id2, "3D 32-bit float  20x10x5",
                        HDF5Constants.H5T_NATIVE_FLOAT, dataspace_id2,
                        HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT, HDF5Constants.H5P_DEFAULT);
                if (dataset_id >= 0)
                    H5.H5Dclose(dataset_id);
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Terminate access to the data space.
        try {
            if (dataspace_id2 >= 0)
                H5.H5Sclose(dataspace_id2);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (dataspace_id1 >= 0)
                H5.H5Sclose(dataspace_id1);
        }
        catch (Exception e) {
            e.printStackTrace();
        }

        // Close the groups.
        try {
            if (group_id2 >= 0)
                H5.H5Gclose(group_id2);
        }
        catch (Exception e) {
            e.printStackTrace();
        }
        try {
            if (group_id1 >= 0)
                H5.H5Gclose(group_id1);
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
