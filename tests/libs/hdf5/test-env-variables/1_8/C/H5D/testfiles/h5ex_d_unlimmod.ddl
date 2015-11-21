HDF5 "h5ex_d_unlimmod.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 6, 10 ) / ( H5S_UNLIMITED, H5S_UNLIMITED ) }
      DATA {
      (0,0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      (1,0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      (2,0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      (3,0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      (4,0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
      (5,0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
      }
   }
}
}
