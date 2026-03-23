HDF5 "h5ex_d_compact.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 4, 7 ) / ( 4, 7 ) }
      DATA {
      (0,0): 0, -1, -2, -3, -4, -5, -6,
      (1,0): 0, 0, 0, 0, 0, 0, 0,
      (2,0): 0, 1, 2, 3, 4, 5, 6,
      (3,0): 0, 2, 4, 6, 8, 10, 12
      }
   }
}
}
