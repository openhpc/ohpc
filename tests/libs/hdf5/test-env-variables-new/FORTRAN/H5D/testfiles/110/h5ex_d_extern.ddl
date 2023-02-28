HDF5 "h5ex_d_extern.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SIMPLE { ( 7, 4 ) / ( 7, 4 ) }
      DATA {
      (0,0): 0, 0, 0, 0,
      (1,0): -1, 0, 1, 2,
      (2,0): -2, 0, 2, 4,
      (3,0): -3, 0, 3, 6,
      (4,0): -4, 0, 4, 8,
      (5,0): -5, 0, 5, 10,
      (6,0): -6, 0, 6, 12
      }
   }
}
}
