HDF5 "h5ex_t_arrayatt_F03.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  NULL
      DATA {
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_ARRAY { [5][3] H5T_STD_I64LE }
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): [ 0, 0, 0,
               0, 0, 1,
               2, 3, 0,
               2, 4, 6,
               0, 1, 2 ],
         (1): [ 3, -1, 1,
               3, 5, -2,
               1, 4, 7,
               0, 2, 4,
               6, -2, 1 ],
         (2): [ 4, 7, -4,
               0, 4, 8,
               0, 3, 6,
               9, -3, 1,
               5, 9, -6 ],
         (3): [ -1, 4, 9,
               0, 4, 8,
               12, -4, 1,
               6, 11, -8,
               -2, 4, 10 ]
         }
      }
   }
}
}
