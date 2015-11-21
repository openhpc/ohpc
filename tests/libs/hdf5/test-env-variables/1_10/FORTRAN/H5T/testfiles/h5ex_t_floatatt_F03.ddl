HDF5 "h5ex_t_floatatt_F03.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  NULL
      DATA {
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SIMPLE { ( 7, 4 ) / ( 7, 4 ) }
         DATA {
         (0,0): 0, 2, 4, 6,
         (1,0): 1, 1.66667, 2.33333, 3,
         (2,0): 2, 2.4, 2.8, 3.2,
         (3,0): 3, 3.28571, 3.57143, 3.85714,
         (4,0): 4, 4.22222, 4.44444, 4.66667,
         (5,0): 5, 5.18182, 5.36364, 5.54545,
         (6,0): 6, 6.15385, 6.30769, 6.46154
         }
      }
   }
}
}
