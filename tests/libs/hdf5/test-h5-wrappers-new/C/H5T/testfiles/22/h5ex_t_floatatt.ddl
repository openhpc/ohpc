HDF5 "h5ex_t_floatatt.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  NULL
      DATA {
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_IEEE_F64LE
         DATASPACE  SIMPLE { ( 4, 7 ) / ( 4, 7 ) }
         DATA {
         (0,0): 0, 1, 2, 3, 4, 5, 6,
         (1,0): 2, 1.66667, 2.4, 3.28571, 4.22222, 5.18182, 6.15385,
         (2,0): 4, 2.33333, 2.8, 3.57143, 4.44444, 5.36364, 6.30769,
         (3,0): 6, 3, 3.2, 3.85714, 4.66667, 5.54545, 6.46154
         }
      }
   }
}
}
