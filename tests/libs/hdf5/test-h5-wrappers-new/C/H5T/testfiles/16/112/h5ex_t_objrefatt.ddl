HDF5 "h5ex_t_objrefatt.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SCALAR
      DATA {
      (0): 0
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
         DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
         DATA {
            GROUP "h5ex_t_objrefatt.h5/G1"
            DATASET "h5ex_t_objrefatt.h5/DS2"
               DATA {
               (0): 0
               }
         }
      }
   }
   DATASET "DS2" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SCALAR
      DATA {
      (0): 0
      }
   }
   GROUP "G1" {
   }
}
}
