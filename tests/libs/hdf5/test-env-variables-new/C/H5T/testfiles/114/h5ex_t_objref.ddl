HDF5 "h5ex_t_objref.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
         GROUP "h5ex_t_objref.h5/G1"
         DATASET "h5ex_t_objref.h5/DS2"
            DATA {
            }
      }
   }
   DATASET "DS2" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  NULL
      DATA {
      }
   }
   GROUP "G1" {
   }
}
}
