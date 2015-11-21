HDF5 "h5ex_t_objref.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_REFERENCE { H5T_STD_REF_OBJECT }
      DATASPACE  SIMPLE { ( 2 ) / ( 2 ) }
      DATA {
      (0): GROUP 1400 /G1 , DATASET 800 /DS2 
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
