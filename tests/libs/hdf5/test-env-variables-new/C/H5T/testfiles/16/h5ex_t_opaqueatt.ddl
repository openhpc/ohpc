HDF5 "h5ex_t_opaqueatt.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SCALAR
      DATA {
      (0): 0
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_OPAQUE {
            OPAQUE_TAG "Character array";
         }
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): 4f:50:41:51:55:45:30, 4f:50:41:51:55:45:31,
         (2): 4f:50:41:51:55:45:32, 4f:50:41:51:55:45:33
         }
      }
   }
}
}
