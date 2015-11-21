HDF5 "h5ex_t_opaque.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_OPAQUE {
         OPAQUE_TAG "Character array";
      }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): 4f:50:41:51:55:45:30, 4f:50:41:51:55:45:31, 4f:50:41:51:55:45:32,
      (3): 4f:50:41:51:55:45:33
      }
   }
}
}
