HDF5 "h5ex_t_string.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STRING {
         STRSIZE 7;
         STRPAD H5T_STR_SPACEPAD;
         CSET H5T_CSET_ASCII;
         CTYPE H5T_C_S1;
      }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): "Parting", "is such", "sweet  ", "sorrow."
      }
   }
}
}
