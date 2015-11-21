HDF5 "h5ex_t_vlstring.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STRING {
         STRSIZE H5T_VARIABLE;
         STRPAD H5T_STR_NULLPAD;
         CSET H5T_CSET_ASCII;
         CTYPE H5T_C_S1;
      }
      DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
      DATA {
      (0): "Parting", "is such", "sweet", "sorrow."
      }
   }
}
}
