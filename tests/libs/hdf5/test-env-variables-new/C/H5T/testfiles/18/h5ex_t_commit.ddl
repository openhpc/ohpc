HDF5 "h5ex_t_commit.h5" {
GROUP "/" {
   DATATYPE "Sensor_Type" H5T_COMPOUND {
      H5T_STD_I64BE "Serial number";
      H5T_STRING {
         STRSIZE H5T_VARIABLE;
         STRPAD H5T_STR_NULLTERM;
         CSET H5T_CSET_ASCII;
         CTYPE H5T_C_S1;
      } "Location";
      H5T_IEEE_F64BE "Temperature (F)";
      H5T_IEEE_F64BE "Pressure (inHg)";
   }
}
}
