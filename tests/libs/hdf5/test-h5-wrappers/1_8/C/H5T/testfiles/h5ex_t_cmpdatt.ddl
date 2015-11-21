HDF5 "h5ex_t_cmpdatt.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  NULL
      DATA {
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_COMPOUND {
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
         DATASPACE  SIMPLE { ( 4 ) / ( 4 ) }
         DATA {
         (0): {
               1153,
               "Exterior (static)",
               53.23,
               24.57
            },
         (1): {
               1184,
               "Intake",
               55.12,
               22.95
            },
         (2): {
               1027,
               "Intake manifold",
               103.55,
               31.23
            },
         (3): {
               1313,
               "Exhaust manifold",
               1252.89,
               84.11
            }
         }
      }
   }
}
}
