HDF5 "h5ex_t_enumatt_F03.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  NULL
      DATA {
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_ENUM {
            H5T_STD_I16BE;
            "SOLID"            0;
            "LIQUID"           1;
            "GAS"              2;
            "PLASMA"           3;
         }
         DATASPACE  SIMPLE { ( 7, 4 ) / ( 7, 4 ) }
         DATA {
         (0,0): SOLID, SOLID, SOLID, SOLID,
         (1,0): SOLID, LIQUID, GAS, PLASMA,
         (2,0): SOLID, GAS, SOLID, GAS,
         (3,0): SOLID, PLASMA, GAS, LIQUID,
         (4,0): SOLID, SOLID, SOLID, SOLID,
         (5,0): SOLID, LIQUID, GAS, PLASMA,
         (6,0): SOLID, GAS, SOLID, GAS
         }
      }
   }
}
}
