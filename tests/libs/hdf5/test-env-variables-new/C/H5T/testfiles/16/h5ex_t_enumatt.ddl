HDF5 "h5ex_t_enumatt.h5" {
GROUP "/" {
   DATASET "DS1" {
      DATATYPE  H5T_STD_I32LE
      DATASPACE  SCALAR
      DATA {
      (0): 0
      }
      ATTRIBUTE "A1" {
         DATATYPE  H5T_ENUM {
            H5T_STD_I16BE;
            "SOLID"            0;
            "LIQUID"           1;
            "GAS"              2;
            "PLASMA"           3;
         }
         DATASPACE  SIMPLE { ( 4, 7 ) / ( 4, 7 ) }
         DATA {
         (0,0): SOLID, SOLID, SOLID, SOLID, SOLID, SOLID, SOLID,
         (1,0): SOLID, LIQUID, GAS, PLASMA, SOLID, LIQUID, GAS,
         (2,0): SOLID, GAS, SOLID, GAS, SOLID, GAS, SOLID,
         (3,0): SOLID, PLASMA, GAS, LIQUID, SOLID, PLASMA, GAS
         }
      }
   }
}
}
