#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
if (${H5_LIBVER_DIR} EQUAL 16)
  set (16DIR "16/")
else ()
  set (16DIR "")
endif ()
set (examples
    ${16DIR}h5ex_t_array.c
    ${16DIR}h5ex_t_arrayatt.c
    ${16DIR}h5ex_t_bit.c
    ${16DIR}h5ex_t_bitatt.c
    ${16DIR}h5ex_t_cmpd.c
    ${16DIR}h5ex_t_cmpdatt.c
    ${16DIR}h5ex_t_enum.c
    ${16DIR}h5ex_t_enumatt.c
    ${16DIR}h5ex_t_float.c
    ${16DIR}h5ex_t_floatatt.c
    ${16DIR}h5ex_t_int.c
    ${16DIR}h5ex_t_intatt.c
    ${16DIR}h5ex_t_objref.c
    ${16DIR}h5ex_t_objrefatt.c
    ${16DIR}h5ex_t_opaque.c
    ${16DIR}h5ex_t_opaqueatt.c
    ${16DIR}h5ex_t_regref.c
    ${16DIR}h5ex_t_regrefatt.c
    ${16DIR}h5ex_t_string.c
    ${16DIR}h5ex_t_stringatt.c
    ${16DIR}h5ex_t_vlen.c
    ${16DIR}h5ex_t_vlenatt.c
    ${16DIR}h5ex_t_vlstring.c
    ${16DIR}h5ex_t_vlstringatt.c
    ${16DIR}h5ex_t_cpxcmpd.c
    ${16DIR}h5ex_t_cpxcmpdatt.c
    ${16DIR}h5ex_t_commit.c
    ${16DIR}h5ex_t_convert.c
)
