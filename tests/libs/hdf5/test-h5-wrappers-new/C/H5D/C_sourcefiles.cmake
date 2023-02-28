#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
if (${H5_LIBVER_DIR} EQUAL 16)
  set (examples
      16/h5ex_d_alloc.c
      16/h5ex_d_checksum.c
      16/h5ex_d_chunk.c
      16/h5ex_d_compact.c
      16/h5ex_d_extern.c
      16/h5ex_d_fillval.c
      16/h5ex_d_hyper.c
      16/h5ex_d_rdwr.c
      16/h5ex_d_unlimadd.c
      16/h5ex_d_unlimmod.c
  )
else ()
  set (examples
      h5ex_d_alloc.c
      h5ex_d_checksum.c
      h5ex_d_chunk.c
      h5ex_d_compact.c
      h5ex_d_extern.c
      h5ex_d_fillval.c
      h5ex_d_hyper.c
      h5ex_d_nbit.c
      h5ex_d_rdwr.c
      h5ex_d_sofloat.c
      h5ex_d_soint.c
      h5ex_d_transform.c
      h5ex_d_unlimadd.c
      h5ex_d_unlimmod.c
  )
endif ()

if (HDF5_ENABLE_Z_LIB_SUPPORT)
  if (${H5_LIBVER_DIR} EQUAL 16)
    set (examples ${examples}
        16/h5ex_d_gzip.c
        16/h5ex_d_shuffle.c
        16/h5ex_d_unlimgzip.c
    )
  else ()
    set (examples ${examples}
        h5ex_d_gzip.c
        h5ex_d_shuffle.c
        h5ex_d_unlimgzip.c
    )
  endif ()
endif ()
if (HDF5_ENABLE_SZIP_SUPPORT)
  if (${H5_LIBVER_DIR} EQUAL 16)
    set (examples ${examples} 16/h5ex_d_szip.c)
  else ()
    set (examples ${examples} h5ex_d_szip.c)
  endif ()
endif ()
