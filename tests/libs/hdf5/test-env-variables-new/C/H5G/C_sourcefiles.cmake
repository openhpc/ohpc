#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
if (${H5_LIBVER_DIR} EQUAL 16)
  set (examples
      16/h5ex_g_create.c
      16/h5ex_g_iterate.c
      16/h5ex_g_traverse.c
  )
else ()
  set (examples
      h5ex_g_compact.c
      h5ex_g_corder.c
      h5ex_g_create.c
      h5ex_g_phase.c
      h5ex_g_iterate.c
      h5ex_g_traverse.c
      h5ex_g_intermediate.c
      h5ex_g_visit.c
  )
endif ()
