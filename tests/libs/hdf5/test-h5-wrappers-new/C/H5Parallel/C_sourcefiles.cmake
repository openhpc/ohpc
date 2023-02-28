#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (examples
  ph5example.c
  ph5_filtered_writes.c
  ph5_filtered_writes_no_sel.c
)
if (${HDF5_ENABLE_SUBFILING_VFD})
    list (APPEND examples ph5_subfiling.c)
endif ()
