#-----------------------------------------------------------------------------
# Define Sources, one file per application
#-----------------------------------------------------------------------------
set (HDF_JAVA_EXAMPLES
    HDF5FileCreate.java
    HDF5GroupCreate.java
    HDF5DatasetCreate.java
    HDF5AttributeCreate.java
    HDF5DatasetRead.java
    HDF5SubsetSelect.java
)
if (NOT ${${EXAMPLE_VARNAME}_USE_110_API} AND ${H5_LIBVER_DIR} EQUAL 110)
  set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES}
      110/HDF5FileStructure.java
  )
else ()
  set (HDF_JAVA_EXAMPLES ${HDF_JAVA_EXAMPLES}
      HDF5FileStructure.java
  )
endif ()
