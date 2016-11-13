adios_groupsize = 4 \
                + 4 \
                + 8 * (NX) * (NY) \
                + 4 * (NX);
adios_group_size (adios_handle, adios_groupsize, &adios_totalsize);
adios_write (adios_handle, "NX", &NX);
adios_write (adios_handle, "NY", &NY);
adios_write (adios_handle, "var_double_2Darray", t);
adios_write (adios_handle, "var_int_1Darray", p);
