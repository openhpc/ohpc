!
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

!
! Fortran 90 API for writing ADIOS files
!
! Use this module in your source code to ensure that
! you are calling the adios_* writing functions with
! the correct arguments
!
module adios_write_mod

    use adios_defs_mod

    interface

        subroutine adios_init (config, comm, err)
            implicit none
            character(*),   intent(in)  :: config
            integer,        intent(in)  :: comm
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_init_noxml (comm, err)
            implicit none
            integer,        intent(in)  :: comm
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_finalize (mype, err)
            implicit none
            integer,        intent(in)  :: mype
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_open (fd, group_name, filename, mode, comm, err)
            implicit none
            integer*8,      intent(out) :: fd
            character(*),   intent(in)  :: group_name
            character(*),   intent(in)  :: filename
            character(*),   intent(in)  :: mode
            integer,        intent(in)  :: comm
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_group_size (fd, data_size, total_size, err)
            implicit none
            integer*8,      intent(out) :: fd
            integer*8,      intent(in)  :: data_size
            integer*8,      intent(in)  :: total_size
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_set_path (fd, path, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: path
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_set_path_var (fd, path, varname, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: path
            character(*),   intent(in)  :: varname
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_end_iteration (err)
            implicit none
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_start_calculation (err)
            implicit none
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_stop_calculation (err)
            implicit none
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_close (fd, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_timing_write_xml (fd, filename, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: filename
            integer,        intent(out) :: err
        end subroutine

        !
        ! No-XML calls
        !
        subroutine adios_declare_group (id, groupname, time_index, stats_flag, err)
            implicit none
            integer*8,      intent(out) :: id
            character(*),   intent(in)  :: groupname
            character(*),   intent(in)  :: time_index
            integer,        intent(in)  :: stats_flag
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_define_var (group_id, varname, path, vartype, dimensions, global_dimensions, local_offsets, id)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: varname
            character(*),   intent(in)  :: path
            integer,        intent(in)  :: vartype
            character(*),   intent(in)  :: dimensions
            character(*),   intent(in)  :: global_dimensions
            character(*),   intent(in)  :: local_offsets
            integer*8,      intent(out) :: id
        end subroutine

        subroutine adios_delete_vardefs (group_id, err)
            implicit none
            integer*8,      intent(in)  :: group_id
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_set_transform (var_id, transform_method, err)
            implicit none
            integer*8,      intent(in)  :: var_id
            character(*),   intent(in)  :: transform_method
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_define_attribute (group_id, attrname, path, attrtype, value, varname, err)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: attrname
            character(*),   intent(in)  :: path
            integer,        intent(in)  :: attrtype
            character(*),   intent(in)  :: value
            character(*),   intent(in)  :: varname
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_delete_attrdefs (group_id, err)
            implicit none
            integer*8,      intent(in)  :: group_id
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_select_method (group_id, method, parameters, base_path, err)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: method
            character(*),   intent(in)  :: parameters
            character(*),   intent(in)  :: base_path
            integer,        intent(out) :: err
        end subroutine


        subroutine adios_allocate_buffer (sizeMB, err)
            implicit none
            integer,        intent(in)  :: sizeMB
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_define_schema_version (group_id, schema_version)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: schema_version
        end subroutine
        
        subroutine adios_define_var_mesh (group_id, varname, meshname)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: varname
            character(*),   intent(in)  :: meshname
        end subroutine

        subroutine adios_define_var_centering (group_id, varname, centering)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: varname
            character(*),   intent(in)  :: centering
        end subroutine

        subroutine adios_define_mesh_timevarying (timevarying, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: name
            character(*),   intent(in)  :: timevarying
        end subroutine

        subroutine adios_define_mesh_file (group_id, name, file)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: name
            character(*),   intent(in)  :: file
        end subroutine

        subroutine adios_define_var_timesteps (timesteps, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: timesteps
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_var_timescale (timescale, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: timescale
            character(*),   intent(in)  :: name
        end subroutine
        
        subroutine adios_define_var_timeseriesformat (timeseries, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: timeseries
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_var_hyperslab (hyperslab, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: hyperslab
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_mesh_timesteps (timesteps, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: timesteps
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_mesh_timescale (timescale, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: timescale
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_mesh_timeseriesformat (timeseries, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: timeseries
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_mesh_group (group, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: group
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_mesh_uniform (dimensions, origin, spacing, maximum, nspace, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: dimensions
            character(*),   intent(in)  :: origin
            character(*),   intent(in)  :: spacing
            character(*),   intent(in)  :: maximum
            character(*),   intent(in)  :: nspace
            character(*),   intent(in)  :: name
        end subroutine
        
        subroutine adios_define_mesh_rectilinear (dimensions, coordinates, nspace, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: dimensions
            character(*),   intent(in)  :: coordinates
            character(*),   intent(in)  :: nspace
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_mesh_structured (dimensions, points, nspace, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: dimensions
            character(*),   intent(in)  :: points
            character(*),   intent(in)  :: nspace
            character(*),   intent(in)  :: name
        end subroutine

        subroutine adios_define_mesh_unstructured (points, data, count, cell_type, npoints, nspace, group_id, name)
            implicit none
            integer*8,      intent(in)  :: group_id
            character(*),   intent(in)  :: points
            character(*),   intent(in)  :: data
            character(*),   intent(in)  :: count
            character(*),   intent(in)  :: cell_type
            character(*),   intent(in)  :: npoints
            character(*),   intent(in)  :: nspace
            character(*),   intent(in)  :: name
        end subroutine

    end interface

    !
    !
    ! ADIOS_WRITE generic interface
    !
    ! Usage: call adios_write (fd, varname, data, err)
    !
    !
    interface adios_write
        module procedure adios_write_int1_d0
        module procedure adios_write_int2_d0
        module procedure adios_write_int4_d0
        module procedure adios_write_int8_d0
        module procedure adios_write_real4_d0
        module procedure adios_write_real8_d0
        module procedure adios_write_complex8_d0
        module procedure adios_write_complex16_d0
        !module procedure adios_write_char_d0
        module procedure adios_write_logical1_d0
        module procedure adios_write_logical2_d0
        module procedure adios_write_logical4_d0
        module procedure adios_write_logical8_d0
        module procedure adios_write_int1_d1
        module procedure adios_write_int2_d1
        module procedure adios_write_int4_d1
        module procedure adios_write_int8_d1
        module procedure adios_write_real4_d1
        module procedure adios_write_real8_d1
        module procedure adios_write_complex8_d1
        module procedure adios_write_complex16_d1
        module procedure adios_write_char_d1
        module procedure adios_write_logical1_d1
        module procedure adios_write_logical2_d1
        module procedure adios_write_logical4_d1
        module procedure adios_write_logical8_d1
        module procedure adios_write_int1_d2
        module procedure adios_write_int2_d2
        module procedure adios_write_int4_d2
        module procedure adios_write_int8_d2
        module procedure adios_write_real4_d2
        module procedure adios_write_real8_d2
        module procedure adios_write_complex8_d2
        module procedure adios_write_complex16_d2
        module procedure adios_write_char_d2
        module procedure adios_write_logical1_d2
        module procedure adios_write_logical2_d2
        module procedure adios_write_logical4_d2
        module procedure adios_write_logical8_d2
        module procedure adios_write_int1_d3
        module procedure adios_write_int2_d3
        module procedure adios_write_int4_d3
        module procedure adios_write_int8_d3
        module procedure adios_write_real4_d3
        module procedure adios_write_real8_d3
        module procedure adios_write_complex8_d3
        module procedure adios_write_complex16_d3
        module procedure adios_write_char_d3
        module procedure adios_write_logical1_d3
        module procedure adios_write_logical2_d3
        module procedure adios_write_logical4_d3
        module procedure adios_write_logical8_d3
        module procedure adios_write_int1_d4
        module procedure adios_write_int2_d4
        module procedure adios_write_int4_d4
        module procedure adios_write_int8_d4
        module procedure adios_write_real4_d4
        module procedure adios_write_real8_d4
        module procedure adios_write_complex8_d4
        module procedure adios_write_complex16_d4
        module procedure adios_write_char_d4
        module procedure adios_write_logical1_d4
        module procedure adios_write_logical2_d4
        module procedure adios_write_logical4_d4
        module procedure adios_write_logical8_d4
        module procedure adios_write_int1_d5
        module procedure adios_write_int2_d5
        module procedure adios_write_int4_d5
        module procedure adios_write_int8_d5
        module procedure adios_write_real4_d5
        module procedure adios_write_real8_d5
        module procedure adios_write_complex8_d5
        module procedure adios_write_complex16_d5
        module procedure adios_write_char_d5
        module procedure adios_write_logical1_d5
        module procedure adios_write_logical2_d5
        module procedure adios_write_logical4_d5
        module procedure adios_write_logical8_d5
        module procedure adios_write_int1_d6
        module procedure adios_write_int2_d6
        module procedure adios_write_int4_d6
        module procedure adios_write_int8_d6
        module procedure adios_write_real4_d6
        module procedure adios_write_real8_d6
        module procedure adios_write_complex8_d6
        module procedure adios_write_complex16_d6
        module procedure adios_write_char_d6
        module procedure adios_write_logical1_d6
        module procedure adios_write_logical2_d6
        module procedure adios_write_logical4_d6
        module procedure adios_write_logical8_d6
    end interface


    !
    !
    ! ADIOS_WRITE_BYID generic interface
    !
    ! Usage: call adios_write_byid (fd, varid, data, err)
    !
    !
    interface adios_write_byid
        module procedure adios_write_byid_int1_d0
        module procedure adios_write_byid_int2_d0
        module procedure adios_write_byid_int4_d0
        module procedure adios_write_byid_int8_d0
        module procedure adios_write_byid_real4_d0
        module procedure adios_write_byid_real8_d0
        module procedure adios_write_byid_complex8_d0
        module procedure adios_write_byid_complex16_d0
        !module procedure adios_write_byid_char_d0
        module procedure adios_write_byid_logical1_d0
        module procedure adios_write_byid_logical2_d0
        module procedure adios_write_byid_logical4_d0
        module procedure adios_write_byid_logical8_d0
        module procedure adios_write_byid_int1_d1
        module procedure adios_write_byid_int2_d1
        module procedure adios_write_byid_int4_d1
        module procedure adios_write_byid_int8_d1
        module procedure adios_write_byid_real4_d1
        module procedure adios_write_byid_real8_d1
        module procedure adios_write_byid_complex8_d1
        module procedure adios_write_byid_complex16_d1
        module procedure adios_write_byid_char_d1
        module procedure adios_write_byid_logical1_d1
        module procedure adios_write_byid_logical2_d1
        module procedure adios_write_byid_logical4_d1
        module procedure adios_write_byid_logical8_d1
        module procedure adios_write_byid_int1_d2
        module procedure adios_write_byid_int2_d2
        module procedure adios_write_byid_int4_d2
        module procedure adios_write_byid_int8_d2
        module procedure adios_write_byid_real4_d2
        module procedure adios_write_byid_real8_d2
        module procedure adios_write_byid_complex8_d2
        module procedure adios_write_byid_complex16_d2
        module procedure adios_write_byid_char_d2
        module procedure adios_write_byid_logical1_d2
        module procedure adios_write_byid_logical2_d2
        module procedure adios_write_byid_logical4_d2
        module procedure adios_write_byid_logical8_d2
        module procedure adios_write_byid_int1_d3
        module procedure adios_write_byid_int2_d3
        module procedure adios_write_byid_int4_d3
        module procedure adios_write_byid_int8_d3
        module procedure adios_write_byid_real4_d3
        module procedure adios_write_byid_real8_d3
        module procedure adios_write_byid_complex8_d3
        module procedure adios_write_byid_complex16_d3
        module procedure adios_write_byid_char_d3
        module procedure adios_write_byid_logical1_d3
        module procedure adios_write_byid_logical2_d3
        module procedure adios_write_byid_logical4_d3
        module procedure adios_write_byid_logical8_d3
        module procedure adios_write_byid_int1_d4
        module procedure adios_write_byid_int2_d4
        module procedure adios_write_byid_int4_d4
        module procedure adios_write_byid_int8_d4
        module procedure adios_write_byid_real4_d4
        module procedure adios_write_byid_real8_d4
        module procedure adios_write_byid_complex8_d4
        module procedure adios_write_byid_complex16_d4
        module procedure adios_write_byid_char_d4
        module procedure adios_write_byid_logical1_d4
        module procedure adios_write_byid_logical2_d4
        module procedure adios_write_byid_logical4_d4
        module procedure adios_write_byid_logical8_d4
        module procedure adios_write_byid_int1_d5
        module procedure adios_write_byid_int2_d5
        module procedure adios_write_byid_int4_d5
        module procedure adios_write_byid_int8_d5
        module procedure adios_write_byid_real4_d5
        module procedure adios_write_byid_real8_d5
        module procedure adios_write_byid_complex8_d5
        module procedure adios_write_byid_complex16_d5
        module procedure adios_write_byid_char_d5
        module procedure adios_write_byid_logical1_d5
        module procedure adios_write_byid_logical2_d5
        module procedure adios_write_byid_logical4_d5
        module procedure adios_write_byid_logical8_d5
        module procedure adios_write_byid_int1_d6
        module procedure adios_write_byid_int2_d6
        module procedure adios_write_byid_int4_d6
        module procedure adios_write_byid_int8_d6
        module procedure adios_write_byid_real4_d6
        module procedure adios_write_byid_real8_d6
        module procedure adios_write_byid_complex8_d6
        module procedure adios_write_byid_complex16_d6
        module procedure adios_write_byid_char_d6
        module procedure adios_write_byid_logical1_d6
        module procedure adios_write_byid_logical2_d6
        module procedure adios_write_byid_logical4_d6
        module procedure adios_write_byid_logical8_d6
    end interface


    !
    !
    ! ADIOS_READ generic interface
    !
    ! Usage: call adios_read
    !
    !
    interface adios_read
        module procedure adios_read_int1_d0
        module procedure adios_read_int2_d0
        module procedure adios_read_int4_d0
        module procedure adios_read_int8_d0
        module procedure adios_read_real4_d0
        module procedure adios_read_real8_d0
        module procedure adios_read_complex8_d0
        module procedure adios_read_complex16_d0
        !module procedure adios_read_char_d0
        module procedure adios_read_logical1_d0
        module procedure adios_read_logical2_d0
        module procedure adios_read_logical4_d0
        module procedure adios_read_logical8_d0
        module procedure adios_read_int1_d1
        module procedure adios_read_int2_d1
        module procedure adios_read_int4_d1
        module procedure adios_read_int8_d1
        module procedure adios_read_real4_d1
        module procedure adios_read_real8_d1
        module procedure adios_read_complex8_d1
        module procedure adios_read_complex16_d1
        module procedure adios_read_char_d1
        module procedure adios_read_logical1_d1
        module procedure adios_read_logical2_d1
        module procedure adios_read_logical4_d1
        module procedure adios_read_logical8_d1
        module procedure adios_read_int1_d2
        module procedure adios_read_int2_d2
        module procedure adios_read_int4_d2
        module procedure adios_read_int8_d2
        module procedure adios_read_real4_d2
        module procedure adios_read_real8_d2
        module procedure adios_read_complex8_d2
        module procedure adios_read_complex16_d2
        module procedure adios_read_char_d2
        module procedure adios_read_logical1_d2
        module procedure adios_read_logical2_d2
        module procedure adios_read_logical4_d2
        module procedure adios_read_logical8_d2
        module procedure adios_read_int1_d3
        module procedure adios_read_int2_d3
        module procedure adios_read_int4_d3
        module procedure adios_read_int8_d3
        module procedure adios_read_real4_d3
        module procedure adios_read_real8_d3
        module procedure adios_read_complex8_d3
        module procedure adios_read_complex16_d3
        module procedure adios_read_char_d3
        module procedure adios_read_logical1_d3
        module procedure adios_read_logical2_d3
        module procedure adios_read_logical4_d3
        module procedure adios_read_logical8_d3
        module procedure adios_read_int1_d4
        module procedure adios_read_int2_d4
        module procedure adios_read_int4_d4
        module procedure adios_read_int8_d4
        module procedure adios_read_real4_d4
        module procedure adios_read_real8_d4
        module procedure adios_read_complex8_d4
        module procedure adios_read_complex16_d4
        module procedure adios_read_char_d4
        module procedure adios_read_logical1_d4
        module procedure adios_read_logical2_d4
        module procedure adios_read_logical4_d4
        module procedure adios_read_logical8_d4
        module procedure adios_read_int1_d5
        module procedure adios_read_int2_d5
        module procedure adios_read_int4_d5
        module procedure adios_read_int8_d5
        module procedure adios_read_real4_d5
        module procedure adios_read_real8_d5
        module procedure adios_read_complex8_d5
        module procedure adios_read_complex16_d5
        module procedure adios_read_char_d5
        module procedure adios_read_logical1_d5
        module procedure adios_read_logical2_d5
        module procedure adios_read_logical4_d5
        module procedure adios_read_logical8_d5
        module procedure adios_read_int1_d6
        module procedure adios_read_int2_d6
        module procedure adios_read_int4_d6
        module procedure adios_read_int8_d6
        module procedure adios_read_real4_d6
        module procedure adios_read_real8_d6
        module procedure adios_read_complex8_d6
        module procedure adios_read_complex16_d6
        module procedure adios_read_char_d6
        module procedure adios_read_logical1_d6
        module procedure adios_read_logical2_d6
        module procedure adios_read_logical4_d6
        module procedure adios_read_logical8_d6
    end interface
    contains

    !
    !
    ! ADIOS_WRITE generic interface
    !
    ! Usage: call adios_write (fd, varname, data, err)
    !
    !
        !
    !
    ! ADIOS_WRITE generic interface
    !
    ! Usage: call adios_write (fd, varname, data, err)
    !
    !
        !
        ! scalars
        !

        ! INTEGER*1 scalar
        subroutine adios_write_int1_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*2 scalar
        subroutine adios_write_int2_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*4 scalar
        subroutine adios_write_int4_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4,      intent(in) :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*8 scalar
        subroutine adios_write_int8_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*4 scalar
        subroutine adios_write_real4_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,         intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*8 scalar
        subroutine adios_write_real8_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,         intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! COMPLEX (*8) scalar
        subroutine adios_write_complex8_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,        intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! DOUBLE-COMPLEX scalar
        subroutine adios_write_complex16_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,     intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! CHARACTER scalar (Same as 1D?)
        !subroutine adios_write_char_d0 (fd, varname, data, err)
        !    implicit none
        !    integer*8,      intent(in)  :: fd
        !    character(*),   intent(in)  :: varname
        !    character(*),   intent(inout) :: data
        !    integer,        intent(in)  :: err
        !
        !    call adios_write_f2c (fd, varname, data, err)
        !end subroutine

        ! LOGICAL*1 scalar
        subroutine adios_write_logical1_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*2 scalar
        subroutine adios_write_logical2_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*4 scalar
        subroutine adios_write_logical4_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*8 scalar
        subroutine adios_write_logical8_d0 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine


        !
        ! 1D data
        !

        ! INTEGER*1 array
        subroutine adios_write_int1_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_int2_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_int4_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_int8_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_real4_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_real8_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_complex8_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_complex16_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_char_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),   intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_logical1_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_logical2_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_logical4_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_logical8_d1 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        !
        ! 2D data
        !

        ! INTEGER*1 array
        subroutine adios_write_int1_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_int2_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_int4_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_int8_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_real4_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_real8_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_complex8_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_complex16_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_char_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_logical1_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_logical2_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_logical4_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_logical8_d2 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        !
        ! 3D data
        !

        ! INTEGER*1 array
        subroutine adios_write_int1_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_int2_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_int4_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_int8_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_real4_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_real8_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_complex8_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_complex16_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_char_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:),  intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_logical1_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_logical2_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_logical4_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_logical8_d3 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        !
        ! 4D data
        !

        ! INTEGER*1 array
        subroutine adios_write_int1_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_int2_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_int4_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_int8_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_real4_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_real8_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_complex8_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_complex16_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_char_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_logical1_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_logical2_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_logical4_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_logical8_d4 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        !
        ! 5D data
        !

        ! INTEGER*1 array
        subroutine adios_write_int1_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_int2_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_int4_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_int8_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_real4_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_real8_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_complex8_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_complex16_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_char_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:,:,:),intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_logical1_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_logical2_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_logical4_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_logical8_d5 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        !
        ! 6D data
        !

        ! INTEGER*1 array
        subroutine adios_write_int1_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_int2_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_int4_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_int8_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_real4_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_real8_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_complex8_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_complex16_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_char_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:,:,:,:),intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_logical1_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_logical2_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_logical4_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_logical8_d6 (fd, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_f2c (fd, varname, data, err)
        end subroutine

    ! end of ADIOS_WRITE functions


    !
    !
    ! ADIOS_WRITE_BYID generic interface
    !
    ! Usage: call adios_write_byid (fd, varid, data, err)
    !
    !
        !
        ! scalars
        !

        ! INTEGER*1 scalar
        subroutine adios_write_byid_int1_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*1,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*2 scalar
        subroutine adios_write_byid_int2_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*2,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*4 scalar
        subroutine adios_write_byid_int4_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*4,      intent(in) :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*8 scalar
        subroutine adios_write_byid_int8_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*8,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*4 scalar
        subroutine adios_write_byid_real4_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*4,         intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*8 scalar
        subroutine adios_write_byid_real8_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*8,         intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! COMPLEX (*8) scalar
        subroutine adios_write_byid_complex8_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex,        intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! DOUBLE-COMPLEX scalar
        subroutine adios_write_byid_complex16_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex*16,     intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! CHARACTER scalar (Same as 1D?)
        !subroutine adios_write_byid_char_d0 (fd, varid, data, err)
        !    implicit none
        !    integer*8,      intent(in)  :: fd
        !    integer*8,      intent(in)  :: varid
        !    character(*),   intent(inout) :: data
        !    integer,        intent(in)  :: err
        !
        !    call adios_write_byid_f2c (fd, varid, data, err)
        !end subroutine

        ! LOGICAL*1 scalar
        subroutine adios_write_byid_logical1_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*1,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*2 scalar
        subroutine adios_write_byid_logical2_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*2,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*4 scalar
        subroutine adios_write_byid_logical4_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*4,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*8 scalar
        subroutine adios_write_byid_logical8_d0 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*8,      intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine


        !
        ! 1D data
        !

        ! INTEGER*1 array
        subroutine adios_write_byid_int1_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*1, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_byid_int2_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*2, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_byid_int4_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*4, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_byid_int8_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*8, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_byid_real4_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*4,    dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_byid_real8_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*8,   dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_byid_complex8_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex,   dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_byid_complex16_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex*16,dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_byid_char_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            character(*),   intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_byid_logical1_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*1, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_byid_logical2_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*2, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_byid_logical4_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*4, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_byid_logical8_d1 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*8, dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        !
        ! 2D data
        !

        ! INTEGER*1 array
        subroutine adios_write_byid_int1_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*1, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_byid_int2_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*2, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_byid_int4_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*4, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_byid_int8_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*8, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_byid_real4_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*4,    dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_byid_real8_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*8,   dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_byid_complex8_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex,   dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_byid_complex16_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex*16,dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_byid_char_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            character(*),dimension(*), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_byid_logical1_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*1, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_byid_logical2_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*2, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_byid_logical4_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*4, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_byid_logical8_d2 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*8, dimension(:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        !
        ! 3D data
        !

        ! INTEGER*1 array
        subroutine adios_write_byid_int1_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*1, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_byid_int2_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*2, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_byid_int4_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*4, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_byid_int8_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*8, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_byid_real4_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*4,    dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_byid_real8_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*8,   dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_byid_complex8_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex,   dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_byid_complex16_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex*16,dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_byid_char_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            character(*),dimension(:,:),  intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_byid_logical1_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*1, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_byid_logical2_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*2, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_byid_logical4_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*4, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_byid_logical8_d3 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*8, dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        !
        ! 4D data
        !

        ! INTEGER*1 array
        subroutine adios_write_byid_int1_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*1, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_byid_int2_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*2, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_byid_int4_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*4, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_byid_int8_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*8, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_byid_real4_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*4,    dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_byid_real8_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*8,   dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_byid_complex8_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex,   dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_byid_complex16_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex*16,dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_byid_char_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            character(*),dimension(:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_byid_logical1_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*1, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_byid_logical2_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*2, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_byid_logical4_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*4, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_byid_logical8_d4 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*8, dimension(:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        !
        ! 5D data
        !

        ! INTEGER*1 array
        subroutine adios_write_byid_int1_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*1, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_byid_int2_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*2, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_byid_int4_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*4, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_byid_int8_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*8, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_byid_real4_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*4,    dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_byid_real8_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*8,   dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_byid_complex8_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex,   dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_byid_complex16_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex*16,dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_byid_char_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            character(*),dimension(:,:,:,:),intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_byid_logical1_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*1, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_byid_logical2_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*2, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_byid_logical4_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*4, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_byid_logical8_d5 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*8, dimension(:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        !
        ! 6D data
        !

        ! INTEGER*1 array
        subroutine adios_write_byid_int1_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*1, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_write_byid_int2_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*2, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_write_byid_int4_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*4, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_write_byid_int8_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            integer*8, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_write_byid_real4_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*4,    dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_write_byid_real8_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            real*8,   dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_write_byid_complex8_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex,   dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_write_byid_complex16_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            complex*16,dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_write_byid_char_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            character(*),dimension(:,:,:,:,:),intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_write_byid_logical1_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*1, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_write_byid_logical2_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*2, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_write_byid_logical4_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*4, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_write_byid_logical8_d6 (fd, varid, data, err)
            implicit none
            integer*8,      intent(in)  :: fd
            integer*8,      intent(in)  :: varid
            logical*8, dimension(:,:,:,:,:,:), intent(in)  :: data
            integer,        intent(in)  :: err

            call adios_write_byid_f2c (fd, varid, data, err)
        end subroutine

    ! end of ADIOS_WRITE functions




    !
    !
    ! ADIOS_READ generic interface
    !
    ! Usage: call adios_read (fd, varname, buffer, buffer_size, err)
    !
    !
        !
        ! scalar data
        !

        ! INTEGER*1 scalar
        subroutine adios_read_int1_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*2 scalar
        subroutine adios_read_int2_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*4 scalar
        subroutine adios_read_int4_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*8 scalar
        subroutine adios_read_int8_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*4 scalar
        subroutine adios_read_real4_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,         intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*8 scalar
        subroutine adios_read_real8_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,         intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! COMPLEX (*8) scalar
        subroutine adios_read_complex8_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,        intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! DOUBLE-COMPLEX scalar
        subroutine adios_read_complex16_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,     intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! CHARACTER scalar
        !subroutine adios_read_char_d0 (fd, varname, buffer, buffer_size, err)
        !    implicit none
        !    integer*8,      intent(in)  :: fd
        !    character(*),   intent(in)  :: varname
        !    character(*),   intent(out) :: buffer
        !    integer*8,      intent(in)  :: buffer_size
        !    integer,        intent(in)  :: err
        !
        !    call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        !end subroutine

        ! LOGICAL*1 scalar
        subroutine adios_read_logical1_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*2 scalar
        subroutine adios_read_logical2_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*4 scalar
        subroutine adios_read_logical4_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*8 scalar
        subroutine adios_read_logical8_d0 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8,      intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine


        !
        ! 1D data
        !

        ! INTEGER*1 array
        subroutine adios_read_int1_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_read_int2_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_read_int4_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_read_int8_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_read_real4_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_read_real8_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_read_complex8_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_read_complex16_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_read_char_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),   intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_read_logical1_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_read_logical2_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_read_logical4_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_read_logical8_d1 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        !
        ! 2D buffer
        !

        ! INTEGER*1 array
        subroutine adios_read_int1_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_read_int2_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_read_int4_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_read_int8_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_read_real4_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_read_real8_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_read_complex8_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_read_complex16_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_read_char_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(*), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_read_logical1_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_read_logical2_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_read_logical4_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_read_logical8_d2 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        !
        ! 3D buffer
        !

        ! INTEGER*1 array
        subroutine adios_read_int1_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_read_int2_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_read_int4_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_read_int8_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_read_real4_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_read_real8_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_read_complex8_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_read_complex16_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_read_char_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:),  intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_read_logical1_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_read_logical2_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_read_logical4_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_read_logical8_d3 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        !
        ! 4D buffer
        !

        ! INTEGER*1 array
        subroutine adios_read_int1_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_read_int2_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_read_int4_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_read_int8_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_read_real4_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_read_real8_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_read_complex8_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_read_complex16_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_read_char_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_read_logical1_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_read_logical2_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_read_logical4_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_read_logical8_d4 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        !
        ! 5D buffer
        !

        ! INTEGER*1 array
        subroutine adios_read_int1_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_read_int2_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_read_int4_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_read_int8_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_read_real4_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_read_real8_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_read_complex8_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_read_complex16_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_read_char_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:,:,:),intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_read_logical1_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_read_logical2_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_read_logical4_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_read_logical8_d5 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        !
        ! 6D buffer
        !

        ! INTEGER*1 array
        subroutine adios_read_int1_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*1, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_read_int2_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*2, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_read_int4_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*4, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_read_int8_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            integer*8, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_read_real4_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*4,    dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_read_real8_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            real*8,   dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_read_complex8_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex,   dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_read_complex16_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            complex*16,dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_read_char_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            character(*),dimension(:,:,:,:,:),intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_read_logical1_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*1, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_read_logical2_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*2, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_read_logical4_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*4, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_read_logical8_d6 (fd, varname, buffer, buffer_size, err)
            implicit none
            integer*8,      intent(in)  :: fd
            character(*),   intent(in)  :: varname
            logical*8, dimension(:,:,:,:,:,:), intent(out) :: buffer
            integer*8,      intent(in)  :: buffer_size
            integer,        intent(in)  :: err

            call adios_read_f2c (fd, varname, buffer, buffer_size, err)
        end subroutine

    ! end of ADIOS_READ functions

end module

