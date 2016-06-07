!
!  ADIOS is freely available under the terms of the BSD license described
!  in the COPYING file in the top level directory of this source distribution.
!
!  Copyright (c) 2008 - 2009.  UT-BATTELLE, LLC. All rights reserved.
!

!
! Read Fortran 90 API for ADIOS BP format files
!
! Use this module in your source code to ensure that
! you are calling the adios_* reading functions with
! the correct arguments
!
module adios_read_mod

    use adios_defs_mod

    interface

        subroutine adios_errmsg (msg)
            implicit none
            character(*),   intent(out) :: msg
        end subroutine

        subroutine adios_read_init_method (method, comm, parameters, err)
            implicit none
            integer,        intent(in)  :: method
            integer,        intent(in)  :: comm
            character(*),   intent(in)  :: parameters
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_read_finalize_method (method, err)
            implicit none
            integer,        intent(in)  :: method
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_read_open (fp, fname, method, comm, lockmode, timeout_sec, err)
            implicit none
            integer*8,      intent(out) :: fp
            character(*),   intent(in)  :: fname
            integer,        intent(in)  :: method
            integer,        intent(in)  :: comm
            integer,        intent(in)  :: lockmode
            real,           intent(in)  :: timeout_sec
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_read_open_file (fp, fname, method, comm, err)
            implicit none
            integer*8,      intent(out) :: fp
            character(*),   intent(in)  :: fname
            integer,        intent(in)  :: method
            integer,        intent(in)  :: comm
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_advance_step (fp, last, timeout_sec, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(in)  :: last
            real,           intent(in)  :: timeout_sec
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_release_step (fp, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_reset_dimension_order (fp, flag)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(in)  :: flag
        end subroutine

        subroutine adios_read_close (fp, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_inq_ngroups (fp, groups_count, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(out) :: groups_count
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_inq_groupnames (fp, gnamelist, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*), dimension(*), intent(inout) :: gnamelist
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_group_view (fp, groupid, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(in)  :: groupid
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_inq_file (fp, vars_count, attrs_count, current_step, last_step, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(out) :: vars_count
            integer,        intent(out) :: attrs_count
            integer,        intent(out) :: current_step
            integer,        intent(out) :: last_step
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_inq_varnames (fp, vnamelist, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*), dimension(*), intent(inout) :: vnamelist
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_inq_attrnames (fp, anamelist, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*), dimension(*), intent(inout) :: anamelist
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_inq_var (fp, varname, vartype, nsteps, ndim, dims, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer,        intent(out) :: vartype
            integer,        intent(out) :: nsteps
            integer,        intent(out) :: ndim
            integer*8, dimension(*), intent(out) :: dims
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_inq_attr (fp, attrname, attrtype, attrsize, err)
            implicit none
            integer*8,      intent(in) :: fp
            character(*),   intent(in)  :: attrname
            integer,        intent(out) :: attrtype
            integer,        intent(out) :: attrsize
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_perform_reads (fp, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer,        intent(out) :: err
        end subroutine

        subroutine adios_selection_boundingbox (sel, ndim, start, count)
            implicit none
            integer*8,      intent(out)          :: sel
            integer,        intent(in)           :: ndim
            integer*8, dimension(*), intent(in)  :: start
            integer*8, dimension(*), intent(in)  :: count
        end subroutine

        subroutine adios_selection_points (sel, ndim, npoints, points)
            implicit none
            integer*8,      intent(out)          :: sel
            integer,        intent(in)           :: ndim
            integer*8,      intent(in)           :: npoints
            integer*8, dimension(*), intent(in)  :: points
        end subroutine

        subroutine adios_selection_writeblock (sel, index)
            implicit none
            integer*8,      intent(out)          :: sel
            integer,        intent(in)           :: index
        end subroutine

        subroutine adios_selection_auto (sel, hints)
            implicit none
            integer*8,      intent(out)          :: sel
            character(*),   intent(in)           :: hints
        end subroutine

        subroutine adios_selection_delete (sel)
            implicit none
            integer*8,      intent(in)           :: sel
        end subroutine

    end interface

    !
    ! ADIOS_GET_SCALAR generic interface
    !
    ! Usage: call adios_get_scalar (gp, varname, data, err)
    !
    interface adios_get_scalar

        ! INTEGER*1 scalar
        subroutine adios_get_scalar_int1 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*1,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*2 scalar
        subroutine adios_get_scalar_int2 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*2,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*4 scalar
        subroutine adios_get_scalar_int4 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*4,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*8 scalar
        subroutine adios_get_scalar_int8 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*8,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! REAL*4 scalar
        subroutine adios_get_scalar_real4 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            real*4,         intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! REAL*8 scalar
        subroutine adios_get_scalar_real8 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            real*8,         intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! COMPLEX*8 scalar
        subroutine adios_get_scalar_complex8 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            complex*8,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! COMPLEX*16 scalar
        subroutine adios_get_scalar_complex16 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            complex*16,     intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! CHARACTER(*) string
        subroutine adios_get_scalar_char (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            character(*),   intent(inout) :: data
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*1 scalar
        subroutine adios_get_scalar_logical1 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*1,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*2 scalar
        subroutine adios_get_scalar_logical2 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*2,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*4 scalar
        subroutine adios_get_scalar_logical4 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*4,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*8 scalar
        subroutine adios_get_scalar_logical8 (fp, varname, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*8,      intent(out) :: data
            integer,        intent(out) :: err
        end subroutine

    end interface

    !
    ! ADIOS_GET_ATTR generic interface
    !
    ! Usage: call adios_get_attr (fp, varname, attr, err)
    !
    interface adios_get_attr

        ! INTEGER*1
        subroutine adios_get_attr_int1 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            integer*1,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*2
        subroutine adios_get_attr_int2 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            integer*2,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*4
        subroutine adios_get_attr_int4 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            integer*4,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*8
        subroutine adios_get_attr_int8 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            integer*8,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! REAL*4
        subroutine adios_get_attr_real4 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            real*4,         intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! REAL*8
        subroutine adios_get_attr_real8 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            real*8,         intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! COMPLEX*8
        subroutine adios_get_attr_complex8 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            complex,        intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! COMPLEX*16
        subroutine adios_get_attr_complex16 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            complex*16,     intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! CHARACTER(*)
        subroutine adios_get_attr_char (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            character(*),   intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*1
        subroutine adios_get_attr_logical1 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            logical*1,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*2
        subroutine adios_get_attr_logical2 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            logical*2,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*4
        subroutine adios_get_attr_logical4 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            logical*4,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*8
        subroutine adios_get_attr_logical8 (fp, attrname, attr, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: attrname
            logical*8,      intent(out) :: attr
            integer,        intent(out) :: err
        end subroutine

    end interface

    !
    ! ADIOS_SCHEDULE_READ generic interface
    !
    ! Usage: call adios_schedule_read (fp, sel, varname, from_step, nsteps,  data, err)
    !
    interface adios_schedule_read
        module procedure adios_schedule_read_int1_d0
        module procedure adios_schedule_read_int2_d0
        module procedure adios_schedule_read_int4_d0
        module procedure adios_schedule_read_int8_d0
        module procedure adios_schedule_read_real4_d0
        module procedure adios_schedule_read_real8_d0
        module procedure adios_schedule_read_complex8_d0
        module procedure adios_schedule_read_complex16_d0
        !module procedure adios_schedule_read_char_d0
        module procedure adios_schedule_read_logical1_d0
        module procedure adios_schedule_read_logical2_d0
        module procedure adios_schedule_read_logical4_d0
        module procedure adios_schedule_read_logical8_d0
        module procedure adios_schedule_read_int1_d1
        module procedure adios_schedule_read_int2_d1
        module procedure adios_schedule_read_int4_d1
        module procedure adios_schedule_read_int8_d1
        module procedure adios_schedule_read_real4_d1
        module procedure adios_schedule_read_real8_d1
        module procedure adios_schedule_read_complex8_d1
        module procedure adios_schedule_read_complex16_d1
        module procedure adios_schedule_read_char_d1
        module procedure adios_schedule_read_logical1_d1
        module procedure adios_schedule_read_logical2_d1
        module procedure adios_schedule_read_logical4_d1
        module procedure adios_schedule_read_logical8_d1
        module procedure adios_schedule_read_int1_d2
        module procedure adios_schedule_read_int2_d2
        module procedure adios_schedule_read_int4_d2
        module procedure adios_schedule_read_int8_d2
        module procedure adios_schedule_read_real4_d2
        module procedure adios_schedule_read_real8_d2
        module procedure adios_schedule_read_complex8_d2
        module procedure adios_schedule_read_complex16_d2
        module procedure adios_schedule_read_char_d2
        module procedure adios_schedule_read_logical1_d2
        module procedure adios_schedule_read_logical2_d2
        module procedure adios_schedule_read_logical4_d2
        module procedure adios_schedule_read_logical8_d2
        module procedure adios_schedule_read_int1_d3
        module procedure adios_schedule_read_int2_d3
        module procedure adios_schedule_read_int4_d3
        module procedure adios_schedule_read_int8_d3
        module procedure adios_schedule_read_real4_d3
        module procedure adios_schedule_read_real8_d3
        module procedure adios_schedule_read_complex8_d3
        module procedure adios_schedule_read_complex16_d3
        module procedure adios_schedule_read_char_d3
        module procedure adios_schedule_read_logical1_d3
        module procedure adios_schedule_read_logical2_d3
        module procedure adios_schedule_read_logical4_d3
        module procedure adios_schedule_read_logical8_d3
        module procedure adios_schedule_read_int1_d4
        module procedure adios_schedule_read_int2_d4
        module procedure adios_schedule_read_int4_d4
        module procedure adios_schedule_read_int8_d4
        module procedure adios_schedule_read_real4_d4
        module procedure adios_schedule_read_real8_d4
        module procedure adios_schedule_read_complex8_d4
        module procedure adios_schedule_read_complex16_d4
        module procedure adios_schedule_read_char_d4
        module procedure adios_schedule_read_logical1_d4
        module procedure adios_schedule_read_logical2_d4
        module procedure adios_schedule_read_logical4_d4
        module procedure adios_schedule_read_logical8_d4
        module procedure adios_schedule_read_int1_d5
        module procedure adios_schedule_read_int2_d5
        module procedure adios_schedule_read_int4_d5
        module procedure adios_schedule_read_int8_d5
        module procedure adios_schedule_read_real4_d5
        module procedure adios_schedule_read_real8_d5
        module procedure adios_schedule_read_complex8_d5
        module procedure adios_schedule_read_complex16_d5
        module procedure adios_schedule_read_char_d5
        module procedure adios_schedule_read_logical1_d5
        module procedure adios_schedule_read_logical2_d5
        module procedure adios_schedule_read_logical4_d5
        module procedure adios_schedule_read_logical8_d5
        module procedure adios_schedule_read_int1_d6
        module procedure adios_schedule_read_int2_d6
        module procedure adios_schedule_read_int4_d6
        module procedure adios_schedule_read_int8_d6
        module procedure adios_schedule_read_real4_d6
        module procedure adios_schedule_read_real8_d6
        module procedure adios_schedule_read_complex8_d6
        module procedure adios_schedule_read_complex16_d6
        module procedure adios_schedule_read_char_d6
        module procedure adios_schedule_read_logical1_d6
        module procedure adios_schedule_read_logical2_d6
        module procedure adios_schedule_read_logical4_d6
        module procedure adios_schedule_read_logical8_d6

    end interface

    !
    ! adios_GET_STATISTICS generic interface
    !
    ! Usage: call adios_get_statistics (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
    !
    interface adios_get_statistics

        ! INTEGER*1 arrays
        subroutine adios_get_statistics_int1 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*1,      intent(out) :: value
            integer*1,      intent(out) :: gmin
            integer*1,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            integer*1, dimension(*), intent(out) :: mins
            integer*1, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,dimension(*), intent(out) :: err
        end subroutine

        ! INTEGER*2 arrays
        subroutine adios_get_statistics_int2 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*2,      intent(out) :: value
            integer*2,      intent(out) :: gmin
            integer*2,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            integer*2, dimension(*), intent(out) :: mins
            integer*2, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*4 arrays
        subroutine adios_get_statistics_int4 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*4,      intent(out) :: value
            integer*4,      intent(out) :: gmin
            integer*4,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            integer*4, dimension(*), intent(out) :: mins
            integer*1, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! INTEGER*8 arrays
        subroutine adios_get_statistics_int8 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            integer*8,      intent(out) :: value
            integer*8,      intent(out) :: gmin
            integer*8,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            integer*8, dimension(*), intent(out) :: mins
            integer*8, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! REAL*4 arrays
        subroutine adios_get_statistics_real4 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            real*4,         intent(out) :: value
            real*4,         intent(out) :: gmin
            real*4,         intent(out) :: gmax
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            real*4, dimension(*), intent(out) :: mins
            real*4, dimension(*), intent(out) :: maxs
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            integer,        intent(out) :: err
        end subroutine

        ! REAL*8 arrays
        subroutine adios_get_statistics_real8 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            real*8,         intent(out) :: value
            real*8,         intent(out) :: gmin
            real*8,         intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            real*8, dimension(*), intent(out) :: mins
            real*8, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! COMPLEX*8 arrays
        subroutine adios_get_statistics_complex8 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            complex,        intent(out) :: value
            complex,        intent(out) :: gmin
            complex,        intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            complex, dimension(*), intent(out) :: mins
            complex, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! COMPLEX*16 arrays
        subroutine adios_get_statistics_complex16 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            complex*16,     intent(out) :: value
            complex*16,     intent(out) :: gmin
            complex*16,     intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            complex*16, dimension(*), intent(out) :: mins
            complex*16, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! CHARACTER(*) arrays
        subroutine adios_get_statistics_char (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            character(*),   intent(out) :: value
            character(*),   intent(out) :: gmin
            character(*),   intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            character(*), dimension(*), intent(out) :: mins
            character(*), dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! LOFICAL*1 arrays
        subroutine adios_get_statistics_logical1 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*1,      intent(out) :: value
            logical*1,      intent(out) :: gmin
            logical*1,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            logical*1, dimension(*), intent(out) :: mins
            logical*1, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*2 arrays
        subroutine adios_get_statistics_logical2 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*2,      intent(out) :: value
            logical*2,      intent(out) :: gmin
            logical*2,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            logical*2, dimension(*), intent(out) :: mins
            logical*2, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*4 arrays
        subroutine adios_get_statistics_logical4 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*4,      intent(out) :: value
            logical*4,      intent(out) :: gmin
            logical*4,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            logical*4, dimension(*), intent(out) :: mins
            logical*4, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

        ! LOGICAL*8 arrays
        subroutine adios_get_statistics_logical8 (fp, varname, value, gmin, gmax, gavg, gstd_dev, mins, maxs, avgs, std_devs, err)
            implicit none
            integer*8,      intent(in)  :: fp
            character(*),   intent(in)  :: varname
            logical*8,      intent(out) :: value
            logical*8,      intent(out) :: gmin
            logical*8,      intent(out) :: gmax
            real*8,         intent(out) :: gavg
            real*8,         intent(out) :: gstd_dev
            logical*8, dimension(*), intent(out) :: mins
            logical*8, dimension(*), intent(out) :: maxs
            real*8, dimension(*), intent(out) :: avgs
            real*8, dimension(*), intent(out) :: std_devs
            integer,        intent(out) :: err
        end subroutine

    end interface


    contains

    !
    ! ADIOS_SCHEDULE_READ procedures
    !
    ! Usage: call adios_schedule_read (fp, sel, varname, from_step, nsteps,  data, err)
    !
        !
        ! scalars
        !

        ! INTEGER*1 scalar
        subroutine adios_schedule_read_int1_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*1,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*2 scalar
        subroutine adios_schedule_read_int2_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*2,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*4 scalar
        subroutine adios_schedule_read_int4_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*4,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*8 scalar
        subroutine adios_schedule_read_int8_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*8,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*4 scalar
        subroutine adios_schedule_read_real4_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*4,         intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*8 scalar
        subroutine adios_schedule_read_real8_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*8,        intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! COMPLEX (*8) scalar
        subroutine adios_schedule_read_complex8_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex,        intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! DOUBLE-COMPLEX scalar
        subroutine adios_schedule_read_complex16_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex*16,     intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! CHARACTER scalar
        !subroutine adios_schedule_read_char_d0 (fp, sel, varname, from_step, nsteps, data, err)
        !    implicit none
        !    integer*8,      intent(in)  :: fp
        !    integer*8,      intent(in)  :: sel
        !    character(*),   intent(in)  :: varname
        !    integer,        intent(in)  :: from_step
        !    integer,        intent(in)  :: nsteps
        !    character(*),   intent(out) :: data
        !    integer,        intent(in)  :: err
        !
        !    call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        !end subroutine

        ! LOGICAL*1 scalar
        subroutine adios_schedule_read_logical1_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*1,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*2 scalar
        subroutine adios_schedule_read_logical2_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*2,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*4 scalar
        subroutine adios_schedule_read_logical4_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*4,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*8 scalar
        subroutine adios_schedule_read_logical8_d0 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*8,      intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        !
        ! 1D data
        !

        ! INTEGER*1 array
        subroutine adios_schedule_read_int1_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*1, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_schedule_read_int2_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*2, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_schedule_read_int4_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*4, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_schedule_read_int8_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*8, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_schedule_read_real4_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*4,    dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_schedule_read_real8_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*8,   dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_schedule_read_complex8_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex,   dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_schedule_read_complex16_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex*16,dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_schedule_read_char_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            character(*),   intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_schedule_read_logical1_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*1, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_schedule_read_logical2_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*2, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_schedule_read_logical4_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*4, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_schedule_read_logical8_d1 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*8, dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        !
        ! 2D data
        !

        ! INTEGER*1 array
        subroutine adios_schedule_read_int1_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*1, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_schedule_read_int2_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*2, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_schedule_read_int4_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*4, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_schedule_read_int8_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*8, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_schedule_read_real4_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*4,    dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_schedule_read_real8_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*8,   dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_schedule_read_complex8_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex,   dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_schedule_read_complex16_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex*16,dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_schedule_read_char_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            character(*),dimension(*), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_schedule_read_logical1_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*1, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_schedule_read_logical2_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*2, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_schedule_read_logical4_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*4, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_schedule_read_logical8_d2 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*8, dimension(:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        !
        ! 3D data
        !

        ! INTEGER*1 array
        subroutine adios_schedule_read_int1_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*1, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_schedule_read_int2_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*2, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_schedule_read_int4_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*4, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_schedule_read_int8_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*8, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_schedule_read_real4_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*4,    dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_schedule_read_real8_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*8,   dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_schedule_read_complex8_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex,   dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_schedule_read_complex16_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex*16,dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_schedule_read_char_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            character(*),dimension(:,:),  intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_schedule_read_logical1_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*1, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_schedule_read_logical2_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*2, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_schedule_read_logical4_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*4, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_schedule_read_logical8_d3 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*8, dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        !
        ! 4D data
        !

        ! INTEGER*1 array
        subroutine adios_schedule_read_int1_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*1, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_schedule_read_int2_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*2, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_schedule_read_int4_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*4, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_schedule_read_int8_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*8, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_schedule_read_real4_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*4,    dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_schedule_read_real8_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*8,   dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_schedule_read_complex8_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex,   dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_schedule_read_complex16_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex*16,dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_schedule_read_char_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            character(*),dimension(:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_schedule_read_logical1_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*1, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_schedule_read_logical2_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*2, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_schedule_read_logical4_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*4, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_schedule_read_logical8_d4 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*8, dimension(:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        !
        ! 5D data
        !

        ! INTEGER*1 array
        subroutine adios_schedule_read_int1_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*1, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_schedule_read_int2_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*2, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_schedule_read_int4_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*4, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_schedule_read_int8_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*8, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_schedule_read_real4_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*4,    dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_schedule_read_real8_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*8,   dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_schedule_read_complex8_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex,   dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_schedule_read_complex16_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex*16,dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_schedule_read_char_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            character(*),dimension(:,:,:,:),intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_schedule_read_logical1_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*1, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_schedule_read_logical2_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*2, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_schedule_read_logical4_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*4, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_schedule_read_logical8_d5 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*8, dimension(:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        !
        ! 6D data
        !

        ! INTEGER*1 array
        subroutine adios_schedule_read_int1_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*1, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*2 array
        subroutine adios_schedule_read_int2_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*2, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*4 array
        subroutine adios_schedule_read_int4_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*4, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! INTEGER*8 array
        subroutine adios_schedule_read_int8_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            integer*8, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*4 array
        subroutine adios_schedule_read_real4_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*4,    dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! REAL*8 array
        subroutine adios_schedule_read_real8_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            real*8,   dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! COMPLEX (*8) array
        subroutine adios_schedule_read_complex8_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex,   dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! DOUBLE-COMPLEX array
        subroutine adios_schedule_read_complex16_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            complex*16,dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! CHARACTER array
        subroutine adios_schedule_read_char_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            character(*),dimension(:,:,:,:,:),intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*1 array
        subroutine adios_schedule_read_logical1_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*1, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*2 array
        subroutine adios_schedule_read_logical2_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*2, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*4 array
        subroutine adios_schedule_read_logical4_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*4, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

        ! LOGICAL*8 array
        subroutine adios_schedule_read_logical8_d6 (fp, sel, varname, from_step, nsteps, data, err)
            implicit none
            integer*8,      intent(in)  :: fp
            integer*8,      intent(in)  :: sel
            character(*),   intent(in)  :: varname
            integer,        intent(in)  :: from_step
            integer,        intent(in)  :: nsteps
            logical*8, dimension(:,:,:,:,:,:), intent(out) :: data
            integer,        intent(in)  :: err

            call adios_schedule_read_f2c(fp, sel, varname, from_step, nsteps, data, err)
        end subroutine

    !
    ! Backward compatibility for 1.4
    !
        subroutine adios_read_open_stream (fp, fname, method, comm, lockmode, timeout_sec, err)
            implicit none
            integer*8,      intent(out) :: fp
            character(*),   intent(in)  :: fname
            integer,        intent(in)  :: method
            integer,        intent(in)  :: comm
            integer,        intent(in)  :: lockmode
            real,           intent(in)  :: timeout_sec
            integer,        intent(out) :: err
            call adios_read_open (fp, fname, method, comm, lockmode, timeout_sec, err)
        end subroutine

end module

