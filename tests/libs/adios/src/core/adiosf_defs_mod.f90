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
module adios_defs_mod

    !
    ! ADIOS Data types                                   ! (size in bytes)
    !
    integer, parameter :: adios_unknown = -1             
    integer, parameter :: adios_byte = 0                 ! (1) 
    integer, parameter :: adios_short = 1                ! (2) 
    integer, parameter :: adios_integer = 2              ! (4) 
    integer, parameter :: adios_long = 4                 ! (8) 
    integer, parameter :: adios_unsigned_byte = 50       ! (1) 
    integer, parameter :: adios_unsigned_short = 51      ! (2) 
    integer, parameter :: adios_unsigned_integer = 52    ! (4) 
    integer, parameter :: adios_unsigned_long = 54       ! (8) 
    integer, parameter :: adios_real = 5                 ! (4) 
    integer, parameter :: adios_double = 6               ! (8) 
    integer, parameter :: adios_long_double = 7          ! (16) 
    integer, parameter :: adios_string = 9               ! C strings
    integer, parameter :: adios_complex = 10             ! (8) 


    !
    ! Read methods (used in init/finalize/open calls)
    !
    integer, parameter :: ADIOS_READ_METHOD_BP           = 0
    integer, parameter :: ADIOS_READ_METHOD_BP_AGGREGATE = 1
    !integer, parameter :: ADIOS_READ_METHOD_BP_STAGED1   = 2
    integer, parameter :: ADIOS_READ_METHOD_DATASPACES   = 3
    integer, parameter :: ADIOS_READ_METHOD_DIMES        = 4
    integer, parameter :: ADIOS_READ_METHOD_FLEXPATH     = 5
    integer, parameter :: ADIOS_READ_METHOD_ICEE         = 6
    integer, parameter :: ADIOS_READ_METHOD_BP_STAGED  = ADIOS_READ_METHOD_BP_AGGREGATE

    ! 
    ! Stream Locking modes
    !
    integer, parameter :: ADIOS_LOCKMODE_NONE    = 0
    integer, parameter :: ADIOS_LOCKMODE_CURRENT = 0
    integer, parameter :: ADIOS_LOCKMODE_ALL     = 0


    !
    ! ADIOS error codes
    !
    integer, parameter :: err_no_error                        = 0
    integer, parameter :: err_no_memory                       = -1
    integer, parameter :: err_file_open_error                 = -2
    integer, parameter :: err_file_not_found                  = -3
    integer, parameter :: err_invalid_file_pointer            = -4
    integer, parameter :: err_invalid_group                   = -5
    integer, parameter :: err_invalid_group_struct            = -6
    integer, parameter :: err_invalid_varid                   = -7
    integer, parameter :: err_invalid_varname                 = -8
    integer, parameter :: err_corrupted_variable              = -9

    integer, parameter :: err_invalid_attrid                  = -10
    integer, parameter :: err_invalid_attrname                = -11
    integer, parameter :: err_corrupted_attribute             = -12
    integer, parameter :: err_invalid_attribute_reference     = -13
    integer, parameter :: err_invalid_timestep                = -14
    integer, parameter :: err_no_data_at_timestep             = -15
    integer, parameter :: err_time_at_wrong_dimension         = -16
    integer, parameter :: err_invalid_read_method             = -17
    integer, parameter :: err_connection_failed               = -18
    integer, parameter :: err_out_of_bound                    = -19

    ! Stream reading specific errors
    integer, parameter :: err_operation_not_supported         = -20
    integer, parameter :: err_end_of_stream                   = -21
    integer, parameter :: err_step_notready                   = -22
    integer, parameter :: err_step_disappeared                = -23
    integer, parameter :: err_too_many_files                  = -24

    ! Write method errors
    integer, parameter :: err_invalid_file_mode               = -100
    integer, parameter :: err_invalid_file_version            = -101
    integer, parameter :: err_invalid_data                    = -102
    integer, parameter :: err_buffer_overflow                 = -103
    integer, parameter :: err_too_many_variables              = -104
    integer, parameter :: err_invalid_write_method            = -105
    integer, parameter :: err_unspecified                     = -200


end module adios_defs_mod
