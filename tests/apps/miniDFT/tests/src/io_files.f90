!
! Copyright (C) 2002-2010 Quantum ESPRESSO group
! This file is distributed under the terms of the
! GNU General Public License. See the file `License'
! in the root directory of the present distribution,
! or http://www.gnu.org/copyleft/gpl.txt .
!
!=----------------------------------------------------------------------------=!
MODULE io_files
!=----------------------------------------------------------------------------=!
  !
  USE parameters, ONLY: ntypx
  !
  ! ... The name of the files
  !
  IMPLICIT NONE
  !
  SAVE
  !
  CHARACTER(len=256) :: tmp_dir = './'            ! directory for temporary files
  CHARACTER(len=256) :: wfc_dir = 'undefined'     ! directory for large files on each node, should be kept 'undefined' if not known 
  CHARACTER(len=256) :: prefix  = 'os'            ! prepended to file names
  CHARACTER(len=6)   :: nd_nmbr = '000000'        ! node number (used only in parallel case)
  CHARACTER(len=256) :: pseudo_dir = './'    ! original location of PP files
  CHARACTER(len=256) :: pseudo_dir_cur = ' ' ! current location when restarting
  CHARACTER(len=256) :: psfile( ntypx ) = 'UPF'
  CHARACTER(len=256) :: outdir  = './'
  !
  CHARACTER(len=256) :: qexml_version = ' '       ! the format of the current qexml datafile 
  LOGICAL            :: qexml_version_init = .FALSE.  ! whether the fmt has been read or not
  !
  CHARACTER(LEN=256) :: input_drho = ' '          ! name of the file with the input drho
  CHARACTER(LEN=256) :: output_drho = ' '         ! name of the file with the output drho
  !
  CHARACTER(LEN=5 ), PARAMETER :: crash_file    = 'CRASH'
  CHARACTER (LEN=261) :: &
    exit_file = "os.EXIT"    ! file required for a soft exit  
  !
  CHARACTER (LEN=9),  PARAMETER :: xmlpun_base = 'data-file'
  CHARACTER (LEN=13), PARAMETER :: xmlpun      = xmlpun_base // '.xml'
  !
  ! ... The units where various variables are saved
  !
  INTEGER :: rhounit     = 17
  INTEGER :: crashunit   = 15
  INTEGER :: pseudounit  = 10
  INTEGER :: opt_unit    = 20 ! optional unit 
  !
  ! ... units in pwscf
  !
  INTEGER :: iunres      =  1 ! unit for the restart of the run
  INTEGER :: iunpun      =  4 ! unit for saving the final results
  INTEGER :: iunwfc      = 10 ! unit with wavefunctions
  INTEGER :: iunoldwfc   = 11 ! unit with old wavefunctions
  INTEGER :: iunoldwfc2  = 12 ! as above at step -2
  INTEGER :: iunat       = 13 ! unit for saving (orthogonal) atomic wfcs 
  INTEGER :: iunsat      = 14 ! unit for saving (orthogonal) atomic wfcs * S
  INTEGER :: iunocc      = 15 ! unit for saving the atomic n_{ij}
  INTEGER :: iunigk      = 16 ! unit for saving indices
  INTEGER :: iunpaw      = 17 ! unit for saving paw becsum and D_Hxc
  !
  INTEGER :: iunexit     = 26 ! unit for a soft exit  
  INTEGER :: iunupdate   = 27 ! unit for saving old positions (extrapolation)
  INTEGER :: iunnewimage = 28 ! unit for parallelization among images
  INTEGER :: iunlock     = 29 ! as above (locking file)
  !
  INTEGER :: iunbfgs     = 30 ! unit for the bfgs restart file
  INTEGER :: iunatsicwfc = 31 ! unit for sic wfc
  !
  INTEGER :: iuntmp      = 90 ! temporary unit, when used must be closed ASAP
  !
  INTEGER :: nwordwfc    =  2 ! length of record in wavefunction file
  INTEGER :: nwordatwfc  =  2 ! length of record in atomic wfc file
  INTEGER :: nwordwann   =  2 ! length of record in sic wfc file
  !
  ! ... "path" specific
  !
  !... finite electric field (Umari)
  !
  INTEGER :: iunefield   = 31 ! unit to store wavefunction for calculatin electric field operator
  !
  INTEGER :: iunefieldm  = 32 !unit to store projectors for hermitean electric field potential
  !
  INTEGER :: iunefieldp  = 33 !unit to store projectors for hermitean electric field potential
  !
  ! ... For Wannier Hamiltonian
  !
  INTEGER :: iunwpp   = 113
  INTEGER :: iunwf    = 114
  INTEGER :: nwordwpp = 2
  INTEGER :: nwordwf  = 2
  !
  INTEGER, EXTERNAL :: find_free_unit
CONTAINS
  !
  !--------------------------------------------------------------------------
  SUBROUTINE delete_if_present( filename, in_warning )
    !--------------------------------------------------------------------------
    !
    USE io_global, ONLY : ionode, stdout
    !
    IMPLICIT NONE
    !
    CHARACTER(LEN=*),  INTENT(IN) :: filename
    LOGICAL, OPTIONAL, INTENT(IN) :: in_warning
    LOGICAL                       :: exst, warning
    INTEGER                       :: iunit
    !
    IF ( .NOT. ionode ) RETURN
    !
    INQUIRE( FILE = filename, EXIST = exst )
    !
    IF ( exst ) THEN
       !
       iunit = find_free_unit()
       !
       warning = .FALSE.
       !
       IF ( PRESENT( in_warning ) ) warning = in_warning
       !
       OPEN(  UNIT = iunit, FILE = filename , STATUS = 'OLD' )
       CLOSE( UNIT = iunit, STATUS = 'DELETE' )
       !
       IF ( warning ) &
          WRITE( UNIT = stdout, FMT = '(/,5X,"WARNING: ",A, &
               & " file was present; old file deleted")' ) filename
       !
    END IF
    !
    RETURN
    !
  END SUBROUTINE delete_if_present
  !
  !--------------------------------------------------------------------------
  FUNCTION check_writable ( file_path, process_id ) RESULT ( ios )
    !--------------------------------------------------------------------------
    !
    ! ... if run by multiple processes, specific "process_id" to avoid
    ! ... opening, closing, deleting the same file from different processes
    !
    !
    IMPLICIT NONE
    !
    CHARACTER(LEN=*),  INTENT(IN) :: file_path
    INTEGER, OPTIONAL, INTENT(IN) :: process_id
    !
    INTEGER :: ios
    !
    CHARACTER(LEN=6), EXTERNAL :: int_to_char
    !
    ! ... check whether the scratch directory is writable
    ! ... note that file_path should end by a "/"
    !
    IF ( PRESENT (process_id ) ) THEN
       OPEN( UNIT = 4, FILE = TRIM(file_path) // 'test' // &
           & TRIM( int_to_char ( process_id ) ), &
           & STATUS = 'UNKNOWN', FORM = 'UNFORMATTED', IOSTAT = ios )
    ELSE
       OPEN( UNIT = 4, FILE = TRIM(file_path) // 'test', &
             STATUS = 'UNKNOWN', FORM = 'UNFORMATTED', IOSTAT = ios )
    END IF
    !
    CLOSE( UNIT = 4, STATUS = 'DELETE' )
    !
    !-----------------------------------------------------------------------
  END FUNCTION check_writable 
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine diropn (unit, extension, recl, exst, tmp_dir_)
  !-----------------------------------------------------------------------
  !
  !     this routine opens a file named "prefix"."extension" in tmp_dir 
  !     for direct I/O access
  !     If appropriate, the node number is added to the file name
  !
#if defined(__SX6)
#  define DIRECT_IO_FACTOR 1
#else
#  define DIRECT_IO_FACTOR 8 
#endif
  !
  ! the  record length in direct-access I/O is given by the number of
  ! real*8 words times DIRECT_IO_FACTOR (may depend on the compiler)
  !
  USE kinds
  implicit none
  !
  !    first the input variables
  !
  character(len=*) :: extension
  ! input: name of the file to open
  character(len=*), optional :: tmp_dir_
  ! optional variable, if present it is used as tmp_dir
  integer :: unit, recl
  ! input: unit of the file to open
  ! input: length of the records
  logical :: exst
  ! output: if true the file exists
  !
  !    local variables
  !
  character(len=256) :: tempfile, filename
  ! complete file name
  integer :: ios
  integer*8 :: unf_recl
  ! used to check I/O operations
  ! length of the record
  logical :: opnd

  ! Check if the optional variable tmp_dir is included
  !

  ! if true the file is already opened
  !
  if (unit < 0) call errore ('diropn', 'wrong unit', 1)
  !
  !    we first check that the file is not already openend
  !
  ios = 0
  inquire (unit = unit, opened = opnd)
  if (opnd) call errore ('diropn', "can't open a connected unit", abs(unit))
  !
  !      then we check the filename extension
  !
  if (extension == ' ') call errore ('diropn','filename extension not given',2)
  filename = trim(prefix) // "." // trim(extension)
  if (present(tmp_dir_)) then
     tempfile = trim(tmp_dir_) // trim(filename) //nd_nmbr
  else
     tempfile = trim(tmp_dir) // trim(filename) //nd_nmbr
  endif

  inquire (file = tempfile, exist = exst)
  !
  !      the unit for record length is unfortunately machine-dependent
  !
  unf_recl = DIRECT_IO_FACTOR * int(recl, kind=kind(unf_recl))
  if (unf_recl <= 0) call errore ('diropn', 'wrong record length', 3)
  !
  open (unit, file = trim(adjustl(tempfile)), iostat = ios, form = 'unformatted', &
       status = 'unknown', access = 'direct', recl = unf_recl)

  if (ios /= 0) call errore ('diropn', 'error opening '//trim(tempfile), unit)
  return
  !-----------------------------------------------------------------------
end subroutine diropn
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
subroutine seqopn (unit, extension, formatt, exst, tmp_dir_)
  !-----------------------------------------------------------------------
  !
  !     this routine opens a file named "prefix"."extension"
  !     in tmp_dir for sequential I/O access
  !     If appropriate, the node number is added to the file name
  !
  implicit none
  !
  !    first the dummy variables
  !
  character(len=*) :: formatt, extension
  ! input: name of the file to connect
  ! input: 'formatted' or 'unformatted'
  character(len=*), optional :: tmp_dir_
  ! optional variable, if present it is used as tmp_dir
  integer :: unit
  ! input: unit to connect
  logical :: exst
  ! output: true if the file already exist
  !
  !    here the local variables
  !
  character(len=256) :: tempfile, filename
  ! complete file name
  integer :: ios
  ! integer variable to test I/O status
  logical :: opnd
  ! true if the file is already opened


  if (unit < 1) call errore ('seqopn', 'wrong unit', 1)
  !
  !    test if the file is already opened
  !
  ios = 0
  inquire (unit = unit, opened = opnd)
  if (opnd) call errore ('seqopn', "can't open a connected unit", &
       abs (unit) )
  !
  !      then we check the extension of the filename
  !
  if (extension.eq.' ') call errore ('seqopn','filename extension  not given',2)
  filename = trim(prefix) // "." // trim(extension)
  ! Use the tmp_dir from input, if available
  if ( present(tmp_dir_) ) then
    tempfile = trim(tmp_dir_) // trim(filename)
  else
    tempfile = trim(tmp_dir) // trim(filename)
  end if
  if ( trim(nd_nmbr) == '1' .or. trim(nd_nmbr) == '01'.or. &
       trim(nd_nmbr) == '001' .or. trim(nd_nmbr) == '0001'.or. &
       trim(nd_nmbr) == '00001' .or. trim(nd_nmbr) == '000001' ) then
     !
     ! do not add processor number to files opened by processor 1
     ! in parallel execution: if only the first processor writes,
     ! we do not want the filename to be dependent on the number
     ! of processors
     !
     !tempfile = tempfile
  else
     tempfile = trim(tempfile) // nd_nmbr
  end if
  inquire (file = tempfile, exist = exst)
  !
  !    Open the file
  !

  open (unit = unit, file = tempfile, form = formatt, status = &
       'unknown', iostat = ios)

  if (ios /= 0) call errore ('seqopn', 'error opening '//trim(tempfile), unit)
  return
  !-----------------------------------------------------------------------
end subroutine seqopn
!-----------------------------------------------------------------------
!
!=----------------------------------------------------------------------------=!
END MODULE io_files
!=----------------------------------------------------------------------------=!
!
!----------------------------------------------------------------------------
SUBROUTINE davcio( vect, nword, unit, nrec, io )
  !----------------------------------------------------------------------------
  !
  ! ... direct-access vector input/output
  ! ... read/write nword words starting from the address specified by vect
  !
  USE io_global, ONLY : stdout
  USE kinds,     ONLY : DP
  !
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: nword, unit, nrec, io
    ! input: the dimension of vect
    ! input: the unit where to read/write
    ! input: the record where to read/write
    ! input: flag if < 0 reading if > 0 writing
  REAL(DP), INTENT(INOUT) :: vect(nword)
   ! input/output: the vector to read/write
  !
  INTEGER :: ios
    ! integer variable for I/O control
  LOGICAL :: opnd
  !
  !
  CALL start_clock( 'davcio' )
  !
  INQUIRE( UNIT = unit )
  !
  IF ( unit  <= 0 ) CALL errore(  'davcio', 'wrong unit', 1 )
  IF ( nrec  <= 0 ) CALL errore(  'davcio', 'wrong record number', 2 )
  IF ( nword <= 0 ) CALL errore(  'davcio', 'wrong record length', 3 )
  IF ( io    == 0 ) CALL infomsg( 'davcio', 'nothing to do?' )
  !
  INQUIRE( UNIT = unit, OPENED = opnd )
  !
  IF ( .NOT. opnd ) &
     CALL errore(  'davcio', 'unit is not opened', unit )
  !
  ios = 0
  !
  IF ( io < 0 ) THEN
     !
     READ( UNIT = unit, REC = nrec, IOSTAT = ios ) vect
     IF ( ios /= 0 ) &
        CALL errore( 'davcio', 'error while reading from file', unit )
     !
  ELSE IF ( io > 0 ) THEN
     !
     WRITE( UNIT = unit, REC = nrec, IOSTAT = ios ) vect
     IF ( ios /= 0 ) &
        CALL errore( 'davcio', 'error while writing to file', unit )
     !
  END IF
  !
  CALL stop_clock( 'davcio' )
  !
  RETURN
  !
END SUBROUTINE davcio

