!-----------------------------------------------------------------------
! Solar EUV irradiance data
!-----------------------------------------------------------------------
module solar_euv_data
  use shr_kind_mod,     only: r8 => shr_kind_r8
  use spmd_utils,       only: masterproc
  use cam_abortutils,   only: endrun
  use cam_pio_utils,    only: cam_pio_openfile
  use cam_logfile,      only: iulog
  use pio,              only: pio_get_var, pio_inq_varid, pio_inq_dimid, pio_inq_dimlen, pio_seterrorhandling, &
                              PIO_NOERR, pio_bcast_error, pio_internal_error, file_desc_t

  implicit none

  save
  private
  public :: solar_euv_data_readnl
  public :: solar_euv_data_init
  public :: solar_euv_data_advance
  public :: solar_euv_data_etf
  public :: solar_euv_data_active

  real(r8), target, allocatable :: solar_euv_data_etf(:)
  logical, protected :: solar_euv_data_active = .false.

  integer :: nbins
  integer :: ntimes
  real(r8), allocatable :: irradi(:,:)

  real(r8), allocatable :: data_times(:)

  integer :: last_index = 1
  type(file_desc_t) :: file_id
  integer :: ssi_vid

  logical :: initialized = .false.

  real(r8), allocatable :: dellam(:)
  real(r8), allocatable :: lambda(:)
  real(r8), allocatable :: we(:)

! namelist vars
  character(len=256) :: solar_euv_data_file = ' '

  integer, parameter :: nrecords = 2
  logical, parameter :: debug = .false.

contains

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  subroutine solar_euv_data_readnl( nlfile )
    use namelist_utils,  only: find_group_name
    use units,           only: getunit, freeunit
#ifdef SPMD
    use mpishorthand,    only: mpichar, mpicom
#endif

    ! arguments
    character(len=*), intent(in) :: nlfile  ! filepath for file containing namelist input

    ! local vars
    integer :: unitn, ierr

    namelist /solar_euv_nl/ solar_euv_data_file
    
    if (masterproc) then
       unitn = getunit()
       open( unitn, file=trim(nlfile), status='old' )
       call find_group_name(unitn, 'solar_euv_nl', status=ierr)
       if (ierr == 0) then
          read(unitn, solar_euv_nl, iostat=ierr)
          if (ierr /= 0) then
             call endrun('solar_euv_data_readnl: ERROR reading namelist')
          end if
       end if
       close(unitn)
       call freeunit(unitn)
    end if

#ifdef SPMD
    ! broadcast the options to all MPI tasks
    call mpibcast(solar_euv_data_file, len(solar_euv_data_file), mpichar, 0, mpicom)
#endif

    solar_euv_data_active = len_trim(solar_euv_data_file)>0
    if (masterproc .and. solar_euv_data_active) then
       write(iulog,*) 'solar_euv_data_readnl: solar_euv_data_file = ',trim(solar_euv_data_file)
    endif

  end subroutine solar_euv_data_readnl

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  subroutine solar_euv_data_init

    use ioFileMod, only : getfil

    integer :: astat, dimid, vid
    character(len=256) :: filen   
    integer,  allocatable :: dates(:)
    integer,  allocatable :: datesecs(:)

    integer :: ierr

    if ( .not.solar_euv_data_active ) return

    call getfil( solar_euv_data_file, filen, 0 )
    call cam_pio_openfile( file_id, filen, 0 )

    if(masterproc)  write(iulog,*)'solar_euv_data_init: data file = ',trim(filen)

    ierr = pio_inq_varid( file_id, 'ssi', ssi_vid )
    ierr = pio_inq_dimid( file_id, 'time', dimid )
    ierr = pio_inq_dimlen( file_id, dimid, ntimes )
    ierr = pio_inq_dimid( file_id, 'bin', dimid )
    ierr = pio_inq_dimlen( file_id, dimid, nbins )
    
    allocate(irradi(nbins,nrecords), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate irradi; error = ',astat
       call endrun('solar_data_init')
    end if

    allocate(lambda(nbins), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate lambda; error = ',astat
       call endrun('solar_euv_data_init')
    end if
    allocate(dellam(nbins), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate dellam; error = ',astat
       call endrun('solar_euv_data_init')
    end if
    allocate(data_times(ntimes), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate data_times; error = ',astat
       call endrun('solar_euv_data_init')
    end if

    allocate(dates(ntimes), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate dates; error = ',astat
       call endrun('solar_euv_data_init')
    end if

    allocate(datesecs(ntimes), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate datesecs; error = ',astat
       call endrun('solar_euv_data_init')
    end if

    allocate(solar_euv_data_etf(nbins), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate solar_euv_data_etf; error = ',astat
       call endrun('solar_euv_data_init')
    end if

    ierr = pio_inq_varid( file_id, 'date', vid  )
    ierr = pio_get_var( file_id, vid, dates )
    call pio_seterrorhandling(file_id, pio_bcast_error)
    ierr = pio_inq_varid( file_id, 'datesec', vid )
    call pio_seterrorhandling(file_id, pio_internal_error)
    if (ierr==PIO_NOERR) then
       ierr = pio_get_var( file_id, vid, datesecs )
    else
       datesecs(:) = 0
    endif
       
    ierr = pio_inq_varid( file_id, 'wavelength', vid )
    ierr = pio_get_var( file_id, vid, lambda )
    ierr = pio_inq_varid( file_id, 'band_width', vid  )
    ierr = pio_get_var( file_id, vid, dellam )
    
    allocate(we(nbins+1), stat=astat )
    if( astat /= 0 ) then
       write(iulog,*) 'solar_euv_data_init: failed to allocate we; error = ',astat
       call endrun('solar_euv_data_init')
    end if

    we(:nbins)  = lambda(:nbins) - 0.5_r8*dellam(:nbins)
    we(nbins+1) = lambda(nbins)  + 0.5_r8*dellam(nbins)

    call convert_dates( dates, datesecs, data_times ) 

    deallocate(lambda)
    deallocate(dellam)
    deallocate(datesecs)
    deallocate(dates)

    ! need to force data loading when the model starts at a time =/ 00:00:00.000
    ! -- may occur in restarts also
    call solar_euv_data_advance()
    initialized = .true.

  end subroutine solar_euv_data_init

!-----------------------------------------------------------------------
! Reads in the ETF data for the current date.  
!-----------------------------------------------------------------------
  subroutine solar_euv_data_advance()

    integer  :: year, month, day, sec
    integer  :: index, i
    logical  :: read_data
    real(r8) :: time, delt
    integer  :: ierr
    integer  :: offset(2), count(2)

    if (.not.solar_euv_data_active) return

    index = -1
    call get_model_time( time, year=year, month=month, day=day, seconds=sec )
    read_data = time > data_times(last_index) .or. .not.initialized

    if ( read_data ) then

       find_ndx: do i = last_index, ntimes
          if ( data_times(i) > time ) then
             index = i-1
             exit find_ndx
          endif
       enddo find_ndx

       last_index = index+1

       if ( index < 1 ) then
          write(iulog,102) year,month,day,sec
          call endrun('solar_euv_data_advance: failed to read data from '//trim(solar_euv_data_file))
       endif

       ! get the surrounding time slices
       offset = (/ 1, index /)
       count =  (/ nbins, nrecords /)

       ierr = pio_get_var( file_id, ssi_vid, offset, count, irradi )

    else
       index = last_index - 1
    endif

    delt = ( time - data_times(index) ) / ( data_times(index+1) - data_times(index) )

    solar_euv_data_etf(:) = irradi(:,1) + delt*( irradi(:,2) - irradi(:,1) )

    if ( masterproc .and. debug) then
       write(iulog,101) year, month, day, sec, solar_euv_data_etf
    endif

 101 FORMAT('solar_euv_data_advance: date, eft : ',i4.4,'-',i2.2,'-',i2.2,'-',i5.5,',  ', 25e12.4 )
 102 FORMAT('solar_euv_data_advance: not able to find data for : ',i4.4,'-',i2.2,'-',i2.2,'-',i5.5)

  end subroutine solar_euv_data_advance

  !---------------------------------------------------------------------------
  ! private methods
  !---------------------------------------------------------------------------
  subroutine convert_dates( dates, secs, times )

    use time_manager, only: set_time_float_from_date

    integer,  intent(in)  :: dates(:)
    integer,  intent(in)  :: secs(:)

    real(r8), intent(out) :: times(:)

    integer :: year, month, day, sec,n ,i

    n = size( dates ) 

    do i=1,n
       year = dates(i)/10000
       month = (dates(i)-year*10000)/100
       day = dates(i)-year*10000-month*100
       sec = secs(i)
       call set_time_float_from_date( times(i), year, month, day, sec )
    enddo

  end subroutine convert_dates

  !---------------------------------------------------------------------------
  !---------------------------------------------------------------------------
  subroutine convert_date( date, sec, time )

    integer,  intent(in)  :: date
    integer,  intent(in)  :: sec
    real(r8), intent(out) :: time

    integer :: dates(1), secs(1)
    real(r8) :: times(1)
    dates(1) = date
    secs(1) = sec
    call convert_dates( dates, secs, times )
    time = times(1)
  end subroutine convert_date

!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
  subroutine get_model_time( time, year, month, day, seconds )

    use time_manager, only: get_curr_date

    real(r8), intent(out) :: time
    integer, optional, intent(out) :: year, month, day, seconds

    integer  :: yr, mn, dy, sc, date

    call get_curr_date(yr, mn, dy, sc)
    date = yr*10000 + mn*100 + dy
    call convert_date( date, sc, time )

    if (present(year))    year = yr
    if (present(month))   month = mn
    if (present(day))     day = dy
    if (present(seconds)) seconds = sc

  end subroutine get_model_time

end module solar_euv_data
