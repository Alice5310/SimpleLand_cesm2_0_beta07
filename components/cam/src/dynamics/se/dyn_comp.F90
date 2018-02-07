module dyn_comp

! This module implements the CAM interfaces to the SE Dynamical Core

use shr_kind_mod,           only: r8=>shr_kind_r8, shr_kind_cl
use spmd_utils,             only: iam, masterproc
use dyn_grid,               only: timelevel, dom_mt, hvcoord
use cam_control_mod,        only: initial_run
use time_manager,           only: is_first_step

use cam_logfile,            only: iulog
use cam_abortutils,         only: endrun

use perf_mod,               only: t_startf, t_stopf

use fvm_control_volume_mod, only: fvm_struct
use element_mod,            only: element_t, elem_state_t
use time_mod,               only: nsplit
use hybrid_mod,             only: hybrid_t

implicit none
private
save

public :: &
   dyn_import_t, &
   dyn_export_t, &
   dyn_readnl,   &
   dyn_register, &
   dyn_init,     &
   dyn_run,      &
   dyn_final

type dyn_import_t
   type (element_t), pointer :: elem(:) => null()
   type (fvm_struct), pointer :: fvm(:) => null()
end type dyn_import_t

type dyn_export_t
   type (element_t), pointer :: elem(:) => null()
   type (fvm_struct), pointer :: fvm(:) => null()
end type dyn_export_t

! Parameters for namelist string lengths (from namelist_definition.xml)
integer, parameter  :: METHOD_LEN = 32

integer, parameter  :: DYN_RUN_SUCCESS = 0
integer, parameter  :: DYN_RUN_FAILURE = -1

! Frontogenesis indices
integer, public :: frontgf_idx = -1
integer, public :: frontga_idx = -1

!=========================================================================================
contains
!=========================================================================================

subroutine dyn_readnl(NLFileName)
  use namelist_utils, only: find_group_name
  use namelist_mod,   only: homme_set_defaults, homme_postprocess_namelist
  use units,          only: getunit, freeunit
  use spmd_utils,     only: masterproc, masterprocid, mpicom, npes
  use spmd_utils,     only: mpi_real8, mpi_integer, mpi_character, mpi_logical
  use control_mod,    only: TRACERTRANSPORT_SE_GLL, TRACERTRANSPORT_LAGRANGIAN_FVM
  use control_mod,    only: TRACERTRANSPORT_FLUXFORM_FVM, tracer_transport_type
  use control_mod,    only: TRACER_GRIDTYPE_GLL, TRACER_GRIDTYPE_FVM, tracer_grid_type
  use control_mod,    only: energy_fixer, hypervis_order, hypervis_subcycle
  use control_mod,    only: hypervis_subcycle_q, integration, statefreq, runtype
  use control_mod,    only: nu, nu_div, nu_p, nu_q, nu_top, qsplit, rsplit
  use control_mod,    only: vert_remap_q_alg, tstep_type, rk_stage_user
  use control_mod,    only: ftype, limiter_option, partmethod
  use control_mod,    only: topology, sub_case, numnodes, tasknum, moisture
  use control_mod,    only: columnpackage, remapfreq, remap_type
  use control_mod,    only: initial_total_mass, use_semi_lagrange_transport
  use control_mod,    only: disable_diagnostics
  use control_mod,    only: fine_ne, hypervis_power, hypervis_scaling
  use control_mod,    only: max_hypervis_courant
  use fvm_mod,        only: fvm_ideal_test, fvm_test_type
  use fvm_mod,        only: fvm_get_test_type
  use dimensions_mod, only: qsize, qsize_d, ntrac, ntrac_d, npsq, ne, npart
  use constituents,   only: pcnst
  use params_mod,     only: SFCURVE
  use parallel_mod,   only: par, initmp
  use native_mapping, only: native_mapping_readnl
!!XXgoldyXX: v For future CSLAM / physgrid commit
!    use dp_grids,       only: fv_nphys, fv_nphys2, nphys_pts, write_phys_grid, phys_grid_file
!!XXgoldyXX: ^ For future CSLAM / physgrid commit

  ! Dummy argument
  character(len=*), intent(in) :: NLFileName

  ! Local variables
  integer                      :: unitn, ierr
 ! SE Namelist variables
  integer                      :: se_fine_ne
  integer                      :: se_ftype
  integer                      :: se_hypervis_order
  real(r8)                     :: se_hypervis_power
  real(r8)                     :: se_hypervis_scaling
  integer                      :: se_hypervis_subcycle
  integer                      :: se_hypervis_subcycle_q
  integer                      :: se_limiter_option
  real(r8)                     :: se_max_hypervis_courant
  character(len=SHR_KIND_CL)   :: se_mesh_file
  integer                      :: se_ne
  integer                      :: se_npes
  integer                      :: se_nsplit
  real(r8)                     :: se_nu
  real(r8)                     :: se_nu_div
  real(r8)                     :: se_nu_p
  real(r8)                     :: se_nu_q
  real(r8)                     :: se_nu_top
  integer                      :: se_qsplit
  logical                      :: se_refined_mesh
  integer                      :: se_rsplit
  integer                      :: se_statefreq
  integer                      :: se_tstep_type
  integer                      :: se_vert_remap_q_alg
!!XXgoldyXX: v For future CSLAM / physgrid commit
!    character(len=METHOD_LEN)     :: se_tracer_transport_method
!    character(len=METHOD_LEN)     :: se_cslam_ideal_test
!    character(len=METHOD_LEN)     :: se_cslam_test_type
!    character(len=METHOD_LEN)     :: se_write_phys_grid
!    character(len=shr_kind_cl)    :: se_phys_grid_file
!    integer                       :: se_fv_nphys = 0
!!XXgoldyXX: ^ For future CSLAM / physgrid commit

  namelist /dyn_se_inparm/      &
       se_fine_ne,              & ! For refined meshes
       se_ftype,                & ! forcing type
       se_hypervis_order,       &
       se_hypervis_power,       &
       se_hypervis_scaling,     &
       se_hypervis_subcycle,    &
       se_hypervis_subcycle_q,  &
       se_limiter_option,       &
       se_max_hypervis_courant, &
       se_mesh_file,            & ! Refined mesh definition file
       se_ne,                   &
       se_npes,                 &
       se_nsplit,               & ! # of dynamics steps per physics timestep
       se_nu,                   &
       se_nu_div,               &
       se_nu_p,                 &
       se_nu_q,                 &
       se_nu_top,               &
       se_qsplit,               &
       se_refined_mesh,         &
       se_rsplit,               &
       se_statefreq,            & ! number of steps per printstate call
       se_tstep_type,           &
       se_vert_remap_q_alg
!!XXgoldyXX: v For future physgrid commit
!         se_fv_nphys,          & ! Linear size of FV physics grid
!         se_write_phys_grid,   &
!         se_phys_grid_file,    &
!!XXgoldyXX: ^ For future physgrid commit

!!XXgoldyXX: v For future CSLAM / physgrid commit
!    namelist /cslam_nl/ se_tracer_transport_method, se_cslam_ideal_test, se_cslam_test_type
!!XXgoldyXX: ^ For future CSLAM / physgrid commit

  !--------------------------------------------------------------------------

 ! namelist default values should be in namelist (but you know users . . .)
  ! NB: Of course, these should keep up with what is in namelist_defaults ...
  se_fine_ne              = -1
  se_ftype                = 0
  se_hypervis_order       = 2
  se_hypervis_power       = 0
  se_hypervis_scaling     = 0
  se_hypervis_subcycle    = 3
  se_hypervis_subcycle_q  = 1
  se_limiter_option       = 8
  se_max_hypervis_courant = 1.0e99_r8
  se_mesh_file            = ''
  se_ne                   = -1
  se_npes                 = npes
  se_nsplit               = 2
  se_nu                   = 1.0e15_r8
  se_nu_div               = 2.5e15_r8
  se_nu_p                 = 1.0e15_r8
  se_nu_q                 = -1.0_r8
  se_nu_top               = 2.5e5_r8
  se_qsplit               = 1
  se_refined_mesh         = .false.
  se_rsplit               = 3
  se_statefreq            = 480
  se_tstep_type           = 5
  se_vert_remap_q_alg     = 1
!!XXgoldyXX: v For future CSLAM / physgrid commit
!    character(len=METHOD_LEN)     :: se_tracer_transport_method
!    character(len=METHOD_LEN)     :: se_cslam_ideal_test
!    character(len=METHOD_LEN)     :: se_cslam_test_type
!    character(len=METHOD_LEN)     :: se_write_phys_grid
!    character(len=shr_kind_cl)    :: se_phys_grid_file
!    integer                       :: se_fv_nphys = 0
!!XXgoldyXX: ^ For future CSLAM / physgrid commit

  ! Read the namelist (dyn_se_inparm)
  call MPI_barrier(mpicom, ierr)
  if (masterproc) then
    write(iulog, *) "dyn_readnl: reading dyn_se_inparm namelist..."
    unitn = getunit()
    open( unitn, file=trim(NLFileName), status='old' )
    call find_group_name(unitn, 'dyn_se_inparm', status=ierr)
    if (ierr == 0) then
      read(unitn, dyn_se_inparm, iostat=ierr)
      if (ierr /= 0) then
        call endrun('dyn_readnl: ERROR reading dyn_se_inparm namelist')
      end if
    end if
    close(unitn)
    call freeunit(unitn)
  end if

  ! Broadcast namelist values to all PEs
  call MPI_bcast(se_fine_ne, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_ftype, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_hypervis_order, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_hypervis_power, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_hypervis_scaling, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_hypervis_subcycle, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_hypervis_subcycle_q, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_limiter_option, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_max_hypervis_courant, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_mesh_file, SHR_KIND_CL,  mpi_character, masterprocid, mpicom, ierr)
  call MPI_bcast(se_ne, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_npes, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_nsplit, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_nu, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_nu_div, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_nu_p, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_nu_q, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_nu_top, 1, mpi_real8, masterprocid, mpicom, ierr)
  call MPI_bcast(se_qsplit, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_refined_mesh, 1, mpi_logical, masterprocid, mpicom, ierr)
  call MPI_bcast(se_rsplit, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_statefreq, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_tstep_type, 1, mpi_integer, masterprocid, mpicom, ierr)
  call MPI_bcast(se_vert_remap_q_alg, 1, mpi_integer, masterprocid, mpicom, ierr)
!!XXgoldyXX: v For future physgrid commit
!    call MPI_bcast(fv_nphys, 1, mpi_integer, masterprocid, mpicom, ierr)
!    call MPI_bcast(write_phys_grid, 80,  mpi_character, masterprocid, mpicom, ierr)
!    call MPI_bcast(phys_grid_file,  256, mpi_character, masterprocid, mpicom, ierr)
!    fv_nphys2 = fv_nphys * fv_nphys
!    if (fv_nphys > 0) then
!      nphys_pts = fv_nphys2
!    else
!      nphys_pts = npsq
!    end if
!!XXgoldyXX: ^ For future physgrid commit

  ! Initialize the SE structure that holds the MPI decomposition information
  if (se_npes <= 0) then
    se_npes = npes
  end if
  par = initmp(se_npes)

!!XXgoldyXX: v For future CSLAM/physgrid commit
!    ! Next, read CSLAM nl
!    se_tracer_transport_method = 'se_gll'
!    se_cslam_ideal_test = 'off'
!    se_cslam_test_type = 'boomerang'
!    if (masterproc) then
!      write(iulog, *) "dyn_readnl: reading CSLAM namelist..."
!      unitn = getunit()
!      open( unitn, file=trim(NLFileName), status='old' )
!      call find_group_name(unitn, 'cslam_nl', status=ierr)
!      if (ierr == 0) then
!        read(unitn, cslam_nl, iostat=ierr)
!        if (ierr /= 0) then
!          call endrun('dyn_readnl: ERROR reading cslam namelist')
!        end if
!      end if
!      close(unitn)
!      call freeunit(unitn)

!      ! Set and broadcast tracer transport type
!      if (trim(se_tracer_transport_method) == 'se_gll') then
!        tracer_transport_type = TRACERTRANSPORT_SE_GLL
!        tracer_grid_type = TRACER_GRIDTYPE_GLL
!#ifdef FVM_TRACERS
!      else if (trim(se_tracer_transport_method) == 'cslam_fvm') then
!        tracer_transport_type = TRACERTRANSPORT_LAGRANGIAN_FVM
!        tracer_grid_type = TRACER_GRIDTYPE_FVM
!      else if (trim(se_tracer_transport_method) == 'flux_form_cslam_fvm') then
!        tracer_transport_type = TRACERTRANSPORT_FLUXFORM_FVM
!        tracer_grid_type = TRACER_GRIDTYPE_FVM
!#endif
!      else
!        call endrun('Unknown tracer transport method: '//trim(se_tracer_transport_method))
!      end if

!      ! Set and broadcast CSLAM options
!      call fvm_get_test_type(se_cslam_ideal_test, cslam_test_type, fvm_ideal_test, fvm_test_type)
!    end if
!#ifdef SPMD
!    ! Broadcast namelist variables
!    call MPI_bcast(se_tracer_transport_type,1,mpi_integer,masterprocid,mpicom,ierr)
!    call MPI_bcast(tracer_grid_type,1,mpi_integer,masterprocid,mpicom,ierr)
!    call MPI_bcast(fvm_ideal_test,1,mpi_integer,masterprocid,mpicom,ierr)
!    call MPI_bcast(fvm_test_type,1,mpi_integer,masterprocid,mpicom,ierr)
!#endif

!    ! Set and broadcast tracer transport type
!    if (tracer_transport_type == TRACERTRANSPORT_SE_GLL) then
!      qsize = pcnst
!      ntrac = 0
!    else if (tracer_transport_type == TRACERTRANSPORT_LAGRANGIAN_FVM) then
!!phl      qsize = 1
!      qsize = pcnst !add phl
!      ntrac = pcnst
!    else if (tracer_transport_type == TRACERTRANSPORT_FLUXFORM_FVM) then
!      qsize = 1
!      qsize = pcnst !add phl
!      ntrac = pcnst
!    else
!      call endrun('Unknown tracer transport type')
!    end if
!!XXgoldyXX: ^ For future physgrid commit

  ! Fix up unresolved default values
  ! default diffusion coefficiets
  if (se_nu_q < 0) then
    se_nu_q = se_nu
  end if
  if (se_nu_div < 0) then
    se_nu_div = se_nu
  end if
  ! Go ahead and enforce ne = 0 for refined mesh runs
  if (se_refined_mesh) then
    se_ne = 0
  end if

  ! Set HOMME defaults
  call homme_set_defaults()
  ! Set HOMME variables not in CAM's namelist but with different CAM defaults
  partmethod           = SFCURVE
  npart                = se_npes
  energy_fixer         = -1      ! no fixer, non-staggered-in-time formulas
  ! CAM requires forward-in-time, subcycled dynamics
  ! RK2 3 stage tracers, sign-preserving conservative
  rk_stage_user        = 3
  integration          = 'explicit'
  qsize                = qsize_d
  topology             = "cube"
  ! Finally, set the HOMME variables which have different names
  fine_ne              = se_fine_ne
  ftype                = se_ftype
  hypervis_order       = se_hypervis_order
  hypervis_power       = se_hypervis_power
  hypervis_scaling     = se_hypervis_scaling
  hypervis_subcycle    = se_hypervis_subcycle
  hypervis_subcycle_q  = se_hypervis_subcycle_q
  limiter_option       = se_limiter_option
  max_hypervis_courant = se_max_hypervis_courant
  ne                   = se_ne
  nsplit               = se_nsplit
  nu                   = se_nu
  nu_div               = se_nu_div
  nu_p                 = se_nu_p
  nu_q                 = se_nu_q
  nu_top               = se_nu_top
  qsplit               = se_qsplit
  rsplit               = se_rsplit
  statefreq            = se_statefreq
  tstep_type           = se_tstep_type
  vert_remap_q_alg     = se_vert_remap_q_alg

  ! if restart or branch run
  if (.not. initial_run) runtype = 1

!!XXgoldyXX: v For future physgrid commit
!    fv_nphys = se_fv_nphys
!    if (fv_nphys > 0) then
!      ! Only set these if the physics grid is active
!      write_phys_grid = trim(se_write_phys_grid)
!      phys_grid_file = trim(se_phys_grid_file)
!    end if
!!XXgoldyXX: ^ For future physgrid commit

  ! HOMME wants 'none' to indicate no mesh file
  if (len_trim(se_mesh_file) == 0) then
    se_mesh_file = 'none'
    if (se_refined_mesh) then
      call endrun('dyn_readnl ERROR: se_refined_mesh=.true. but no se_mesh_file')
    end if
  end if
  call homme_postprocess_namelist(se_mesh_file, par)

  if (masterproc) then
    write(iulog, '(a,i0)') 'dyn_readnl: se_ftype = ',se_ftype
    write(iulog, '(a,i0)') 'dyn_readnl: se_hypervis_order = ',se_hypervis_order
    write(iulog, '(a,i0)') 'dyn_readnl: se_hypervis_subcycle = ',se_hypervis_subcycle
    write(iulog, '(a,i0)') 'dyn_readnl: se_hypervis_subcycle_q = ',se_hypervis_subcycle_q
    write(iulog, '(a,i0)') 'dyn_readnl: se_limiter_option = ',se_limiter_option
    if (.not. se_refined_mesh) then
      write(iulog, '(a,i0)') 'dyn_readnl: se_ne = ',se_ne
    end if
    write(iulog, '(a,i0)') 'dyn_readnl: se_npes = ',se_npes
    write(iulog, '(a,i0)') 'dyn_readnl: se_nsplit = ',se_nsplit
    write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu = ',se_nu
    write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu_div = ',se_nu_div
    write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu_p = ',se_nu_p
    write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu_q = ',se_nu_q
    write(iulog, '(a,e9.2)') 'dyn_readnl: se_nu_top = ',se_nu_top
    write(iulog, '(a,i0)') 'dyn_readnl: se_qsplit = ',se_qsplit
    write(iulog, '(a,i0)') 'dyn_readnl: se_rsplit = ',se_rsplit
    write(iulog, '(a,i0)') 'dyn_readnl: se_statefreq = ',se_statefreq
    write(iulog, '(a,i0)') 'dyn_readnl: se_tstep_type = ',se_tstep_type
    write(iulog, '(a,i0)') 'dyn_readnl: se_vert_remap_q_alg = ',se_vert_remap_q_alg
    if (se_refined_mesh) then
      write(iulog, *) 'dyn_readnl: Refined mesh simulation'
      write(iulog, *) 'dyn_readnl: se_mesh_file = ',trim(se_mesh_file)
      if (abs(se_hypervis_power) < 1.0e-12_r8) then
        write(iulog, '(a,e11.4)') 'dyn_readnl: se_hypervis_power = ',se_hypervis_power, ', (tensor hyperviscosity)'
        write(iulog, '(a,e11.4)') 'dyn_readnl: se_hypervis_scaling = ',se_hypervis_scaling
      else if (abs(se_hypervis_power - 3.322_r8) < 1.0e-12_r8) then
        write(iulog, '(a,e11.4)') 'dyn_readnl: se_hypervis_power = ',se_hypervis_power, ', (scalar hyperviscosity)'
        write(iulog, '(a,i0)') 'dyn_readnl: se_fine_ne = ',se_fine_ne
      else
        write(iulog, '(a,i0)') 'dyn_readnl: se_hypervis_power = ',se_hypervis_power
        write(iulog, '(a,e11.4)') 'dyn_readnl: se_hypervis_scaling = ',se_hypervis_scaling
        write(iulog, '(a,e11.4)') 'dyn_readnl: se_fine_ne = ',se_fine_ne
      end if
      write(iulog, '(a,e11.4)') 'dyn_readnl: se_max_hypervis_courant = ',se_max_hypervis_courant
    end if


!!XXgoldyXX: v For future physgrid commit
!      write(iulog,*) 'dyn_readnl: fv_nphys = ', fv_nphys, ', nphys_pts = ', nphys_pts
!      if (fv_nphys > 0) then
!        if (trim(write_phys_grid) == 'grid') then
!          write(iulog,*) "dyn_readnl: write physics grid file = ", trim(phys_grid_file)
!        else if (trim(write_phys_grid) == 'interp') then
!          write(iulog,*) "dyn_readnl: write physics interp file = ", trim(phys_grid_file)
!        else
!          write(iulog,*) "dyn_readnl: do not write physics grid or interp file"
!        end if
!      end if
!!XXgoldyXX: ^ For future physgrid commit
 end if

 call native_mapping_readnl(NLFileName)

end subroutine dyn_readnl

!=============================================================================================

subroutine dyn_register()

   use physics_buffer,  only: pbuf_add_field, dtype_r8
   use ppgrid,          only: pcols, pver
   use phys_control,    only: use_gw_front, use_gw_front_igw

   ! These fields are computed by the dycore and passed to the physics via the
   ! physics buffer.

   if (use_gw_front .or. use_gw_front_igw) then
      call pbuf_add_field("FRONTGF", "global", dtype_r8, (/pcols,pver/), &
         frontgf_idx)
      call pbuf_add_field("FRONTGA", "global", dtype_r8, (/pcols,pver/), &
         frontga_idx)
   end if

end subroutine dyn_register

!=============================================================================================

subroutine dyn_init(dyn_in, dyn_out)

   use dyn_grid,         only: elem, fvm
   use cam_control_mod,  only: aqua_planet, ideal_phys, adiabatic
   use cam_instance,     only: inst_index
   use native_mapping,   only: create_native_mapping_files
   use cam_pio_utils,    only: clean_iodesc_list

   use dimensions_mod,   only: nlev, nelemd
   use prim_driver_mod,  only: prim_init2, prim_run
   use prim_si_ref_mod,  only: prim_set_mass
   use hybrid_mod,       only: hybrid_create
   use parallel_mod,     only: par
   use time_mod,         only: time_at
   use control_mod,      only: moisture, runtype
   use thread_mod,       only: nthreads, omp_get_thread_num
   use nctopo_util_mod,  only: nctopo_util_driver

   type (dyn_import_t), intent(out) :: dyn_in
   type (dyn_export_t), intent(out) :: dyn_out

   integer :: ithr, nets, nete, ie, k
   real(r8), parameter :: Tinit=300.0_r8
   type(hybrid_t) :: hybrid
   !----------------------------------------------------------------------------

   ! Initialize the import/export objects
   dyn_in%elem  => elem
   dyn_in%fvm   => fvm

   dyn_out%elem => elem
   dyn_out%fvm  => fvm

   ! Create mapping files using SE basis functions if requested
   call create_native_mapping_files(par, elem, 'native')
   call create_native_mapping_files(par, elem, 'bilin')
   
   if (initial_run) then
      call read_inidat(dyn_in)
      call clean_iodesc_list()
   end if

   if (iam < par%nprocs) then

#if (defined HORIZ_OPENMP)
       !$OMP PARALLEL DEFAULT(SHARED), PRIVATE(ie,ithr,nets,nete,hybrid)
#endif
      ithr=omp_get_thread_num()
      nets=dom_mt(ithr)%start
      nete=dom_mt(ithr)%end
      hybrid = hybrid_create(par,ithr,NThreads)

      moisture='moist'

      if (adiabatic) then

         moisture='dry'
         if (runtype == 0) then
            do ie = nets, nete
               elem(ie)%state%q(:,:,:,:)      = 0.0_r8
               elem(ie)%derived%fq(:,:,:,:,:) = 0.0_r8
            end do
         end if

      else if (ideal_phys) then

         moisture='dry'
         if (runtype == 0) then
            do ie = nets, nete
               elem(ie)%state%lnps(:,:,:)  = log(hvcoord%ps0)
               elem(ie)%state%ps_v(:,:,:)  = hvcoord%ps0
               elem(ie)%state%phis(:,:)    = 0.0_r8
               elem(ie)%state%T(:,:,:,:)   = Tinit
               elem(ie)%state%v(:,:,:,:,:) = 0.0_r8
               elem(ie)%state%q(:,:,:,:)   = 0.0_r8
            end do
         end if

      else if (aqua_planet .and. runtype==0)  then

         do ie = nets, nete
            !          elem(ie)%state%lnps(:,:,:) = LOG(hvcoord%ps0)
            !          elem(ie)%state%ps_v(:,:,:) = hvcoord%ps0
            elem(ie)%state%phis(:,:) = 0.0_r8
         end do

      end if

      do ie = nets, nete
         elem(ie)%derived%FM = 0.0_r8
         elem(ie)%derived%FT = 0.0_r8
         elem(ie)%derived%FQ = 0.0_r8
      end do

      ! scale PS to achieve prescribed dry mass
      if (runtype == 0) then
         ! new run, scale mass to value given in namelist, if needed
         call prim_set_mass(elem, TimeLevel, hybrid, hvcoord, nets, nete)
      endif

      call prim_init2(elem, fvm, hybrid, nets, nete, TimeLevel, hvcoord)

      ! This subroutine is used to create nc_topo files, if requested
      call nctopo_util_driver(elem,hybrid,nets,nete)

#if (defined HORIZ_OPENMP)
       !$OMP END PARALLEL 
#endif
   end if

   if (inst_index == 1) then
      call write_grid_mapping(par, elem)
   end if

end subroutine dyn_init

!=============================================================================================

  subroutine dyn_run( dyn_state, rc )

    use parallel_mod,     only: par
    use prim_driver_mod,  only: prim_run, prim_run_subcycle
    use dimensions_mod,   only: nlev
    use thread_mod,       only: omp_get_thread_num, nthreads
    use time_mod,         only: tstep, nsplit
    use hybrid_mod,       only: hybrid_create

    type (dyn_export_t), intent(inout)       :: dyn_state   !  container
    type(hybrid_t) :: hybrid

    integer, intent(out)               :: rc      ! Return code
    integer ::  n
    integer :: nets, nete, ithr
    integer :: ie


    if(iam < par%nprocs) then
#if (defined HORIZ_OPENMP)
       !$OMP PARALLEL DEFAULT(SHARED), PRIVATE(ithr,nets,nete,hybrid,n)
#endif
       ithr=omp_get_thread_num()
       nets=dom_mt(ithr)%start
       nete=dom_mt(ithr)%end
       hybrid = hybrid_create(par,ithr,NThreads)

       do n=1, nsplit
          ! forward-in-time RK, with subcycling
          call prim_run_subcycle(dyn_state%elem,dyn_state%fvm,hybrid,nets,nete,&
               tstep, TimeLevel, hvcoord, n)
       end do


#if (defined HORIZ_OPENMP)
       !$OMP END PARALLEL
#endif
    end if
    rc = DYN_RUN_SUCCESS


  end subroutine dyn_run

!=============================================================================================

  subroutine dyn_final(DYN_STATE, RESTART_FILE)

    type (elem_state_t), target     :: DYN_STATE
    character(LEN=*)   , intent(IN) :: RESTART_FILE

  end subroutine dyn_final

!=============================================================================================

  subroutine read_inidat(dyn_in)

    use shr_vmath_mod,       only: shr_vmath_log
    use physconst,           only: pi
    use hycoef,              only: ps0
    use dyn_grid,            only: get_horiz_grid_dim_d, dyn_decomp
    use dyn_grid,            only: pelat_deg, pelon_deg
    use constituents,        only: cnst_name, cnst_read_iv, qmin
    use const_init,          only: cnst_init_default
    use cam_control_mod,     only: ideal_phys, aqua_planet
    use cam_initfiles,       only: initial_file_get_id, topo_file_get_id, pertlim
    use ncdio_atm,           only: infld
    use cam_history_support, only: max_fieldname_len
    use cam_grid_support,    only: cam_grid_get_local_size, cam_grid_get_gcid
    use cam_map_utils,       only: iMap

    use phys_control,        only: phys_getopts

    use parallel_mod,        only: par
    use bndry_mod,           only: bndry_exchangev
    use dimensions_mod,      only: nelemd, nlev, np, npsq
    use dof_mod,             only: putUniquePoints
    use edge_mod,            only: edgevpack, edgevunpack, InitEdgeBuffer, FreeEdgeBuffer
    use edge_mod,            only: EdgeBuffer_t
    use nctopo_util_mod,     only: nctopo_util_inidat
    use inic_analytic,       only: analytic_ic_active, analytic_ic_set_ic
    use cam_grid_support,    only: cam_grid_id, cam_grid_get_latvals, cam_grid_get_lonvals
    use dyn_tests_utils,     only: vc_moist_pressure, vc_dry_pressure

    use pio,                 only: file_desc_t, io_desc_t, pio_double, &
         pio_get_local_array_size, pio_freedecomp

    type (dyn_import_t), target, intent(inout) :: dyn_in   ! dynamics import

    type(file_desc_t), pointer :: fh_ini, fh_topo

    type(element_t), pointer :: elem(:)
    real(r8), allocatable :: tmp(:,:,:)    ! (npsp,nlev,nelemd)
    real(r8), allocatable :: phis_tmp(:,:) ! (npsp,nelemd)
    integer :: ie, k, t
    character(len=max_fieldname_len) :: fieldname
    logical :: found
    integer :: kptr, m_cnst
    type(EdgeBuffer_t) :: edge
    integer :: lsize

    integer,parameter :: pcnst = PCNST
    integer(iMap), pointer :: ldof(:) => NULL() ! Basic (2D) grid dof
    logical,       pointer :: mask(:) => NULL() ! mask based on ldof
    real(r8),  allocatable :: lat(:)
    real(r8),  allocatable :: lon(:)

    integer :: rndm_seed_sz
    integer, allocatable :: rndm_seed(:)
    real(r8) :: pertval
    integer :: i, j, indx
    real(r8), parameter :: D0_0 = 0.0_r8
    real(r8), parameter :: D0_5 = 0.5_r8
    real(r8), parameter :: D1_0 = 1.0_r8
    real(r8), parameter :: D2_0 = 2.0_r8
    real(r8), parameter :: deg2rad = pi / 180.0_r8
    ! Variables for analytic initial conditions
    integer,  allocatable            :: glob_ind(:)
    integer,  allocatable            :: m_ind(:)
    real(r8), allocatable            :: dbuf4(:,:,:,:)
    integer                          :: grid_id
    real(r8), pointer                :: latdegp(:) ! Latitudes in degrees
    real(r8), pointer                :: londegp(:) ! Longitudes in degrees
    real(r8), allocatable            :: latvals(:) ! Latitudes in radians
    real(r8), allocatable            :: lonvals(:) ! Longitudes in radians
    integer                          :: vcoord
!!!!!!!!!!!!!!! Temporary until physgrid branch is integrated !!!!!!!!!!!!
    logical, parameter :: ldry_mass_vertical_coordinates = .false.
!!!!!!!!!!!!!!! Temporary until physgrid branch is integrated !!!!!!!!!!!!


    character(len=*), parameter :: subname='READ_INIDAT'

    fh_ini  => initial_file_get_id()
    fh_topo => topo_file_get_id()

    if(iam < par%nprocs) then
      elem=> dyn_in%elem
    else
      nullify(elem)
    end if

    lsize = cam_grid_get_local_size(dyn_decomp)	

    if (lsize /= (np*np*nelemd)) then
      call endrun(trim(subname)//': mismatch in local input array size')
    end if

    if (iam < par%nprocs) then
      if(elem(1)%idxP%NumUniquePts <=0 .or. elem(1)%idxP%NumUniquePts > np*np) then
        write(iulog,*)  elem(1)%idxP%NumUniquePts
        call endrun(trim(subname)//': invalid idxP%NumUniquePts')
      end if
    end if

    allocate(phis_tmp(npsq,nelemd))
    if (associated(ldof)) then
      call endrun(trim(subname)//': ldof should not be associated')
    end if
    call cam_grid_get_gcid(dyn_decomp, ldof)
    if (associated(mask)) then
      call endrun(trim(subname)//': mask should not be associated')
    end if
    !! The mask is to indicate which tracer values should be initialized
    allocate(mask(size(ldof)))
    mask = (ldof /= 0)
    deallocate(ldof)
    nullify(ldof)
    ! We need coordinates for to initialize some variables
    grid_id = cam_grid_id('GLL')
    latdegp => cam_grid_get_latvals(grid_id)
    allocate(latvals(size(latdegp)))
    latvals(:) = latdegp(:) * deg2rad
    londegp => cam_grid_get_lonvals(grid_id)
    allocate(lonvals(size(londegp)))
    lonvals(:) = londegp(:) * deg2rad

    if (analytic_ic_active()) then
      if (ldry_mass_vertical_coordinates) then
        vcoord = vc_dry_pressure
      else
        vcoord = vc_moist_pressure
      end if
      allocate(glob_ind(npsq * nelemd))
      j = 1
      do ie = 1, nelemd
        do i = 1, npsq
          ! Create a global(ish) column index
          glob_ind(j) = elem(ie)%GlobalId
          j = j + 1
        end do
      end do

      allocate(m_ind(pcnst))
      do m_cnst = 1, pcnst
        m_ind(m_cnst) = m_cnst
      end do

      ! First, initialize all the variables, then assign
      allocate(dbuf4(npsq, nlev, nelemd, (pcnst + 4)))
      dbuf4 = 0.0_r8
      phis_tmp = 0.0_r8
      call analytic_ic_set_ic(vcoord, latvals, lonvals, glob_ind,              &
           PS=dbuf4(:,1,:,(pcnst+1)), PHIS=phis_tmp, U=dbuf4(:,:,:,(pcnst+2)), &
           V=dbuf4(:,:,:,(pcnst+3)), T=dbuf4(:,:,:,(pcnst+4)),                 &
           Q=dbuf4(:,:,:,1:pcnst), m_cnst=m_ind, mask=mask(:))
      do ie=1,nelemd
        elem(ie)%state%ps_v = 0.0_r8
        elem(ie)%state%v    = 0.0_r8
        elem(ie)%state%T    = 0.0_r8
        indx = 1
        do j = 1, np
          do i = 1, np
            ! PS
            elem(ie)%state%ps_v(i,j,1) = dbuf4(indx, 1, ie, (pcnst+1))
            ! PHIS is done assigned later
            ! U
            elem(ie)%state%v(i,j,1,:,1) = dbuf4(indx, :, ie, (pcnst+2))
            ! V
            elem(ie)%state%v(i,j,2,:,1) = dbuf4(indx, :, ie, (pcnst+3))
            ! T
            elem(ie)%state%T(i,j,:,1) = dbuf4(indx, :, ie, (pcnst+4))
            indx = indx + 1
          end do
        end do
      end do
      !
      ! tracers
      !
      do m_cnst = 1, pcnst
        do ie = 1, nelemd
          elem(ie)%state%Q(:,:,:,m_cnst) = 0.0_r8
          indx = 1
          do j = 1, np
            do i = 1, np
              elem(ie)%state%Q(i, j, :, m_cnst) = dbuf4(indx, :, ie, m_cnst)
              indx = indx + 1
            end do
          end do
        end do
      end do
      deallocate(dbuf4)
      deallocate(m_ind)
    else
      allocate(tmp(npsq,nlev,nelemd))

      fieldname = 'U'
      tmp = 0.0_r8
      call infld(fieldname, fh_ini, 'ncol', 'lev', 1, npsq,          &
           1, nlev, 1, nelemd, tmp, found, gridname='GLL')
      if(.not. found) then
        call endrun('Could not find U field on input datafile')
      end if

      do ie=1,nelemd
        elem(ie)%state%v=0.0_r8
        indx = 1
        do j = 1, np
          do i = 1, np
            elem(ie)%state%v(i,j,1,:,1) = tmp(indx,:,ie)
            indx = indx + 1
          end do
        end do
      end do

      fieldname = 'V'
      tmp = 0.0_r8
      call infld(fieldname, fh_ini, 'ncol', 'lev', 1, npsq,          &
           1, nlev, 1, nelemd, tmp, found, gridname='GLL')
      if(.not. found) then
        call endrun('Could not find V field on input datafile')
      end if

      do ie=1,nelemd
        indx = 1
        do j = 1, np
          do i = 1, np
            elem(ie)%state%v(i,j,2,:,1) = tmp(indx,:,ie)
            indx = indx + 1
          end do
        end do
      end do

      fieldname = 'T'
      tmp = 0.0_r8
      call infld(fieldname, fh_ini, 'ncol', 'lev', 1, npsq,          &
           1, nlev, 1, nelemd, tmp, found, gridname='GLL')
      if(.not. found) then
        call endrun('Could not find T field on input datafile')
      end if

      do ie=1,nelemd
        elem(ie)%state%T=0.0_r8
        indx = 1
        do j = 1, np
          do i = 1, np
            elem(ie)%state%T(i,j,:,1) = tmp(indx,:,ie)
            indx = indx + 1
          end do
        end do
      end do

      if (pertlim /= 0.0_r8) then
        if(masterproc) then
          write(iulog,*) trim(subname), ': Adding random perturbation bounded',&
               'by +/- ', pertlim, ' to initial temperature field'
        end if

        call random_seed(size=rndm_seed_sz)
        allocate(rndm_seed(rndm_seed_sz))

        do ie=1,nelemd
          ! seed random number generator based on element ID
          ! (possibly include a flag to allow clock-based random seeding)
          rndm_seed = elem(ie)%GlobalId
          call random_seed(put=rndm_seed)
          do i=1,np
            do j=1,np
              do k=1,nlev
                call random_number(pertval)
                pertval = D2_0*pertlim*(D0_5 - pertval)
                elem(ie)%state%T(i,j,k,1) = elem(ie)%state%T(i,j,k,1)*(D1_0 + pertval)
              end do
            end do
          end do
        end do

        deallocate(rndm_seed)
      end if

      do m_cnst = 1, pcnst

        found = .false.

        if(cnst_read_iv(m_cnst)) then
          tmp = 0.0_r8
          call infld(cnst_name(m_cnst), fh_ini, 'ncol', 'lev',      &
               1, npsq, 1, nlev, 1, nelemd, tmp, found, gridname='GLL')
        end if

        if(.not. found) then
          call cnst_init_default(m_cnst, latvals, lonvals, tmp, mask)
        end if

        do ie=1,nelemd
          indx = 1
          do j = 1, np
            do i = 1, np
              if (mask(indx+((ie-1)*npsq))) then
                if (analytic_ic_active()) then
                  ! Don't reset analytic IC tracer values
                  elem(ie)%state%Q(i,j,:,m_cnst) = tmp(indx,:,ie)
                else
                  elem(ie)%state%Q(i,j,:,m_cnst) = max(tmp(indx,:,ie), qmin(m_cnst))
                end if
              else
                elem(ie)%state%Q(i,j,:,m_cnst) = 0.0_r8
              end if
              indx = indx + 1
            end do
          end do
        end do
      end do

      fieldname = 'PS'
      tmp(:,1,:) = 0.0_r8
      call infld(fieldname, fh_ini, 'ncol',      &
           1, npsq, 1, nelemd, tmp(:,1,:), found, gridname='GLL')
      if(.not. found) then
        call endrun('Could not find PS field on input datafile')
      end if

      ! Check read-in data to make sure it is in the appropriate units
      if(minval(tmp(:,1,:), mask=reshape(mask, (/npsq,nelemd/))) < 10000._r8) then
        call endrun('Problem reading ps field')
      end if

      do ie=1,nelemd
        elem(ie)%state%ps_v=0.0_r8
        indx = 1
        do j = 1, np
          do i = 1, np
            elem(ie)%state%ps_v(i,j,1) = tmp(indx,1,ie)
            indx = indx + 1
          end do
        end do
      end do

      if (associated(fh_topo)) then
        fieldname = 'PHIS'
        phis_tmp(:,:) = 0.0_r8
        call infld(fieldname, fh_topo, 'ncol',      &
             1, npsq, 1, nelemd, phis_tmp(:,:), found, gridname='GLL')
        if(.not. found) then
          call endrun('Could not find PHIS field on input datafile')
        end if
      else if (.not. analytic_ic_active()) then
        phis_tmp(:,:) = 0._r8
      end if

      deallocate(tmp)
    end if ! analytic_ic_active
    deallocate(latvals)
    deallocate(lonvals)

    do ie = 1, nelemd
      elem(ie)%state%phis = 0.0_r8
      indx = 1
      do j = 1, np
        do i = 1, np
          elem(ie)%state%phis(i,j) = phis_tmp(indx,ie)
          indx = indx + 1
        end do
      end do
    end do
    deallocate(phis_tmp)

    ! once we've read all the fields we do a boundary exchange to 
    ! update the redundent columns in the dynamics
    if(iam < par%nprocs) then
      call initEdgeBuffer(par, edge, (3+pcnst)*nlev+2)
    end if
    do ie=1,nelemd
      kptr=0
      call edgeVpack(edge, elem(ie)%state%ps_v(:,:,1),1,kptr,elem(ie)%desc)
      kptr=kptr+1
      call edgeVpack(edge, elem(ie)%state%phis,1,kptr,elem(ie)%desc)
      kptr=kptr+1
      call edgeVpack(edge, elem(ie)%state%v(:,:,:,:,1),2*nlev,kptr,elem(ie)%desc)
      kptr=kptr+2*nlev
      call edgeVpack(edge, elem(ie)%state%T(:,:,:,1),nlev,kptr,elem(ie)%desc)
      kptr=kptr+nlev
      call edgeVpack(edge, elem(ie)%state%Q(:,:,:,:),nlev*pcnst,kptr,elem(ie)%desc)
    end do
    if(iam < par%nprocs) then
      call bndry_exchangeV(par,edge)
    end if
    do ie=1,nelemd
      kptr=0
      call edgeVunpack(edge, elem(ie)%state%ps_v(:,:,1),1,kptr,elem(ie)%desc)
      kptr=kptr+1
      call edgeVunpack(edge, elem(ie)%state%phis,1,kptr,elem(ie)%desc)
      kptr=kptr+1
      call edgeVunpack(edge, elem(ie)%state%v(:,:,:,:,1),2*nlev,kptr,elem(ie)%desc)
      kptr=kptr+2*nlev
      call edgeVunpack(edge, elem(ie)%state%T(:,:,:,1),nlev,kptr,elem(ie)%desc)
      kptr=kptr+nlev
      call edgeVunpack(edge, elem(ie)%state%Q(:,:,:,:),nlev*pcnst,kptr,elem(ie)%desc)
    end do

    !$omp parallel do private(ie, t, m_cnst)
    do ie=1,nelemd
      do t=2,3
        elem(ie)%state%ps_v(:,:,t)=elem(ie)%state%ps_v(:,:,1)
        elem(ie)%state%v(:,:,:,:,t)=elem(ie)%state%v(:,:,:,:,1)
        elem(ie)%state%T(:,:,:,t)=elem(ie)%state%T(:,:,:,1)
      end do
      call shr_vmath_log(elem(ie)%state%ps_v,elem(ie)%state%lnps,size(elem(ie)%state%lnps))
    end do

    if(iam < par%nprocs) then
      call FreeEdgeBuffer(edge)
    end if

    !
    ! This subroutine is used to create nc_topo files, if requested
    ! 

    if (associated(fh_topo)) then
      call nctopo_util_inidat(fh_topo, elem)
    end if

    ! Cleanup
    if (associated(ldof)) then
      deallocate(ldof)
      nullify(ldof)
    end if
    if (associated(mask)) then
      deallocate(mask)
      nullify(mask)
    end if

  end subroutine read_inidat

!=============================================================================================

  subroutine write_grid_mapping(par, elem)
    use parallel_mod,     only: parallel_t
    use element_mod, only: element_t
    use cam_pio_utils, only: cam_pio_createfile, pio_subsystem
    use pio, only: file_desc_t, pio_def_dim, var_desc_t, pio_int, pio_def_var, &
         pio_enddef, pio_closefile, pio_initdecomp, io_desc_t, pio_write_darray, &
         pio_freedecomp, pio_setdebuglevel
    use dimensions_mod, only: np, nelem, nelemd
    use dof_mod, only: createmetadata

    type(parallel_t) :: par
    type(element_t) :: elem(:)
    type(file_desc_t) :: nc
    type(var_desc_t) :: vid
    type(io_desc_t) :: iodesc
    integer :: dim1, dim2, ierr, i, j, ie, cc, base, ii, jj
    integer, parameter :: npm12 = (np-1)*(np-1)
    integer :: subelement_corners(npm12*nelemd,4)
    integer :: dof(npm12*nelemd*4)


    ! Create a CS grid mapping file for postprocessing tools

       ! write meta data for physics on GLL nodes
       call cam_pio_createfile(nc, 'SEMapping.nc', 0)
   
       ierr = pio_def_dim(nc, 'ncenters', npm12*nelem, dim1)
       ierr = pio_def_dim(nc, 'ncorners', 4, dim2)
       ierr = pio_def_var(nc, 'element_corners', PIO_INT, (/dim1,dim2/),vid)
    
       ierr = pio_enddef(nc)
       call createmetadata(par, elem, subelement_corners)

       jj=0
       do cc=0,3
          do ie=1,nelemd
             base = ((elem(ie)%globalid-1)+cc*nelem)*npm12
             ii=0
             do j=1,np-1
                do i=1,np-1
                   ii=ii+1
                   jj=jj+1
                   dof(jj) = base+ii
                end do
             end do
          end do
       end do

       call pio_initdecomp(pio_subsystem, pio_int, (/nelem*npm12,4/), dof, iodesc)

       call pio_write_darray(nc, vid, iodesc, reshape(subelement_corners,(/nelemd*npm12*4/)), ierr)
       
       call pio_freedecomp(nc, iodesc)
       
       call pio_closefile(nc)

  end subroutine write_grid_mapping

!=============================================================================================

end module dyn_comp
