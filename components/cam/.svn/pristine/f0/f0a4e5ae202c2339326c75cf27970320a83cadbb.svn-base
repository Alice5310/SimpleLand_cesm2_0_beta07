module ic_baroclinic
  !-----------------------------------------------------------------------
  !
  ! Purpose: Set idealized initial conditions for the Ullrich, Melvin,
  !          Jablonowski and Staniforth (QJRMS, 2014) baroclinic
  !          instability test.
  !
  !-----------------------------------------------------------------------
  use cam_logfile,         only: iulog
  use shr_kind_mod,        only: r8 => shr_kind_r8
  use cam_abortutils,      only: endrun
  use spmd_utils,          only: masterproc
  use shr_sys_mod,         only: shr_sys_flush

  use physconst, only : rair, cpair, gravit, rearth, pi, omega
  use hycoef,     only: hyai, hybi, hyam, hybm, ps0

  implicit none
  private

  real(r8), parameter :: deg2rad = pi/180.0_r8

  !=======================================================================
  !    Baroclinic wave test case parameters
  !=======================================================================
  real(r8), parameter, private :: Mvap = 0.608_r8   ! Ratio of molar mass dry air/water vapor
  real(r8), parameter, private :: psurf_moist = 100000.0_r8 !moist surface pressure

  real(r8), parameter, private ::     &
       T0E        = 310.0_r8,         & ! temperature at equatorial surface (K)
       T0P        = 240.0_r8,         & ! temperature at polar surface (K)
       B          = 2.0_r8,           & ! jet half-width parameter
       KK         = 3.0_r8,           & ! jet width parameter
       lapse      = 0.005_r8             ! lapse rate parameter

  real(r8), parameter, private ::     &
       pertu0     = 0.5_r8,           & ! SF Perturbation wind velocity (m/s)
       pertr      = 1.0_r8/6.0_r8,    & ! SF Perturbation radius (Earth radii)
       pertup     = 1.0_r8,           & ! Exp. perturbation wind velocity (m/s)
       pertexpr   = 0.1_r8,           & ! Exp. perturbation radius (Earth radii)
       pertlon    = pi/9.0_r8,        & ! Perturbation longitude
       pertlat    = 2.0_r8*pi/9.0_r8, & ! Perturbation latitude
       pertz      = 15000.0_r8,       & ! Perturbation height cap
       dxepsilon  = 1.0e-5_r8           ! Small value for numerical derivatives

  real(r8), parameter, private ::     &
       moistqlat  = 2.0_r8*pi/9.0_r8, & ! Humidity latitudinal width
       moistqp    = 34000.0_r8,       & ! Humidity vertical pressure width
       moistq0    = 0.018_r8             ! Maximum specific humidity


  integer,  parameter :: deep  = 0! Deep (1) or Shallow (0) test case
  integer,  parameter :: pertt = 0!! 0: exponential, 1: streamfunction
  real(r8), parameter :: bigx  = 1.0  ! factor for a reduced size earth
  integer,  parameter :: moist = 1 ! moist (1) or dry (0) baroclinic wave

  ! Public interface
  public :: bc_wav_set_ic

contains

  subroutine bc_wav_set_ic(vcoord,latvals, lonvals, U, V, T, PS, PHIS, &
       Q, m_cnst, mask, verbose)
    use dyn_tests_utils, only: vc_moist_pressure, vc_dry_pressure, vc_height
    use constituents,    only: cnst_name
    use const_init,      only: cnst_init_default

    !-----------------------------------------------------------------------
    !
    ! Purpose: Set baroclinic wave initial values for dynamics state variables
    !
    !-----------------------------------------------------------------------

    ! Dummy arguments
    integer, intent(in)               :: vcoord
    real(r8),           intent(in)    :: latvals(:) ! lat in degrees (ncol)
    real(r8),           intent(in)    :: lonvals(:) ! lon in degrees (ncol)
                                                    ! z_k for vccord 1)
    real(r8), optional, intent(inout) :: U(:,:)     ! zonal velocity
    real(r8), optional, intent(inout) :: V(:,:)     ! meridional velocity
    real(r8), optional, intent(inout) :: T(:,:)     ! temperature
    real(r8), optional, intent(inout) :: PS(:)      ! surface pressure
    real(r8), optional, intent(inout) :: PHIS(:)    ! surface geopotential
    real(r8), optional, intent(inout) :: Q(:,:,:)   ! tracer (ncol, lev, m)
    integer,  optional, intent(in)    :: m_cnst(:)  ! tracer indices (reqd. if Q)
    logical,  optional, intent(in)    :: mask(:)    ! Only init where .true.
    logical,  optional, intent(in)    :: verbose    ! For internal use
    ! Local variables
    logical, allocatable              :: mask_use(:)
    logical                           :: verbose_use
    integer                           :: i, k, m
    integer                           :: ncol
    integer                           :: nlev
    integer                           :: ncnst
    character(len=*), parameter       :: subname = 'BC_WAV_SET_IC'
    real(r8)                          :: ztop,ptop
    real(r8)                          :: uk,vk,Tk,qk,rhok,zk,pk !mid-level state
    real(r8)                          :: thetav,surface_geo,psurface,eta
    real(r8)                          :: wvp,zdummy,qdry
    logical                           :: lU, lV, lT, lPS, lPHIS, lQ, l3d_vars
    real(r8), allocatable             :: pdry_half(:), pwet_half(:)

    if ((vcoord == vc_moist_pressure) .or. (vcoord == vc_dry_pressure)) then
      !
      ! pressure-based vertical coordinate
      !
      ptop = hyai(1) * ps0
      if (ptop > 1.0e5_r8) then
        call endrun(subname//' ERROR: For iterate_z_given_pressure to work ptop must be less than 100hPa')
      end if
      ztop = iterate_z_given_pressure(ptop,.false.,ptop,0.0_r8,0.0_r8,-1000._r8) !Find height of top pressure surface
    else if (vcoord == vc_height) then
      !
      ! height-based vertical coordinate
      !
!      ztop=
      call endrun(subname//' ERROR: z-based vertical coordinate not coded yet')
    else
      call endrun(subname//' ERROR: vcoord value out of range')
    end if

    allocate(mask_use(size(latvals)))
    if (present(mask)) then
      if (size(mask_use) /= size(mask)) then
        call endrun(subname//': input, mask, is wrong size')
      end if
      mask_use = mask
    else
      mask_use = .true.
    end if

    if (present(verbose)) then
      verbose_use = verbose
    else
      verbose_use = .true.
    end if

    if(masterproc .and. verbose .and. present(PS)) then
      write(iulog,*) subname, ': Model top (in km) is at z= ',ztop/1000.0_r8
    end if

    ncol = size(latvals, 1)
    nlev = -1
    !
    !*******************************
    !
    ! initialize surface pressure
    !
    !*******************************
    !
    if (present(PS)) then
      if (vcoord == vc_moist_pressure) then
        where(mask_use)
          PS = psurf_moist
        end where
      else if(vcoord == vc_dry_pressure) then
        !
        ! compute dry surface pressure (subtract water vapor in coloumn)
        !
        do i=1,ncol
          if (mask_use(i)) then
            wvp = weight_of_water_vapor_given_z(0.0_r8,latvals(i),lonvals(i),ztop)
            ps(i) = psurf_moist-wvp
          end if
        end do
      endif

      if(masterproc .and. verbose_use) then
        write(iulog,*) '          PS initialized by "',subname,'"'
      end if
    end if
    !
    !*******************************
    !
    ! Initialize PHIS
    !
    !*******************************
    !
    if (present(PHIS)) then
      where(mask_use)
        PHIS = 0.0_r8
      end where
      if(masterproc .and. verbose_use) then
        write(iulog,*) '          PHIS initialized by "',subname,'"'
      end if
    end if
    !
    !*******************************
    !
    ! Initialize 3D vars
    !
    !
    !*******************************
    !
    lu = present(U)
    lv = present(V)
    lT = present(T)
    lq = present(Q)
    l3d_vars = lu .or. lv .or. lt .or.lq
    nlev = -1
    if (l3d_vars) then
      if (lu) nlev = size(U, 2)
      if (lv) nlev = size(V, 2)
      if (lt) nlev = size(T, 2)
      if (lq) nlev = size(Q, 2)
      if (lq .and. (vcoord == vc_dry_pressure)) then
        allocate(pdry_half(nlev+1))
        allocate(pwet_half(nlev+1))
      end if
      do i=1,ncol
        if (mask_use(i)) then
          if (vcoord == vc_moist_pressure) then
            psurface = psurf_moist
            wvp = -99
          else if (vcoord == vc_dry_pressure) then
            !
            ! convert surface pressure to dry
            !
            wvp = weight_of_water_vapor_given_z(0.0_r8,latvals(i),lonvals(i),ztop)
            psurface = psurf_moist-wvp
          end if

          do k=1,nlev
            pk =  hyam(k)*ps0 + hybm(k)*psurface
            call baroclinic_wave_test(moist,pk,ptop,zk,uk,vk,tk,thetav,&
                 surface_geo,rhok,qk,&
                 (vcoord==vc_dry_pressure),latvals(i),lonvals(i),ztop)
            if (lt) T(i,k)   = tk
            if (lu) U(i,k)   = uk
            if (lv) V(i,k)   = vk
            if (lq) Q(i,k,1) = qk
          end do
          if (lq .and. (vcoord==vc_dry_pressure) .and. (moist/= 0)) then
            !
            ! for dry pressure vertical coordinate
            !
            do k=1,nlev+1
              pdry_half(k) =  hyai(k)*ps0 + hybi(k)*psurf_moist!psurface
              !Find height of pressu!re surface
              zdummy = iterate_z_given_pressure(pdry_half(k),.true.,ptop,latvals(i),lonvals(i),ztop)
              pwet_half(k) = moist_pressure_given_z(zdummy,latvals(i),lonvals(i))
            end do
            do k=1,nlev
              qdry =((pwet_half(k+1)-pwet_half(k))/(pdry_half(k+1)-pdry_half(k)))-1.0_r8
              !
              ! CAM expects water vapor mixing ratio to be wet - convert from dry to wet:
              !
              Q(i,k,1) = qdry / (1.0_r8 + qdry)
              Q(i,k,1) = max(1.0e-12_r8, Q(i,k,1))
            end do
          end if
        end if
      end do
      if(lu .and. masterproc.and. verbose_use)  write(iulog,*) '          U initialized by "',subname,'"'
      if(lv .and. masterproc.and. verbose_use)  write(iulog,*) '          V initialized by "',subname,'"'
      if(lt .and. masterproc.and. verbose_use)  write(iulog,*) '          T initialized by "',subname,'"'
      if(lq .and. masterproc.and. verbose_use)  write(iulog,*) &
           '          ', trim(cnst_name(m_cnst(1))), ' initialized by "',subname,'"'
    end if

    if (lq) then
      ncnst = size(m_cnst, 1)
      if ((vcoord == vc_moist_pressure) .or. (vcoord == vc_dry_pressure)) then
        do m = 2, ncnst
          call cnst_init_default(m_cnst(m), latvals, lonvals, Q(:,:,m_cnst(m)),&
               mask=mask_use, verbose=verbose_use, notfound=.false.)
#if 0
          do k = 1, nlev
            do i=1,ncol
              if (mask_use(i)) then
                Q(i,k,m_cnst(m)) = test_func(latvals(i),lonvals(i), k, m)
              end if
            end do
          end do
          if(masterproc .and. verbose_use) then
            write(iulog,*) '          ', trim(cnst_name(m_cnst(m))), ' initialized by "',subname,'"'
          end if
#endif
        end do
      end if
    end if

    deallocate(mask_use)

  end subroutine bc_wav_set_ic

  !-----------------------------------------------------------------------
  !  SUBROUTINE baroclinic_wave_test(
  !    moist,pertt,X,lon,lat,p,z,zcoords,u,v,w,t,phis,ps,rho,q)
  !
  !  Options:
  !    moist    include moisture (1 = yes or 0 = no)
  !    pertt    type of perturbation (0 = exponential, 1 = stream function)
  !        X    Earth scaling factor1
  !
  !  Given a point specified by: 
  !      lon    longitude (radians) 
  !      lat    latitude (radians) 
  !      p/z    pressure (Pa) / height (m)
  !  zcoords    1 if z is specified, 0 if p is specified
  !
  !  the functions will return:
  !        p    pressure if z is specified and zcoords = 1 (Pa)
  !        u    zonal wind (m s^-1)
  !        v    meridional wind (m s^-1)
  !        t    temperature (K)
  !   thetav    virtual potential temperature (K)
  !     phis    surface geopotential (m^2 s^-2)
  !       ps    surface pressure (Pa)
  !      rho    density (kj m^-3)
  !        q    water vapor mixing ratio (kg/kg)
  !
  !
  !  Author: Paul Ullrich
  !          University of California, Davis
  !          Email: paullrich@ucdavis.edu
  !
  !-----------------------------------------------------------------------


  SUBROUTINE baroclinic_wave_test(moist,p,ptop,z,u,v,temp,thetav,phis,rho,q,&
       ldry_mass_vertical_coordinates,lat,lon,ztop)
    IMPLICIT NONE
    
    !-----------------------------------------------------------------------
    !     input/output params parameters at given location
    !-----------------------------------------------------------------------
    integer, INTENT(IN)  :: &
         moist        ! Moist (1) or Dry (0) test case
    
    
    real(r8), INTENT(IN) :: &
         p            ,&! Pressure at the full model level (Pa)
         ptop         ,&!
         lat          ,&! latitude
         lon          ,&! longitude
         ztop           ! model top height

    logical, intent(in) :: ldry_mass_vertical_coordinates
    
    real(r8), INTENT(OUT) :: &
         u,          & ! Zonal wind (m s^-1)
         v,          & ! Meridional wind (m s^-1)
         temp,       & ! Temperature (K)
         thetav,     & ! Virtual potential temperature (K)
         phis,       & ! Surface Geopotential (m^2 s^-2)
!         ps,         & ! Surface Pressure (Pa)
         rho,        & ! density (kg m^-3)
         q,          & ! water vapor mixing ratio (kg/kg)
         z             ! Altitude (m)


    z = iterate_z_given_pressure(p,ldry_mass_vertical_coordinates,ptop,lat,lon,ztop) !Find height of pressure surface
    call uv_given_z(z,u,v,lat,lon)
    temp = Tv_given_z(z,lat,lon)
    phis = 0.d0
    if (moist .eq. 1) then
       q = qv_given_moist_pressure(moist_pressure_given_z(z,lat,lon),lat,lon)       
    else
       q = 0.d0                  ! dry
    end if
    !
    ! Convert virtual temperature to temperature
    !
    temp = temp / (1.d0 + Mvap * q)
    rho = p / (Rair * temp * (1.d0 + 0.61d0 * q))
    thetav = temp * (1.d0 + 0.61d0 * q) * (psurf_moist / p)**(Rair / cpair)
!    if (ldry_mass_vertical_coordinates) then
!       q=q/(1-q)! CAM expects water vapor to be 'wet' mixing ratio so do not convert to dry
!    end if
  END SUBROUTINE baroclinic_wave_test

  real(r8) FUNCTION iterate_z_given_pressure(p,ldry_mass_vertical_coordinates,ptop,lat,lon,ztop)
    implicit none
    real(r8), INTENT(IN)  :: &
         p,              &! Pressure (Pa)
         ptop,&! Pressure (Pa)
         lat,&! latitude
         lon,&! longitude
         ztop

    logical, INTENT(IN)  :: ldry_mass_vertical_coordinates

    integer :: ix

    real(r8) :: z0, z1, z2
    real(r8) :: p0, p1, p2
    z0 = 0.0_r8
    z1 = 10000.0_r8

    if (ldry_mass_vertical_coordinates) then
       p0 = weight_of_dry_air_given_z(z0,ptop,lat,lon,ztop)
       p1 = weight_of_dry_air_given_z(z1,ptop,lat,lon,ztop)
    else
       p0 =  moist_pressure_given_z(z0,lat,lon)
       p1 =  moist_pressure_given_z(z1,lat,lon)
    endif

    DO ix = 1, 100
       z2 = z1 - (p1 - p) * (z1 - z0) / (p1 - p0)
       if (ldry_mass_vertical_coordinates) then
          p2 = weight_of_dry_air_given_z(z2,ptop,lat,lon,ztop)
       else
          p2 = moist_pressure_given_z(z2,lat,lon)
       end if

       IF (ABS((p2 - p)/p) < 1.0e-13_r8) THEN
          EXIT
       END IF

       z0 = z1
       p0 = p1

       z1 = z2
       p1 = p2
    END DO
    if (ix==101) then
      call endrun('iteration did not converge in iterate_z_given_pressure')
    end if
    iterate_z_given_pressure = z2
  END FUNCTION iterate_z_given_pressure

  real(r8) FUNCTION moist_pressure_given_z(z,lat,lon)
    IMPLICIT NONE
    real(r8), INTENT(IN) :: z,lat,lon
    real(r8) :: aref, omegaref
    real(r8) :: T0, constA, constB, constC, constH, scaledZ
    real(r8) :: tau1, tau2, inttau1, inttau2
    real(r8) :: rratio, inttermT,pwet,wvp
    !--------------------------------------------
    ! Constants
    !--------------------------------------------
    aref = rearth / bigX
    omegaref = omega * bigX

    T0 = 0.5_r8 * (T0E + T0P)
    constA = 1.0_r8 / lapse
    constB = (T0 - T0P) / (T0 * T0P)
    constC = 0.5_r8 * (KK + 2.0_r8) * (T0E - T0P) / (T0E * T0P)
    constH = Rair * T0 / gravit

    scaledZ = z / (B * constH)

    !--------------------------------------------
    !    tau values
    !--------------------------------------------
    tau1 = constA * lapse / T0 * exp(lapse * z / T0) &
         + constB * (1.0_r8 - 2.0_r8 * scaledZ**2) * exp(- scaledZ**2)
    tau2 = constC * (1.0_r8 - 2.0_r8 * scaledZ**2) * exp(- scaledZ**2)

    inttau1 = constA * (exp(lapse * z / T0) - 1.0_r8) &
         + constB * z * exp(- scaledZ**2)
    inttau2 = constC * z * exp(- scaledZ**2)
    !--------------------------------------------
    !    radius ratio
    !--------------------------------------------
    if (deep .eq. 0) then
       rratio = 1.0_r8
    else
       rratio = (z + aref) / aref;
    end if

    !--------------------------------------------
    !    interior term on temperature expression
    !--------------------------------------------
    inttermT = (rratio * cos(lat))**KK &
         - KK / (KK + 2.0_r8) * (rratio * cos(lat))**(KK + 2.0_r8)

    !--------------------------------------------
    !    hydrostatic pressure
    !--------------------------------------------
    moist_pressure_given_z = psurf_moist * exp(- gravit / Rair * (inttau1 - inttau2 * inttermT))
  END FUNCTION moist_pressure_given_z

  real(r8) FUNCTION Tv_given_z(z,lat,lon)
    IMPLICIT NONE
    real(r8), INTENT(IN) :: z, lat, lon
    real(r8) :: aref, omegaref
    real(r8) :: T0, constA, constB, constC, constH, scaledZ
    real(r8) :: tau1, tau2, inttau1, inttau2
    real(r8) :: rratio, inttermT
    !--------------------------------------------
    ! Constants
    !--------------------------------------------
    aref = rearth / bigX
    omegaref = omega * bigX

    T0 = 0.5_r8 * (T0E + T0P)
    constA = 1.0_r8 / lapse
    constB = (T0 - T0P) / (T0 * T0P)
    constC = 0.5_r8 * (KK + 2.0_r8) * (T0E - T0P) / (T0E * T0P)
    constH = Rair * T0 / gravit

    scaledZ = z / (B * constH)

    !--------------------------------------------
    !    tau values
    !--------------------------------------------
    tau1 = constA * lapse / T0 * exp(lapse * z / T0) &
         + constB * (1.0_r8 - 2.0_r8 * scaledZ**2) * exp(- scaledZ**2)
    tau2 = constC * (1.0_r8 - 2.0_r8 * scaledZ**2) * exp(- scaledZ**2)

    inttau1 = constA * (exp(lapse * z / T0) - 1.0_r8) &
         + constB * z * exp(- scaledZ**2)
    inttau2 = constC * z * exp(- scaledZ**2)

    !--------------------------------------------
    !    radius ratio
    !--------------------------------------------
    if (deep .eq. 0) then
       rratio = 1.0_r8
    else
       rratio = (z + aref) / aref;
    end if

    !--------------------------------------------
    !    interior term on temperature expression
    !--------------------------------------------
    inttermT = (rratio * cos(lat))**KK &
         - KK / (KK + 2.0_r8) * (rratio * cos(lat))**(KK + 2.0_r8)

    !--------------------------------------------
    !    temperature
    !--------------------------------------------
    Tv_given_z = 1.0_r8 / (rratio**2 * (tau1 - tau2 * inttermT))
  END FUNCTION Tv_given_z

  SUBROUTINE uv_given_z(z,u,v,lat,lon)
    IMPLICIT NONE
    real(r8), INTENT(IN)  :: z, lat, lon
    real(r8), INTENT(OUT) :: u,v
    real(r8) :: aref, omegaref
    real(r8) :: T0, constH, constC, scaledZ, inttau2, rratio
    real(r8) :: inttermU, bigU, rcoslat, omegarcoslat
    !------------------------------------------------
    !   Compute test case constants
    !------------------------------------------------
    aref = rearth / bigx
    omegaref = omega * bigx

    T0 = 0.5_r8 * (T0E + T0P)

    constH = Rair * T0 / gravit

    constC = 0.5_r8 * (KK + 2.0_r8) * (T0E - T0P) / (T0E * T0P)

    scaledZ = z / (B * constH)

    inttau2 = constC * z * exp(- scaledZ**2)

    ! radius ratio
    if (deep .eq. 0) then
       rratio = 1.0_r8
    else
       rratio = (z + aref) / aref;
    end if
    !-----------------------------------------------------
    !   Initialize velocity field
    !-----------------------------------------------------
    inttermU = (rratio * cos(lat))**(KK - 1.0_r8) - (rratio * cos(lat))**(KK + 1.0_r8)
    bigU = gravit / aref * KK * inttau2 * inttermU * Tv_given_z(z,lat,lon)
    if (deep .eq. 0) then
       rcoslat = aref * cos(lat)
    else
       rcoslat = (z + aref) * cos(lat)
    end if

    omegarcoslat = omegaref * rcoslat

    u = - omegarcoslat + sqrt(omegarcoslat**2 + rcoslat * bigU)
    v = 0.0_r8

    !-----------------------------------------------------
    !   Add perturbation to the velocity field
    !-----------------------------------------------------
!    if (.false.) then !xxxx
    ! Exponential type
    if (pertt .eq. 0) then
       u = u + evaluate_exponential(z,lat,lon)

       ! Stream function type
    elseif (pertt .eq. 1) then
       u = u - 1.0_r8 / (2.0_r8 * dxepsilon) *                       &
            ( evaluate_streamfunction(lon, lat + dxepsilon, z)    &
            - evaluate_streamfunction(lon, lat - dxepsilon, z))

       v = v + 1.0_r8 / (2.0_r8 * dxepsilon * cos(lat)) *            &
            ( evaluate_streamfunction(lon + dxepsilon, lat, z)    &
            - evaluate_streamfunction(lon - dxepsilon, lat, z))
    end if
!    endif!xxx
  END SUBROUTINE uv_given_z

  !-----------------------------------------------------------------------
  !    Exponential perturbation function
  !-----------------------------------------------------------------------
  real(r8) FUNCTION evaluate_exponential(z,lat,lon)
    real(r8), INTENT(IN)  :: &
         z,&! Altitude (meters)
         lat,lon

    real(r8) :: greatcircler, perttaper

    ! Great circle distance
    greatcircler = 1.0_r8 / pertexpr &
         * acos(sin(pertlat) * sin(lat) + cos(pertlat) * cos(lat) * cos(lon - pertlon))

    ! Vertical tapering of stream function
    if (z < pertz) then
       perttaper = 1.0_r8 - 3.0_r8 * z**2 / pertz**2 + 2.0_r8 * z**3 / pertz**3
    else
       perttaper = 0.0_r8
    end if

    ! Zonal velocity perturbation
    if (greatcircler < 1.0_r8) then
       evaluate_exponential = pertup * perttaper * exp(- greatcircler**2)
    else
       evaluate_exponential = 0.0_r8
    end if

  END FUNCTION evaluate_exponential

  !-----------------------------------------------------------------------
  !    Stream function perturbation function
  !-----------------------------------------------------------------------
  real(r8) FUNCTION evaluate_streamfunction(z,lon_local,lat_local)

    real(r8), INTENT(IN)  :: &
         lon_local, lat_local,&
         z             ! Altitude (meters)

    real(r8) :: greatcircler, perttaper, cospert

    ! Great circle distance
    greatcircler = 1.0_r8 / pertr &
         * acos(sin(pertlat) * sin(lat_local) + cos(pertlat) * cos(lat_local) * cos(lon_local - pertlon))

    ! Vertical tapering of stream function
    if (z < pertz) then
       perttaper = 1.0_r8 - 3.0_r8 * z**2 / pertz**2 + 2.0_r8 * z**3 / pertz**3
    else
       perttaper = 0.0_r8
    end if

    ! Horizontal tapering of stream function
    if (greatcircler < 1.0_r8) then
       cospert = cos(0.5_r8 * pi * greatcircler)
    else
       cospert = 0.0_r8
    end if

    evaluate_streamfunction = &
         (- pertu0 * pertr * perttaper * cospert**4)

  END FUNCTION evaluate_streamfunction

  real(r8) FUNCTION qv_given_moist_pressure(pwet,lat,lon)
    implicit none
    real(r8), INTENT(IN)  :: pwet, lat, lon

    real(r8)  :: eta
    if (moist==0) then
      qv_given_moist_pressure = 0.0_r8
    else
      eta = pwet/psurf_moist
      if (eta > 0.1_r8) then  ! intialize q if p > 100 hPa
        qv_given_moist_pressure = moistq0 * exp(- (lat/moistqlat)**4)          &
             * exp(- ((eta-1.0_r8)*psurf_moist/moistqp)**2)
      else
        qv_given_moist_pressure = 1.0e-12_r8 ! above 100 hPa set q to 1e-12 to avoid supersaturation
      endif
    end if
  END FUNCTION qv_given_moist_pressure

  real(r8) FUNCTION weight_of_water_vapor_given_z(z,lat, lon,ztop)
    implicit none
    real(r8), INTENT(IN)  :: z,lat, lon, ztop
    real (r8)  :: dx,xm,xr,gaussw(10),gaussx(10),integral, tmp1, tmp2
    real(r8)   :: temp, rho, qv, pressure, z1, z2, Tv,pwet, ztmp
    integer   :: jgw
    SAVE gaussw,gaussx
    DATA gaussw/0.1527533871307258_r8,0.1491729864726037_r8,0.1420961093183820_r8,0.1316886384491766_r8,0.1181945319615184_r8,&
         0.1019301198172404_r8,0.0832767415767048_r8,0.0626720483341091_r8,0.0406014298003869_r8,0.0176140071391521_r8/
    DATA gaussx/0.0765265211334973_r8,0.2277858511416451_r8,0.3737060887154195_r8,0.5108670019508271_r8,0.6360536807265150_r8,&
         0.7463319064601508_r8,0.8391169718222188_r8,0.9122344282513259_r8,0.9639719272779138_r8,0.9931285991850949_r8/

    if (moist==0) then
      !
      ! dry case
      !
      weight_of_water_vapor_given_z = 0.0_r8
    else
      z1=z
      z2=ztop
      xm=0.5_r8*(z1+z2)
      xr=0.5_r8*(z2-z1)
      integral=0
      do jgw=1,10
        dx=xr*gaussx(jgw)
        ztmp=xm+dx
        pwet = moist_pressure_given_z(ztmp,lat,lon); qv= qv_given_moist_pressure(pwet,lat,lon);Tv= Tv_given_z(ztmp,lat,lon)
        tmp1=gravit*pwet*qv/(Rair*Tv)

        ztmp=xm-dx
        pwet = moist_pressure_given_z(ztmp,lat,lon); qv= qv_given_moist_pressure(pwet,lat,lon);Tv= Tv_given_z(ztmp,lat,lon)
        tmp2=gravit*pwet*qv/(Rair*Tv)
        integral=integral+gaussw(jgw)*(tmp1+tmp2)
      enddo
      integral=xr*integral    ! Scale the answer to the range of integration.

      weight_of_water_vapor_given_z = integral
    end if
  end FUNCTION weight_of_water_vapor_given_z


  real(r8) FUNCTION weight_of_dry_air_given_z(z,ptop,lat,lon,ztop)
    implicit none
    real (r8), INTENT(IN)  :: z,ptop, lat, lon, ztop
    real (r8)  :: dx,xm,xr,gaussw(10),gaussx(10),integral, tmp1, tmp2
    real(r8)   :: temp, rho, qv, pressure, z1, z2, Tv,pwet, ztmp
    integer    :: jgw
    SAVE gaussw,gaussx
    DATA gaussw/0.1527533871307258_r8,0.1491729864726037_r8,0.1420961093183820_r8,0.1316886384491766_r8,0.1181945319615184_r8,&
         0.1019301198172404_r8,0.0832767415767048_r8,0.0626720483341091_r8,0.0406014298003869_r8,0.0176140071391521_r8/
    DATA gaussx/0.0765265211334973_r8,0.2277858511416451_r8,0.3737060887154195_r8,0.5108670019508271_r8,0.6360536807265150_r8,&
         0.7463319064601508_r8,0.8391169718222188_r8,0.9122344282513259_r8,0.9639719272779138_r8,0.9931285991850949_r8/

    z1=z
    z2=ztop
    xm=0.5*(z1+z2)
    xr=0.5*(z2-z1)
    integral=0
    do jgw=1,10
       dx=xr*gaussx(jgw)
       ztmp=xm+dx
       pwet = moist_pressure_given_z(ztmp,lat,lon); qv= qv_given_moist_pressure(pwet,lat,lon);Tv= Tv_given_z(ztmp,lat,lon)
       tmp1=gravit*pwet*(1-qv)/(Rair*Tv)

       ztmp=xm-dx
       pwet = moist_pressure_given_z(ztmp,lat,lon); qv= qv_given_moist_pressure(pwet,lat,lon);Tv= Tv_given_z(ztmp,lat,lon)
       tmp2=gravit*pwet*(1-qv)/(Rair*Tv)
       integral=integral+gaussw(jgw)*(tmp1+tmp2)
    enddo
    integral=xr*integral    ! Scale the answer to the range of integration.

    weight_of_dry_air_given_z = integral+ptop
  end FUNCTION weight_of_dry_air_given_z

  ! Some simple analytic functions
  ! All functions multiplied by factor (default 1.0)
  function test_func(lat, lon, k, funcnum) result(fout)
    use shr_sys_mod,     only: shr_sys_flush

    real(r8), intent(in) :: lon
    real(r8), intent(in) :: lat
    integer,  intent(in) :: k
    integer,  intent(in) :: funcnum
    real(r8)             :: fout
    real(r8)             :: lon1,lat1,R0,Rg1,Rg2,lon2,lat2,cl,cl2
    real(r8)             :: eta_c

    real(r8)             :: radius      = 10.0_r8 ! radius of the perturbation
    real(r8)             :: perturb_lon = 20.0_r8 ! longitudinal position, 20E
    real(r8)             :: perturb_lat = 40.0_r8 ! latitudinal position, 40N
    real(r8)             :: cos_tmp, sin_tmp, eta

    select case(funcnum)
    case(4)
      !
      !   Non-smooth scalar field (slotted cylinder)
      !
      R0 = 0.5_r8
      lon1 = 4.0_r8 * PI / 5.0_r8
      lat1 = 0.0_r8
      Rg1 = acos(sin(lat1)*sin(lat)+cos(lat1)*cos(lat)*cos(lon-lon1))
      lon2 = 6.0_r8 * PI / 5.0_r8
      lat2 = 0.0_r8
      Rg2 = acos(sin(lat2)*sin(lat)+cos(lat2)*cos(lat)*cos(lon-lon2))

      if ((Rg1 <= R0) .AND. (abs(lon-lon1) >= R0/6)) then
        fout = 2.0_r8
      elseif ((Rg2 <= R0) .AND. (abs(lon-lon2) >= R0/6)) then
        fout = 2.0_r8
      elseif ((Rg1 <= R0) .AND. (abs(lon-lon1) < R0/6) &
           .AND. (lat-lat1 < -5.0_r8*R0/12.0_r8)) then
        fout = 2.0_r8
      elseif ((Rg2 <= R0) .AND. (abs(lon-lon2) < R0/6) &
           .AND. (lat-lat2 > 5.0_r8*R0/12.0_r8)) then
        fout = 2.0_r8
      else
        fout = 1.0_r8
      endif
    case(5)
      !
      ! Smooth Gaussian "ball"
      !
      R0    = 10.0_r8           ! radius of the perturbation
      lon1  = 20.0_r8*deg2rad   ! longitudinal position, 20E
      lat1  = 40.0_r8 *deg2rad  ! latitudinal position, 40N
      eta_c = 0.6_r8
      sin_tmp = SIN(lat1)*SIN(lat)
      cos_tmp = COS(lat1)*COS(lat)
      Rg1 = ACOS( sin_tmp + cos_tmp*COS(lon-lon1) )    ! great circle distance
      eta =  (hyam(k)*ps0 + hybm(k)*psurf_moist)/psurf_moist
      fout = EXP(- ((Rg1*R0)**2 + ((eta-eta_c)/0.1_r8)**2))
      IF (ABS(fout) < 1.0E-8) fout = 0.0_r8
    case(6)
      !
      !
      !
      fout = 0.5_r8 * ( tanh( 3.0_r8*abs(lat)-pi ) + 1.0_r8)
    case(7)
      fout = 1.0e-8_r8
    case(8)
      !
      ! approximately Y^2_2 spherical harmonic
      !
      fout = 0.5_r8 + 0.5_r8*(cos(lat)*cos(lat)*cos(2.0_r8*lon))
    case(9)
      !
      ! approximately Y32_16 spherical harmonic
      !
      fout = 0.5_r8 + 0.5_r8*(cos(16*lon)*(sin(2_r8*lat)**16))
    case(10)
      fout = 2.0_r8 + lat
    case(11)
      fout = 2.0_r8 + cos(lon)
    case default
      call endrun("Illegal funcnum_arg in test_func")
    end select
  end function test_func

end module ic_baroclinic
