module fv3_shield_cap

  !-----------------------------------------------------------------------------
  ! Basic NUOPC Model cap
  ! Interfaces the FV3-SHiELD coupled atmos-land-oml model with other
  ! NUOPC compliant model components (e.g. WW3 waves and MOM6 ocean)
  ! Following template:
  ! https://github.com/esmf-org/nuopc-app-prototypes/tree/main/AtmOcnMedProto/
  ! Reference fv3 cap for ufs:
  ! https://github.com/NOAA-EMC/fv3atm/blob/9743346431c46642958712690e2c2733763ce5de/fv3_cap.F90
  ! Reference for input files:
  ! https://ufs-weather-model.readthedocs.io/en/ufs-v1.0.0/InputsOutputs.html
  ! Reference MOM6 cap for interfacing with GFDL's FMS:
  ! https://ncar.github.io/MOM6/APIs/mom__cap_8F90_source.html
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS    => SetServices
    
  !-----------------------------------------------------------------------------
  ! add use statements for your model's initialization
  ! and run subroutines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! Used by: 
  ! https://github.com/NOAA-GFDL/SHiELD_physics/blob/main/simple_coupler/coupler_main.F90
  ! in docker container: fv3_gfsphysics/simple_coupler/coupler_main.F90
  ! program coupler_main
  !-----------------------------------------------------------------------
  !
  !   program that couples component models for the atmosphere,
  !   ocean (amip), land, and sea-ice using the exchange module. 
  !
  !-----------------------------------------------------------------------

  use time_manager_mod,  only: time_type, set_calendar_type, set_time,    &
                               set_date, days_in_month, month_name,       &
                               operator(+), operator (<), operator (>),   &
                               operator (/=), operator (/), operator (==),&
                               operator (*), THIRTY_DAY_MONTHS, JULIAN,   &
                               NOLEAP, NO_CALENDAR, date_to_string,       &
                               get_date

  use  atmos_model_mod,  only: atmos_model_init, atmos_model_end,  &
                               update_atmos_model_dynamics,        &
                               update_atmos_radiation_physics,     &
                               update_atmos_model_state,           &
                               atmos_data_type, atmos_model_restart

  use constants_mod,     only: constants_init
  use mpp_mod,           only: input_nml_file
  use fms_affinity_mod,  only: fms_affinity_init, fms_affinity_set

  use fms_mod,           only: check_nml_error,                 &
                               error_mesg, fms_init, fms_end,   &
                               write_version_number, uppercase
  use fms2_io_mod,       only: ascii_read, file_exists
  use mpp_mod,           only: mpp_init, mpp_pe, mpp_root_pe, mpp_npes, mpp_get_current_pelist, &
                               mpp_set_current_pelist, stdlog, mpp_error, NOTE, FATAL, WARNING
  use mpp_mod,           only: mpp_clock_id, mpp_clock_begin, mpp_clock_end, mpp_sync

  use mpp_domains_mod,   only: mpp_get_global_domain, mpp_global_field, CORNER
  use memutils_mod,      only: print_memuse_stats
  use sat_vapor_pres_mod,only: sat_vapor_pres_init

  use diag_manager_mod,  only: diag_manager_init, diag_manager_end, &
                               get_base_date, diag_manager_set_time_end

  use data_override_mod, only: data_override_init

  ! End insert
  !-----------------------------------------------------------------------------

  implicit none
  
  !-----------------------------------------------------------------------
  ! Insert leading instantiations from coupler_main
  !

  character(len=128) :: version = '$Id: fv3_shield_cap.F90,v 1.0 2022/07/28 23:59:59 Steve.Penny Exp $'
  character(len=128) :: tag = '$Name: main_old $'

  !---- model defined-types ----
  type (atmos_data_type) :: Atm

  ! ----- coupled model time -----
  type (time_type) :: Time_atmos, Time_init, Time_end,  &
                      Time_step_atmos, Time_step_ocean, &
                      Time_restart, Time_step_restart,  &
                      Time_start_restart, Time_restart_aux, &
                      Time_step_restart_aux, Time_start_restart_aux, &
                      Time_duration_restart_aux, Time_restart_end_aux

  integer :: num_cpld_calls, num_atmos_calls, nc, na, ret

  ! ----- coupled model initial date -----
  integer :: date_init(6)
  integer :: calendar_type = -99

  ! ----- timing flags -----
  integer :: initClock, mainClock, termClock
  integer, parameter :: timing_level = 1

  ! ----- namelist -----
  integer, dimension(6) :: current_date = (/ 0, 0, 0, 0, 0, 0 /)
  character(len=17) :: calendar = '                 '
  logical :: force_date_from_namelist = .false.  ! override restart values for date
  integer :: months=0, days=0, hours=0, minutes=0, seconds=0
  integer :: iau_offset = 0
  integer :: dt_atmos = 0
  integer :: dt_ocean = 0
  integer :: restart_days = 0
  integer :: restart_secs = 0
  integer :: restart_start_days = 0
  integer :: restart_start_secs = 0
  integer :: restart_days_aux = 0
  integer :: restart_secs_aux = 0
  integer :: restart_start_days_aux = 0
  integer :: restart_start_secs_aux = 0
  integer :: restart_duration_days_aux = 0
  integer :: restart_duration_secs_aux = 0
  integer :: atmos_nthreads = 1
  logical :: memuse_verbose = .false.
  logical :: use_hyper_thread = .false.

  namelist /coupler_nml/ current_date, calendar, force_date_from_namelist, &
                         months, days, hours, minutes, seconds, iau_offset,  &
                         dt_atmos, dt_ocean, atmos_nthreads, memuse_verbose, &
                         use_hyper_thread, restart_secs, restart_days, &
                         restart_start_secs, restart_start_days, &
                         restart_secs_aux, restart_days_aux, &
                         restart_start_secs_aux, restart_start_days_aux, &
                         restart_duration_secs_aux, restart_duration_days_aux

  ! ----- local variables -----
  character(len=32) :: timestamp
  logical :: intrm_rst, intrm_rst_1step
  
  ! End insert
  !-----------------------------------------------------------------------------
  
  private
  
  public :: SetServices
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  contains
  
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------
  !-----------------------------------------------------------------------------

  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! derive from NUOPC_Model
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! specialize model
    call NUOPC_CompSpecialize(model, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(model, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_CompSpecialize(model, specLabel=label_SetClock, &
      specRoutine=SetClock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call NUOPC_CompSpecialize(model, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !STEVE adding:
    call NUOPC_CompSpecialize(model, specLabel=label_Finalize, &
      specRoutine=Finalize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Advertise(model, rc) 
! See for reference:
! https://ncar.github.io/MOM6/APIs/mom__cap_8F90_source.html
!   use esmf,  only: esmf_timeinterval, esmf_maxstr, esmf_vmgetcurrent
!   use esmf,  only: esmf_vmget, esmf_timeget, esmf_timeintervalget, esmf_meshget
!   use esmf,  only: esmf_logerr_passthru, esmf_kind_r8, esmf_rc_val_wrong

    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State) :: importState, exportState

    ! local variables (for mpi with fms)
    type(esmf_vm)                          :: vm
    integer                                :: mpi_comm_fv3

    logical :: dodebug = .true.
    
    rc = ESMF_SUCCESS 

    ! First get the esmf/nuopc mpi information to pass to fms
    call ESMF_vmgetcurrent(vm, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_vmget(vm, mpicommunicator=mpi_comm_fv3, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !-------------------------------------------------
    ! FV3-SHiELD model initialization routines:
    !-------------------------------------------------

    ! Start insert
    if (dodebug) print *, "fv3_shield_cap:: calling fms_init(mpi_comm_fv3)..."
    call fms_init(mpi_comm_fv3)
    if (dodebug) print *, "fv3_shield_cap:: calling mpp_init(mpi_comm_fv3)..."
    call mpp_init(mpi_comm_fv3)
    if (dodebug) print *, "fv3_shield_cap:: calling mpp_clock_id..."
    initClock = mpp_clock_id( 'Initialization' )
    if (dodebug) print *, "fv3_shield_cap:: calling mpp_clock_begin..."
    call mpp_clock_begin (initClock) !nesting problem

    if (dodebug) print *, "fv3_shield_cap:: calling fms_init..."
    call fms_init(mpi_comm_fv3)
    if (dodebug) print *, "fv3_shield_cap:: calling constants_init..."
    call constants_init
    if (dodebug) print *, "fv3_shield_cap:: calling fms_affinity_init..."
    call fms_affinity_init
    if (dodebug) print *, "fv3_shield_cap:: calling fms_sat_vapor_pres_init..."
    call sat_vapor_pres_init

    if (dodebug) print *, "fv3_shield_cap:: calling coupler_init..."
    call coupler_init
    call print_memuse_stats('after coupler init')
    ! End insert

    !-------------------------------------------------
    ! ESMF-NUOPC commands:
    ! Advertise the model's import and export fields
    ! note: this is simply a way for models to 
    !       communicate the standard names of fields
    !-------------------------------------------------

    ! query for importState and exportState
    call NUOPC_ModelGet(model, importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    !----------
    ! Import
    !----------

    ! importable field: wave_induced_charnock_parameter
    call NUOPC_Advertise(importState, &
    StandardName="wave_induced_charnock_parameter", name="charno", &
    TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    ! importable field: wave_z0_roughness_length
    call NUOPC_Advertise(importState, &
    StandardName="wave_z0_roughness_length", name="z0rlen", &
    TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    !----------
    ! Export
    !----------

    ! exportable field: eastward_wind_at_10m_height
    call NUOPC_Advertise(exportState, &
    StandardName="eastward_wind_at_10m_height", name="u10m", &
    TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    ! exportable field: northward_wind_at_10m_height
    call NUOPC_Advertise(exportState, &
    StandardName="northward_wind_at_10m_height", name="v10m", &
    TransferOfferGeomObject="will provide", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out


  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Realize(model, rc) 
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: gridIn
    type(ESMF_Grid)         :: gridOut
    
    rc = ESMF_SUCCESS  

    !-----------------------------------------
    ! query for importState and exportState
    !-----------------------------------------
    call NUOPC_ModelGet(model, importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !-----------------------------------------
    ! create a Grid object for Fields
    ! https://earthsystemmodeling.org/docs/release/ESMF_8_3_0/ESMC_crefdoc/node5.html
    ! https://earthsystemmodeling.org/docs/release/ESMF_8_3_0/ESMC_crefdoc/node5.html#SECTION05051000000000000000
    !-----------------------------------------
!   gridIn = ESMF_GridCreateNoPeriDimUfrm(maxIndex=(/100, 20/), &
!     minCornerCoord=(/10._ESMF_KIND_R8, 20._ESMF_KIND_R8/), &
!     maxCornerCoord=(/100._ESMF_KIND_R8, 200._ESMF_KIND_R8/), &
!     coordSys=ESMF_COORDSYS_CART, staggerLocList=(/ESMF_STAGGERLOC_CENTER/), &
!     rc=rc)

    ! https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node5.html#SECTION050862700000000000000
    ! Create a six-tile ESMF_Grid for a Cubed Sphere grid using regular decomposition. 
    ! Each tile can have different decomposition. The grid coordinates are generated 
    ! based on the algorithm used by GEOS-5, The tile resolution is defined by tileSize.

!   gridIn = ESMF_GridCreateCubedSphereReg(tileSize=6,           &
!              regDecompPTile=1, decompflagPTile=,                        &
!              coordSys, coordTypeKind,                                &
!              deLabelList, staggerLocList,                            &
!              delayout, indexflag, name, transformArgs, rc)

    ! Use the existing grid_spec.nc mosaic to get the grid definition
    ! https://earthsystemmodeling.org/docs/release/latest/ESMF_refdoc/node5.html#SECTION050862900000000000000
    ! function ESMF_GridCreateMosaicReg(filename,regDecompPTile, decompflagPTile, &
    !    coordTypeKind, deLabelList, staggerLocList, delayout, indexflag, name, tileFilePath, rc)
    gridIn = ESMF_GridCreateMosaic(filename='INPUT/C96_mosaic.nc', name='fv3-shield-grid', tileFilePath='INPUT/', rc=rc)

    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    gridOut = gridIn ! for now out same as in

    !-----------------------------------------
    ! Get import and send export fields
    !-----------------------------------------

    !------------------
    ! importable field: wave_induced_charnock_parameter
    !------------------
    field = ESMF_FieldCreate(name="charno", grid=gridIn, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !------------------
    ! importable field: wave_z0_roughness_length
    !------------------
    field = ESMF_FieldCreate(name="z0rlen", grid=gridIn, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !------------------
    ! exportable field: eastward_wind_at_10m_height
    !------------------
    field = ESMF_FieldCreate(name="u10m", grid=gridOut, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !------------------
    ! exportable field: northward_wind_at_10m_height
    !------------------
    field = ESMF_FieldCreate(name="v10m", grid=gridOut, typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


  end subroutine

  !-----------------------------------------------------------------------------

  subroutine SetClock(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_TimeInterval)       :: stabilityTimeStep

    rc = ESMF_SUCCESS

    ! query for clock
    call NUOPC_ModelGet(model, modelClock=clock, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! initialize internal clock
    ! here: parent Clock and stability timeStep determine actual model timeStep
    !TODO: stabilityTimeStep should be read in from configuation
    !TODO: or computed from internal Grid information
    !STEVE: does this set a maximum timestep?
    !       https://earthsystemmodeling.org/docs/release/ESMF_8_3_0/NUOPC_refdoc/node4.html#SECTION000463000000000000000
    call ESMF_TimeIntervalSet(stabilityTimeStep, m=15, rc=rc) ! 15 minute maximum time steps
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetClock(model, clock, stabilityTimeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine Advance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    type(ESMF_Time)             :: currTime
    type(ESMF_TimeInterval)     :: timeStep
    character(len=160)          :: msgString

    logical :: dodebug = .true.

    rc = ESMF_SUCCESS
    
    !-----------------------------------------
    ! query the Component for its clock, importState and exportState
    !-----------------------------------------
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep

    ! Because of the way that the internal Clock was set in SetClock(),
    ! its timeStep is likely smaller than the parent timeStep. As a consequence
    ! the time interval covered by a single parent timeStep will result in
    ! multiple calls to the Advance() routine. Every time the currTime
    ! will come in by one internal timeStep advanced. This goes until the
    ! stopTime of the internal Clock has been reached.    

    !------------------
    ! Print the current time
    !------------------
    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing FV3-SHiELD ATM/LND/OML from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !------------------
    ! Write to log
    !------------------
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !------------------
    ! Get the current time and time step
    !------------------
    call ESMF_ClockGet(clock, currTime=currTime, timeStep=timeStep, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !-----------------------------------------
    ! FV3-SHiELD model advance routines:
    !-----------------------------------------
    
    ! Start insert
    if (dodebug) print *, "fv3_shield_cap:: model time stepping..."
!   Time_atmos = Time_atmos + Time_step_atmos  !STEVE: replace with NUOPC clock
    if (dodebug) print *, "fv3_shield_cap:: calling update_atmos_model_dynamics..."
    call update_atmos_model_dynamics (Atm)
    if (dodebug) print *, "fv3_shield_cap:: calling update_atmos_radiation_physics..."
    call update_atmos_radiation_physics (Atm)
    if (dodebug) print *, "fv3_shield_cap:: calling update_atmos_model_state..."
    call update_atmos_model_state (Atm)
    ! End insert


    !--- intermediate restart
    if (intrm_rst) then
      if (nc /= num_cpld_calls) then
        if (intrm_rst_1step .and. nc == 1) then
          timestamp = date_to_string (Time_atmos)
          call atmos_model_restart(Atm, timestamp)
          call coupler_res(timestamp)
        endif
        if (Time_atmos == Time_restart .or. Time_atmos == Time_restart_aux) then
          if (Time_atmos == Time_restart) then
            timestamp = date_to_string (Time_restart)
          else
            timestamp = date_to_string (Time_restart_aux)
          endif
          call atmos_model_restart(Atm, timestamp)
          call coupler_res(timestamp)
          if (Time_atmos == Time_restart) &
              Time_restart = Time_restart + Time_step_restart
          if ((restart_secs_aux > 0 .or. restart_days_aux > 0) .and. &
              Time_atmos == Time_restart_aux .and. &
              Time_restart_aux < Time_restart_end_aux) then
            Time_restart_aux = Time_restart_aux + Time_step_restart_aux
          endif
        endif
      endif
    endif

    call print_memuse_stats('after full step')

    !------------------
    ! Print the new time after model timestep
    !------------------
    call ESMF_TimePrint(currTime + timeStep, &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !------------------
    ! Write to log
    !------------------
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out


      
  end subroutine


  !-----------------------------------------------------------------------------

  subroutine Finalize(model, rc)

    ! input arguments
    type(ESMF_GridComp)  :: model
    integer, intent(out)       :: rc

    ! local variables
    character(len=*),parameter :: subname='(fv3_shield_cap::Finalize)'
    integer                    :: i, urc
    type(ESMF_VM)              :: vm
    real(kind=8)               :: MPI_Wtime, timeffs
  !
  !-----------------------------------------------------------------------------
  !*** finialize forecast

    timeffs = MPI_Wtime()
    rc = ESMF_SUCCESS
!
    call ESMF_GridCompGet(model,vm=vm,rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
            line=__LINE__, &
            file=__FILE__)) return

!   call ESMF_StateDestroy(fcstState, rc=rc)
!   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!           line=__LINE__, 
!           file=__FILE__)) return
!   call ESMF_GridCompDestroy(fcstComp, rc=rc)
!   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
!           line=__LINE__, 
!           file=__FILE__)) return
!
!   if(mype==0)print *,' wrt grid comp destroy time=',MPI_Wtime()-timeffs


    !-----------------------------------------------------------------------

    call mpp_set_current_pelist()
    call mpp_clock_end(mainClock)
    call mpp_clock_begin(termClock)

    call coupler_end
    call mpp_set_current_pelist()
    call mpp_clock_end(termClock)

    call fms_end

    !-----------------------------------------------------------------------

  end subroutine Finalize
  
  !#######################################################################

  subroutine coupler_init

    !-----------------------------------------------------------------------
    !   initialize all defined exchange grids and all boundary maps
    !   Note: This is the FMS coupler for the fv3 / land / OML components
    !-----------------------------------------------------------------------
    integer :: total_days, total_seconds, ierr, io
    integer :: n, gnlon, gnlat
    integer :: date(6), flags
    type (time_type) :: Run_length
    character(len=9) :: month
    logical :: use_namelist

    logical, allocatable, dimension(:,:) :: mask
    real,    allocatable, dimension(:,:) :: glon_bnd, glat_bnd
    character(len=:), dimension(:), allocatable :: restart_file !< Restart file saved as a string
    integer :: time_stamp_unit !< Unit of the time_stamp file
    integer :: ascii_unit  !< Unit of a dummy ascii file

    logical :: dodebug = .true.

    !-----------------------------------------------------------------------
    !----- initialization timing identifiers ----
    !-----------------------------------------------------------------------

    !----- read namelist -------
    !----- for backwards compatibilty read from file coupler.nml -----

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: reading input_nml_file..."
    read(input_nml_file, nml=coupler_nml, iostat=io)
    ierr = check_nml_error(io, 'coupler_nml')

    !----- write namelist to logfile -----
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling write_version_number..."
    call write_version_number (version, tag)
    if (mpp_pe() == mpp_root_pe()) write(stdlog(),nml=coupler_nml)

    !----- allocate and set the pelist (to the global pelist) -----
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: allocating Atm%pelist..."
    allocate( Atm%pelist  (mpp_npes()) )
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling mpp_get_current_pelist(Atm%pelist)..."
    call mpp_get_current_pelist(Atm%pelist)

    !----- read restart file -----
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: read INPUT/coupler.res (if it exists)..."
    if (file_exists('INPUT/coupler.res')) then
        call ascii_read('INPUT/coupler.res', restart_file)
        read(restart_file(1), *) calendar_type
        read(restart_file(2), *) date_init
        read(restart_file(3), *) date
        deallocate(restart_file)
    else
        force_date_from_namelist = .true.
    endif

    !----- use namelist value (either no restart or override flag on) ---

    if ( force_date_from_namelist ) then

        if ( sum(current_date) <= 0 ) then
            call error_mesg ('program coupler', 'no namelist value for current_date', FATAL)
        else
            date = current_date
        endif

        !----- override calendar type with namelist value -----

        select case( uppercase(trim(calendar)) )
        case( 'JULIAN' )
            calendar_type = JULIAN
        case( 'NOLEAP' )
            calendar_type = NOLEAP
        case( 'THIRTY_DAY' )
            calendar_type = THIRTY_DAY_MONTHS
        case( 'NO_CALENDAR' )
            calendar_type = NO_CALENDAR
        case default
            call mpp_error ( FATAL, 'COUPLER_MAIN: coupler_nml entry calendar must '// &
                                    'be one of JULIAN|NOLEAP|THIRTY_DAY|NO_CALENDAR.' )
        end select

    endif

    !--- setting affinity
!$  call fms_affinity_set('ATMOS', use_hyper_thread, atmos_nthreads)
!$  call omp_set_num_threads(atmos_nthreads)

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling set_calendar_type..."
    call set_calendar_type (calendar_type)


    !STEVE: Replace the input date/time with the NUOPC-ESMF date/time here:
    ! date = year, month, day, hour, min, sec
    ! date = 
    ! date_init = 

    !----- write current/initial date actually used to logfile file -----

    if ( mpp_pe() == mpp_root_pe() ) then
        write (stdlog(),16) date(1),trim(month_name(date(2))),date(3:6)
    endif

16 format ('  current date used = ',i4,1x,a,2i3,2(':',i2.2),' gmt')

    !-----------------------------------------------------------------------
    !------ initialize diagnostics manager ------

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling diag_manager_init..."
    call diag_manager_init (TIME_INIT=date)

    !----- always override initial/base date with diag_manager value -----

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling get_base_date..."
    call get_base_date ( date_init(1), date_init(2), date_init(3), date_init(4), date_init(5), date_init(6)  )

    !----- use current date if no base date ------

    if ( date_init(1) == 0 ) date_init = date

    !----- set initial and current time types ------

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling set_date (Time_init)..."
    Time_init  = set_date (date_init(1), date_init(2), date_init(3), date_init(4), date_init(5), date_init(6))

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling set_date (Time_atmos)..."
    Time_atmos = set_date (date(1), date(2), date(3), date(4), date(5), date(6))

    !-----------------------------------------------------------------------
    !----- compute the ending time (compute days in each month first) -----
    !-----------------------------------------------------------------------

    !   (NOTE: if run length in months then starting day must be <= 28)

    if ( months > 0 .and. date(3) > 28 )     &
        call error_mesg ('program coupler',  &
        'if run length in months then starting day must be <= 28', FATAL)

    Time_end = Time_atmos
    total_days = 0
    do n = 1, months
       total_days = total_days + days_in_month(Time_end)
       Time_end = Time_atmos + set_time (0,total_days)
    enddo

    total_days    = total_days + days
    total_seconds = hours*3600 + minutes*60 + seconds
    Run_length    = set_time (total_seconds,total_days)
    Time_end      = Time_atmos + Run_length

    !Need to pass Time_end into diag_manager for multiple thread case.
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling diag_manager_set_time_end..."
    call diag_manager_set_time_end(Time_end)

    !-----------------------------------------------------------------------
    !----- write time stamps (for start time and end time) ------
    !-----------------------------------------------------------------------

    if ( mpp_pe().EQ.mpp_root_pe() ) open(newunit = time_stamp_unit, file='time_stamp.out', status='replace', form='formatted')

    month = month_name(date(2))
    if ( mpp_pe() == mpp_root_pe() ) write (time_stamp_unit,20) date, month(1:3)

    call get_date (Time_end, date(1), date(2), date(3), date(4), date(5), date(6))
    month = month_name(date(2))
    if ( mpp_pe() == mpp_root_pe() ) write (time_stamp_unit,20) date, month(1:3)

    if ( mpp_pe().EQ.mpp_root_pe() ) close(time_stamp_unit)

20  format (6i4,2x,a3)

    !-----------------------------------------------------------------------
    !----- compute the time steps ------
    !-----------------------------------------------------------------------
    
    Time_step_atmos = set_time (dt_atmos,0)
    Time_step_ocean = set_time (dt_ocean,0)
    num_cpld_calls  = Run_length / Time_step_ocean
    num_atmos_calls = Time_step_ocean / Time_step_atmos
    Time_step_restart = set_time (restart_secs, restart_days)
    if (restart_start_secs > 0 .or. restart_start_days > 0) then
        Time_start_restart = set_time (restart_start_secs, restart_start_days)
        Time_restart = Time_atmos + Time_start_restart
    else
        Time_restart = Time_atmos + Time_step_restart
    end if
    
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling set_time(s)..."
    Time_step_restart_aux = set_time (restart_secs_aux, restart_days_aux)
    Time_duration_restart_aux = set_time (restart_duration_secs_aux, restart_duration_days_aux)
    Time_start_restart_aux = set_time (restart_start_secs_aux, restart_start_days_aux)
    Time_restart_aux = Time_atmos + Time_start_restart_aux
    Time_restart_end_aux = Time_restart_aux + Time_duration_restart_aux
    intrm_rst = .false.
    intrm_rst_1step = .false.
    if (restart_days > 0 .or. restart_secs > 0) intrm_rst = .true.
    if (intrm_rst .and. restart_start_secs == 0 .and. &
        restart_start_days == 0) intrm_rst_1step = .true.

    !-----------------------------------------------------------------------
    !------------------- some error checks ---------------------------------
    !-----------------------------------------------------------------------

    !----- initial time cannot be greater than current time -------

    if ( Time_init > Time_atmos ) call error_mesg ('program coupler',  &
                    'initial time is greater than current time', FATAL)

    !----- make sure run length is a multiple of ocean time step ------

    if ( num_cpld_calls * Time_step_ocean /= Run_length )  &
         call error_mesg ('program coupler',  &
         'run length must be multiple of ocean time step', FATAL)

    ! ---- make sure cpld time step is a multiple of atmos time step ----

    if ( num_atmos_calls * Time_step_atmos /= Time_step_ocean )  &
         call error_mesg ('program coupler',   &
         'atmos time step is not a multiple of the ocean time step', FATAL)

    !------ initialize component models ------
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling atmos_model_init..."
    call  atmos_model_init (Atm,  Time_init, Time_atmos, Time_step_atmos) !, iau_offset) !STEVE: needs more recent version of fv3 (main post 202204)

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling print_memuse_stats..."
    call print_memuse_stats('after atmos model init')

    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling mpp_get_global_domain..."
    call mpp_get_global_domain(Atm%Domain, xsize=gnlon, ysize=gnlat)
     
    allocate ( glon_bnd(gnlon+1,gnlat+1), glat_bnd(gnlon+1,gnlat+1) )
     
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling mpp_global_field (lon)..."
    call mpp_global_field(Atm%Domain, Atm%lon_bnd, glon_bnd, position=CORNER)
    if (dodebug) print *, "fv3_shield_cap::coupler_init:: calling mpp_global_field (lat)..."
    call mpp_global_field(Atm%Domain, Atm%lat_bnd, glat_bnd, position=CORNER)

!   if (.NOT.Atm%bounded_domain) then !STEVE: needs more recent version of fv3 (main post 202204)
    call data_override_init (Atm_domain_in  = Atm%domain)
!   endif
                             ! Atm_domain_in  = Atm%domain, &
                             ! Ice_domain_in  = Ice%domain, &
                             ! Land_domain_in = Land%domain )

    !-----------------------------------------------------------------------
    !---- open and close dummy file in restart dir to check if dir exists --
    if (mpp_pe() == 0 ) then !one pe should do this check only in case of a nest
        open(newunit = ascii_unit, file='RESTART/file', status='replace', form='formatted')
        close(ascii_unit,status="delete")
    endif
    !-----------------------------------------------------------------------

  end subroutine coupler_init

  !#######################################################################
   subroutine coupler_res(timestamp)
    character(len=32), intent(in) :: timestamp

    integer :: date(6)
    integer :: restart_unit !< Unit for the coupler restart file

!----- compute current date ------

      call get_date (Time_atmos, date(1), date(2), date(3),  &
                                 date(4), date(5), date(6))

!----- write restart file ------
    call mpp_set_current_pelist()
    if (mpp_pe() == mpp_root_pe())then
        open(newunit = restart_unit, file='RESTART/'//trim(timestamp)//'.coupler.res', status='replace', form='formatted')
        write(restart_unit, '(i6,8x,a)' )calendar_type, &
             '(Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)'

        write(restart_unit, '(6i6,8x,a)' )date_init, &
             'Model start time:   year, month, day, hour, minute, second'
        write(restart_unit, '(6i6,8x,a)' )date, &
             'Current model time: year, month, day, hour, minute, second'
        close(restart_unit)
    endif
   end subroutine coupler_res

!#######################################################################

   subroutine coupler_end

   integer :: date(6)
   integer :: restart_unit !< Unit for the coupler restart file
!-----------------------------------------------------------------------

      call atmos_model_end (Atm)

!----- compute current date ------

      call get_date (Time_atmos, date(1), date(2), date(3),  &
                                 date(4), date(5), date(6))

!----- check time versus expected ending time ----

      if (Time_atmos /= Time_end) call error_mesg ('program coupler',  &
              'final time does not match expected ending time', WARNING)

!----- write restart file ------
    call mpp_set_current_pelist()
    if (mpp_pe() == mpp_root_pe())then
        open(newunit = restart_unit, file='RESTART/coupler.res', status='replace', form='formatted')
        write(restart_unit, '(i6,8x,a)' )calendar_type, &
             '(Calendar: no_calendar=0, thirty_day_months=1, julian=2, gregorian=3, noleap=4)'

        write(restart_unit, '(6i6,8x,a)' )date_init, &
             'Model start time:   year, month, day, hour, minute, second'
        write(restart_unit, '(6i6,8x,a)' )date, &
             'Current model time: year, month, day, hour, minute, second'
        close(restart_unit)
    endif

!----- final output of diagnostic fields ----

   call diag_manager_end (Time_atmos)

!-----------------------------------------------------------------------

   end subroutine coupler_end

end module fv3_shield_cap
