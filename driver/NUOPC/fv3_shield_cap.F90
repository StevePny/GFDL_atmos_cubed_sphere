module FV3_SHiELD_CAP

  !-----------------------------------------------------------------------------
  ! Basic NUOPC Model cap
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Model, &
    model_routine_SS    => SetServices, &
    model_label_Advance => label_Advance
    
  !-----------------------------------------------------------------------------
  ! add use statements for your model's initialization
  ! and run subroutines
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------
  ! Used by: fv3_gfsphysics/simple_coupler/coupler_main.F90
  ! program coupler_main
  !-----------------------------------------------------------------------
  !
  !   program that couples component models for the atmosphere,
  !   ocean (amip), land, and sea-ice using the exchange module. 
  !
  !-----------------------------------------------------------------------

!  use time_manager_mod,  only: time_type, set_calendar_type, set_time,    &
!                             set_date, days_in_month, month_name,       &
!                             operator(+), operator (<), operator (>),   &
!                             operator (/=), operator (/), operator (==),&
!                             operator (*), THIRTY_DAY_MONTHS, JULIAN,   &
!                             NOLEAP, NO_CALENDAR, date_to_string,       &
!                             get_date

!  use  atmos_model_mod,  only: atmos_model_init, atmos_model_end,  &
!                             update_atmos_model_dynamics,        &
!                             update_atmos_radiation_physics,     &
!                             update_atmos_model_state,           &
!                             atmos_data_type, atmos_model_restart

!  use constants_mod,     only: constants_init
!  use mpp_mod,           only: input_nml_file
!  use fms_affinity_mod,  only: fms_affinity_init, fms_affinity_set

!  use       fms_mod,     only: check_nml_error,                 &
!                             error_mesg, fms_init, fms_end,   &
!                             write_version_number, uppercase
!  use fms2_io_mod,       only: ascii_read, file_exists
!  use mpp_mod,           only: mpp_init, mpp_pe, mpp_root_pe, mpp_npes, mpp_get_current_pelist, &
!                             mpp_set_current_pelist, stdlog, mpp_error, NOTE, FATAL, WARNING
!  use mpp_mod,           only: mpp_clock_id, mpp_clock_begin, mpp_clock_end, mpp_sync

!  use mpp_domains_mod,   only: mpp_get_global_domain, mpp_global_field, CORNER
!  use memutils_mod,      only: print_memuse_stats
!  use sat_vapor_pres_mod,only: sat_vapor_pres_init

!  use  diag_manager_mod, only: diag_manager_init, diag_manager_end, &
!                             get_base_date, diag_manager_set_time_end

!  use data_override_mod, only: data_override_init

  ! End insert
  !-----------------------------------------------------------------------------

  implicit none
  
  !-----------------------------------------------------------------------
  ! Insert leading instantiations from coupler_main
  !

!  character(len=128) :: version = '$Id: coupler_main.F90,v 19.0.4.1.2.3 2014/09/09 23:51:59 Rusty.Benson Exp $'
!  character(len=128) :: tag = '$Name: ulm_201505 $'

  !-----------------------------------------------------------------------
  !---- model defined-types ----

!  type (atmos_data_type) :: Atm

  !-----------------------------------------------------------------------
  ! ----- coupled model time -----

!  type (time_type) :: Time_atmos, Time_init, Time_end,  &
!                       Time_step_atmos, Time_step_ocean, &
!                       Time_restart, Time_step_restart
!  integer :: num_cpld_calls, num_atmos_calls, nc, na, ret

  ! ----- coupled model initial date -----

!  integer :: date_init(6)
!  integer :: calendar_type = -99

  ! ----- timing flags -----

!  integer :: initClock, mainClock, termClock
!  integer, parameter :: timing_level = 1

  ! ----- namelist -----
!  integer, dimension(6) :: current_date = (/ 0, 0, 0, 0, 0, 0 /)
!  character(len=17) :: calendar = '                 '
!  logical :: force_date_from_namelist = .false.  ! override restart values for date
!  integer :: months=0, days=0, hours=0, minutes=0, seconds=0
!  integer :: dt_atmos = 0
!  integer :: dt_ocean = 0
!  integer :: restart_days = 0
!  integer :: restart_secs = 0
!  integer :: atmos_nthreads = 1
!  logical :: memuse_verbose = .false.
!  logical :: use_hyper_thread = .false.

!  namelist /coupler_nml/ current_date, calendar, force_date_from_namelist, &
!                          months, days, hours, minutes, seconds,  &
!                          dt_atmos, dt_ocean, atmos_nthreads, memuse_verbose, &
!                          use_hyper_thread, restart_secs, restart_days

  ! ----- local variables -----
!  character(len=32) :: timestamp
!  logical :: intrm_rst
  
  ! End insert
  !-----------------------------------------------------------------------------
  
  private
  
  public :: SetServices
  
  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------
  
  subroutine SetServices(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS
    
    ! the NUOPC model component will register the generic methods
    call NUOPC_CompDerive(model, model_routine_SS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! set entry point for methods that require specific implementation
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv04p1"/), userRoutine=AdvertiseFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSetEntryPoint(model, ESMF_METHOD_INITIALIZE, &
      phaseLabelList=(/"IPDv04p3"/), userRoutine=RealizeFields, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    ! attach specializing method(s)
    call NUOPC_CompSpecialize(model, specLabel=model_label_Advance, &
      specRoutine=ModelAdvance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine AdvertiseFields(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS 
    
    ! Eventually, you will advertise your model's import and
    ! export fields in this phase.  For now, however, call
    ! your model's initialization routine(s).
    
    ! call my_model_init()

    ! Start insert
!    call fms_init()
!    call mpp_init()
!    initClock = mpp_clock_id( 'Initialization' )
!    call mpp_clock_begin (initClock) !nesting problem

!    call fms_init
!    call constants_init
!    call fms_affinity_init
!    call sat_vapor_pres_init

!    call coupler_init
!    call print_memuse_stats('after coupler init')
    ! End insert
    
  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine RealizeFields(model, importState, exportState, clock, rc)
    type(ESMF_GridComp)  :: model
    type(ESMF_State)     :: importState, exportState
    type(ESMF_Clock)     :: clock
    integer, intent(out) :: rc
    
    rc = ESMF_SUCCESS  
    
    ! Eventually, you will realize your model's fields here,
    ! but leave empty for now.

  end subroutine
  
  !-----------------------------------------------------------------------------

  subroutine ModelAdvance(model, rc)
    type(ESMF_GridComp)  :: model
    integer, intent(out) :: rc
    
    ! local variables
    type(ESMF_Clock)              :: clock
    type(ESMF_State)              :: importState, exportState

    rc = ESMF_SUCCESS
    
    ! query the Component for its clock, importState and exportState
    call NUOPC_ModelGet(model, modelClock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MODEL ADVANCES: currTime -> currTime + timeStep
    
    ! Because of the way that the internal Clock was set by default,
    ! its timeStep is equal to the parent timeStep. As a consequence the
    ! currTime + timeStep is equal to the stopTime of the internal Clock
    ! for this call of the ModelAdvance() routine.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing MODEL from: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="--------------------------------> to: ", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! Call your model's timestep routine here
    
    ! call my_model_update()

    ! Start insert
!   Time_atmos = Time_atmos + Time_step_atmos
!   call update_atmos_model_dynamics (Atm)
!   call update_atmos_radiation_physics (Atm)
!   call update_atmos_model_state (Atm)
    ! End insert
      
  end subroutine

end module FV3_SHiELD_CAP

