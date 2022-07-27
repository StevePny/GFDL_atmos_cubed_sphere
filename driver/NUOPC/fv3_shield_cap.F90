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

! use FMS
! use FMSconstants,    only: fmsconstants_init
! use atmos_model_mod, only: atmos_model_init, atmos_model_end,  &
!                          update_atmos_model_dynamics,        &
!                          update_atmos_radiation_physics,     &
!                          update_atmos_model_state,           &
!                          atmos_data_type, atmos_model_restart
! !--- FMS old io
! use fms_io_mod, only: fms_io_exit!< This can't be removed until fms_io is not used at all

  ! End insert
  !-----------------------------------------------------------------------------

  implicit none
  
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
!   call fms_init()
!   call sat_vapor_pres_init()
!   call fmsconstants_init()

!   initClock = mpp_clock_id( 'Initialization' )
!   call mpp_clock_begin (initClock) !nesting problem

!   call coupler_init
!   call print_memuse_stats('after coupler init')

!   call mpp_set_current_pelist()
!   call mpp_clock_end (initClock) !end initialization
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

