module MED

  !-----------------------------------------------------------------------------
  ! Mediator Component for coupled FV3-Shield atmosphere and WW3 wave models.
  !-----------------------------------------------------------------------------

  use ESMF
  use NUOPC
  use NUOPC_Mediator, &
    mediatorSS             => SetServices

  implicit none

  private

  public SetServices

  !-----------------------------------------------------------------------------
  contains
  !-----------------------------------------------------------------------------

  subroutine SetServices(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    rc = ESMF_SUCCESS

    ! derive from NUOPC_Mediator
    call NUOPC_CompDerive(mediator, mediatorSS, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! specialize mediator
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advertise, &
      specRoutine=Advertise, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_RealizeProvided, &
      specRoutine=Realize, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_CompSpecialize(mediator, specLabel=label_Advance, &
      specRoutine=Advance, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Advertise(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    !----------
    ! Import
    !----------

    ! importable field: wave_induced_charnock_parameter
    call NUOPC_Advertise(importState, &
    StandardName="wave_induced_charnock_parameter", name="charno", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    ! importable field: wave_z0_roughness_length
    call NUOPC_Advertise(importState, &
    StandardName="wave_z0_roughness_length", name="z0rlen", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    ! importable field: eastward_wind_at_10m_height
    call NUOPC_Advertise(importState, &
      StandardName="eastward_wind_at_10m_height", name="u10m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: northward_wind_at_10m_height
    call NUOPC_Advertise(importState, &
      StandardName="northward_wind_at_10m_height", name="v10m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    !----------
    ! Export
    !----------
    
    ! exportable field: wave_induced_charnock_parameter
    call NUOPC_Advertise(exportState, &
    StandardName="wave_induced_charnock_parameter", name="charno", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    ! exportable field: wave_z0_roughness_length
    call NUOPC_Advertise(exportState, &
    StandardName="wave_z0_roughness_length", name="z0rlen", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    ! exportable field: eastward_wind_at_10m_height
    call NUOPC_Advertise(exportState, &
    StandardName="eastward_wind_at_10m_height", name="u10m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out

    ! exportable field: northward_wind_at_10m_height
    call NUOPC_Advertise(exportState, &
    StandardName="northward_wind_at_10m_height", name="v10m", rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return ! bail out
      
  end subroutine

  !-----------------------------------------------------------------------------

  subroutine Realize(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_State)        :: importState, exportState
    type(ESMF_Field)        :: field
    type(ESMF_Grid)         :: grid_atmos
    type(ESMF_Grid)         :: grid_wave

    rc = ESMF_SUCCESS

    ! query for importState and exportState
    call NUOPC_MediatorGet(mediator, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! create a Grid object for Fields
    grid_atmos = ESMF_GridCreateMosaic(filename='INPUT/C96_mosaic.nc', name='fv3-shield-grid', tileFilePath='INPUT/', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    !STEVE: update with WW3 grid, for now copying FV3-SHiELD...
    grid_wave = ESMF_GridCreateMosaic(filename='INPUT/C96_mosaic.nc', name='fv3-shield-grid', tileFilePath='INPUT/', rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    
    !----------
    ! Import
    !----------

    ! importable field: wave_induced_charnock_parameter
    field = ESMF_FieldCreate(name="charno", grid=grid_wave, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: wave_z0_roughness_length
    field = ESMF_FieldCreate(name="z0rlen", grid=grid_wave, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! importable field: eastward_wind_at_10m_height
    field = ESMF_FieldCreate(name="u10m", grid=grid_atmos, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! importable field: northward_wind_at_10m_height
    field = ESMF_FieldCreate(name="v10m", grid=grid_atmos, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(importState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    !----------
    ! Export
    !----------

    ! exportable field: wave_induced_charnock_parameter
    field = ESMF_FieldCreate(name="charno", grid=grid_wave, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: wave_z0_roughness_length
    field = ESMF_FieldCreate(name="z0rlen", grid=grid_wave, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! exportable field: eastward_wind_at_10m_height
    field = ESMF_FieldCreate(name="u10m", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call NUOPC_Realize(exportState, field=field, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
      
    ! exportable field: northward_wind_at_10m_height
    field = ESMF_FieldCreate(name="v10m", grid=gridOut, &
      typekind=ESMF_TYPEKIND_R8, rc=rc)
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

  subroutine Advance(mediator, rc)
    type(ESMF_GridComp)  :: mediator
    integer, intent(out) :: rc

    ! local variables
    type(ESMF_Clock)            :: clock
    type(ESMF_State)            :: importState, exportState
    character(len=160)          :: msgString

    rc = ESMF_SUCCESS

    ! query for clock, importState and exportState
    call ESMF_GridCompGet(mediator, clock=clock, importState=importState, &
      exportState=exportState, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    ! HERE THE MEDIATOR does the mediation of Fields that come in on the
    ! importState with a timestamp consistent to the currTime of the
    ! mediators Clock.

    ! The Mediator uses the data on the import Fields to update the data
    ! held by Fields in the exportState.

    ! After this routine returns the generic Mediator will correctly
    ! timestamp the export Fields at currTime, and update the Mediator Clock to:
    !
    !       currTime -> currTime + timeStep
    !
    ! Where the timeStep is equal to the parent timeStep.

    call ESMF_ClockPrint(clock, options="currTime", &
      preString="------>Advancing MED from: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

    call ESMF_ClockPrint(clock, options="stopTime", &
      preString="---------------------> to: ", unit=msgString, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
    call ESMF_LogWrite(msgString, ESMF_LOGMSG_INFO, rc=rc)
    if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

  end subroutine

  !-----------------------------------------------------------------------------

end module
