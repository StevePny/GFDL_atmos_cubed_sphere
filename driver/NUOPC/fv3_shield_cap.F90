module fv3_shield_cap

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

  use  diag_manager_mod, only: diag_manager_init, diag_manager_end, &
                               get_base_date, diag_manager_set_time_end

  use data_override_mod, only: data_override_init

  ! End insert
  !-----------------------------------------------------------------------------

  implicit none
  
  !-----------------------------------------------------------------------
  ! Insert leading instantiations from coupler_main
  !

  !-----------------------------------------------------------------------

  character(len=128) :: version = '$Id: coupler_main.F90,v 19.0.4.1.2.3 2014/09/09 23:51:59 Rusty.Benson Exp $'
  character(len=128) :: tag = '$Name: ulm_201505 $'

  !-----------------------------------------------------------------------
  !---- model defined-types ----

  type (atmos_data_type) :: Atm

  !-----------------------------------------------------------------------
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
    call fms_init()
    call mpp_init()
    initClock = mpp_clock_id( 'Initialization' )
    call mpp_clock_begin (initClock) !nesting problem

    call fms_init
    call constants_init
    call fms_affinity_init
    call sat_vapor_pres_init

    call coupler_init
    call print_memuse_stats('after coupler init')
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
    Time_atmos = Time_atmos + Time_step_atmos
    call update_atmos_model_dynamics (Atm)
    call update_atmos_radiation_physics (Atm)
    call update_atmos_model_state (Atm)
    ! End insert
      
  end subroutine
  
  !#######################################################################

  subroutine coupler_init

    !-----------------------------------------------------------------------
    !   initialize all defined exchange grids and all boundary maps
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

    !-----------------------------------------------------------------------
    !----- initialization timing identifiers ----
    !-----------------------------------------------------------------------

    !----- read namelist -------
    !----- for backwards compatibilty read from file coupler.nml -----

    read(input_nml_file, nml=coupler_nml, iostat=io)
    ierr = check_nml_error(io, 'coupler_nml')

    !----- write namelist to logfile -----
    call write_version_number (version, tag)
    if (mpp_pe() == mpp_root_pe()) write(stdlog(),nml=coupler_nml)

    !----- allocate and set the pelist (to the global pelist) -----
    allocate( Atm%pelist  (mpp_npes()) )
    call mpp_get_current_pelist(Atm%pelist)

    !----- read restart file -----
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

    call set_calendar_type (calendar_type)

    !----- write current/initial date actually used to logfile file -----

    if ( mpp_pe() == mpp_root_pe() ) then
        write (stdlog(),16) date(1),trim(month_name(date(2))),date(3:6)
    endif

16 format ('  current date used = ',i4,1x,a,2i3,2(':',i2.2),' gmt')

    !-----------------------------------------------------------------------
    !------ initialize diagnostics manager ------

    call diag_manager_init (TIME_INIT=date)

    !----- always override initial/base date with diag_manager value -----

    call get_base_date ( date_init(1), date_init(2), date_init(3), date_init(4), date_init(5), date_init(6)  )

    !----- use current date if no base date ------

    if ( date_init(1) == 0 ) date_init = date

    !----- set initial and current time types ------

    Time_init  = set_date (date_init(1), date_init(2), date_init(3), date_init(4), date_init(5), date_init(6))

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
     call  atmos_model_init (Atm,  Time_init, Time_atmos, Time_step_atmos, iau_offset)

     call print_memuse_stats('after atmos model init')

     call mpp_get_global_domain(Atm%Domain, xsize=gnlon, ysize=gnlat)
     
     allocate ( glon_bnd(gnlon+1,gnlat+1), glat_bnd(gnlon+1,gnlat+1) )
     
     call mpp_global_field(Atm%Domain, Atm%lon_bnd, glon_bnd, position=CORNER)
     call mpp_global_field(Atm%Domain, Atm%lat_bnd, glat_bnd, position=CORNER)

     if (.NOT.Atm%bounded_domain) call data_override_init (Atm_domain_in  = Atm%domain)
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

end module fv3_shield_cap

