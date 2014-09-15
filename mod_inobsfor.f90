module mod_inobsfor

  implicit none

  LOGICAL                               :: L_windrlx
  REAL                                  :: tau_rlx
  LOGICAL                               :: L_vertadv
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: t_inc
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: q_star
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: u_inc
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: v_inc
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: w_inc
  INTEGER                               :: ichgf
  LOGICAL                               :: l_flux_bc
  REAL, DIMENSION(:,:,:),   ALLOCATABLE :: flux_h
  REAL, DIMENSION(:,:,:),   ALLOCATABLE :: flux_e
  REAL, DIMENSION(:,:,:),   ALLOCATABLE :: tstar_forcing

  NAMELIST /INOBSFOR/                                   &
      L_windrlx, tau_rlx, L_vertadv, t_inc, q_star,     &
      u_inc, v_inc, w_inc, ichgf, l_flux_bc, flux_h,    &
      flux_e, tstar_forcing

contains

  SUBROUTINE init_inobsfor()
    use mod_dimensions
    implicit none

    if ( allocated( t_inc         )) deallocate( t_inc )
    if ( allocated( q_star        )) deallocate( q_star )
    if ( allocated( u_inc         )) deallocate( u_inc )
    if ( allocated( v_inc         )) deallocate( v_inc )
    if ( allocated( w_inc         )) deallocate( w_inc )
    if ( allocated( flux_h        )) deallocate( flux_h )
    if ( allocated( flux_e        )) deallocate( flux_e )
    if ( allocated( tstar_forcing )) deallocate( tstar_forcing )

    allocate( t_inc(row_length, rows, max_nfor, max_model_levels) )
    allocate( q_star(row_length, rows, max_nfor, max_wet_levels) )
    allocate( u_inc(row_length, rows, max_nfor, max_model_levels) )
    allocate( v_inc(row_length, rows, max_nfor, max_model_levels) )
    allocate( w_inc(row_length, rows, max_nfor, max_model_levels) )
    allocate( flux_h(row_length, rows, max_nfor) )
    allocate( flux_e(row_length, rows, max_nfor) )
    allocate( tstar_forcing(row_length, rows, max_nfor) )

  END SUBROUTINE init_inobsfor

  SUBROUTINE default_inobsfor()
    use mod_parameters
    implicit none

    if (                                  &
        .not. allocated( t_inc )  .or.    &
        .not. allocated( q_star ) .or.    &
        .not. allocated( u_inc )  .or.    &
        .not. allocated( v_inc )  .or.    &
        .not. allocated( w_inc )  .or.    &
        .not. allocated( flux_h ) .or.    &
        .not. allocated( flux_e ) .or.    &
        .not. allocated( tstar_forcing )  &
    ) then
        write(*,*) "please call init_inobsfor before default_inobsfor"
        stop 7
    end if

    L_windrlx     = .FALSE.
    tau_rlx       = REAL_NOVALUE
    L_vertadv     = .FALSE.
    t_inc         = REAL_NOVALUE
    q_star        = REAL_NOVALUE
    u_inc         = REAL_NOVALUE
    v_inc         = REAL_NOVALUE
    w_inc         = REAL_NOVALUE
    ichgf         = INT_NOVALUE
    l_flux_bc     = .FALSE.
    flux_h        = REAL_NOVALUE
    flux_e        = REAL_NOVALUE
    tstar_forcing = REAL_NOVALUE


  END SUBROUTINE default_inobsfor


end module mod_inobsfor
