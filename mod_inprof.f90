module mod_inprof

  implicit none

  REAL,    DIMENSION(:,:,:),   ALLOCATABLE :: ui, vi, wi
  REAL,    DIMENSION(:,:,:),   ALLOCATABLE :: theta, qi
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: smci
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: canopy_wateri
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: tstari
  REAL,    DIMENSION(:,:,:),   ALLOCATABLE :: t_deep_soili
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: snodepi
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: z0mseai
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: z0m_scm
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: z0h_scm
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: ccai
  INTEGER, DIMENSION(:,:),     ALLOCATABLE :: iccbi
  INTEGER, DIMENSION(:,:),     ALLOCATABLE :: iccti
  REAL,    DIMENSION(:),       ALLOCATABLE :: layer_depth
  REAL,    DIMENSION(:,:,:,:), ALLOCATABLE :: tracer
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: sil_orog_land
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: ho2r2_orog
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: ice_fract
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: di
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: u_0
  REAL,    DIMENSION(:,:),     ALLOCATABLE :: v_0
  REAL,    DIMENSION(:,:,:),   ALLOCATABLE :: p_in
  REAL,    DIMENSION(:,:,:),   ALLOCATABLE :: w_advi
  REAL                                     :: canopy_gbi

  NAMELIST /INPROF/                                     &
      ui, vi, wi, theta, qi, smci, canopy_wateri,       &
      tstari, t_deep_soili, snodepi, z0mseai, z0m_scm,  &
      z0h_scm, ccai, iccbi, iccti, layer_depth, tracer, &
      sil_orog_land, ho2r2_orog, ice_fract, di, u_0,    &
      v_0, p_in, w_advi, canopy_gbi

contains

  SUBROUTINE init_inprof()
    use mod_dimensions
    implicit none

    if( allocated( ui ))            deallocate( ui )
    if( allocated( vi ))            deallocate( vi )
    if( allocated( wi ))            deallocate( wi )
    if( allocated( theta ))         deallocate( theta )
    if( allocated( qi ))            deallocate( qi )
    if( allocated( smci ))          deallocate( smci )
    if( allocated( canopy_wateri )) deallocate( canopy_wateri )
    if( allocated( tstari ))        deallocate( tstari )
    if( allocated( t_deep_soili ))  deallocate( t_deep_soili )
    if( allocated( snodepi ))       deallocate( snodepi )
    if( allocated( z0mseai ))       deallocate( z0mseai )
    if( allocated( z0m_scm ))       deallocate( z0m_scm )
    if( allocated( z0h_scm ))       deallocate( z0h_scm )
    if( allocated( ccai ))          deallocate( ccai )
    if( allocated( iccbi ))         deallocate( iccbi )
    if( allocated( iccti ))         deallocate( iccti )
    if( allocated( layer_depth ))   deallocate( layer_depth )
    if( allocated( tracer ))        deallocate( tracer )
    if( allocated( sil_orog_land )) deallocate( sil_orog_land )
    if( allocated( ho2r2_orog ))    deallocate( ho2r2_orog )
    if( allocated( ice_fract ))     deallocate( ice_fract )
    if( allocated( di ))            deallocate( di )
    if( allocated( u_0 ))           deallocate( u_0 )
    if( allocated( v_0 ))           deallocate( v_0 )
    if( allocated( p_in ))          deallocate( p_in )
    if( allocated( w_advi ))        deallocate( w_advi )

    allocate( ui            (row_length, rows, max_model_levels) )
    allocate( vi            (row_length, rows, max_model_levels) )
    allocate( wi            (row_length, rows, max_model_levels + 1) )
    allocate( theta         (row_length, rows, max_model_levels) )
    allocate( qi            (row_length, rows, max_wet_levels) )
    allocate( smci          (row_length, rows) )
    allocate( canopy_wateri (row_length, rows) )
    allocate( tstari        (row_length, rows) )
    allocate( t_deep_soili  (row_length, rows, max_soil_temp_levs) )
    allocate( snodepi       (row_length, rows) )
    allocate( z0mseai       (row_length, rows) )
    allocate( z0m_scm       (row_length, rows) )
    allocate( z0h_scm       (row_length, rows) )
    allocate( ccai          (row_length, rows) )
    allocate( iccbi         (row_length, rows) )
    allocate( iccti         (row_length, rows) )
    allocate( layer_depth   (max_soil_temp_levs) )
    allocate( tracer        (row_length, rows, max_tr_levels, max_tr_vars) )
    allocate( sil_orog_land (row_length, rows) )
    allocate( ho2r2_orog    (row_length, rows) )
    allocate( ice_fract     (row_length, rows) )
    allocate( di            (row_length, rows) )
    allocate( u_0           (row_length, rows) )
    allocate( v_0           (row_length, rows) )
    allocate( p_in          (row_length, rows, max_model_levels + 1) )
    allocate( w_advi        (row_length, rows, 0:max_model_levels) )

  END SUBROUTINE init_inprof

  SUBROUTINE default_inprof()
    use mod_parameters
    implicit none

    if (                                       &
        .not. allocated( ui )             .or. &
        .not. allocated( vi )             .or. &
        .not. allocated( wi )             .or. &
        .not. allocated( theta )          .or. &
        .not. allocated( qi )             .or. &
        .not. allocated( smci )           .or. &
        .not. allocated( canopy_wateri )  .or. &
        .not. allocated( tstari )         .or. &
        .not. allocated( t_deep_soili )   .or. &
        .not. allocated( snodepi )        .or. &
        .not. allocated( z0mseai )        .or. &
        .not. allocated( z0m_scm )        .or. &
        .not. allocated( z0h_scm )        .or. &
        .not. allocated( ccai )           .or. &
        .not. allocated( iccbi )          .or. &
        .not. allocated( iccti )          .or. &
        .not. allocated( layer_depth )    .or. &
        .not. allocated( tracer )         .or. &
        .not. allocated( sil_orog_land )  .or. &
        .not. allocated( ho2r2_orog )     .or. &
        .not. allocated( ice_fract )      .or. &
        .not. allocated( di )             .or. &
        .not. allocated( u_0 )            .or. &
        .not. allocated( v_0 )            .or. &
        .not. allocated( p_in )           .or. &
        .not. allocated( w_advi )              &
    ) then
        write(*,*) "please call init_inprof before default_inprof"
        stop 7
    end if


    ui            = REAL_NOVALUE
    vi            = REAL_NOVALUE
    wi            = REAL_NOVALUE
    theta         = REAL_NOVALUE
    qi            = REAL_NOVALUE
    smci          = REAL_NOVALUE
    canopy_wateri = REAL_NOVALUE
    tstari        = REAL_NOVALUE
    t_deep_soili  = REAL_NOVALUE
    snodepi       = REAL_NOVALUE
    z0mseai       = REAL_NOVALUE
    z0m_scm       = REAL_NOVALUE
    z0h_scm       = REAL_NOVALUE
    ccai          = REAL_NOVALUE
    iccbi         = INT_NOVALUE
    iccti         = INT_NOVALUE
    layer_depth   = REAL_NOVALUE
    tracer        = REAL_NOVALUE
    sil_orog_land = REAL_NOVALUE
    ho2r2_orog    = REAL_NOVALUE
    ice_fract     = REAL_NOVALUE
    di            = REAL_NOVALUE
    u_0           = REAL_NOVALUE
    v_0           = REAL_NOVALUE
    p_in          = REAL_NOVALUE
    w_advi        = REAL_NOVALUE
    canopy_gbi    = REAL_NOVALUE

  END SUBROUTINE default_inprof


end module mod_inprof
