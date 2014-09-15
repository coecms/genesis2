module mod_radcloud

  implicit none

  REAL, DIMENSION(:,:),   ALLOCATABLE :: cca_rad
  REAL, DIMENSION(:,:),   ALLOCATABLE :: iccb_rad
  REAL, DIMENSION(:,:),   ALLOCATABLE :: icct_rad
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: layer_cloud_rad
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: qcl_rad
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: qcf_rad
  REAL, DIMENSION(:,:),   ALLOCATABLE :: ccwpin_rad

  NAMELIST /RADCLOUD/                                   &
    cca_rad, iccb_rad, icct_rad, layer_cloud_rad,       &
    qcl_rad, qcf_rad, ccwpin_rad 

contains

  SUBROUTINE init_radcloud()
    use mod_dimensions
    implicit none

    if ( allocated(  cca_rad )) deallocate(  cca_rad )
    if ( allocated(  iccb_rad )) deallocate(  iccb_rad )
    if ( allocated(  icct_rad )) deallocate(  icct_rad )
    if ( allocated(  layer_cloud_rad )) deallocate(  layer_cloud_rad )
    if ( allocated(  qcl_rad )) deallocate(  qcl_rad )
    if ( allocated(  qcf_rad )) deallocate(  qcf_rad )
    if ( allocated(  ccwpin_rad )) deallocate(  ccwpin_rad )

    allocate( cca_rad         (row_length, rows) )
    allocate( iccb_rad        (row_length, rows) )
    allocate( icct_rad        (row_length, rows) )
    allocate( layer_cloud_rad (row_length, rows, max_wet_levels) )
    allocate( qcl_rad         (row_length, rows, max_wet_levels) )
    allocate( qcf_rad         (row_length, rows, max_wet_levels) )
    allocate( ccwpin_rad      (row_length, rows) )

  END SUBROUTINE init_radcloud

  SUBROUTINE default_radcloud()
    use mod_parameters
    implicit none

    if (                                        &
        .not. allocated( cca_rad )         .or. &
        .not. allocated( iccb_rad )        .or. &
        .not. allocated( icct_rad )        .or. &
        .not. allocated( layer_cloud_rad ) .or. &
        .not. allocated( qcl_rad )         .or. &
        .not. allocated( qcf_rad )         .or. &
        .not. allocated( ccwpin_rad )           &
    ) then
        write(*,*) "please call init_radcloud before default_radcloud"
        stop 7
    end if

    cca_rad         = REAL_NOVALUE
    iccb_rad        = REAL_NOVALUE
    icct_rad        = REAL_NOVALUE
    layer_cloud_rad = REAL_NOVALUE
    qcl_rad         = REAL_NOVALUE
    qcf_rad         = REAL_NOVALUE
    ccwpin_rad      = REAL_NOVALUE

  END SUBROUTINE default_radcloud


end module mod_radcloud
