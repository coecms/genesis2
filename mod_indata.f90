module mod_indata

  implicit none

  INTEGER, DIMENSION(:,:), ALLOCATABLE  :: SOIL_TYPE
  INTEGER, DIMENSION(:,:), ALLOCATABLE  :: VEG_TYPE
  INTEGER                               :: tapeyear_init
  INTEGER                               :: tapemonth_init
  INTEGER                               :: tapeday_init
  INTEGER                               :: tapehour_init
  INTEGER                               :: tapemin_init
  INTEGER                               :: tapesec_init
  INTEGER                               :: year_init
  INTEGER                               :: month_init
  INTEGER                               :: day_init
  INTEGER                               :: hour_init
  INTEGER                               :: min_init
  INTEGER                               :: sec_init
  REAL   , DIMENSION(:,:), ALLOCATABLE  :: tconst
  REAL   , DIMENSION(:,:), ALLOCATABLE  :: dtday
  REAL   , DIMENSION(:,:), ALLOCATABLE  :: dtyear
  REAL   , DIMENSION(:,:), ALLOCATABLE  :: lat
  REAL   , DIMENSION(:,:), ALLOCATABLE  :: long
  REAL   , DIMENSION(:,:), ALLOCATABLE  :: gridbox_area
  INTEGER                               :: salt_dim1
  INTEGER                               :: salt_dim2
  INTEGER                               :: salt_dim3
  LOGICAL                               :: gather

  NAMELIST /INDATA/ SOIL_TYPE, VEG_TYPE, tapeyear_init,   &
      tapemonth_init, tapeday_init, tapehour_init,        &
      tapemin_init, tapesec_init, year_init, month_init,  &
      day_init, hour_init, min_init, sec_init, tconst,    &
      dtday, dtyear, lat, long, gridbox_area, salt_dim1,  &
      salt_dim2, salt_dim3, gather
  
contains

  SUBROUTINE init_indata(row_length, rows)
    implicit none
    integer, intent(in)   :: row_length, rows

    if (allocated(SOIL_TYPE)) deallocate (SOIL_TYPE)
    allocate( SOIL_TYPE(row_length, rows) )

    if (allocated(VEG_TYPE)) deallocate (VEG_TYPE)
    allocate( VEG_TYPE(row_length, rows) )

    if (allocated(tconst)) deallocate (tconst)
    allocate( tconst(row_length, rows) )

    if (allocated(dtday)) deallocate (dtday)
    allocate( dtday(row_length, rows) )

    if (allocated(dtyear)) deallocate (dtyear)
    allocate( dtyear(row_length, rows) )

    if (allocated(lat)) deallocate (lat)
    allocate( lat(row_length, rows) )

    if (allocated(long)) deallocate (long)
    allocate( long(row_length, rows) )

    if (allocated(gridbox_area)) deallocate (gridbox_area)
    allocate( gridbox_area(row_length, rows) )

  END SUBROUTINE init_indata

  SUBROUTINE default_indata()
    use mod_parameters
    implicit none

    if (.not. allocated(SOIL_TYPE) .or.           &
        .not. allocated(VEG_TYPE)  .or.           &
        .not. allocated(tconst  )  .or.           &
        .not. allocated(dtday   )  .or.           &
        .not. allocated(dtyear  )  .or.           &
        .not. allocated(lat     )  .or.           &
        .not. allocated(long    )  .or.           &
        .not. allocated(gridbox_area)) then
      write(*,*) "First use init_indata, then default_indata"
      stop 2
    end if

    SOIL_TYPE(:,:)  = INT_NOVALUE
    VEG_TYPE(:,:)   = INT_NOVALUE
    tapeyear_init   = INT_NOVALUE
    tapemonth_init  = INT_NOVALUE
    tapeday_init    = INT_NOVALUE
    tapehour_init   = INT_NOVALUE
    tapemin_init    = INT_NOVALUE
    tapesec_init    = INT_NOVALUE
    year_init       = INT_NOVALUE
    month_init      = INT_NOVALUE
    day_init        = INT_NOVALUE
    hour_init       = INT_NOVALUE
    min_init        = INT_NOVALUE
    sec_init        = INT_NOVALUE
    tconst(:,:)     = REAL_NOVALUE
    dtday(:,:)      = REAL_NOVALUE
    dtyear(:,:)     = REAL_NOVALUE
    lat(:,:)        = REAL_NOVALUE
    long(:,:)       = REAL_NOVALUE
    gridbox_area(:,:) = REAL_NOVALUE
    salt_dim1       = INT_NOVALUE
    salt_dim2       = INT_NOVALUE
    salt_dim3       = INT_NOVALUE
    gather          = .FALSE.


  END SUBROUTINE default_indata


end module mod_indata
