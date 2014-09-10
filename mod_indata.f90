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

    SOIL_TYPE(:,:)  = 3
    VEG_TYPE(:,:)   = 5
    tapeyear_init   = 1998
    tapemonth_init  = 1
    tapeday_init    = 1
    tapehour_init   = 0
    tapemin_init    = 0
    tapesec_init    = 0
    year_init       = 1998
    month_init      = 1
    day_init        = 1
    hour_init       = 0
    min_init        = 0
    sec_init        = 0
    tconst(:,:)     = 0.0
    dtday(:,:)      = 0.0
    dtyear(:,:)     = 0.0
    lat(:,:)        = 0.0
    long(:,:)       = 0.0
    gridbox_area(:,:) = 1e5
    salt_dim1       = 1
    salt_dim2       = 1
    salt_dim3       = 1
    gather          = .FALSE.


  END SUBROUTINE default_indata


end module mod_indata
