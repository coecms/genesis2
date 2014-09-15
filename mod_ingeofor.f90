module mod_ingeofor

  implicit none

  REAL, DIMENSION(:,:),   ALLOCATABLE :: ug
  REAL, DIMENSION(:,:),   ALLOCATABLE :: vg

  NAMELIST /INGEOFOR/ ug, vg

contains

  SUBROUTINE init_ingeofor()
    use mod_dimensions
    implicit none

    if ( allocated( ug )) deallocate( ug )
    if ( allocated( vg )) deallocate( vg )

    allocate( ug (row_length, rows) )
    allocate( vg (row_length, rows) )

  END SUBROUTINE init_ingeofor

  SUBROUTINE default_ingeofor()
    use mod_parameters
    implicit none

    if (                           &
        .not. allocated( ug ) .or. &
        .not. allocated( vg )      &
    ) then
        write(*,*) "please call init_ingeofor before default_ingeofor"
        stop 7
    end if

    ug = REAL_NOVALUE
    vg = REAL_NOVALUE

  END SUBROUTINE default_ingeofor


end module mod_ingeofor
