module mod_cntlscm

  implicit none
  
  INTEGER     :: nfor
  INTEGER     :: model_levels_nml
  INTEGER     :: land_points
!  INTEGER     :: ts_log_opt         =    2  ! MRD removed

  NAMELIST /CNTLSCM/                                    &
      nfor, model_levels_nml, land_points

contains

  SUBROUTINE init_cntlscm(row_length, rows)
    implicit none
    integer, intent(in)   :: row_length, rows


  END SUBROUTINE init_cntlscm

  SUBROUTINE default_cntlscm()
    implicit none

    nfor = 1
    model_levels_nml = 1
    land_points = 1
!    ts_log_opt = 2

  END SUBROUTINE default_cntlscm


end module mod_cntlscm
