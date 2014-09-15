module mod_cntlscm


  implicit none
  
  INTEGER     :: nfor
  INTEGER     :: model_levels_nml
  INTEGER     :: land_points
  INTEGER     :: ts_log_opt 

  NAMELIST /CNTLSCM/                                    &
      nfor, model_levels_nml, land_points

contains

  SUBROUTINE init_cntlscm()
    implicit none


  END SUBROUTINE init_cntlscm

  SUBROUTINE default_cntlscm()
    use mod_parameters
    implicit none

    nfor = INT_NOVALUE
    model_levels_nml = INT_NOVALUE
    land_points = INT_NOVALUE
    ts_log_opt = INT_NOVALUE

  END SUBROUTINE default_cntlscm


end module mod_cntlscm
