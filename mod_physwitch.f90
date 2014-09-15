module mod_physwitch

  implicit none
  
  INTEGER     :: conv_mode

  NAMELIST /PHYSWITCH/ conv_mode

contains

  SUBROUTINE init_physwitch()
    implicit none


  END SUBROUTINE init_physwitch

  SUBROUTINE default_physwitch()
    use mod_parameters
    implicit none

    conv_mode = INT_NOVALUE

  END SUBROUTINE default_physwitch


end module mod_physwitch
