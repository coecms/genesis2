module mod_dimensions
  
  implicit none

  integer :: row_length          = 1
  integer :: rows                = 1
  integer :: max_model_levels    = 38
  integer :: max_wet_levels      = 38
  integer :: max_ozone_levels    = 38
  integer :: max_soil_moist_levs = 6
  integer :: ntype               = 18
  integer :: ntfp                = 5
  integer :: max_no_ntiles       = 17
  integer :: max_soil_temp_levs  = 6
  integer :: max_tr_levels       = 38
  integer :: max_tr_vars         = 30
  integer :: max_nfor            = 109

contains

  subroutine set_all_levels_to( levels )
    implicit none
    integer, intent(in) :: levels
    max_model_levels = levels
    max_wet_levels   = levels
    max_ozone_levels = levels
    max_tr_levels    = levels
  end subroutine set_all_levels_to

end module mod_dimensions
