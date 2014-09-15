module mod_dimensions
  
  implicit none

  integer :: row_length, rows
  integer :: max_model_levels
  integer :: max_wet_levels
  integer :: max_ozone_levels
  integer :: max_soil_moist_levs
  integer :: ntype
  integer :: ntfp
  integer :: max_no_ntiles
  integer :: max_soil_temp_levs
  integer :: max_tr_levels
  integer :: max_tr_vars
  integer :: max_nfor

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
