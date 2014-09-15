program genesis2
  
  use mod_dimensions
  use mod_cntlscm
  use mod_rundata
  use mod_indata
  use mod_logic
  use mod_inmoses
  use mod_inprof
  use mod_inobsfor
  use mod_radcloud
  use mod_physwitch
  use mod_ingeofor
  use mod_namelist

  implicit none

  INTEGER, PARAMETER    :: template_handle = 50
  INTEGER, PARAMETER    :: namelist_handle = 6

  row_length          = 1
  rows                = 1
  max_soil_moist_levs = 6
  max_soil_temp_levs  = 6
  ntype               = 18
  max_soil_moist_levs = 6
  ntfp                = 5
  max_no_ntiles       = 17
  ntype               = 18
  max_tr_vars         = 30

  open(template_handle, file='template.scm')

  call read_namelist( template_handle )

  close(template_handle)

  call write_namelist( namelist_handle )


end program genesis2
