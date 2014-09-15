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

  call init_cntlscm()
  call default_cntlscm()
  read(template_handle, CNTLSCM)

  call set_all_levels_to( model_levels_nml )
  max_nfor = nfor

  call init_indata()
  call default_indata()
  read(template_handle, INDATA)

  call init_rundata()
  call default_rundata()
  read(template_handle, RUNDATA)

  call init_logic()
  call default_logic()
  read(template_handle, LOGIC)

  call init_inmoses()
  call default_inmoses()
  read(template_handle, INMOSES)

  call init_inprof()
  call default_inprof()
  read(template_handle, INPROF)

  call init_inobsfor()
  call default_inobsfor()
  read(template_handle, INOBSFOR)

  call init_radcloud()
  call default_radcloud()
  read(template_handle, RADCLOUD)

  close(template_handle)

  write(namelist_handle, CNTLSCM)
  write(namelist_handle, INDATA)
  write(namelist_handle, RUNDATA)
  write(namelist_handle, LOGIC)
  write(namelist_handle, INMOSES)
  write(namelist_handle, INPROF)
  write(namelist_handle, INOBSFOR)
  write(namelist_handle, RADCLOUD)

end program genesis2
