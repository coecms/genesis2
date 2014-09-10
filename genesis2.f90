program genesis2
  
  use mod_cntlscm
  use mod_rundata
  use mod_indata
  use mod_logic
  use mod_inmoses

  implicit none

  INTEGER, PARAMETER    :: template_handle = 50
  INTEGER, PARAMETER    :: namelist_handle = 6


  open(template_handle, file='template.scm')

  call init_cntlscm(1, 1)
  call default_cntlscm()
  read(template_handle, CNTLSCM)

  call init_indata(1, 1)
  call default_indata()
  read(template_handle, INDATA)

  call init_rundata(row_length=1, rows = 1,         &
      max_model_levels=model_levels_nml,            &
      ntypes=1,                                     &
      max_ozone_levels=model_levels_nml,            &
      max_wet_levels=model_levels_nml,              &
      max_soil_moist_levs=6, ntype=18)
  call default_indata()
  read(template_handle, RUNDATA)

  call init_logic(1, 1)
  call default_logic()
  read(template_handle, LOGIC)

  call init_inmoses(row_length=1, rows=1,           &
        max_soil_moist_levs=6,                      &
        ntfp=5, max_no_ntiles=17, ntype=18)
  call default_inmoses()
  read(template_handle, INMOSES)

  close(template_handle)

  write(namelist_handle, CNTLSCM)
  write(namelist_handle, INDATA)
  write(namelist_handle, RUNDATA)
  write(namelist_handle, LOGIC)
  write(namelist_handle, INMOSES)

end program genesis2
