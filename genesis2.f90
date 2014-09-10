program genesis2
  
  use mod_cntlscm
  use mod_indata
  use mod_rundata

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
      max_soil_moist_levs=1, ntype=1)
  call default_indata()
  read(template_handle, RUNDATA)

  close(template_handle)

  write(namelist_handle, CNTLSCM)
  write(namelist_handle, INDATA)
  write(namelist_handle, RUNDATA)

end program genesis2
