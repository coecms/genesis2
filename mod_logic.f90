module mod_logic

  implicit none
  LOGICAL :: ancyc, altdat, tapein, altsoil, tapeout,   &
      test, obs, stats, prindump_step, prindump_day,    &
      prindump_days, prindump_obs
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: land_sea_mask,&
      land_ice_mask, soil_mask
  LOGICAL :: noforce, grafdump_day, grafdump_days,      &
      geoforce, geoinit, grafdump_step
  LOGICAL, DIMENSION(:,:), ALLOCATABLE :: cumulus
  LOGICAL :: l_area_cloud, local_time, l_do_t,          &
      l_do_inc_vels, l_do_inc_q, prinstat,              &
      l_hydrology, l_spec_z0, l_murk_rad,               &
      radcloud_fixed
  
  NAMELIST /LOGIC/                                      &
      ancyc, altdat, tapein, altsoil, tapeout, test,    &
      obs, stats, prindump_step, prindump_day,          &
      prindump_days, prindump_obs, land_sea_mask,       &
      land_ice_mask, soil_mask, noforce, grafdump_day,  &
      grafdump_days, geoforce, geoinit, grafdump_step,  &
      cumulus, l_area_cloud, local_time, l_do_t,        &
      l_do_inc_vels, l_do_inc_q, prinstat, l_hydrology, &
      l_spec_z0, l_murk_rad, radcloud_fixed
 
contains

  SUBROUTINE init_logic()
    use mod_dimensions
    implicit none

    if ( allocated( land_sea_mask ) ) deallocate( land_sea_mask )
    if ( allocated( land_ice_mask ) ) deallocate( land_ice_mask )
    if ( allocated( soil_mask ) )     deallocate( soil_mask )
    if ( allocated( cumulus ) )       deallocate( cumulus )

    allocate( land_sea_mask( row_length, rows ))
    allocate( land_ice_mask( row_length, rows ))
    allocate( soil_mask( row_length, rows ))
    allocate( cumulus( row_length, rows ))


  END SUBROUTINE init_logic

  SUBROUTINE default_logic()
    implicit none

    if (                                                &
        .not. allocated( land_sea_mask ) .or.           &
        .not. allocated( land_ice_mask ) .or.           &
        .not. allocated( soil_mask )     .or.           &
        .not. allocated( cumulus )                      &
        ) then
      write(*,*) "please call init_logic before default_logic"
      stop 5
    end if

    ancyc    = .true.
    altdat    = .true.
    tapein    = .false.
    altsoil    = .false.
    tapeout    = .false.
    test    = .false.
    obs    = .false.
    stats    = .false.
    prindump_step    = .false.
    prindump_day    = .false.
    prindump_days    = .false.
    prindump_obs    = .false.
    land_sea_mask    = .true.
    land_ice_mask    = .false.
    soil_mask    = .true.
    noforce    = .false.
    grafdump_day    = .false.
    grafdump_days    = .false.
    geoforce    = .false.
    geoinit    = .false.
    grafdump_step    = .false.
    cumulus    = .false.
    l_area_cloud    = .false.
    local_time    = .true.
    l_do_t    = .false.
    l_do_inc_vels    = .false.
    l_do_inc_q    = .false.
    prinstat    = .false.
    l_hydrology    = .false.
    l_spec_z0    = .false.
    l_murk_rad    = .false.
    radcloud_fixed    = .false.

  END SUBROUTINE default_logic


end module mod_logic
