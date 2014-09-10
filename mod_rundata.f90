module mod_rundata

  implicit none

  INTEGER :: ndayin, nminin, nsecin
  REAL :: timestep
  INTEGER :: ntrad, ntrad1
  CHARACTER(len=8)  :: exname_in, exname_out
  INTEGER :: runno_in, runno_out
  INTEGER :: resdump_days, dump_step
  INTEGER, DIMENSION(4) :: dump_days
  INTEGER :: change_clim, min_trop_level, max_trop_level
  INTEGER :: ntml, nbdsc, ntdsc
  REAL, DIMENSION(:,:), ALLOCATABLE :: zh, snow_free_albedo,  &
        deep_snow_albedo
  REAL, DIMENSION(:), ALLOCATABLE :: albsoil
  REAL, DIMENSION(:,:), ALLOCATABLE :: sice_alb, land_alb,    &
        fland_ctile, tstar_land, tstar_sea, tstar_sice,       &
        dolr_rts, cort, cord, corvn, corw, cclwp, orog,       &
        sum_eng_fluxes, sum_moist_flux, aerosol_em, so2_em,   &
        nh3_em, dms_em, soot_em, soot_hilem, soot
  REAL :: co2start, co2end, co2rate
  REAL, DIMENSION(:,:), ALLOCATABLE :: co2_emits, co2flux
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: co2
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: tile_frac, aerosol,  &
        so2, sthu, so4_aitken, so4_accu, so4_diss, dms, nh3,  &
        soot_new, soot_cld, soot_aged, ozone

  NAMELIST /RUNDATA/                                          &
        ndayin, nminin, nsecin, timestep, ntrad, ntrad1,      &
        exname_in, exname_out, runno_in, runno_out,           &
        resdump_days, dump_step, dump_days, change_clim,      &
        min_trop_level, max_trop_level, ntml, nbdsc, ntdsc,   &
        zh, snow_free_albedo, deep_snow_albedo, albsoil,      &
        sice_alb, land_alb, fland_ctile, tstar_land,          &
        tstar_sea, tstar_sice, dolr_rts, cort, cord, corvn,   &
        corw, cclwp, orog, sum_eng_fluxes, sum_moist_flux,    &
        aerosol_em, so2_em, nh3_em, dms_em, soot_em,          &
        soot_hilem, soot, co2start, co2end, co2rate,          &
        co2_emits, co2flux, co2, tile_frac, ozone, aerosol,   &
        so2, sthu, so4_aitken, so4_accu, so4_diss, dms, nh3,  &
        soot_new, soot_cld, soot_aged
 
contains

  SUBROUTINE init_rundata(row_length, rows, max_model_levels, ntypes, &
        max_ozone_levels, max_wet_levels, max_soil_moist_levs, ntype)
    implicit none
    integer, intent(in)   :: row_length, rows, max_model_levels, ntypes, &
      max_ozone_levels, max_wet_levels, max_soil_moist_levs, ntype


    if( allocated( zh )) deallocate( zh ) 
    if( allocated( snow_free_albedo )) deallocate( snow_free_albedo ) 
    if( allocated( deep_snow_albedo )) deallocate( deep_snow_albedo ) 
    if( allocated( albsoil )) deallocate( albsoil ) 
    if( allocated( sice_alb )) deallocate( sice_alb ) 
    if( allocated( land_alb )) deallocate( land_alb ) 
    if( allocated( fland_ctile )) deallocate( fland_ctile ) 
    if( allocated( tstar_land )) deallocate( tstar_land ) 
    if( allocated( tstar_sea )) deallocate( tstar_sea ) 
    if( allocated( tstar_sice )) deallocate( tstar_sice ) 
    if( allocated( dolr_rts )) deallocate( dolr_rts ) 
    if( allocated( cort )) deallocate( cort ) 
    if( allocated( cord )) deallocate( cord ) 
    if( allocated( corvn )) deallocate( corvn ) 
    if( allocated( corw )) deallocate( corw ) 
    if( allocated( cclwp )) deallocate( cclwp ) 
    if( allocated( orog )) deallocate( orog ) 
    if( allocated( sum_eng_fluxes )) deallocate( sum_eng_fluxes ) 
    if( allocated( sum_moist_flux )) deallocate( sum_moist_flux ) 
    if( allocated( aerosol_em )) deallocate( aerosol_em ) 
    if( allocated( so2_em )) deallocate( so2_em ) 
    if( allocated( nh3_em )) deallocate( nh3_em ) 
    if( allocated( dms_em )) deallocate( dms_em ) 
    if( allocated( soot_em )) deallocate( soot_em ) 
    if( allocated( soot_hilem )) deallocate( soot_hilem ) 
    if( allocated( soot )) deallocate( soot ) 
    if( allocated( co2_emits )) deallocate( co2_emits ) 
    if( allocated( co2flux )) deallocate( co2flux ) 
    if( allocated( co2 )) deallocate( co2 ) 
    if( allocated( tile_frac )) deallocate( tile_frac ) 
    if( allocated( ozone )) deallocate( ozone )
    if( allocated( aerosol )) deallocate( aerosol ) 
    if( allocated( so2 )) deallocate( so2 ) 
    if( allocated( sthu )) deallocate( sthu ) 
    if( allocated( so4_aitken )) deallocate( so4_aitken ) 
    if( allocated( so4_accu )) deallocate( so4_accu ) 
    if( allocated( so4_diss )) deallocate( so4_diss ) 
    if( allocated( dms )) deallocate( dms ) 
    if( allocated( nh3 )) deallocate( nh3 ) 
    if( allocated( soot_new )) deallocate( soot_new ) 
    if( allocated( soot_cld )) deallocate( soot_cld ) 
    if( allocated( soot_aged )) deallocate( soot_aged ) 

    allocate( zh(row_length, rows) )
    allocate( snow_free_albedo(row_length, rows) )
    allocate( deep_snow_albedo(row_length, rows) )
    allocate( albsoil(row_length * rows) )
    allocate( sice_alb(row_length, rows) )
    allocate( land_alb(row_length, rows) )
    allocate( fland_ctile(row_length, rows) )
    allocate( tstar_land(row_length, rows) )
    allocate( tstar_sea(row_length, rows) )
    allocate( tstar_sice(row_length, rows) )
    allocate( dolr_rts(row_length, rows) )
    allocate( cort(row_length, rows) )
    allocate( cord(row_length, rows) )
    allocate( corvn(row_length, rows) )
    allocate( corw(row_length, rows) )
    allocate( cclwp(row_length, rows) )
    allocate( orog(row_length, rows) )
    allocate( sum_eng_fluxes(row_length, rows) )
    allocate( sum_moist_flux(row_length, rows) )
    allocate( aerosol_em(1:row_length, 1:rows) )
    allocate( so2_em(row_length, rows) )
    allocate( nh3_em(row_length, rows) )
    allocate( dms_em(row_length, rows) )
    allocate( soot_em(row_length, rows) )
    allocate( soot_hilem(row_length, rows) )
    allocate( soot(row_length, rows) )
    allocate( co2_emits(row_length, rows) )
    allocate( co2flux(row_length, rows) )
    allocate( co2(row_length, rows, max_model_levels) )
    allocate( tile_frac(row_length, rows, ntype) )
    allocate( ozone(row_length, rows, max_ozone_levels) )
    allocate( aerosol(row_length, rows, max_wet_levels) )
    allocate( so2(row_length, rows, max_wet_levels) )
    allocate( sthu(row_length, rows, max_soil_moist_levs) )
    allocate( so4_aitken(row_length, rows, max_wet_levels) )
    allocate( so4_accu(row_length, rows, max_wet_levels) )
    allocate( so4_diss(row_length, rows, max_wet_levels) )
    allocate( dms(row_length, rows, max_model_levels) )
    allocate( nh3(row_length, rows, max_model_levels) )
    allocate( soot_new(row_length, rows, max_model_levels) )
    allocate( soot_cld(row_length, rows, max_model_levels) )
    allocate( soot_aged(row_length, rows, max_model_levels) )


  END SUBROUTINE init_rundata

  SUBROUTINE default_rundata()
    use mod_parameters
    implicit none

    if ( .not. allocated(zh) .or.    &
      .not. allocated(snow_free_albedo) .or.    &
      .not. allocated(deep_snow_albedo) .or.    &
      .not. allocated(albsoil) .or.    &
      .not. allocated(sice_alb) .or.    &
      .not. allocated(land_alb) .or.    &
      .not. allocated(fland_ctile) .or.    &
      .not. allocated(tstar_land) .or.    &
      .not. allocated(tstar_sea) .or.    &
      .not. allocated(tstar_sice) .or.    &
      .not. allocated(dolr_rts) .or.    &
      .not. allocated(cort) .or.    &
      .not. allocated(cord) .or.    &
      .not. allocated(corvn) .or.    &
      .not. allocated(corw) .or.    &
      .not. allocated(cclwp) .or.    &
      .not. allocated(orog) .or.    &
      .not. allocated(sum_eng_fluxes) .or.    &
      .not. allocated(sum_moist_flux) .or.    &
      .not. allocated(aerosol_em) .or.    &
      .not. allocated(so2_em) .or.    &
      .not. allocated(nh3_em) .or.    &
      .not. allocated(dms_em) .or.    &
      .not. allocated(soot_em) .or.    &
      .not. allocated(soot_hilem) .or.    &
      .not. allocated(soot) .or.    &
      .not. allocated(co2_emits) .or.    &
      .not. allocated(co2flux) .or.    &
      .not. allocated(co2) .or.    &
      .not. allocated(tile_frac) .or.    &
      .not. allocated(ozone) .or.    &
      .not. allocated(aerosol) .or.    &
      .not. allocated(so2) .or.    &
      .not. allocated(sthu) .or.    &
      .not. allocated(so4_aitken) .or.    &
      .not. allocated(so4_accu) .or.    &
      .not. allocated(so4_diss) .or.    &
      .not. allocated(dms) .or.    &
      .not. allocated(nh3) .or.    &
      .not. allocated(soot_new) .or.    &
      .not. allocated(soot_cld) .or.    &
      .not. allocated(soot_aged)) then
      write(*,*) "First use init_rundata, then default_rundata"
      stop 3
    end if

    ndayin           = INT_NOVALUE
    nminin           = INT_NOVALUE
    nsecin           = INT_NOVALUE
    timestep         = REAL_NOVALUE
    ntrad            = INT_NOVALUE
    ntrad1           = INT_NOVALUE
    exname_in        = 'XXXXXXXX'
    exname_out       = 'XXXXXXXX'
    runno_in         = INT_NOVALUE
    runno_out        = INT_NOVALUE
    resdump_days     = INT_NOVALUE
    dump_step        = INT_NOVALUE
    dump_days        = INT_NOVALUE
    change_clim      = INT_NOVALUE
    min_trop_level   = INT_NOVALUE
    max_trop_level   = INT_NOVALUE
    ntml             = INT_NOVALUE
    nbdsc            = INT_NOVALUE
    ntdsc            = INT_NOVALUE
    zh               = REAL_NOVALUE
    snow_free_albedo = REAL_NOVALUE
    deep_snow_albedo = REAL_NOVALUE
    albsoil          = REAL_NOVALUE
    sice_alb         = REAL_NOVALUE
    land_alb         = REAL_NOVALUE
    fland_ctile      = REAL_NOVALUE
    tstar_land       = REAL_NOVALUE
    tstar_sea        = REAL_NOVALUE
    tstar_sice       = REAL_NOVALUE
    dolr_rts         = REAL_NOVALUE
    cort             = REAL_NOVALUE
    cord             = REAL_NOVALUE
    corvn            = REAL_NOVALUE
    corw             = REAL_NOVALUE
    cclwp            = REAL_NOVALUE
    orog             = REAL_NOVALUE
    sum_eng_fluxes   = REAL_NOVALUE
    sum_moist_flux   = REAL_NOVALUE
    aerosol_em       = REAL_NOVALUE
    so2_em           = REAL_NOVALUE
    nh3_em           = REAL_NOVALUE
    dms_em           = REAL_NOVALUE
    soot_em          = REAL_NOVALUE
    soot_hilem       = REAL_NOVALUE
    soot             = REAL_NOVALUE
    co2start         = REAL_NOVALUE
    co2end           = REAL_NOVALUE
    co2rate          = REAL_NOVALUE
    co2_emits        = REAL_NOVALUE
    co2flux          = REAL_NOVALUE
    co2              = REAL_NOVALUE
    tile_frac        = REAL_NOVALUE
    ozone            = REAL_NOVALUE
    aerosol          = REAL_NOVALUE
    so2              = REAL_NOVALUE
    sthu             = REAL_NOVALUE
    so4_aitken       = REAL_NOVALUE
    so4_accu         = REAL_NOVALUE
    so4_diss         = REAL_NOVALUE
    dms              = REAL_NOVALUE
    nh3              = REAL_NOVALUE
    soot_new         = REAL_NOVALUE
    soot_cld         = REAL_NOVALUE
    soot_aged        = REAL_NOVALUE

  END SUBROUTINE default_rundata


end module mod_rundata
