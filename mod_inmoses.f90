module mod_inmoses


  implicit none
  LOGICAL :: init_m_smcl, init_m_fsmc, init_m_sth
  INTEGER :: smi_opt
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: smcli
  REAL, DIMENSION(:,:), ALLOCATABLE   :: fsmc
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: sth
  REAL, DIMENSION(:,:), ALLOCATABLE   :: canopy_height
  REAL, DIMENSION(:,:), ALLOCATABLE   :: catch
  REAL, DIMENSION(:,:), ALLOCATABLE   :: snow_tile
  REAL, DIMENSION(:,:), ALLOCATABLE   :: lai
  REAL, DIMENSION(:,:), ALLOCATABLE   :: z0_tile
  REAL, DIMENSION(:,:), ALLOCATABLE   :: tstar_tile
  REAL, DIMENSION(:,:), ALLOCATABLE   :: canopy_tile
  REAL, DIMENSION(:), ALLOCATABLE   :: canopy
  REAL, DIMENSION(:,:), ALLOCATABLE   :: frac
  REAL, DIMENSION(:), ALLOCATABLE     :: frac_disturb
  REAL, DIMENSION(:,:), ALLOCATABLE   :: infil_tile
  REAL, DIMENSION(:,:), ALLOCATABLE   :: rgrain
  REAL, DIMENSION(:), ALLOCATABLE     :: cs
  REAL, DIMENSION(:), ALLOCATABLE     :: gs
  REAL, DIMENSION(:,:), ALLOCATABLE   :: g_leaf_acc
  REAL, DIMENSION(:,:), ALLOCATABLE   :: g_leaf_phen_acc
  REAL, DIMENSION(:), ALLOCATABLE     :: npp_ft_acc
  REAL, DIMENSION(:,:), ALLOCATABLE   :: resp_w_ft_acc
  REAL, DIMENSION(:), ALLOCATABLE     :: resp_s_acc
  REAL, DIMENSION(:,:), ALLOCATABLE   :: lw_down
  REAL, DIMENSION(:,:), ALLOCATABLE   :: frac_typ

 
  NAMELIST /INMOSES/                                    &
    init_m_smcl, init_m_fsmc, init_m_sth, smi_opt,      &
    smcli, fsmc,  &
    sth, canopy_height, catch, snow_tile, lai, z0_tile, &
    tstar_tile, canopy_tile, frac, frac_disturb,        &
    infil_tile, rgrain, cs, gs, g_leaf_acc,             &
    g_leaf_phen_acc, npp_ft_acc, resp_w_ft_acc,         &
    resp_s_acc, lw_down, frac_typ, canopy

 
contains

  SUBROUTINE init_inmoses(row_length, rows, max_soil_moist_levs,    &
        ntfp, max_no_ntiles, ntype)
    implicit none
    integer, intent(in)   :: row_length, rows, max_soil_moist_levs, &
        ntfp, max_no_ntiles, ntype

    if( allocated( smcli )) deallocate( smcli ) 
    if( allocated( fsmc )) deallocate( fsmc ) 
    if( allocated( sth )) deallocate( sth ) 
    if( allocated( canopy_height )) deallocate( canopy_height ) 
    if( allocated( catch )) deallocate( catch ) 
    if( allocated( snow_tile )) deallocate( snow_tile ) 
    if( allocated( lai )) deallocate( lai ) 
    if( allocated( z0_tile )) deallocate( z0_tile ) 
    if( allocated( tstar_tile )) deallocate( tstar_tile ) 
    if( allocated( canopy_tile )) deallocate( canopy_tile ) 
    if( allocated( frac )) deallocate( frac ) 
    if( allocated( frac_disturb )) deallocate( frac_disturb ) 
    if( allocated( infil_tile )) deallocate( infil_tile ) 
    if( allocated( rgrain )) deallocate( rgrain ) 
    if( allocated( cs )) deallocate( cs ) 
    if( allocated( gs )) deallocate( gs ) 
    if( allocated( g_leaf_acc )) deallocate( g_leaf_acc ) 
    if( allocated( g_leaf_phen_acc )) deallocate( g_leaf_phen_acc ) 
    if( allocated( npp_ft_acc )) deallocate( npp_ft_acc ) 
    if( allocated( resp_w_ft_acc )) deallocate( resp_w_ft_acc ) 
    if( allocated( resp_s_acc )) deallocate( resp_s_acc ) 
    if( allocated( lw_down )) deallocate( lw_down ) 
    if( allocated( frac_typ )) deallocate( frac_typ ) 
    if( allocated( canopy )) deallocate( canopy )


    allocate( smcli(row_length, rows, max_soil_moist_levs) )
    allocate( fsmc(row_length, rows) )
    allocate( sth(row_length, rows, max_soil_moist_levs) )
    allocate( canopy_height(row_length * rows, ntfp) )
    allocate( catch(row_length * rows, max_no_ntiles) )
    allocate( snow_tile(row_length * rows, max_no_ntiles) )
    allocate( lai(row_length * rows, ntfp) )
    allocate( z0_tile(row_length * rows, ntype) )
    allocate( tstar_tile(row_length * rows, ntype) )
    allocate( canopy_tile(row_length * rows, max_no_ntiles) )
    allocate( frac_typ(row_length * rows, ntype) )
    allocate( frac(row_length * rows, ntype) )
    allocate( frac_disturb(row_length * rows) )
    allocate( infil_tile(row_length * rows, max_no_ntiles) )
    allocate( rgrain(row_length * rows, max_no_ntiles) )
    allocate( cs(row_length * rows) )
    allocate( gs(row_length * rows) )
    allocate( g_leaf_acc(row_length * rows, ntfp) )
    allocate( g_leaf_phen_acc(row_length * rows, ntfp) )
    allocate( npp_ft_acc(row_length * rows) )
    allocate( resp_w_ft_acc(row_length * rows, ntfp) )
    allocate( resp_s_acc(row_length * rows) )
    allocate( lw_down(row_length, rows) )
    allocate( canopy(max_no_ntiles) )

  END SUBROUTINE init_inmoses

  SUBROUTINE default_inmoses()
    use mod_parameters
    implicit none

    if (                                            &
          .not. allocated( smcli ) .or.             &
          .not. allocated( fsmc ) .or.              &
          .not. allocated( sth ) .or.               &
          .not. allocated( canopy_height ) .or.     &
          .not. allocated( catch ) .or.             &
          .not. allocated( snow_tile ) .or.         &
          .not. allocated( lai ) .or.               &
          .not. allocated( z0_tile ) .or.           &
          .not. allocated( tstar_tile ) .or.        &
          .not. allocated( canopy_tile ) .or.       & 
          .not. allocated( frac ) .or.              &
          .not. allocated( frac_disturb ) .or.      &
          .not. allocated( infil_tile ) .or.        &
          .not. allocated( rgrain ) .or.            &
          .not. allocated( cs ) .or.                &
          .not. allocated( gs ) .or.                &
          .not. allocated( g_leaf_acc ) .or.        &
          .not. allocated( g_leaf_phen_acc ) .or.   &
          .not. allocated( npp_ft_acc ) .or.        &
          .not. allocated( resp_w_ft_acc ) .or.     &
          .not. allocated( resp_s_acc ) .or.        &
          .not. allocated( frac_typ ) .or.          &
          .not. allocated( canopy   ) .or.          &
          .not. allocated( lw_down )                &
          ) then
      write(*,*) "please call init_inmoses before default_inmoses"
      stop 5
    end if

    init_m_smcl     = .FALSE.
    init_m_fsmc     = .FALSE.
    init_m_sth      = .FALSE.
    smi_opt         = INT_NOVALUE
    smcli           = REAL_NOVALUE
    fsmc            = REAL_NOVALUE
    sth             = REAL_NOVALUE
    canopy_height   = REAL_NOVALUE
    catch           = REAL_NOVALUE
    snow_tile       = REAL_NOVALUE
    lai             = REAL_NOVALUE
    z0_tile         = REAL_NOVALUE
    tstar_tile      = REAL_NOVALUE
    canopy_tile     = REAL_NOVALUE
    frac            = REAL_NOVALUE
    frac_disturb    = REAL_NOVALUE
    infil_tile      = REAL_NOVALUE
    rgrain          = REAL_NOVALUE
    cs              = REAL_NOVALUE
    gs              = REAL_NOVALUE
    g_leaf_acc      = REAL_NOVALUE
    g_leaf_phen_acc = REAL_NOVALUE
    npp_ft_acc      = REAL_NOVALUE
    resp_w_ft_acc   = REAL_NOVALUE
    resp_s_acc      = REAL_NOVALUE
    lw_down         = REAL_NOVALUE
    frac_typ        = REAL_NOVALUE

  END SUBROUTINE default_inmoses


end module mod_inmoses
