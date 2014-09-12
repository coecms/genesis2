 &CNTLSCM
 NFOR    =         109,
 MODEL_LEVELS_NML        =          38,
 LAND_POINTS     =           1
 /
 &INDATA
 SOIL_TYPE       =           3,
 TAPEYEAR_INIT   =           0,
 TAPEMONTH_INIT  =           1,
 TAPEDAY_INIT    =           1,
 TAPEHOUR_INIT   =           0,
 TAPEMIN_INIT    =           0,
 TAPESEC_INIT    =           0,
 YEAR_INIT       =        2001,
 MONTH_INIT      =           2,
 DAY_INIT        =           1,
 HOUR_INIT       =          12,
 MIN_INIT        =           0,
 SEC_INIT        =           0,
 LAT     =  -35.70000    ,
 LONG    =   148.2000    ,
 GRIDBOX_AREA    =   100000.0    ,
 SALT_DIM1       =           1,
 SALT_DIM2       =           1,
 SALT_DIM3       =           1,
 GATHER  = F
 /
 &RUNDATA
 NDAYIN  =           0,
 NMININ  =       14400,
 NSECIN  =           0,
 TIMESTEP        =   1800.000    ,
 NTRAD   =           6,
 NTRAD1  =           1,
 EXNAME_IN       = XXXXXXXX,
 EXNAME_OUT      = XXXXXXXX,
 RUNNO_IN        =           1,
 RUNNO_OUT       =           1,
 RESDUMP_DAYS    =           1,
 DUMP_DAYS       =           1,         360,        3600,           1,
 CHANGE_CLIM     =          10,
 MIN_TROP_LEVEL  =          11,
 MAX_TROP_LEVEL  =          31,
 NTML    =           5,
 NBDSC   =           0,
 NTDSC   =           0,
 ZH      =   1000.000    ,
 ALBSOIL =  0.2577000    ,
 LAND_ALB        =  0.2577000    ,
 FLAND_CTILE     =   1.000000    ,
 TSTAR_SEA       =   293.0000    ,
 DOLR_RTS        =  0.0000000E+00,
 CORT    =  0.9000000    ,
 CORD    =  0.9000000    ,
 CORVN   =  0.5000000    ,
 CORW    =  0.5000000    ,
 CCLWP   =  0.0000000E+00,
 OROG    =   436.0000    ,
 SUM_ENG_FLUXES  =  0.2429324    ,
 SUM_MOIST_FLUX  =  0.2429324    ,
 AEROSOL_EM      =  0.0000000E+00,
 SO2_EM  =  0.0000000E+00,
 NH3_EM  =  0.0000000E+00,
 DMS_EM  =  0.0000000E+00,
 SOOT_EM =  0.0000000E+00,
 SOOT_HILEM      =  0.0000000E+00,
 SOOT    =  0.0000000E+00,
 CO2START        =  4.9000001E-04,
 CO2END  =  4.9000001E-04,
 CO2RATE =  0.0000000E+00,
 CO2     = 38*4.9000001E-04  ,
 OZONE   =  6.0142398E-08,  5.9040101E-08,  7.1541002E-08,  8.7667203E-08,  8.8342198E-08,  8.4813401E-08,  8.6529802E-08,
   9.0025502E-08,  9.2927699E-08,  9.6471901E-08,  1.0159700E-07,  1.0734200E-07,  1.1270700E-07,  1.1666900E-07,  1.1812700E-07,
   1.1886200E-07,  1.2171400E-07,  1.2710299E-07,  1.3538900E-07,  1.4919800E-07,  1.7371200E-07,  2.0649500E-07,  2.4364800E-07,
   3.2176200E-07,  4.8411698E-07,  6.2242702E-07,  6.6710197E-07,  9.9807903E-07,  1.7634200E-06,  2.5422000E-06,  3.4880300E-06,
   5.1680900E-06,  7.5392700E-06,  9.9401796E-06,  1.1999900E-05,  1.3604100E-05,  1.3163800E-05,  6.0737002E-06,
 AEROSOL = 38*0.0000000E+00  ,
 SO2     = 38*0.0000000E+00  ,
 SO4_AITKEN      = 38*0.0000000E+00  ,
 SO4_ACCU        = 38*0.0000000E+00  ,
 SO4_DISS        = 38*0.0000000E+00  ,
 NH3     = 38*0.0000000E+00  ,
 SOOT_NEW        = 38*0.0000000E+00  ,
 SOOT_CLD        = 38*0.0000000E+00  ,
 SOOT_AGED       = 38*0.0000000E+00  
 /
 &LOGIC
 ANCYC   = T,
 ALTDAT  = F,
 TAPEIN  = F,
 ALTSOIL = F,
 TAPEOUT = F,
 TEST    = T,
 OBS     = T,
 STATS   = F,
 PRINDUMP_STEP   = T,
 PRINDUMP_DAY    = F,
 PRINDUMP_DAYS   = F,
 PRINDUMP_OBS    = F,
 LAND_SEA_MASK   = T,
 LAND_ICE_MASK   = F,
 SOIL_MASK       = T,
 NOFORCE = F,
 GRAFDUMP_DAY    = F,
 GRAFDUMP_DAYS   = F,
 GEOFORCE        = F,
 GEOINIT = F,
 GRAFDUMP_STEP   = T,
 CUMULUS = F,
 L_AREA_CLOUD    = F,
 LOCAL_TIME      = F,
 L_DO_T  = F,
 L_DO_INC_VELS   = F,
 L_DO_INC_Q      = F,
 PRINSTAT        = F,
 L_HYDROLOGY     = F,
 L_SPEC_Z0       = T,
 L_MURK_RAD      = F,
 RADCLOUD_FIXED  = F
 /
 &INMOSES
 INIT_M_SMCL     = F,
 INIT_M_FSMC     = F,
 INIT_M_STH      = F,
 SMI_OPT =           0,
 SMCLI   =   10.00000    ,   20.00000    ,   50.00000    ,   150.0000    ,   300.0000    ,   1000.000    ,
 STH     = 6*0.0000000E+00  ,
 CATCH   = 17*0.7500000      ,
 LAI     =   2.000000    ,   5.000000    , 3*2.000000       ,
 Z0_TILE = 18*2.9999999E-02  ,
 RGRAIN  =  0.8700000    ,   3.020000    ,  0.8880000    ,   2.701000    ,   4.244000    ,   2.509000    ,   4.283000    ,
 CS      =   12.10000    ,
 GS      =  1.6187482E-02,
 G_LEAF_ACC      = 5*-1.000000      ,
 NPP_FT_ACC      =  -1.037607    ,
 RESP_S_ACC      =  -1.037607    ,
 LW_DOWN =  0.0000000E+00,
 /
