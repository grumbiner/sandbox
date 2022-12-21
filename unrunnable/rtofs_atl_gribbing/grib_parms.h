      
C  Parameters and values for working with gribbing hycom fields
C    Parms are: name (mapped to number), table number, parameter #, 
C       desired increment (real), rescaling (if any)
      
C surface, mixed layer, or column-averaged values:
      INTEGER parm_montg1, parm_surfhgt, parm_surflx, parm_salflx
      INTEGER parm_bl_dpth, parm_mix_dpth
      INTEGER parm_tmix, parm_smix, parm_thmix, parm_umix, parm_vmix 
      INTEGER parm_u_btrop, parm_v_btrop

      INTEGER parm_u_vel, parm_v_vel, parm_thknss, parm_temp, parm_salin
      INTEGER parm_density
C assign index:
      PARAMETER(parm_montg1 =  1)
      PARAMETER(parm_surfhgt = 2)
      PARAMETER(parm_surflx =  3)
      PARAMETER(parm_salflx = 4)
      PARAMETER(parm_bl_dpth = 5)
      PARAMETER(parm_mix_dpth = 6)
      PARAMETER(parm_tmix = 7)
      PARAMETER(parm_smix = 8)
      PARAMETER(parm_thmix = 9)
      PARAMETER(parm_umix = 10 )
      PARAMETER(parm_vmix = 11 )
      PARAMETER(parm_u_btrop = 12)
      PARAMETER(parm_v_btrop = 13)
      PARAMETER(parm_u_vel = 14)
      PARAMETER(parm_v_vel = 15)
      PARAMETER(parm_thknss = 16)
      PARAMETER(parm_temp = 17)
      PARAMETER(parm_salin = 18)
      PARAMETER(parm_density = 19)
      INTEGER nparms
      PARAMETER (nparms = 19)

C set up value-holders
C  This really lends itself to user-defined types ...
      INTEGER tables(nparms), parms(nparms)
      REAL increment(nparms), rescale(nparms)
C     note that 0 is a dummy for 'we don't know yet'
      DATA tables /1, 129, 128, 129, 0, 1, 128, 128, 0, 0, 0, 1, 1, 
     1                    1, 1, 0, 1, 1, 1/
      DATA parms /37, 198, 171, 199, 0, 67, 175, 176, 0, 0, 0, 49, 50,
     1             49, 50, 0, 11, 88, 89 /
      DATA increment /1., 0.001, 1, 0.01, 1, 1, 0.01, 0.01, 0.01,
     1           0.01, 0.01, 0.01, 0.01,
     2           0.001, 0.001, 1.0, 0.001, 1.e-6, 0.001 /
      DATA rescale /0., 0., 0., 0., 0., 0., 273.15, 0., 0.,
     1           0.01, 0.01, 0.01, 0.01, 
     2           0.01, 0.01, 0., 273.15, 1.e-3, 1000. /

C Include these for bacio-related things to work properly:
      INCLUDE "locale.inc"
      INCLUDE "clib.inc"
      INTEGER  :: fdes 
      INTEGER  :: newpos 
      INTEGER  :: nactual 
      INTEGER  :: start 
      COMMON /engribbing/ fdes, newpos, nactual, start
      INTEGER bacio

C May be better to put these in common
