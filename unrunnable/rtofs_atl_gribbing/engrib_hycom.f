      SUBROUTINE engrib_hycom(yy, mmm, dd, runhour, verfhour, flag,
     1    ibms, identity, zz, kz, field) 
C Subroutine to carry out the gribbing of hycom-related fields, suitable
C  for being called from something like the standard hycom archv2data
C  sorts of programs
C This version assumes that the grib file has been opened elsewhere,
C   and will be closed elsewhere
C Robert Grumbine 22 July 2005

!     Hycom standard modules for arrays
      use mod_za
      use mod_plot
      IMPLICIT none

!     Parameters for gribbing
      INCLUDE "grib_parms.h"
C Includes for bacio -- embedded in the grib_parms

C Arguments:
      INTEGER yy, mmm, dd, runhour, verfhour
      INTEGER identity
      REAL flag
      INTEGER ibms            !1 for use bit mask, 0 else
      INTEGER kz
      REAL zz(kz)   ! layer depths in z system

      REAL field(idm, jdm, kz)  !data to engrib
 
C gribit parms
      INTEGER griblen
      !PARAMETER (griblen = (100 + 28 + idm*jdm*(8+1)) / 8 )
      PARAMETER (griblen = (100 + 28 + 1200*1684*(8+1)) / 8 )
      CHARACTER grib(griblen) !message
      LOGICAL lbm(idm, jdm)     !bit mask true -> use pt
      INTEGER parmno, level
      INTEGER lgrib, ierr     !returned by gribit
      INTEGER mxbit
      INTEGER itl, il1, il2, iftu
      INTEGER itr, ip1, ip2, ids, inm, ina
      INTEGER igen, icen, ilpds, gridno, idrt
      REAL xlat1, xlon1, xlat2, xlon2, delx, dely, colat1
      INTEGER proj, ortru
      INTEGER iptv
C-----------
      REAL ssh(idm, jdm)
      CHARACTER*90 fname
      INTEGER i, j, k
      REAL mlmax, mlmin
C-------------
C     Center and grib table identification
      ilpds = 28  !pds length
      icen  =  7  !center
      igen  = 45 !generating process; currently using COFS figure
C     Hycom grib-related specs:
      idrt   = 6
      colat1 = 0   ! dummy when gribbing hycom
      gridno = 255 ! marker for non-grib projection

C     Lat long bounding boxes 
      xlat1 = minval(plat(:,:))
      xlon1 = minval(plon(:,:))
      xlat2 = maxval(plat(:,:))
      xlon2 = maxval(plon(:,:))
      delx = (xlon2 - xlon1)/float(idm - 1)
      dely = (xlat2 - xlat1)/float(jdm - 1)
CD      PRINT *,'bounds ',xlat1, xlat2, xlon1, xlon2, delx, dely

C     Parameter identification and packing material
      mxbit = 0    ! maximum number of bits to use, 0 -> infinite

      il1 = 0      ! il1, il2 are vertical level info, defaulting here 
      il2 = 0      !    to surface, will override for 3d parameters
                   ! ON388 table 3

      iftu = 1     ! forecast time unit, 1 -> hours (table 4)
      itr  = 10    ! forecast time range, 10 -> valid time in P1
      ip1  = 0     !  valid time, relative to reference time
      ip2  = 0     ! Averaging period (table 5)

      ina  = 0
      inm  = 0
      


      IDS = -LOG10(increment(identity))
      parmno = parms(identity)
      iptv   = tables(identity)

      IF (identity .LE. parm_v_btrop) THEN
        IF (identity .EQ. parm_u_btrop .OR. 
     1      identity .EQ. parm_v_btrop) THEN
          itl = 201  ! ocean column-average
        ELSE
          itl = 102  ! msl
        ENDIF
          
C Rescale for those parameters which need rescaling -- 2d approach
        IF (identity .EQ. parm_umix .OR.
     1      identity .EQ. parm_vmix .OR.
     1      identity .EQ. parm_u_btrop .OR.
     1      identity .EQ. parm_v_btrop ) THEN
          ssh = rescale(identity)*field(:,:,1)
        ELSE IF (identity .EQ. parm_tmix) THEN
          ssh = rescale(identity) + field(:,:,1)
        ELSE
          ssh = field(:,:,1)
        ENDIF
C       Create a bit mask if requested:
        IF (ibms .EQ. 1) THEN
          lbm = .false.
          DO i = 1, idm
          DO j = 1, jdm
            IF (field(i,j,1) .NE. flag) lbm(i,j) = .true.
          ENDDO
          ENDDO
        ENDIF

C       Debugging
        IF (identity .EQ. parm_mix_dpth) THEN
          mlmax = 0.0
          mlmin = 1.e36
          DO i = 1, idm
          DO j = 1, jdm
            IF (lbm(i,j)) THEN
              mlmax = max(field(i,j,1), mlmax)
              mlmin = min(field(i,j,1), mlmin)
            ENDIF
          ENDDO
          ENDDO
          PRINT *,'ml max, min',mlmax, mlmin
        ENDIF

C Hycom version
        CALL gribit(ssh, lbm, 6, idm, jdm, mxbit, colat1, 
     &                  ilpds, iptv, icen,igen,ibms,parmno,ITL,IL1,IL2,
     &                  yy, mmm, dd,runhour,
     &                  IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  xlat1,xlon1,xlat2,xlon2,delx,dely,ORTRU,PROJ,
     &                  gridno,
     &                  grib,lgrib,ierr)
CD      PRINT *,'bounds ',xlat1, xlat2, xlon1, xlon2, delx, dely


      IF (ierr .NE. 0) THEN
        PRINT *,'error ',ierr,' while trying to construct grib message '
      ELSE
        ierr = bacio(BAWRITE, start, newpos,
     1                SIZEOF_CHARACTER, lgrib, nactual, fdes, fname,
     2                grib)
C       XXX test on ierr from bacio
        start = newpos
      ENDIF

      ELSE !identity >= 14 are for z levels
        itl = 160 ! 160 for z levels
        il1 = 1

        DO k = 1, kz
          il2 = zz(k)
          IF (identity .EQ. parm_u_vel .OR.
     1        identity .EQ. parm_v_vel .OR. 
     2        identity .EQ. parm_salin) THEN
            ssh = field(:,:,k) * rescale(identity)
          ELSE IF (identity .EQ. parm_temp .OR.
     1             identity .EQ. parm_density) THEN
            ssh = field(:,:,k) + rescale(identity)
          ELSE
            ssh = field(:,:,k)
          ENDIF   
C         Create a bit mask if requested:
          IF (ibms .EQ. 1) THEN
            lbm = .false.
            DO i = 1, idm
            DO j = 1, jdm
              IF (field(i,j,k) .NE. flag) lbm(i,j) = .true.
            ENDDO
            ENDDO
          ENDIF
C Hycom version
        CALL gribit(ssh, lbm, 6, idm, jdm, mxbit, colat1,
     &                  ilpds, iptv, icen,igen,ibms,parmno,ITL,IL1,IL2,
     &                  yy, mmm, dd,runhour,
     &                  IFTU,IP1,IP2,ITR,INA,INM,IDS,
     &                  xlat1,xlon1,xlat2,xlon2,delx,dely,ORTRU,PROJ,
     &                  gridno,
     &                  grib,lgrib,ierr)
CD        PRINT *,'bounds ',xlat1, xlat2, xlon1, xlon2, delx, dely

        IF (ierr .NE. 0) THEN
          PRINT *,'error ',ierr,
     1           ' while trying to construct grib message '
        ELSE
          ierr = bacio(BAWRITE, start, newpos,
     1                SIZEOF_CHARACTER, lgrib, nactual, fdes, fname,
     2                grib)
          start = newpos
        ENDIF

      ENDDO

      ENDIF

      RETURN
      END
