      FUNCTION inarea(mod, sid, xobi, yobi, iar, rm1, rm2 )
C**********************************************************************
C     rm1 = rotation matrix element 1,1
C     rm2 = rotation matrix element 1,2
C*
      INCLUDE 'parm.inc'

      CHARACTER*(*) sid
      CHARACTER*3 regions(29)
      COMMON /grdef/ mode(mxarea), imax(mxarea), imin(mxarea), 
     +            jmax(mxarea), jmin(mxarea), alat1(mxarea), 
     +            elon1(mxarea), dxx(mxarea), dyy(mxarea), 
     +            elonv(mxarea), alatan(mxarea), latlong(mxarea), 
     +            lambert(mxarea), polarstereo(mxarea), numreg(mxarea),
     +            ig104(147,110), regions
      LOGICAL latlong, lambert, polarstereo
      CHARACTER*8 stnlist
      real*8 xobi,yobi,xob,yob
      COMMON /stndef/ nstns (mxarea), stnlist (mxarea,maxj)
      LOGICAL vtflg
      COMMON /cnvrsns/ vtflg, nmbgrd (maxmod), concon (maxmod),
     +		       cenlon (maxmod)
C-----------------------------------------------------------------------
      inarea = -1
C
      IF ( xobi .lt. 0.0 ) THEN
	xob = xobi + 360.
      ELSE
	xob = xobi
      END IF
      yob = yobi
c     print*,'xob,yob=',xob,yob
      IF (mode(iar).eq.1.or.mode(iar).eq.2) THEN
C       CHECK WHETHER THE OBSERVATION IS LOCATED WITHIN GRID AREA
C       ---------------------------------------------------------
C       Depending on the grid type,
C       CALCULATE Grid coordinates of obs lat,long 
C       Change has been made for LATLON calculation --- Yuejian Zhu
C       ------------------------------------------
        IF (latlong(iar)) THEN
C         Latitiude - Longitude grid  - NOT global
          xi = (xob-elon1(iar)) / dxx(iar) + 1.0
          yj = (yob-alat1(iar)) / dyy(iar) + 1.0
        END IF
        IF (polarstereo(iar)) THEN
C         Polar Stereographic grid
C         W3FB06 wants grid spacing in meters
          dxm = dxx(iar) * 1000.
          CALL w3fb06(yob,xob,alat1(iar),elon1(iar),dxm,elonv(iar),xi,yj
     +                )
        END IF
        IF (lambert(iar)) THEN
C         Lambert Conic Conformal grid
C         W3FB11 wants grid spacing in meters
          dxm = dxx(iar) * 1000.
          CALL w3fb11(yob,xob,alat1(iar),elon1(iar),dxm,elonv(iar),
     +                alatan(iar),xi,yj)
        END IF
        kxi = nint(xi)
        kyj = nint(yj)
c       PRINT *, 'IMIN,IMAX,JMIN,JMAX=', imin(iar), imax(iar), 
c    +              jmin(iar), jmax(iar), kxi, kyj
C       CHECK IF OB IS WITHIN DOMAIN
C       ----------------------------
        IF (kxi.lt.imin(iar).or.kxi.gt.imax(iar).or.kyj.lt.jmin(iar).or.
     +              kyj.gt.jmax(iar)) THEN
          inarea = -1
c         print*,'inarea is -1'
          RETURN
        ELSE
          IF (mode(iar).eq.1.or.(mode(iar).eq.2.and.ig104(kxi,kyj).eq.
     +                numreg(iar))) inarea = 0
        END IF
      ELSE IF ( mode(iar) .eq. 3 ) THEN
	inarea = -1
	ixs = 0
	DO WHILE ( inarea .eq. -1 .and. ixs .lt. nstns (iar) )
	  ixs = ixs + 1
	  IF ( stnlist (iar,ixs) .eq. sid ) inarea = 0
	END DO
      ELSE
        inarea = -1
        PRINT *, ' INVALID MODE IN INAREA', mode(iar)
      END IF
C
C*	Compute wind rotation matrix elements.
C
	rm1 = 1.0
	rm2 = 0.0
	IF ( nmbgrd (mod) .gt. 0 .and. inarea .eq. 0 ) THEN
	  IF ( cenlon (mod) .gt. 180.0 ) THEN
		rlamc = cenlon (mod) - 360.0
	  ELSE
		rlamc = cenlon (mod)
	  END IF
	  IF ( xob .gt. 180. ) THEN
		rlam = xob - 360.
	  ELSE
		rlam = xob
	  END IF
	  theta = ( rlam - rlamc ) * factor * concon (mod)
	  rm1 = COS ( theta )
	  rm2 = SIN ( theta )
	END IF
C*
      RETURN
      END
