      SUBROUTINE GUVTQP ( lunseq, iyr, imn, idy, ihr, datao, iret )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    GUVTQP      DESCRIPTIVE TITLE NOT PAST COL 70
C   PRGMMR: TUCCILLO         ORG: W/NMCXX    DATE: 88-08-03
C           K. BRILL	     W/NP22          DATE: 96-02
C
C ABSTRACT: THIS SUBROUTINE RETRIEVES NMC POINT FORECAST VALUES FROM A
C   SEQUENTIAL DATA SET.
C
C   FUNCTIONAL DESCRIPTION
C
C      THIS SUBROUTINE READS THE NEXT STATION FROM THE DATABASE
C      AND RETURNS THE DATA.
C
C
C   SPECIAL COMMENTS
C
C     THE PRECIPITATION VALUES READ FROM THE DATABASE REPRESENT
C     CUMULATIVE TOTALS.  TO OBTAIN INDIVIDUAL HOURLY VALUES,
C     THIS PROGRAM MAKES SEQUENTIAL HOURLY READS FOR DATA, RETAINS
C     THE PRECIPITATION VALUE FROM THE PREVIOUS READ AND
C     SUBTRACTS IT FROM THE CURRENT PRECIPITATION VALUE.
C
C PROGRAM HISTORY LOG:
C   88-08-03  TUCCILLO
C   92-04-22  CAVANAUGH   ADDED VALUE FOR VERTICAL VELOCITY
C   95-12     BRILL	  MODIFIED FOR SEQUENTIAL READ ON CRAY
C   96-02     BRILL	  MODIFY TO FEED BUFRIZING PROGRAM
C   96-05-09  BRILL       FIX PRECIP FOR HOURLY ACCUMULATIONS
C
C USAGE:    CALL GUVTQP ( lunseq, iyr, imn, idy, ihr, datao, IRET )
C   INPUT ARGUMENT LIST:
C     LUNSEQ	   INT   Unit number of sequential data
C
C   OUTPUT ARGUMENT LIST:
C     IYR          INT   YEAR
C     IMN          INT   MONTH
C     IDY          INT   DAY
C     IHR          INT   RUN TIME
C     DATAO	   FLT   Data -- see contents below
C     IRET	   INT   Return code (0=normal)
C
C  Contents of DATA:
C
C     Name         Index        Description and units
C
C     FTIM           1      FORECAST TIME (S)
C     STNM           2      STATION NUMBER
C     RLAT           3      STATION LATITUDE (DEGREES)
C     RLON           4      STATION LONGITUDE (DEGREES)
C     SELV           5      STATION ELEVATION    (METERS)
C
C     P(NZ)         6-21    PRESSURE AT LAYER MIDPOINT (PA)
C     T(NZ)        22-37    TEMPERATURE          (K)
C     U(NZ)        38-53    NORTH RELATIVE U-COMPONENT (M/S)
C     V(NZ)        54-69    NORTH RELATIVE V-COMPONENT (M/S)
C     Q(NZ)        70-85    SPECIFIC HUMIDITY    (KG/KG)
C     OMEG(NZ)     86-101   VERTICAL VELOCITY (PA/SEC)
C     DTAR(NZ)    102-117   RADIATIVE HEATING PROFILE (K/S)
C
C     PMSL	    118     MEAN SEA LEVEL PRESSURE (PA)
C     PRSS          119     TERRAIN PRESSURE     (PA)
C     SKTK          120     SKIN TEMPERATURE     (K)
C     P01M          121     1 HOUR ACCUMULATED TOTAL PRECIP. (MM)
C     C01M          122     1 HOUR ACCUMULATED SUB-GRID SCALE PRECIP. (M
C     FXLH          123     LATENT HEAT FLUX (W/M**2)
C     FXSH          124     SENSIBLE HEAT FLUX (W/M**2)
C     FXSS          125     SUB-SOIL FLUX    (W/M**2)
C     SWRD          126     SHORTWAVE RADIATION FLUX  (W/M**2)
C     LWRD          127     NET LONGWAVE RADIATION FLUX (W/M**2)
C     SLTK          128     SUB-SOIL TEMPERATURE (K)
C     LCLD          129     LOW CLOUD AMOUNT (%)
C     MCLD          130     MID CLOUD AMOUNT (%)
C     HCLD          131     HI CLOUD AMOUNT (%)
C     CLPL          132     LO CLOUD PRESSURE (PA)
C     CLPM          133     MID CLOUD PRESSURE (PA)
C     CLPH          134     HI CLOUD PRESSURE (PA)
C     CCLD          135     CONVECTIVE CLOUD AMOUNT (%)
C     CLDL          136     PRESSURE AT BASE OF CLOUD (PA)
C     CLDT          137     PRESSURE AT TOP OF CLOUD (PA)
C
C     Unknowns    138-167   Unknown values
C
C
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C     LUNSEQ  -- points to input sequential file
C
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE:  CRAY
C
C$$$
C**********************************************************************
C
C CHANGE HISTORY
C
C
C**********************************************************************
C
      PARAMETER	(MXSTN=500,MXFHR=49,MXDAT=134)
      PARAMETER ( NZ = 16, IDIMM = 500, NDATA = 146 )
      PARAMETER ( DTR = .01745329 )
      PARAMETER ( RD = 287.04, G = 9.80616 )
      REAL(8) datao (*)
C*
      REAL    ppk (NZ)
      REAL    data ( IDIMM )
C*
      REAL    prcpg (MXSTN), prcpc (MXSTN)
      INTEGER istns (MXSTN), npstn
      COMMON    prcpg, prcpc, istns, npstn
C*
      LOGICAL		found
      CHARACTER*4 	cpack
C*
      DATA    rot /-105./
C*
      DATA ppk/   0.9823040,
     *            0.9431677,
     *            0.8967100,
     *            0.8436723,
     *            0.7848307,
     *            0.7210065,
     *            0.6530743,
     *            0.5819620,
     *            0.5086423,
     *            0.4341393,
     *            0.3595226,
     *            0.2859137,
     *            0.2144695,
     *            0.1463738,
     *            0.0827399,
     *            0.0223290
     *            /
C-----------------------------------------------------------------------
	iret = 0
C
C*	Read the next station record.
C
	READ ( lunseq, IOSTAT=iostat ) cpack,iscc,ista,alat,alon,
     +			( data (ij), ij = 1, NDATA )
	IF ( iostat .lt. 0 ) THEN
	    iret = 1
	    RETURN
	ELSE IF ( iostat .gt. 0 ) THEN
	    PRINT *, ' IOSTAT = ', iostat
	    iret = -1
	    RETURN
	ENDIF
	CALL W3YMDH4 ( cpack, iyr, imn, idy, ihr, 1 )
	datao (1) = FLOAT ( iscc )
	IFHR = iscc / 3600
	datao (2) = ista
C 
C*     OBTAIN BASE VALUES FROM THE DATABASE TO USE IN CALCULATION
C*     OF PROFILE VALUES:
C
      datao (3)   = ALAT
      datao (4)   = ALON
      PSTAR       = DATA ( 4 * NZ + 1 )
      PSTARI      = 1. / PSTAR
C
      PSTAR       = 1000. * PSTAR
      ps	  = PSTAR
      datao (119) = PSTAR * 100.
C
      datao (120) = DATA ( 4 * NZ + 2 )
      TSKIN	  = DATA ( 4 * NZ + 2 )
      datao (5)   = DATA ( 4 * NZ + 6 ) * ( 1004. / 9.8 )
      zsfc        = datao (5)
      DIF         = DTR * ( ROT - ALON )
C
      COSDIF      = COS ( DIF )
C
      SINDIF      = SIN ( DIF )
C
C*    FILL UP THE PROFILE VALUES FOR THE NUMBER OF LEVELS NZ
C*    CALCULATE AND STORE U,V,T,Q AND P FOR LEVEL K
C
      ioff = 5
      DO 100 K = 1, NZ
         PPP 			   = PSTAR * PPK(K)
	 p1			   = PPP
         datao ( ioff + k )        = PPP * 100.0
         datao ( ioff + NZ + k )   = DATA ( 2 * NZ + K ) * PSTARI *
     +					( (PPP*0.001) ** 0.286 )
	 t1			   = datao ( ioff + NZ + k )
         UG         		   = DATA (        K ) * PSTARI
         VG         		   = DATA (   NZ + K ) * PSTARI
         datao ( ioff + 2*NZ + k ) = UG * COSDIF - VG * SINDIF
         datao ( ioff + 3*NZ + k ) = VG * COSDIF + UG * SINDIF
         datao ( ioff + 4*NZ + k ) = DATA ( 3 * NZ + K ) * PSTARI
	 q1			   = datao ( ioff + 4*NZ + k )
100   CONTINUE
C
C*	Compute PMSL (118) using Sheull correction.
C
	IF ( zsfc .ne. 0. ) THEN
	    tv1 = t1 * ( 1. + .608 * q1 )
	    dz1 = - ( RD * tv1 / G ) * ALOG ( p1 / ps )
	    tvs = tv1 + .0065 * dz1
	    tv0 = tv1 + .0065 * ( dz1 + zsfc )
	    IF ( zsfc .gt. 0. ) THEN
	    	IF ( tvs .gt. 290.66 ) THEN
		    tv0 = 290.66 - .005 * ( tvs - 290.66 ) ** 2
	    	ELSE IF ( tv0 .gt. 290.66 ) THEN
		    tv0 = 290.66
	    	END IF
	    END IF
	    ff = (G * zsfc ) / ( RD * .5 * ( tvs + tv0 ) )
	    datao (118) = datao (119) * EXP (ff)
	ELSE
	    datao (118) = datao (119)
	END IF
C
C*    SET VARIABLE CONSTANTS FOR RADIATIVE HEATING PROFILE
C
      KK         = 4 * NZ + 1
      ASTAR      = DATA ( 4*NZ + 1 )
C
C*    -OLD      ASTARI     = ( 3600. * 24. ) / ASTAR
C
      astari     = 1.0 / astar
      DO 110 K = 1, NZ
C
C*       COMPUTE RADIATIVE HEATING VALUE AND VERTICAL VELOCITY
C*       FOR EACH PROFILE LEVEL
C
         PPP  = ( ASTAR * PPK(K) ) ** 0.286
         omeg   = DATA(6 * NZ + 31 + K)
         datao ( ioff + 6*NZ + k ) = DATA(4*NZ+9+K) * ASTARI * PPP
C
C*	 Convert to pa / s.
C
	 datao ( ioff + 5*NZ + k ) = omeg * 100.
110   CONTINUE
C 
C*    DETERMINE OFFSET TO GET CLOUD AMOUNT VALUES
C
      JOFF = 6*NZ + 20
C 
C*    COLLECT CLOUD AMOUNT VALUES
C
      datao (135)          = DATA ( JOFF + 1  ) * 100.
      datao (129)          = DATA ( JOFF + 2  ) * 100.
      datao (130)          = DATA ( JOFF + 3  ) * 100.
      datao (131)          = DATA ( JOFF + 4  ) * 100.
      datao (132)          = DATA ( JOFF + 5  ) * 100000.
      datao (133)          = DATA ( JOFF + 6  ) * 100000.
      datao (134)          = DATA ( JOFF + 7  ) * 100000.
      datao (136)          = DATA ( JOFF + 8  ) * 100000.
      datao (137)          = DATA ( JOFF + 9  ) * 100000.
C
C*    COLLECT ENERGY BUDGET PARAMETERS
C
      datao (128)  = DATA ( 5*NZ + 16 )
      TSUB	   = DATA ( 5*NZ + 16 )
      datao (126)  = DATA ( 5*NZ + 11) * 1000.
      datao (127)  = ( DATA ( 5*NZ + 10) - DATA ( 6*NZ + 19 ) ) * 1000.
      datao (124)  = DATA ( 6*NZ + 30 ) * ( -1000. )
      datao (123)  = DATA ( 6*NZ + 31 ) * ( -1000. * 2.5E+06 )
      datao (125)  = DATA ( 5*NZ + 17 ) * 7.29E-05 * ( TSUB - TSKIN )
C
C*    Load the unknown values.
C
	datao (138) = data ( 4*NZ + 3 )
	datao (139) = data ( 4*NZ + 4 )
	datao (140) = data ( 4*NZ + 5 )
	datao (141) = data ( 4*NZ + 7 )
	datao (142) = data ( 5*NZ + 12 )
	datao (143) = data ( 5*NZ + 13 )
	datao (144) = data ( 5*NZ + 14 )
	datao (145) = data ( 5*NZ + 15 )
	datao (146) = data ( 5*NZ + 16 )
	iq = 146
	DO i = 2, 20
	    iq = iq + 1
	    datao (iq) = data ( 6*NZ + i )
	END DO
	datao (166) = data (144)
	datao (167) = data (145)
	datao (168) = data (146)
C 
C*    COLLECT PRECIP VALUES ONLY IF NOT THE INITIAL FORECAST HOUR
C*    SET THE DEFAULT PRECIP. VALUES TO ZERO
C
      IF (IFHR.EQ.0) THEN
	  datao ( 121 ) = 0.
	  datao ( 122 ) = 0.
	  npstn = npstn + 1
	  istns (npstn) = ista
	  prcpg (npstn) = 0.
	  prcpc (npstn) = 0.
      ELSE
          sgsp     = DATA ( 4*NZ + 8 )
          gsp      = DATA ( 4*NZ + 9 )
	  found = .false.
	  iqs = 0
	  DO WHILE ( .not. found )
	      iqs = iqs + 1
	      IF ( istns (iqs) .eq. ista ) THEN
		  found = .true.
		  xsgsp = sgsp - prcpc (iqs)
		  xgsp = gsp - prcpg (iqs)
		  IF ( MOD ( IFHR, 12 ) .eq. 0. ) THEN
		      prcpc (iqs) = 0.0
		      prcpg (iqs) = 0.0
		  ELSE
		      prcpc (iqs) = sgsp
		      prcpg (iqs) = gsp
		  END IF
	      END IF
	  END DO
	  datao (121) = ( xsgsp + xgsp ) * 1000.
	  datao (122) = ( xsgsp ) * 1000.
      END IF
C*
      RETURN
C*
      END
