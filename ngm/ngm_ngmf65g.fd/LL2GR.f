      SUBROUTINE LL2GR ( ALAT, ALON, ITYPE,
     *           XG, YG, IN00, NGRID, CF00, CF10, CF01, CF11, IERR )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    LL2GR       COMPUTES LAT/LON TO GRID INTERP INFO
C   PRGMMR: J TUCCILLO       ORG: W/NMC22    DATE: 90-06-14
C
C ABSTRACT: COMPUTES WHICH GRID ( FINEST ) A LAT/LON POINT IS
C   .       LOCATED ON, THE ADDRESS OF THE LOWER LEFT OF THE FOUR
C   .       POINTS SURROUNDING THE LAT/LON POINT AND THE BI-LINEAR
C   .       INTERPOLATION COEFFICIENTS FOR THE FOUR POINTS.
C   .
C   READ ME :  THE CODE WILL CHECK FOR A SOUTHERN HEMISPHERIC
C   .          LATITUDE AND RETURN IERR = 1, DESIGNATING AN INVALID
C   .          INPUT. CHECK 'ALATMIN' IN DATA STATEMENT.
C   .
C   .          THIS ROUTINE SHOULD ONLY BE USED IN BETWEEN TIME STEPS.
C   .
C
C PROGRAM HISTORY LOG:
C   90-06-14  J TUCCILLO
C   92-03-17  R WOBUS  -  ADD ITYPE=3
C
C USAGE:  CALL LL2GR ( ALAT, ALON, ITYPE,
C              XG, YG, IN00, NGRID, CF00, CF10, CF01, CF11, IERR )
C   INPUT ARGUMENT LIST:
C     ALAT     - LATITUDE IN DEGREES  ( N (+), S (-) )
C     ALON     - LONGITUDE IN DEGREES ( E (+), W (-) )
C     ITYPE    - 0 FOR H PT, 1 FOR U PT, 2 FOR V PT, 3 FOR MID-POINT   
C
C   OUTPUT ARGUMENT LIST:
C      XG      - REAL LOCATION OF THE LAT/LON PT IN X
C      YG      - REAL LOCATION OF THE LAT/LON PT IN Y
C      IN00    - ADDRESS OF LOWER LEFT POINT
C      NGRID   - FINEST GRID THE LAT/LON POINT IS ON
C      CF00    - WEIGHT FOR LOWER LEFT GRID POINT
C      CF10    - WEIGHT FOR LOWER RIGHT GRID POINT
C      CF01    - WEIGHT FOR UPPER LEFT GRID POINT
C      CF11    - WEIGHT FOR UPPER RIGHT GRID POINT
C      IERR    - = 0 IF EVERYTHING IS OK
C                = 1 IF LAT OR LON IS INVALID
C                = 2 IF POINT IS NOT ON ONE OF THE GRIDS
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C       COMMON   - COMCONST
C
C REMARKS:
C   .
C   - - - - - - - - I N P U T   V A R I A B L E S  - - - - - - - - -
C   .
C   NAMES      MEANING/CONTENT/PURPOSE/UNITS/TYPE        INTERFACE
C   -----      ----------------------------------        ---------
C   .
C   NH         NUMBER OF POINTS MINUS 0.5 FROM POLE TO   /COMCONST/
C   .          EQUATOR FOR THE HEMISPHERIC GRID ( A )
C   .
C   DLAMNGM    ROTATION ANGLE OF GRIDS                   /COMCONST/
C   .
C   IMG        NUMBER OF X-POINTS ON EACH GRID           /COMCONST/
C   .
C   JMG        NUMBER OF Y-POINTS ON EACH GRID           /COMCONST/
C   .
C   NGRDUSE    NUMBER OF GRIDS                           /COMCONST/
C   .
C   XPOLEH     X LOCATION OF POLE                        /COMCONST/
C   .
C   YPOLEH     Y LOCATION OF POLE                        /COMCONST/
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C   MACHINE:  CRAY Y-MP
C
C$$$
C
      INCLUDE 'parmodel'
C
C     COMMON BLOCK /COMCONST/ CONTAINS GRID-RELATED PARAMETERS,
C          AND A FEW OTHER COMMON CONSTANTS.
      COMMON /COMCONST/ IJMAX, KM, LVLTOT, IJRAD, LOFCLDS(2,4),
     1                  ICALLRAD, IPHYSPL, NGRDUSE, NH,
     2                  NTIME, ITIME, NSTEPS,
     3                  IMG(INGRDUSE), JMG(INGRDUSE),
     4                  IAG(INGRDUS1), JAG(INGRDUS1),
     5                  IBG(INGRDUS1), JBG(INGRDUS1),
     6                  IADDRG(INIADDRS, INGRDUSE),
     7                  NPTSFH(2, INGRDUSE),
     8                  KUMULUS, LGRIDPPT, KLIFT1, KLIFT2, IBUCKET,
     9                  XPOLEH(INGRDUSE), YPOLEH(INGRDUSE), RADIUS,
     1                  DELSIG(IKM), PR(IKM), PRESS(IKM),
     2                  SIGINT(IKMP1),
     3                  DTOVDX, ANGVEL,
     4                  SIGMACC, SIGMAGSP, SIGMADHQ, CRITCONV,
     5                  SATDEL, RHFACTOR, QBOUND,
     6                  ANEM, BLKDR, CHARN, CONAUST, DDORF, PKATO,
     7                  SCALEHT, SIGDOT, DLAMNGM
C
      DATA DTR / 0.017453292E0 /
      DATA ALATMIN / 0.E0 /
C
C                    START HERE
C
C     CHECK FOR VALID LAT/LON
C
      IF ( ALAT.GT.90.E0 .OR. ALAT.LT.ALATMIN .OR.
     *   ALON.GT.360.E0 .OR. ALON.LT.-180.E0 ) THEN
         IERR = 1
         RETURN
      END IF
C
      ALATLR  = ALAT    * DTR
      ALONLR  = ALON    * DTR
      ALONLR0 = DLAMNGM * DTR
      DMAP    = (FLOAT(NH)+0.5E0) * COS(ALATLR)/(1.E0 + SIN(ALATLR))
      COSLON  = COS ( ALONLR - ALONLR0 )
      SINLON  = SIN ( ALONLR - ALONLR0 )
      XYMIN   = 1.E0
C
C     THE BOTTOM ROW AND LEFT COLUMN OF H'S ARE NOT REAL POINTS
C
      IF ( ITYPE .EQ. 0 ) XYMIN = 2.E0
C
C     START WITH FINEST GRID
C
      DO 100 NG = NGRDUSE, 1, -1
C
         IM   = IMG(NG)
         JM   = JMG(NG)
         XMAX = FLOAT ( IM ) - 1.E0
         YMAX = FLOAT ( JM ) - 1.E0
         FACT = DMAP * (2.E0**(NG - 1))
         XG   = FACT * COSLON + XPOLEH(NG)
         YG   = FACT * SINLON + YPOLEH(NG)
         IF ( ITYPE .EQ. 1 ) YG = YG - 0.5E0
         IF ( ITYPE .EQ. 2 ) XG = XG - 0.5E0
         IF ( ITYPE .EQ. 3 ) THEN
                             XG = XG - 0.5E0
                             YG = YG - 0.5E0
C
C     THE TOP ROW AND RIGHT COLUMN OF MIDPOINTS ARE NOT USEFUL
C
                             YMAX = YMAX - 1.E0
                             XMAX = XMAX - 1.E0
         END IF  
C
         IF ( XG.GE.XYMIN .AND. XG.LE.XMAX .AND.
     *        YG.GE.XYMIN .AND. YG.LE.YMAX ) THEN
C
C             IF WE GET TO HERE THEN WE HAVE FOUND THE GRID
C
              IXG  = INT ( XG )
              JYG  = INT ( YG )
              DEL  = XG - FLOAT ( IXG )
              EPS  = YG - FLOAT ( JYG )
C
              IN00  = IXG + ( JYG - 1 ) * IM
              NGRID = NG
              CF00  = ( 1.E0 - DEL ) * ( 1.E0 - EPS )
              CF10  = DEL * ( 1.E0 - EPS )
              CF01  = ( 1.E0 - DEL ) * EPS
              CF11  = DEL * EPS
              IERR  = 0
C
              RETURN
C
         END IF
C
100   CONTINUE
C
C     IF WE GET TO HERE THEN THE LAT/LON POINT IS NOT ON THE GRIDS
C     THIS SHOULD NEVER HAPPEN
C
      IERR = 2
C
C
      RETURN
      END
