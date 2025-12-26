      SUBROUTINE ngmPTS(HPTLAT,HPTLON,NHPT,
     &                    UPTLAT,UPTLON,NUPT,
     &                    VPTLAT,VPTLON,NVPT,
     &                    IOTM,NOU,DLAM0)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK   
C                .      .    .                                       .
C SUBPROGRAM:    ngmPTS      NGM BOUNDARY POINT GENERATOR  
C   PRGMMR: R. WOBUS         ORG: W/NMC20    DATE: 90-06-27 
C
C ABSTRACT: GENERATE LIST OF POINT LOCATIONS FOR THE LATERAL 
C   BOUNDARIES OF THE NGM C-GRID
C                        
C PROGRAM HISTORY LOG:    
C   90-06-15  RICHARD WOBUS
C   90-12-10  Geoff DiMego   fixed up to write out required info
C
C USAGE:    CALL ngmPTS(KMAX,HPTLAT,HPTLON,NHPT,
C    &                  UPTLAT,UPTLON,NUPT,
C    &                  VPTLAT,VPTLON,NVPT)
C    &                  IOTM,NOU,DLAM0)
C   INPUT ARGUMENT LIST:
C     KMAX     - NUMBER OF GLOBAL SIGMA LEVELS
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS) 
C     HPTLAT   - ARRAY OF LATITUDES OF H POINTS
C     HPTLON   - ARRAY OF LONGITUDES OF H POINTS
C     NHPT     - NUMBER OF H POINTS           
C     UPTLAT   - ARRAY OF LATITUDES OF U POINTS
C     UPTLON   - ARRAY OF LONGITUDES OF U POINTS
C     NUPT     - NUMBER OF U POINTS             
C     VPTLAT   - ARRAY OF LATITUDES OF V POINTS  
C     VPTLON   - ARRAY OF LONGITUDES OF V POINTS  
C     NVPT     - NUMBER OF V POINTS      
C     IOTM     - NUMBER OF times to be written out
C     NOU      - output unit for boundary data
C     DLAM0    - reference longitude read from input input
C                 needed by WRITER for wind rotation
C
C   INPUT FILES:
C     19       - OPTIONS/GRIDS DATA SET AS READ BY NGM INPUT CODE    
C                                             
C   OUTPUT FILES:
C     FT06F001 - POSSIBLE DIAGNOSTIC PRINTOUT  
C     NOU      - output unit for boundary data
C 
C REMARKS: SOME CUSTOMARY NGM NAMES HAVE BEEN SHORTENED TO 6 CHARS
C                      
C ATTRIBUTES:           
C   LANGUAGE: FORTRAN 77 
C   MACHINE:  CRAY 
C  
C$$$
C
      INCLUDE "parmlbc"
C
C  ARRAYS FOR BOUNDARY POINTS
C
      DIMENSION HPTLAT  (1),HPTLON  (1)
      DIMENSION UPTLAT  (1),UPTLON  (1)
      DIMENSION VPTLAT  (1),VPTLON  (1)
C
C  INTERNAL WORK ARRAYS
C
      DIMENSION LSCRU(LBDIM),LSCRV(LBDIM),LSCRH(LBDIM)
C
C  INTERNAL ARRAYS FOR GRID SPECIFICATION INFORMATION
C
      DIMENSION IMG     (NGUSED),JMG       (NGUSED)
      DIMENSION IAG     (NGUSED),JAG       (NGUSED)
      DIMENSION IBG     (NGUSED),JBG       (NGUSED)
      DIMENSION ISUM    (NGUSED),JSUM      (NGUSED)
      DIMENSION XPOLEH  (NGUSED),YPOLEH    (NGUSED)
      DIMENSION XPOLEU  (NGUSED),YPOLEU    (NGUSED)
      DIMENSION XPOLEV  (NGUSED),YPOLEV    (NGUSED)
C
C  THIS SECTION IS BASED ON THE NGM INPUT CODE
C
C  READ OPTION/GRID DATA SET USED BY INPUT CODE
C
      INUNIT = 19
      IOUPRT = 6
C
C     INPUT WHETHER OPTION 1 OR OPTION 2 IS TO BE USED.
      READ (INUNIT, 620) IOPT
  620 FORMAT (27X, I7)
C
C     ECHO CHECK THE OPTION JUST READ IN.
      WRITE (IOUPRT, 625) IOPT
  625 FORMAT ('0', 'THE OPTION THAT THE INPUT CODE WOULD BE RUN ',
     1             'UNDER IS OPTION', I2, '.')
C
C        SPECIFY THE GRID PARAMETERS OF THE NGM GRID USING
C        INFORMATION ON DATA CARDS.
         READ (INUNIT, 800) NH, DLAMNG, NGRUSE
  800    FORMAT (27X, I7 / 27X, F10.0 / 27X, I7)
C
         NGUSEM = NGRUSE - 1
C
         IF (NGRUSE .GT. 1) THEN
C
            DO 1200 NG=2, NGRUSE
               READ (INUNIT, 1000) IMG(NG), JMG(NG),
     1                              ISUM(NG-1),JSUM(NG-1)
 1000          FORMAT(27X, I7 / 27X, I7 / 27X, I7 / 27X, I7)
 1200       CONTINUE
         END IF
C
C           ALL DATA CARDS HAVE NOW BEEN READ IN.
C
C
C     THE POLAR LOCATIONS OF THE GRIDS ARE CALCULATED.
C
C     FIRST THE H POINTS:
      XPOLEH(1) = FLOAT(NH + 4)
      YPOLEH(1) = XPOLEH(1)
C
      IF (NGRUSE .GT. 1) THEN
         DO 2400 NG=2, NGRUSE
            XPOLEH(NG) = 1.0 + 0.5 * FLOAT(IMG(NG))
     1                + 2.0 * XPOLEH(NG-1) - FLOAT(ISUM(NG-1))
            YPOLEH(NG) = 1.0 + 0.5 * FLOAT(JMG(NG))
     1                + 2.0 * YPOLEH(NG-1) - FLOAT(JSUM(NG-1))
 2400    CONTINUE
C
      END IF
C
C     NOW THE U AND V POINTS:
      DO 2600 NG=1,NGRUSE
         XPOLEU(NG) = XPOLEH(NG)
         YPOLEU(NG) = YPOLEH(NG) - 0.5
C
         XPOLEV(NG) = XPOLEH(NG) - 0.5
         YPOLEV(NG) = YPOLEH(NG)
C
 2600 CONTINUE
C
C              CALCULATE THE LOCATION OF THE CORNERS OF EACH
C              INTERIOR GRID ON THE NEXT OUTER GRID.
C              NOTE THAT THERE SHOULD BE NO REMAINDER
C              WHEN DIVIDING BY 4.
      IF(NGRUSE .GT. 1) THEN
C
         DO 2800 NG=1, NGUSEM
            IAG(NG) = (2 * ISUM(NG) + 13 - IMG(NG+1)) / 4
            IBG(NG) = ISUM(NG) - IAG(NG)
            JAG(NG) = (2 * JSUM(NG) + 13 - JMG(NG+1)) / 4
            JBG(NG) = JSUM(NG) - JAG(NG)
 2800    CONTINUE
C
      END IF
C
C
C
C
C
      WRITE (IOUPRT, 4600) NH, DLAMNG, NGRUSE
C
 4600 FORMAT ('0', 5X, 'THE NUMBER OF GRID POINTS (MINUS 0.5) ',
     1             'ON GRID A BETWEEN THE POLE AND THE EQUATOR ',
     2             'IS', I3, '.'/
     3        ' ', 5X, 'THE ROTATION ANGLE OF THE POSITIVE X AXIS ',
     4             'COUNTERCLOCKWISE FROM THE GREENWICH MERIDIAN ',
     5             'IS', F8.3, '.'/
     6        ' ', 5X, 'THE NUMBER OF FORECAST GRIDS IS', I2, '.'/
     7        '0', 5X, 'INDIVIDUAL GRID PARAMETERS:'/
     8        ' ', T10, 'GRID', T20, 'IMG', T30, 'JMG',
     9             T40, 'XPOLEH', T50, 'YPOLEH', T60, 'IAG',
     1             T70, 'IBG', T80, 'JAG', T90, 'JBG' /)
C
      DO 4800 NG = 1, NGRUSE
         IF (NG .EQ. NGRUSE) THEN
            WRITE (IOUPRT, 4700)
     1             NG, IMG(NG), JMG(NG), XPOLEH(NG), YPOLEH(NG)
C
 4700       FORMAT (' ', T10, I3, T20, I3, T30, I3, T38, F8.3,
     1                   T48, F8.3, T60, I3, T70, I3,
     2                   T80, I3, T90, I3)
C
         ELSE
            WRITE (IOUPRT, 4700)
     1             NG, IMG(NG), JMG(NG), XPOLEH(NG), YPOLEH(NG),
     2             IAG(NG), IBG(NG), JAG(NG), JBG(NG)
         END IF
 4800 CONTINUE

C
C  THIS SECTION IS BASED ON HENRY JUANG'S BOUNDARY POINT CODE
C
      DLAM0 = DLAMNG
      A0 = 6371220.0
      A02 = A0 * 2.0
      DEGREE = 90. / ASIN(1.0)
C   NGBD is passed as a parameter from parmlbc
      NGB = NGBD
      NGBM = NGB - 1
      IMB=IMG(NGB)
      JMB=JMG(NGB)
      ISUMB=ISUM(NGBM)
      JSUMB=JSUM(NGBM)
      XPOLBH = XPOLEH(NGB)
      YPOLBH = YPOLEH(NGB)
      NPOINT = 3
C *********
      DX = 2.0 * A0 / ( NH + 0.5) / ( 2.0**(NGB-1) )
      DXHALF = DX * 0.5
      X00 = -(XPOLBH-1) * DX
      Y00 = -(YPOLBH-1) * DX
C
      WRITE (IOUPRT,221) NH,NGB,IMB,JMB,ISUMB,JSUMB
 221  FORMAT(' NH NG IM JM ISUM JSUM ',6I6)
      WRITE (IOUPRT,223) DX,XPOLBH,YPOLBH
 223  FORMAT(' DX XPOLEH YPOLEH ',3G13.6)
      WRITE (IOUPRT,224) DLAM0,X00,Y00
 224  FORMAT(' DLAM0 X00 Y00 ',3G13.6)
C
C      --- COMPUTE OTHER CONSTANT ---
      LTB = LTBGRI
      LTB2 =  LTB * 2
      LBUO = IMB * LTB2 + LTB2 * (JMB - LTB2)
      LBCH = IMB * LTB2 + LTB2 * (JMB - LTB2)
      LBVO = IMB * LTB2 + LTB2 * (JMB - LTB2)
      WRITE (IOUPRT,122) LBUO,LBCH,LBVO
 122  FORMAT(' LBUO LBCH LBVO ',3I8)
C *****   POINT CONSIDER LATER
      KMLB = 16
      KHLB = KMLB * 2 + 1
      NK = KMAX
C
      LU = 0
      LV = 0
      LH = 0
      LTBP1 = LTB + 1
      LTBM1 = LTB - 1
C     --------------- J FROM 1 TO 4
      DO 500 J= 1, LTB
      JJ = (J-1)*IMB
        DO 500 I=1, IMB
        MU= JJ + I
        MV= JJ + I
        MH= JJ + I
        LU = LU + 1
        LV = LV + 1
        LH = LH + 1
        LSCRU(LU) = MU
        LSCRV(LV) = MV
        LSCRH(LH) = MH
 500  CONTINUE
C     --------------- J FROM 5 TO JMB-3
      JM2 = JMB - LTB
      IM1 = IMB - LTBM1
      DO 510 J=LTBP1,JM2
      JJ = (J-1)*IMB
        DO 520 I=1,LTB
        MU= JJ + I
        MV= JJ + I
        MH= JJ + I
        LU = LU + 1
        LV = LV + 1
        LH = LH + 1
        LSCRU(LU) = MU
        LSCRV(LV) = MV
        LSCRH(LH) = MH
 520    CONTINUE
        DO 530 I = IM1, IMB
        MU= JJ + I
        MV= JJ + I
        MH= JJ + I
        LU = LU + 1
        LV = LV + 1
        LH = LH + 1
        LSCRU(LU) = MU
        LSCRV(LV) = MV
        LSCRH(LH) = MH
 530    CONTINUE
 510  CONTINUE
C     ------------- J FROM JMB-2 TO JMB
      JM1 = JMB - LTBM1
      DO 550 J=JM1, JMB
      JJ = (J-1)*IMB
        DO 550 I=1,IMB
        MU= JJ + I
        MV= JJ + I
        MH= JJ + I
        LU = LU + 1
        LV = LV + 1
        LH = LH + 1
        LSCRU(LU) = MU
        LSCRV(LV) = MV
        LSCRH(LH) = MH
 550  CONTINUE
C
      LBUO = LU
      LBVO = LV
      LBCH = LH
      WRITE (IOUPRT,622) LBUO,LBCH,LBVO
 622  FORMAT(' LBUO LBCH LBVO ',3I6)
C
      WRITE(NOU) NGB,LTB,LBUO,LBVO,LBCH,KMLB,KHLB,NK,IOTM
      WRITE (IOUPRT,644) NGB,LTB,LBUO,LBVO,LBCH,KMLB,KHLB,NK,IOTM
 644  FORMAT('  NG LT LBUO LBVO LBCH KMLB KHLB NK IOTM',9I7)
C
      WRITE(NOU) (LSCRU(I),I=1,LBUO)
      WRITE(NOU) (LSCRV(I),I=1,LBVO)
      WRITE(NOU) (LSCRH(I),I=1,LBCH)
      WRITE(IOUPRT,333) (LSCRU(I),I=1,LBUO)
      WRITE(IOUPRT,333) (LSCRV(I),I=1,LBVO)
      WRITE(IOUPRT,333) (LSCRH(I),I=1,LBCH)
 333  FORMAT (1X,11I12)
      NHPT = LBCH
      NUPT = LBUO
      NVPT = LBVO
C
C --------- LOCATE ALL U V H POINTS -----
C                      NPT = 1 U POINT
C                      NPT = 2 V POINT
C                      NPT = 3 H POINT
C
      DO 700 NPT=1,NPOINT
      IF( NPT .EQ. 1 ) LBXX = LBUO
      IF( NPT .EQ. 2 ) LBXX = LBVO
      IF( NPT .EQ. 3 ) LBXX = LBCH
      IF( NPT .GE. 4 ) GO TO 9999
C
      NP0 = MOD ( NPT, 3 )
      NPXX = NP0 - 1
      NPXX = MAX0( 0, NPXX )
      NPYY = 2 - NP0
      NPYY = MOD ( NPYY, 2 )
      X0 = X00 + NPXX * DXHALF
      Y0 = Y00 + NPYY * DXHALF
C
      DO 710 L= 1, LBXX
        I = MOD ( LSCRU(L), IMB )
        J = LSCRU(L) / IMB + 1
        IF( I .NE. 0 ) GO TO 720
           I = IMB
           J = J - 1
 720    Y = Y0 + (J-1)*DX
        X = X0 + (I-1)*DX
        RR = SQRT( X*X + Y*Y )
        DEGSP = ATAN ( RR / A02 ) * DEGREE
        XLAT = 90. - 2 * DEGSP
C
        IF( X .GT. 0.0 ) DEGY = ATAN ( Y / X ) * DEGREE
        IF( X .LT. 0.0 ) DEGY = ATAN ( Y / X ) * DEGREE + 180.0
        IF( X .NE. 0.0 ) GO TO 730
         IF( Y .GT. 0.0 ) DEGY = 90.0
         IF( Y .EQ. 0.0 ) DEGY = 0.0
         IF( Y .LT. 0.0 ) DEGY = 270.0
 730    CONTINUE
C
        XLON = DEGY + DLAM0
        IF( XLON .GT. 360.0 ) XLON = XLON - 360.0
        IF( XLON .LT.   0.0 ) XLON = XLON + 360.0
C
C         w a s:  in degrees positive EAST!
C  N O W: we want in degrees positive WEST!
C
        wLON = 360.0 - XLON
        IF (NPT.EQ.3) THEN
          HPTLAT(L) = XLAT
          HPTLON(L) = wLON
        END IF
        IF (NPT.EQ.1) THEN
          UPTLAT(L) = XLAT
          UPTLON(L) = wLON
        END IF
        IF (NPT.EQ.2) THEN
          VPTLAT(L) = XLAT
          VPTLON(L) = wLON
        END IF
C       WRITE (IOUPRT,*) ' X Y XLAT XLON ',NPT,X,Y,XLAT,XLON
 710  CONTINUE
C
 700  CONTINUE
C
C     diagnostic print
      NXPT = max(NHPT,NUPT) 
      NXPT = max(NXPT,NVPT) 
      WRITE(6,77776) NHPT, NUPT, NVPT
77776 FORMAT(2X,3(I6,1X),' The following are now degrees west')
      DO 77777 N = 1, NXPT
       WRITE(6,77778) N,HPTLAT(N),HPTLON(N),UPTLAT(N),UPTLON(N),
     +          VPTLAT(N),VPTLON(N)
77778  FORMAT(I5,6(1X,F11.4))
77777 CONTINUE
      RETURN
C
 9999 WRITE (IOUPRT,9990) NPT
 9990 FORMAT(' ******* ERROR AT SETLTB, NPT =',I5)
      STOP 'ngmPTS'
      END
