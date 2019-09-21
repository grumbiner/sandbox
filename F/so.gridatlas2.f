      PROGRAM soatlas
C     Try to read the southern ocean atlas data set.

      INTEGER*4 IBUF1(390), IBUF3(390)
      INTEGER*1 dumb(1562)
      EQUIVALENCE (dumb(1), IBUF1(1))
      EQUIVALENCE (dumb(3), IBUF3(1))

C     Header Info      
      REAL GRDLAT, GRDLON, DFAULT, DOCEAN
      INTEGER NPARMS, NSTAND, NRXRY
      REAL RX(3), RY(3)
C     Level Info
      REAL D(47),T(47),S(47),SIGMAT(47),O2(47),
     1  SIO3(47),PO4(47),ANO2(47)

C     Indices to fields within a level record      
      INTEGER PDEPTH, PTEMP, PSALT, PSIGMA, PO2, PSIO3
      INTEGER PPO4, PANO2, SHIFT
      INTEGER m, N

C     Local Operators
      INTEGER*4 INTGER, recs, TOTLEV
      LOGICAL CHECK
      ZIN2FL(INTGER)=FLOAT(INTGER)/1000.-200.
      IN2IN(INTGER)=INTGER/1000-200
      
      OPEN (10,FILE='grid2.soatlas',FORM='UNFORMATTED'
     1                                    ,STATUS='OLD')
      OPEN (11,FILE='gridout',FORM='FORMATTED',STATUS='NEW')
     
CD      TOTLEV = 0
      DO 1000 recs = 1, 16
        READ (10) dumb
        m = 372
        GRDLAT= ZIN2FL(IBUF1(m))
        GRDLON= ZIN2FL(IBUF1(m+1))
        NPARMS= IN2IN (IBUF1(m+2))
        NSTAND= IN2IN (IBUF1(m+3))
        DFAULT= ZIN2FL(IBUF1(m+4))
        NRXRY = IN2IN (IBUF1(m+5))
        RX(1) = ZIN2FL(IBUF1(m+6))
        RX(2) = ZIN2FL(IBUF1(m+7))
        RX(3) = ZIN2FL(IBUF1(m+8))
        RY(1) = ZIN2FL(IBUF1(m+9))
        RY(2) = ZIN2FL(IBUF1(m+10))
        RY(3) = ZIN2FL(IBUF1(m+11))
        DOCEAN= ZIN2FL(IBUF1(m+12))  ! ibuf1(384)
   
C       NOW BEGIN TO READ IN THE LEVEL DATA
CD        PRINT *,'THE SEARCH FOR 10 M.'
CD        DO 1001 N = 1, 390
CD          IF (ZIN2FL(IBUF1(N)) .EQ. 10.0) PRINT *, 'IBUF1, N = ',N
CD          IF (ZIN2FL(IBUF3(N)) .EQ. 10.0) PRINT *, 'IBUF3, N = ',N
CD 1001   CONTINUE          

        PDEPTH = 1
        PTEMP  = 2
        PSALT  = 3
        PSIGMA = 4
        PO2    = 5
        PSIO3  = 6
        PPO4   = 7
        PANO2  = 8
        SHIFT  = -15
        D(1)      = ZIN2FL(IBUF1(m+12+PDEPTH)) !385
        T(1)      = ZIN2FL(IBUF1(m+12+PTEMP))
        S(1)      = ZIN2FL(IBUF1(m+12+PSALT))
        SIGMAT(1) = ZIN2FL(IBUF1(m+12+PSIGMA)) !388
        O2(1)     = ZIN2FL(IBUF1(m+12+PO2))
        SIO3(1)   = ZIN2FL(IBUF1(m+12+PSIO3)) !390
CD      PO4(1) = SPLIT ACROSS BYTES
        ANO2(1)   = ZIN2FL(IBUF3(8+SHIFT+PANO2 ))
        DO 14 K = 2, 47
          D(K)      = ZIN2FL(IBUF3(8*K+SHIFT+PDEPTH))
          T(K)      = ZIN2FL(IBUF3(8*K+SHIFT+PTEMP ))
          S(K)      = ZIN2FL(IBUF3(8*K+SHIFT+PSALT ))
          SIGMAT(K) = ZIN2FL(IBUF3(8*K+SHIFT+PSIGMA))
          O2(K)     = ZIN2FL(IBUF3(8*K+SHIFT+PO2   ))
          SIO3(K)   = ZIN2FL(IBUF3(8*K+SHIFT+PSIO3 ))
          PO4(K)    = ZIN2FL(IBUF3(8*K+SHIFT+PPO4  ))
          ANO2(K)   = ZIN2FL(IBUF3(8*K+SHIFT+PANO2 ))
CD          IF (T(K) .GE. -3.) TOTLEV = TOTLEV+1
   14   CONTINUE
C       PRINT OUT 13 HEADER VARIABLES
        WRITE(11,33) GRDLAT,GRDLON,NPARMS,NSTAND,DFAULT,NRXRY,
     1    (RX(I),I=1,3),(RY(J),J=1,3),DOCEAN
CD        WRITE(*,33) GRDLAT,GRDLON,NPARMS,NSTAND,DFAULT,NRXRY,
CD     1    (RX(I),I=1,3),(RY(J),J=1,3),DOCEAN

C       PRINT 47 GRID STANDARD LEVELS
        DO 34 I = 1, 47
          WRITE(11,35) D(I),T(I),S(I),SIGMAT(I),O2(I),SIO3(I),
     1         PO4(I),ANO2(I)
CD          WRITE(*,35) D(I),T(I),S(I),SIGMAT(I),O2(I),SIO3(I),
CD     1         PO4(I),ANO2(I)
   34   CONTINUE

 1000 CONTINUE
 
CD      PRINT *,'TOTAL NUMBER OF GRIDDED LEVELS', TOTLEV
CD      WRITE (11,9001) TOTLEV
      
   33 FORMAT(1X,F7.3,F8.3,I2,I3,F4.0,I2,6F6.2,F6.0)
CD   35 FORMAT (1X,F6.0,2F7.3,2F6.2,F6.1,F5.2,F5.1)
   35 FORMAT (8F9.3)
 9001 FORMAT (I8)
 
      PAUSE
      END