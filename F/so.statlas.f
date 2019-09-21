      PROGRAM statlas
C     Try to read the southern ocean atlas data set.
 
C..THIS IS AN OUTLINE OF A PROGRAM TO READ THE VERTICAL GRID POINT
C..PARAMETER TAPE AND UNSCALE THE 32-BIT POSITIVE INTEGERS INTO
C..OCEANOGRAPHIC UNITS
C
C.. NOTE: IBUF & INTGER MUST BE INTEGER*4
      INTEGER levtot, reclen, NREC
      PARAMETER (levtot = 58)
      PARAMETER (reclen = 1688)
      PARAMETER (NREC   = 6313)
      INTEGER*4 IBUF1(reclen/4)
      CHARACTER*1 dumb(reclen)
      INTEGER*4 INTGER, recs
      REAL D(levtot),T(levtot),S(levtot),O2(levtot),
     1  SIO3(levtot),PO4(levtot),ANO2(levtot)
      EQUIVALENCE (dumb(1), IBUF1(1))
      INTEGER ctrcod, crucod, stacod
      INTEGER pday, pmonth, pyear, pgmt
      INTEGER plat, plong, nlevls, cansq
      INTEGER dbotom, dshal, ddeep, pnutr
C     Indices to fields within a level record
      INTEGER PDEPTH, PTEMP, PSALT, PSIGMA, PO2, PSIO3
      INTEGER PPO4, PANO2, SHIFT
      LOGICAL CHECK
      CHARACTER*60 fname
 
      REAL totlev
      ZIN2FL(INTGER)=FLOAT(INTGER)/1000.-200.
 
C     Open data file and an output file
C     PRINT *,'What is the name of the input file?'
C     READ (*,9009) fname
      OPEN (10,FILE='/STATL SOATLAS G',FORM='UNFORMATTED',STATUS='OLD')
C     PRINT *,'What is the name of the output file?'
C     READ (*,9009) fname
      OPEN (11,FILE='/STOUT SOATLAS E', FORM='FORMATTED', STATUS='NEW')
 9009 FORMAT (A60)
C     Set up pointers to fields
      ctrcod = 1
      crucod = 2
      stacod = 3
      pday   = 4
      pmonth = 5
      pyear  = 6
      pgmt   = 7
      plat   = 8
      plong  = 9
      cansq  = 10
      nlevls = 11
      dbotom = 12
      dshal  = 13
      ddeep  = 14
      pnutr  = 15
        PDEPTH = 1
        PTEMP  = 2
        PSALT  = 3
        PO2    = 4
        PSIO3  = 5
        PPO4   = 6
        PANO2  = 7
        SHIFT  = 15
C     Get time in ticks on macintosh
CM    PRINT *,LONG(362)
      totlev = 0.0
      DO 1000 recs = 1, NREC
        CHECK = .TRUE.
        READ (10) dumb
        totlev = totlev + ZIN2FL(IBUF1(nlevls))
        DO 14 K = 1, levtot
          D(K)      = ZIN2FL(IBUF1(7*K+SHIFT+PDEPTH))
          T(K)      = ZIN2FL(IBUF1(7*K+SHIFT+PTEMP ))
          S(K)      = ZIN2FL(IBUF1(7*K+SHIFT+PSALT ))
          O2(K)     = ZIN2FL(IBUF1(7*K+SHIFT+PO2   ))
          SIO3(K)   = ZIN2FL(IBUF1(7*K+SHIFT+PSIO3 ))
          PO4(K)    = ZIN2FL(IBUF1(7*K+SHIFT+PPO4  ))
          ANO2(K)   = ZIN2FL(IBUF1(7*K+SHIFT+PANO2 ))
   14   CONTINUE
C        DO 100 K = 1, levtot
C          IF ((T(K).LT. -2.2 .AND. T(K) .NE. -9.)
C     1        .OR. T(K) .GT. 24.0 )  CHECK = .TRUE.
C          IF ((S(K).LT. 32.5 .AND. S(K) .NE. -9.)
C     1        .OR. S(K) .GT. 36.5 )  CHECK = .TRUE.
C          IF ((O2(K).LT. 0.0 .AND. O2(K) .NE. -9.)
C     1        .OR. O2(K) .GT. 10.0 )  CHECK = .TRUE.
C          IF ((SIO3(K).LT. 0.0 .AND. SIO3(K) .NE. -9.)
C     1        .OR. SIO3(K) .GT. 185.0 )  CHECK = .TRUE.
C          IF ((PO4(K).LT. 0.0 .AND. PO4(K) .NE. -9.)
C     1        .OR. PO4(K) .GT. 3.75 )  CHECK = .TRUE.
C          IF ((ANO2(K).LT. 0.0 .AND. ANO2(K) .NE. -9.)
C     1        .OR. ANO2(K) .GT. 55.0 )  CHECK = .TRUE.
C          IF (CHECK) GO TO 200
C 100    CONTINUE
CD200    IF (CHECK) THEN
CD        WRITE (*,9003) IBUF1(1), IBUF1(2), IBUF1(3)
          WRITE (11,9003) IBUF1(1), IBUF1(2), IBUF1(3)
CD        WRITE (*,9004) (ZIN2FL(IBUF1(J)),J=4,15)
          WRITE (11,9004) (ZIN2FL(IBUF1(J)),J=4,15)
          DO 1100 K = 1, levtot
CD          WRITE (*,9005)  D(K),T(K),S(K),O2(K),
CD   1                        SIO3(K),PO4(K),ANO2(K)
            WRITE (11,9005) D(K),T(K),S(K),O2(K),
     1                        SIO3(K),PO4(K),ANO2(K)
 1100     CONTINUE
CD      ENDIF
 
 1000 CONTINUE
      WRITE (*,9001) totlev
      WRITE (11,9001) totlev
CM    PRINT *,LONG(362)
 
 9001 FORMAT (4E13.6)
 9002 FORMAT (4I13)
 9003 FORMAT (3(I12,1x))
 9004 FORMAT (3F6.0,F6.2/,2F9.2,F8.0,F4.0,4F6.0)
 9005 FORMAT (7F9.3)
 
      END
