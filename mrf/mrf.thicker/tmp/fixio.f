      SUBROUTINE FIXIO (FHOUR,TSEA,SMC,SHELEG,STC,TG3,ZORL,PLANTR,
     &   CV,CVB,CVT,ALBEDO,SLMSK,F10M,CANOPY,IOFLAG,NREAD,NWRIT)
      DIMENSION IDATE(4)
      CHARACTER*8 LABEL(4)
      DIMENSION TSEA  ( 384 , 47 ), SMC ( 384 , 47 , 2 ),
     &          SHELEG( 384 , 47 ), STC ( 384 , 47 , 2 ),
     &          TG3   ( 384 , 47 ),
     &          ZORL  ( 384 , 47 ), SLMSK ( 384 , 47 ),
     &          CV    ( 384 , 47 ), CVB   ( 384 , 47 ),
     &          CVT   ( 384 , 47 ), PLANTR( 384 , 47 ),
     &          F10M  ( 384 , 47 ),CANOPY( 384 , 47 ),
     &          ALBEDO( 384 , 47 )
      DIMENSION WORK( 384 * 47 )
C
C
C  IOFLAG = 0  ...  READ FIXED FIELD FROM UNIT NREAD
C  IOFLAG = 1  ...  WRITE FIXED FIELD TO UNIT NWRIT
C
      LOLA =  384  *  47
      IF(IOFLAG.EQ.0) THEN
      REWIND NREAD
      READ(NREAD) LABEL
      READ(NREAD) GHOUR, IDATE
99    FORMAT(1H ,'FHOUR, IDATE=',F6.2,2X,4(1X,I4))
      PRINT *,'FIX FIELD READ IN FROM UNIT=',NREAD
      PRINT 99,FHOUR, IDATE
      READ(NREAD) TSEA
      READ(NREAD) SMC
      READ(NREAD) SHELEG
      READ(NREAD) STC
      READ(NREAD) TG3
      READ(NREAD) ZORL
      READ(NREAD) CV
      READ(NREAD) CVB
      READ(NREAD) CVT
      READ(NREAD) ALBEDO
      READ(NREAD) SLMSK
      READ(NREAD) PLANTR
      READ(NREAD,ERR=5000) CANOPY
C     READ(NREAD,ERR=5000) F10M
C
      CALL ROW1NS(TSEA)
      CALL ROW1NS(SHELEG)
      CALL ROW1NS(TG3)
      CALL ROW1NS(ZORL)
      CALL ROW1NS(CV)
      CALL ROW1NS(CVB)
      CALL ROW1NS(CVT)
      CALL ROW1NS(ALBEDO)
      CALL ROW1NS(PLANTR)
      CALL ROW1NS(SLMSK)
      CALL ROW1NS(CANOPY)
      DO K = 1,  2
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            WORK(IJ) = SMC(I,J,K)
          ENDDO
        ENDDO
        CALL ROW1NS(WORK)
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            SMC(I,J,K) = WORK(IJ)
          ENDDO
        ENDDO
      ENDDO
      DO K = 1,  2
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            WORK(IJ) = STC(I,J,K)
          ENDDO
        ENDDO
        CALL ROW1NS(WORK)
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            STC(I,J,K) = WORK(IJ)
          ENDDO
        ENDDO
      ENDDO
C
      PRINT 101,FHOUR,IDATE,LOLA
101   FORMAT(1H ,'IN FIXIO FHOUR IDATE LOLA=',F6.2,2X,4(1X,I4),2X,I5)
      ELSE
      CALL ROWSEP(TSEA)
      CALL ROWSEP(SHELEG)
      CALL ROWSEP(TG3)
      CALL ROWSEP(ZORL)
      CALL ROWSEP(CV)
      CALL ROWSEP(CVB)
      CALL ROWSEP(CVT)
      CALL ROWSEP(SLMSK)
      CALL ROWSEP(F10M)
      CALL ROWSEP(CANOPY)
      DO K = 1,  2
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            WORK(IJ) = SMC(I,J,K)
          ENDDO
        ENDDO
        CALL ROWSEP(WORK)
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            SMC(I,J,K) = WORK(IJ)
          ENDDO
        ENDDO
      ENDDO
      DO K = 1,  2
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            WORK(IJ) = STC(I,J,K)
          ENDDO
        ENDDO
        CALL ROWSEP(WORK)
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            STC(I,J,K) = WORK(IJ)
          ENDDO
        ENDDO
      ENDDO
C
      REWIND NREAD
      REWIND NWRIT
      READ(NREAD) LABEL
      WRITE(NWRIT) LABEL
      READ(NREAD) GHOUR, IDATE
      PRINT *,'FIX FIELD READ IN FROM UNIT=',NREAD
      PRINT 99,FHOUR, IDATE
      WRITE(NWRIT) FHOUR, IDATE
      READ(NREAD)
      WRITE(NWRIT) TSEA
      READ(NREAD)
      WRITE(NWRIT) SMC
      READ(NREAD)
      WRITE(NWRIT) SHELEG
      READ(NREAD)
      WRITE(NWRIT) STC
      READ(NREAD)  WORK
      WRITE(NWRIT) WORK
      READ(NREAD)
      WRITE(NWRIT) ZORL
      READ(NREAD)
      WRITE(NWRIT) CV
      READ(NREAD)
      WRITE(NWRIT) CVB
      READ(NREAD)
      WRITE(NWRIT) CVT
      READ(NREAD) WORK
      WRITE(NWRIT) WORK
      READ(NREAD)
      WRITE(NWRIT) SLMSK
      READ(NREAD) WORK
      WRITE(NWRIT) WORK
C     READ(NREAD)
      WRITE(NWRIT) CANOPY
      WRITE(NWRIT) F10M
C
      CALL ROW1NS(TSEA)
      CALL ROW1NS(SHELEG)
      CALL ROW1NS(TG3)
      CALL ROW1NS(ZORL)
      CALL ROW1NS(CV)
      CALL ROW1NS(CVB)
      CALL ROW1NS(CVT)
      CALL ROW1NS(SLMSK)
      CALL ROW1NS(F10M)
      CALL ROW1NS(CANOPY)
      DO K = 1,  2
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            WORK(IJ) = SMC(I,J,K)
          ENDDO
        ENDDO
        CALL ROW1NS(WORK)
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            SMC(I,J,K) = WORK(IJ)
          ENDDO
        ENDDO
      ENDDO
      DO K = 1,  2
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            WORK(IJ) = STC(I,J,K)
          ENDDO
        ENDDO
        CALL ROW1NS(WORK)
        DO J = 1,  47
          DO I = 1,  384
            IJ = (J-1) *  384  + I
            STC(I,J,K) = WORK(IJ)
          ENDDO
        ENDDO
      ENDDO
C
100   FORMAT(1H ,'OUT FIXIO FHOUR IDATE LOLA=',F6.2,2X,4(1X,I4),2X,I5)
103   FORMAT(1H ,'SHALOM FROM FIXIO BENCH NWRIT=',I2)
      PRINT 100,FHOUR,IDATE,LOLA
      PRINT 103,NWRIT
      ENDIF
C
      RETURN
 5000 PRINT *, ' ERROR IN INPUT IN FIXIO'
      STOP
      END
