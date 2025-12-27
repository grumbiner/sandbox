      SUBROUTINE UVSUMS(FPU,FMU,FPV,FMV,TOPULN,TOPVLN,QVV,LEVS,WGT)
C
C            THIS SR ASSUMES JCAP IS EVEN
C            THIS SR ASSUMES JCAP IS EVEN
C            THIS SR ASSUMES JCAP IS EVEN
C
      DIMENSION     FPU(2,0: 62 ,LEVS)
      DIMENSION     FPV(2,0: 62 ,LEVS)
      DIMENSION     FMU(2,0: 62 ,LEVS)
      DIMENSION     FMV(2,0: 62 ,LEVS)
      DIMENSION   TOPULN(2,0: 62 ,LEVS)
      DIMENSION   TOPVLN(2,0: 62 ,LEVS)
      DIMENSION   TOPPLN(0: 62 )
      DIMENSION   QVV( 4158 )
C
C     ----------------------------------------------------------------
C     COMPUTE EXPANSION COEFFS. FOR TOP ROWS OF U AND V
C     ----------------------------------------------------------------
CC
C     WRITE(60,101)WGT
C101  FORMAT(1H ,'WGT=   ',E12.3)
      LEN=2* 63
      J=LEN+1
      DO 10 L=0, 62
      TOPPLN(L) = QVV(J)*WGT
      J=LEN+J
      LEN=LEN-2
   10 CONTINUE
C     WRITE(60,100)TOPPLN
C100  FORMAT(1H ,'TOPPLN ', 63 (1X,E12.3))
CC
       N= 62 +1
CCCCCCCCFPUP$ CNCAL
CMIC$ DO ALL
CMIC$1 AUTOSCOPE
      DO L = 1,  62   ,2
C
C        COMPUTE THE EVEN (N-L) EXPANSION COEFFICIENTS FOR EACH LEVEL
C        ------------------------------------------------------------
C
C        REAL PART
C
            DO K = 1, LEVS
             TOPULN(1,L,K) = TOPULN(1,L,K)+FPU(1,L,K)*TOPPLN(L)
             TOPVLN(1,L,K) = TOPVLN(1,L,K)+FPV(1,L,K)*TOPPLN(L)
            END DO
C
C        IMAGINARY PART
C
            DO K = 1, LEVS
             TOPULN(2,L,K) = TOPULN(2,L,K)+FPU(2,L,K)*TOPPLN(L)
             TOPVLN(2,L,K) = TOPVLN(2,L,K)+FPV(2,L,K)*TOPPLN(L)
            END DO
      END DO
C
CMIC$ DO ALL
CMIC$1 AUTOSCOPE
      DO L = 0,  62   ,2
C        COMPUTE THE ODD (N-L) EXPANSION COEFFICIENTS FOR EACH LEVEL
C        -----------------------------------------------------------
C
C        REAL PART
C
            DO K = 1, LEVS
             TOPULN(1,L,K) = TOPULN(1,L,K)+FMU(1,L,K)*TOPPLN(L)
             TOPVLN(1,L,K) = TOPVLN(1,L,K)+FMV(1,L,K)*TOPPLN(L)
            END DO
C
C        IMAGINARY PART
C
            DO K = 1, LEVS
             TOPULN(2,L,K) = TOPULN(2,L,K)+FMU(2,L,K)*TOPPLN(L)
             TOPVLN(2,L,K) = TOPVLN(2,L,K)+FMV(2,L,K)*TOPPLN(L)
            END DO
      END DO
C
      RETURN
      END
