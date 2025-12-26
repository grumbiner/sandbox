CFPP$ NOCONCUR R
      SUBROUTINE SUMS2R(FLN,AP,QLN,LEVS)
      PARAMETER (LEN0= 128 )
      PARAMETER (LENH= 128 /2)
      PARAMETER (LNT= 2016 )
      PARAMETER (LNT22= 4033 )
      PARAMETER (JCAP= 62 )
      DIMENSION AP(2,0:LEN0,LEVS), QLN(2*LNT), FLN(LNT22,LEVS)
C
C     LOCAL SCALARS
C     -------------
C
      INTEGER I, N, L, K
      REAL    EVENR, EVENI
C
C     STATEMENT FUNCTIONS
C     -------------------
C
C     OFFSET(N,L) IS THE OFFSET IN WORDS
C     TO THE (N,L)-ELEMENT OF A LOWER
C     TRIANGULAR MATRIX OF COMPLEX NUMBERS
C     IN AN ARRAY CONTAINING THE MATRIX
C     PACKED IN COLUMN-MAJOR ORDER,
C     WHERE L AND N RANGE FROM 0 TO JCAP,
C     INCLUSIVE
C
C          LOWER TRIANGULAR MATRIX OF COMPLEX NUMBERS:
C
C                     L -->
C
C                   X
C               N   X X
C                   X X X
C               |   X X X X
C               V   X X X X X
C                   X X X X X X
C
C          ORDER OF THE MATRIX ELEMENTS IN MEMORY:
C
C          (0,0), (1,0), (2,0), ..., (JCAP,0), (1,1), (2,1), (3,1), ...
C
      INTEGER OFFSET
      OFFSET(N,L) = (JCAP+1)*(JCAP+2) - (JCAP-L+1)*(JCAP-L+2) + 2*(N-L)
C
C     ---
C
C     TERM(1,N,L,K) AND TERM(2,N,L,K) ARE
C     THE REAL AND IMAGINARY PART, RESP.,
C     OF EXP((0,1)*L*PHI) TIMES THE (N,L) TERM
C     IN THE EXPANSION IN SPHERICAL
C     HARMONICS OF THE FIELD AT LEVEL K,
C     WHERE PHI IS THE AZIMUTHAL ANGLE
C
      TERM(I,N,L,K) = QLN(OFFSET(N,L)+I)*FLN(OFFSET(N,L)+I,K)
C
C     ZERO THE ACCUMULATORS
C     ---------------------
C
      DO K = 1, LEVS
         DO L = 0, JCAP
            AP(1,L,K) = 0.
            AP(2,L,K) = 0.
            AP(1,LENH+L,K) = 0.
            AP(2,LENH+L,K) = 0.
         END DO
      END DO
C
C     COMPUTE THE EVEN AND ODD (N-L) COMPONENTS
C     OF THE FOURIER COEFFICIENTS
C     ---------------------------------------------------------
C
CFPP$ CNCALL
      DO L = 0, JCAP
         LS=L*((2*JCAP+3)-L)
C
C        COMPUTE THE SUM OF THE EVEN (N-L) TERMS FOR EACH LEVEL
C        ------------------------------------------------------
C
C        REAL PART
C
         CALL SGEMVX1(LEVS,(JCAP+2-L)/2,1.,FLN(LS+1,1),LNT22,4,
     &                QLN(LS+1),4,1.,AP(1,L,1),(LEN0+1)*2)
C
C        IMAGINARY PART
C
         CALL SGEMVX1(LEVS,(JCAP+2-L)/2,1.,FLN(LS+2,1),LNT22,4,
     &                QLN(LS+2),4,1.,AP(2,L,1),(LEN0+1)*2)
C
C        COMPUTE THE SUM OF THE ODD (N-L) TERMS FOR EACH LEVEL
C        -----------------------------------------------------
         IF(L.LT.JCAP) THEN
C
C        REAL PART
C
         CALL SGEMVX1(LEVS,(JCAP+1-L)/2,1.,FLN(LS+3,1),LNT22,4,
     &                QLN(LS+3),4,1.,AP(1,LENH+L,1),(LEN0+1)*2)
C
C        IMAGINARY PART
C
         CALL SGEMVX1(LEVS,(JCAP+1-L)/2,1.,FLN(LS+4,1),LNT22,4,
     &                QLN(LS+4),4,1.,AP(2,LENH+L,1),(LEN0+1)*2)
         ENDIF
      END DO
C
C
C     COMPUTE THE FOURIER COEFFICIENTS FOR EACH LEVEL
C     -----------------------------------------------
C
      DO K = 1, LEVS
         DO L = 0, JCAP
            EVENR = AP(1,L,K)
            EVENI = AP(2,L,K)
            AP(1,L,K) = AP(1,L,K) + AP(1,LENH+L,K)
            AP(2,L,K) = AP(2,L,K) + AP(2,LENH+L,K)
            AP(1,LENH+L,K) = EVENR - AP(1,LENH+L,K)
            AP(2,LENH+L,K) = EVENI - AP(2,LENH+L,K)
         END DO
      END DO
C
      RETURN
      END
