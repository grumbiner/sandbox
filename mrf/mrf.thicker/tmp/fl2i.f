      SUBROUTINE FL2I(FP,FM,FLN,QLN,LEVS)
      PARAMETER (LEN0P= 62 )
      PARAMETER (LEN0M= 62 )
      PARAMETER (LNT= 2016 )
      PARAMETER (LNT22= 4033 )
      PARAMETER (JCAP= 62 )
      DIMENSION FP(2,0:LEN0P,LEVS), FM(2,0:LEN0M,LEVS),
     .          QLN(2*LNT), FLN(LNT22,LEVS)
C
C     LOCAL SCALARS
C     -------------
C
      INTEGER N, L, K
C
C     STATEMENT FUNCTION
C     ------------------
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
      OFFSET(N,L) = (JCAP+1)*(JCAP+2) - (JCAP+1-L)*(JCAP+2-L) + 2*(N-L)
C
C     ----------------------------------------------------------------
C     COMPUTE THE COEFFICIENTS OF THE EXPANSION IN SPHERICAL HARMONICS
C     OF THE FIELD AT EACH LEVEL
C     ----------------------------------------------------------------
C
CFPP$ CNCALL
      DO L = 0, JCAP
         LS=L*((2*JCAP+3)-L)
C
C        COMPUTE THE EVEN (N-L) EXPANSION COEFFICIENTS FOR EACH LEVEL
C        ------------------------------------------------------------
C
C        REAL PART
C
         CALL SGERX1((JCAP+2-L)/2,LEVS,1.,QLN(LS+1),4,
     &               FP(1,L,1),(LEN0P+1)*2,FLN(LS+1,1),4,LNT22)
C
C        IMAGINARY PART
C
         CALL SGERX1((JCAP+2-L)/2,LEVS,1.,QLN(LS+2),4,
     &               FP(2,L,1),(LEN0P+1)*2,FLN(LS+2,1),4,LNT22)
C
C        COMPUTE THE ODD (N-L) EXPANSION COEFFICIENTS FOR EACH LEVEL
C        -----------------------------------------------------------
         IF(L.LT.JCAP) THEN
C
C        REAL PART
C
         CALL SGERX1((JCAP+1-L)/2,LEVS,1.,QLN(LS+3),4,
     &               FM(1,L,1),(LEN0M+1)*2,FLN(LS+3,1),4,LNT22)
C
C        IMAGINARY PART
C
         CALL SGERX1((JCAP+1-L)/2,LEVS,1.,QLN(LS+4),4,
     &               FM(2,L,1),(LEN0M+1)*2,FLN(LS+4,1),4,LNT22)
C
         ENDIF
      END DO
C
      RETURN
      END
