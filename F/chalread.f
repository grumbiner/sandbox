      SUBROUTINE chalread(U, V, S, T, MASK, MASKU, MASKV, DEPTH)
      IMPLICIT none

      INTEGER L, M, N
      PARAMETER (L = 34)
      PARAMETER (M = 360)
      PARAMETER (N = 151)

      CHARACTER*60 fname
      INTEGER NST
      REAL TIMES

      REAL H(0:M+1,0:N+1)
      REAL U(0:M+1,0:N+1, L)
      REAL V(0:M+1,0:N+1, L)
      REAL S(0:M+1,0:N+1, L)
      REAL T(0:M+1,0:N+1, L)
      REAL DEPTH(M, N), MRF(M, 181)
      INTEGER IMASK(M,N,L)
      INTEGER IMASKU(M+1,N,L)
      INTEGER IMASKV(M,N+1,L)
      REAL MASK(0:M+1,0:N+1,L)
      REAL MASKU(0:M+1,0:N+1,L)
      REAL MASKV(0:M+1,0:N+1,L)

      INTEGER i, j, k

      OPEN (14, FILE="restart", FORM="UNFORMATTED", STATUS="OLD")
      READ (14) TIMES, NST, H, U, V, T, S
      CLOSE (14)

      OPEN (14, FILE="masks", FORM="UNFORMATTED", STATUS="OLD")
      READ (14) MRF, IMASK, DEPTH, IMASKU, IMASKV
      CLOSE (14)

      DO 1000 k = 1, L
        DO 1000 j = 0, N+1
          DO 1000 i = 0, M+1
            MASK(i,j,k) = 0
            MASKU(i,j,k) = 0
            MASKV(i,j,k) = 0
 1000 CONTINUE

      DO 1100 k = 1, L
        DO 1100 j = 1, N
          DO 1100 i = 1, M
            MASK(i,j,k) = FLOAT(IMASK(i,j,k))
 1100 CONTINUE

      DO 1200 k = 1, L
        DO 1200 j = 1, N
          DO 1200 i = 1, M+1
            MASKU(i,j,k) = FLOAT(IMASKU(i,j,k))
 1200 CONTINUE

      DO 1300 k = 1, L
        DO 1300 j = 1, N+1
          DO 1300 i = 1, M
            MASKV(i,j,k) = FLOAT(IMASKV(i,j,k))
 1300 CONTINUE
      
      RETURN
      END
