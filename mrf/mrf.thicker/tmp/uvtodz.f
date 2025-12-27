      SUBROUTINE UVTODZ(ULN,VLN,DI,ZE,TOPULN,TOPVLN)
      PARAMETER (JCAP= 62 )
      DIMENSION  DI( 4033 , 28 )
      DIMENSION  ZE( 4033 , 28 )
      DIMENSION ULN( 4033 , 28 )
      DIMENSION VLN( 4033 , 28 )
      DIMENSION EPS( 4033 )
      DIMENSION   TOPULN(2,0: 62 , 28 )
      DIMENSION   TOPVLN(2,0: 62 , 28 )
      DIMENSION   TOPEPS(0: 62 )
C
      SAVE IFIRST,EPS,TOPEPS
C
C     LOCAL SCALARS
C     -------------
C
      INTEGER I, N, L, K
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
C               ?   X X X X
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
C     TERM(I,N,L,K) = DI(OFFSET(N,L)+I,K)
C
C
      DATA IFIRST/0/
      IF(IFIRST.NE.0) GO TO 999
      DO L = 0, JCAP
         DO N = L, JCAP
            TEMP=((N*N-L*L)/(4.*N*N-1.))
            IF(N.EQ.0) TEMP=0.
            TEMP=SQRT(TEMP)
            EPS(OFFSET(N,L)+1)=TEMP
            EPS(OFFSET(N,L)+2)=TEMP
         END DO
            N=JCAP+1
            TEMP=((N*N-L*L)/(4.*N*N-1.))
            TOPEPS(L)=SQRT(TEMP)
      END DO
C
      IFIRST=1
999   CONTINUE
CMIC$ DO ALL
CMIC$1 AUTOSCOPE
               DO 1000      K = 1,  28
C        THE CASE N=L
      DO L = 1, JCAP-1
       RL=L
            N = L
          RN=N
C           DO K = 1,  28
             ZE(OFFSET(N,L)+1,K)=-RL*VLN(OFFSET(N,L)+2,K)
     1       -RN*EPS(OFFSET(N+1,L)+1)*ULN(OFFSET(N+1,L)+1,K)
C
             ZE(OFFSET(N,L)+2,K)= RL*VLN(OFFSET(N,L)+1,K)
     1       -RN*EPS(OFFSET(N+1,L)+2)*ULN(OFFSET(N+1,L)+2,K)
C
             DI(OFFSET(N,L)+1,K)=-RL*ULN(OFFSET(N,L)+2,K)
     1       +RN*EPS(OFFSET(N+1,L)+1)*VLN(OFFSET(N+1,L)+1,K)
C
             DI(OFFSET(N,L)+2,K)= RL*ULN(OFFSET(N,L)+1,K)
     1       +RN*EPS(OFFSET(N+1,L)+2)*VLN(OFFSET(N+1,L)+2,K)
C
C           END DO
      END DO
CCCCCCC
C     DO K=1, 28
       ZE(1,K)=0.
       ZE(2,K)=0.
       DI(1,K)=0.
       DI(2,K)=0.
C     ENDDO
      DO L = 0, JCAP
       RL=L
         DO N = L+1, JCAP-1
          RN=N
C           DO K = 1,  28
             ZE(OFFSET(N,L)+1,K)=-RL*VLN(OFFSET(N,L)+2,K)
     1       -RN*EPS(OFFSET(N+1,L)+1)*ULN(OFFSET(N+1,L)+1,K)
     2       +(RN+1.)*EPS(OFFSET(N,L)+1)*ULN(OFFSET(N-1,L)+1,K)
C
             ZE(OFFSET(N,L)+2,K)= RL*VLN(OFFSET(N,L)+1,K)
     1       -RN*EPS(OFFSET(N+1,L)+2)*ULN(OFFSET(N+1,L)+2,K)
     2       +(RN+1.)*EPS(OFFSET(N,L)+2)*ULN(OFFSET(N-1,L)+2,K)
C
             DI(OFFSET(N,L)+1,K)=-RL*ULN(OFFSET(N,L)+2,K)
     1       +RN*EPS(OFFSET(N+1,L)+1)*VLN(OFFSET(N+1,L)+1,K)
     2       -(RN+1.)*EPS(OFFSET(N,L)+1)*VLN(OFFSET(N-1,L)+1,K)
C
             DI(OFFSET(N,L)+2,K)= RL*ULN(OFFSET(N,L)+1,K)
     1       +RN*EPS(OFFSET(N+1,L)+2)*VLN(OFFSET(N+1,L)+2,K)
     2       -(RN+1.)*EPS(OFFSET(N,L)+2)*VLN(OFFSET(N-1,L)+2,K)
C
C           END DO
         END DO
      END DO
C DO TOP ROW INVOLVING U,V AT N=JCAP+1
CCCCCCC
          N =  JCAP
          RN=N
      DO L = 0, JCAP
       RL=L
C           DO K = 1,  28
             ZE(OFFSET(N,L)+1,K)=-RL*VLN(OFFSET(N,L)+2,K)
     2       +(RN+1.)*EPS(OFFSET(N,L)+1)*ULN(OFFSET(N-1,L)+1,K)
C
             ZE(OFFSET(N,L)+2,K)= RL*VLN(OFFSET(N,L)+1,K)
     2       +(RN+1.)*EPS(OFFSET(N,L)+2)*ULN(OFFSET(N-1,L)+2,K)
C
             DI(OFFSET(N,L)+1,K)=-RL*ULN(OFFSET(N,L)+2,K)
     2       -(RN+1.)*EPS(OFFSET(N,L)+1)*VLN(OFFSET(N-1,L)+1,K)
C
             DI(OFFSET(N,L)+2,K)= RL*ULN(OFFSET(N,L)+1,K)
     2       -(RN+1.)*EPS(OFFSET(N,L)+2)*VLN(OFFSET(N-1,L)+2,K)
C
C           END DO
      END DO
CCCCCCC
      DO L = 0, JCAP
C           DO K = 1,  28
             ZE(OFFSET(N,L)+1,K)=ZE(OFFSET(N,L)+1,K)
     1       -RN*TOPEPS(L)*TOPULN(1,L,K)
C
             ZE(OFFSET(N,L)+2,K)=ZE(OFFSET(N,L)+2,K)
     1       -RN*TOPEPS(L)*TOPULN(2,L,K)
C
             DI(OFFSET(N,L)+1,K)=DI(OFFSET(N,L)+1,K)
     1       +RN*TOPEPS(L)*TOPVLN(1,L,K)
C
             DI(OFFSET(N,L)+2,K)=DI(OFFSET(N,L)+2,K)
     1       +RN*TOPEPS(L)*TOPVLN(2,L,K)
C
C           END DO
      END DO
CCCCCCC
C           DO K = 1,  28
             DO J=1, 4032
              DI(J,K)=DI(J,K)/ 6.3712E+6
              ZE(J,K)=ZE(J,K)/ 6.3712E+6
             END DO
C           END DO
1000  CONTINUE
      RETURN
      END
