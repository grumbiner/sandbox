      SUBROUTINE BOUNDS1
      INCLUDE "glgrid.inc"

      DIMENSION U(NX,NY), V(NX,NY), JO(NX,NY), JOFN(NX,NY)
     *        , TAUAX(NX,NY), TAUAY(NX,NY)
      COMMON / SPEEDY / U , V
      COMMON / TAUS / TAUAX , TAUAY
      COMMON / COEFFNT / C1 , C2 , C3
      COMMON / JOINT / JO , JOFN
      COMMON / GENERAL / N , M , NM1 , MM1
      DO 100 I = 1 , N
      DO 100 J = 1 , M
      JOM = JO ( I , J ) - 1
      IF ( ( JOM .LT. 1 ) .OR. ( JOM .EQ. 9 ) ) GO TO 100
      TAUAXIJ = TAUAX ( I , J )
      TAUAYIJ = TAUAY ( I , J )
      GO TO ( 1 , 2 , 3 , 3 , 1 , 1 , 2 , 2 ) , JOM
1     IF ( U ( I-1 , J ) ) 11 , 10 , 12
2     IF ( U ( I+1 , J ) ) 12 , 9 , 13
9     IF ( TAUAXIJ ) 12 , 13 , 13
10    IF ( TAUAXIJ ) 11 , 11 , 12
11    U(I,J) = U ( I-1 , J )
      GO TO 3
12    U(I,J) = 0.
      GO TO 3
13    U(I,J) = U ( I+1 , J )
3     GO TO ( 100 , 100 , 5 , 6 , 6 , 5 , 5 , 6 ) , JOM
5     IF ( V ( I , J-1 ) ) 21 , 20 , 22
6     IF ( V ( I , J+1 ) ) 22 , 19 , 23
19    IF ( TAUAYIJ ) 22 , 23 , 23
20    IF ( TAUAYIJ ) 21 , 21 , 22
21    V(I,J) = V ( I , J-1 )
      GO TO 100
22    V(I,J) = 0.
      GO TO 100
23    V(I,J) = V ( I , J+1 )
100   CONTINUE
      RETURN
      END
