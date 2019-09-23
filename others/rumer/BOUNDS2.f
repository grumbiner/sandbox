      SUBROUTINE BOUNDS2
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
      IF ( JOM .LT. 1 ) GO TO 100
      TAUAXIJ = TAUAX ( I , J )
      TAUAYIJ = TAUAY ( I , J )
      GO TO ( 1, 2, 14, 15, 11, 11, 13, 13, 30, 31, 32, 33 ) , JOM
1     IF ( U ( I-1 , J ) ) 11 , 10 , 12
2     IF ( U ( I+1 , J ) ) 12 , 9 , 13
9     IF ( TAUAXIJ ) 12 , 13 , 13
10    IF ( TAUAXIJ ) 11 , 11 , 12
11    U(I,J) = U ( I-1 , J )
      GO TO 3
12    U(I,J) = 0.
      GO TO 3
13    U(I,J) = U ( I+1 , J )
      GO TO 3
14    U(I,J) = U ( I , J-1 )
      GO TO 3
15    U(I,J) = U ( I , J+1 )
3     GO TO ( 24 , 25 , 5 , 6 , 23 , 21 , 21 , 23 ) , JOM
5     IF ( V ( I , J-1 ) ) 21 , 20 , 22
6     IF ( V ( I , J+1 ) ) 22 , 19 , 23
19    IF ( TAUAYIJ ) 22 , 23 , 23
20    IF ( TAUAYIJ ) 21 , 21 , 22
21    V(I,J) = V ( I , J-1 )
      GO TO 100
22    V(I,J) = 0.
      GO TO 100
23    V(I,J) = V ( I , J+1 )
      GO TO 100
24    V(I,J) = V ( I-1 , J )
      GO TO 100
25    V(I,J) = V ( I+1 , J )
      GO TO 100
30    IF ( U ( I+1 , J+1 ) ) 41 , 41 , 42
31    IF ( U ( I+1 , J-1 ) ) 51 , 51 , 52
32    IF ( U ( I-1 , J-1 ) ) 61 , 61 , 62
33    IF ( U ( I-1 , J+1 ) ) 71 , 71 , 72
41    U(I,J) = 0.
      GO TO 80
42    U(I,J) = U ( I+1 , J+1 )
      GO TO 80
51    U(I,J) = 0.
      GO TO 81
52    U(I,J) = U ( I+1 , J-1 )
      GO TO 81
61    U(I,J) = U ( I-1 , J-1 )
      GO TO 82
62    U(I,J) = 0.
      GO TO 82
71    U(I,J) = U ( I-1 , J+1 )
      GO TO 83
72    U(I,J) = 0.
      GO TO 83
80    IF ( V ( I+1 , J+1 ) ) 91 , 91 , 92
81    IF ( V ( I+1 , J-1 ) ) 101 , 101 , 102
82    IF ( V ( I-1 , J-1 ) ) 111 , 111 , 112
83    IF ( V ( I-1 , J+1 ) ) 121 , 121 , 122
91    V(I,J) = 0.
      GO TO 100
92    V(I,J) = V ( I+1 , J+1 )
      GO TO 100
101   V(I,J) = V ( I+1 , J-1 )
      GO TO 100
102   V(I,J) = 0.
      GO TO 100
111   V(I,J) = V ( I-1 , J-1 )
      GO TO 100
112   V(I,J) = 0.
      GO TO 100
121   V(I,J) = 0.
      GO TO 100
122   V(I,J) = V ( I-1 , J+1 )
100   CONTINUE
      RETURN
      END
