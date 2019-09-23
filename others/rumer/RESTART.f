      SUBROUTINE RESTART ( T , PT , INDEX , OVERWRT )
      INCLUDE "glgrid.inc"

      INTEGER OVERWRT
      DIMENSION U(NX,NY), V(NX,NY), CN(NX,NY), TICE(NX,NY), DIV(NX,NY),
     *          P(NX,NY)
      COMMON / SPEEDY / U , V
      COMMON / DIVER / DIV
      COMMON / MASS / CN
      COMMON / THICK / TICE
      COMMON / PRESUR / P
      COMMON / GENERAL / N , M , NM1 , MM1
      INDEX = 1
      READ ( 9 , 50 ) IDUM
      READ ( 9 , 50 ) IDUM
      IS = FLOAT( N * M ) / 30. + .97
      ISM = FLOAT( NM1*MM1 ) / 30. + .97
      DO 52 I = 1 , IS
52    READ ( 9 , 50 ) IDUM
      DO 51 I = 1 , ISM
51    READ ( 9 , 50 ) IDUM
      DO 66 K = 1 , 100
c      READ ( 9 , 10 ) T , A , B , C , D , E , F
C      IF ( EOF(9) ) 70, 59
C      made change in above 2 lines as follows
      read(9, 10, end=70) T, A, B, C, D, E, F
59    WRITE ( 6 , 55 ) T , A , B , C , D , E , F
      READ ( 9 , 10 ) (( U(I,J), J = 1 , M ), I = 1, N )
      READ ( 9 , 10 ) (( V(I,J), J = 1 , M ), I = 1, N )
      READ ( 9 , 10 ) (( CN(I,J), J = 1 , MM1 ), I = 1, NM1 )
      READ ( 9 , 10 ) (( P(I,J), J = 1 , MM1 ), I = 1, NM1 )
      READ ( 9 , 10 ) (( TICE(I,J), J = 1 , MM1 ), I = 1, NM1 )
      READ ( 9 , 15 ) (( DIV(I,J), J = 1 , MM1 ), I = 1, NM1 )
      IF ( T .EQ. PT ) GO TO 65
66    CONTINUE
70    INDEX = 0
      GO TO 64
65    WRITE ( 6 , 20 ) T , A , B , C , D , E , F
      GO TO 67
64    WRITE ( 6 , 30 ) PT , T
67    IF ( (OVERWRT .EQ. 3HYES) .OR. (INDEX .EQ. 0) ) REWIND 9
10    FORMAT ( 15F8.0 )
15    FORMAT ( 15E8.1 )
20    FORMAT ( //, 7X,'RESTART FROM T =',F10.1,'SEC.',5X,'WIND ',
     *         'AT TOLEDO    =',F4.1,' M/SEC',2X,F5.1,' DEG',/,42X,
     *    'WIND AT CLEVELAND =',F4.1,' M/SEC',2X,F5.1,' DEG',/,42X,
     *    'WIND AT UFFALO    =',F4.1,' M/SEC',2X,F5.1,' DEG',// )
30    FORMAT (/,10X,'CANNOT LOCATE T =',F10.1,' SEC.',5X,'LAST READ',
     *        ' FROM FILE 9 WAS T =',F10.1,' SEC.',/)
50    FORMAT ( A1 )
55    FORMAT ( // , 20X , 'T =' , F10.1 , 'SEC.' , 5X ,
     *    'WIND AT TOLEDO    =',F4.1,'M/SEC',2X,F5.1,'DEG.',/,42X,
     *    'WIND AT CLEVELAND =',F4.1,'M/SEC',2X,F5.1,'DEG.',/,42X,
     *    'WIND AT BUFFALO   =',F4.1,'M/SEC',2X,F5.1,'DEG.',/ )
      RETURN
      END
