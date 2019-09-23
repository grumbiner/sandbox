      SUBROUTINE PRINTER ( IHOUR, IMIN, ISEC, IB, IE )
      INCLUDE "glgrid.inc"

      CHARACTER TITLE(4)*30
      DIMENSION  JO(NX,NY), JOFN(NX,NY), MM(110), NO(10), TICE(NX,NY),
     *           U(NX,NY), V(NX,NY), CN(NX,NY), BOUNDP(4),
     *           BOUNDM(4), NO2(2)
      COMMON / MASS / CN
      COMMON / SPEEDY / U , V
      COMMON / THICK / TICE
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / JOINT / JO , JOFN
      DATA NO2 / 1H- , 1H /
      DATA NO / 1H0, 1H1, 1H2, 1H3, 1H4, 1H5, 1H6, 1H7, 1H8, 1H9 /
      DATA TITLE(1)/'ICE CONCENTRATION (PERCENT)'/
      DATA TITLE(2)/'ICE THICKNESS (CM)'/
      DATA TITLE(3)/'ICE VELOCITY U (CM/S)'/
      DATA TITLE(4)/'ICE VELOCITY V (CM/S)'/
C ABOVE FOR STATEMENT REPLACE BELOW FOUR STATEMENTS
C      DATA (TITLE(1,I),I=1,3)/10HICE CONCEN,10HTRATION ( ,10HPER CENT )/
C      DATA (TITLE(2,I),I=1,3)/10HICE THICKN,10HESS   ( CM,10H )        /
C      DATA (TITLE(3,I),I=1,3)/10HICE VELOCI,10HTY U  ( CM,10H/S )      /
C      DATA (TITLE(4,I),I=1,3)/10HICE VELOCI,10HTY V  ( CM,10H/S )      /
      DATA BOUNDP / 100.5 , 3*999.5 / , BOUNDM / 2*0.5 , 2*-998.5 /
C
      DO 999 II = IB , IE
      WRITE (6, 1100) TITLE(II),IHOUR,IMIN,ISEC
C ABOVE RELPLACES BELOW STATEMENT
C      WRITE ( 6 , 1100 ) ( TITLE (II,I), I = 1 , 3 ), IHOUR, IMIN, ISEC
      WRITE ( 6 , 1001 ) ( I , I = 10 , 110 , 10 )
      IF ( II .LE. 2 ) GO TO 9
      NG = N
      MG = M
      GO TO 10
    9 NG = NM1
      MG = MM1
C
   10 DO 110 I = 1 , NG
      KK = 5 * I
      DO 115 K = 1 , 110
C  115 MM (K) = 1H
C REPLACED ABOVE STATEMENT WITH BELOW STATEMENT
  115 MM (K) = NO2(2)
      MM (88) = 1HI
      DO 100 J = 1 , MG
      JGN = JOFN (I,J)
      IF ( II .GT. 2 ) JGN = JO (I,J)
      IF ( JGN .EQ. 0 ) GO TO 100
      K = J * 4
C
      GO TO ( 11 , 12 , 13 , 14 ) II
   11 GIJ = CN (I,J) * 100. + 0.5
      GO TO 5
   12 GIJ = TICE (I,J) * 100. + 0.5
      GO TO 5
   13 GIJ = U (I,J) * 100. + 0.5
      GO TO 5
   14 GIJ = V (I,J) * 100. + 0.5
    5 IF ( GIJ ) 120 , 130 , 140
  120 IF ( GIJ .LT. BOUNDM (II) ) GO TO 125
      ISIGN = 1
      GO TO 150
  140 IF ( GIJ .GT. BOUNDP (II) ) GO TO 145
      ISIGN = 2
      GO TO 150
  130 MM (K) = 1H0
      GO TO 100
  125 MM (K) = 1H-
      MM (K-1) = 1H-
      MM (K-2) = 1H-
      GO TO 100
  145 MM (K) = 1H+
      MM (K-1) = 1H+
      MM (K-2) = 1H+
      GO TO 100
  150 IN = ABS (GIJ)
      IN3 = IN / 100
      IN2 = ( IN - IN3 * 100 ) / 10
      IN1 = IN - IN3 * 100 - IN2 * 10
      MM (K) = NO (IN1+1)
      MM (K-1) = NO (IN2+1)
      MM (K-2) = NO (IN3+1)
      IF ( IN3 .EQ. 0 .AND. IN2 .EQ. 0 ) GO TO 160
      IF ( IN3 .EQ. 0 ) GO TO 161
      MM (K-3) = NO2 (ISIGN)
      GO TO 100
C  160 MM (K-2) = 1H
C REPLACE ABOVE STATEMENT WITH BELOW STATEMENT
  160 MM (K-2) = NO2(2)
      MM (K-1) = NO2 (ISIGN)
      GO TO 100
  161 MM (K-2) = NO2 (ISIGN)
  100 CONTINUE
      IF ( I / 2 * 2 .EQ. I ) GO TO 170
      WRITE ( 6 , 1004 ) MM
      GO TO 110
  170 WRITE ( 6 , 1003 ) KK , MM
  110 CONTINUE
      WRITE ( 6 , 1002 ) ( I , I = 10 , 110 , 10 )
  999 CONTINUE
      RETURN
C
 1001 FORMAT ( 9X, '0 ', 11I8, /, 9X, 'I', 11('---.---I') )
 1002 FORMAT ( 9X,'+',/,9X,'I',11('---.---I'),/,9X,'0 ',11I8,///// )
 1003 FORMAT ( 9X , '+' , / , 5X , I4 , 'I' , 110A1 )
 1004 FORMAT ( 9X , '+' , / , 9X , '+' , 110A1 )
 1100 FORMAT ( 1H1, ///// , 15X , 3A10 , ' AT' , I4 , ' HOURS' , I4 ,
     '         ' MINUTES' , I4 , ' SECONDS' , // )
      END
