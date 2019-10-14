      SUBROUTINE CHECK
      INCLUDE "glgrid.inc"

      DIMENSION CN (NX,NY) , TICE (NX,NY)
      COMMON / MASS / CN
      COMMON / THICK / TICE
      COMMON / GENERAL / N , M , NM1 ,MM1
C
      VOLUME = 0.
      AREA = 0.
      DO 100 I = 1 , NM1
      DO 100 J = 1 , MM1
      AREA = AREA + CN (I,J)
      VOLUME = VOLUME + CN (I,J) * TICE (I,J)
  100 CONTINUE
      AREA = AREA * 25000000.
      VOLUME = VOLUME * 25000000.
      WRITE ( 6 , 200 ) AREA , VOLUME
  200 FORMAT ( //,20X,'TOTAL ICE AREA   = ',F15.0,' SQUARE METERS',
     *          //,20X,'TOTAL ICE VOLUME = ',F15.0,' CUBIC METERS',/// )
      RETURN
      END
