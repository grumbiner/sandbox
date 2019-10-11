      SUBROUTINE COORD (IT,JT,IHEMI)
C
C  NORTHERN HEMISPHERE
C  X,Y COORDINATES ARE INPUTTED, LATITUDE AND LONGITUDE ARE OUTPUTTED.
C  DLAT, DLONG REFER TO DEGREES OF LATITUDE AND LONGITUDE
C  RLAT, RLONG REFER TO RADIANS OF LATITUDE AND LONGITUDE
C  NEED X,Y, AND NEED THEM SUCH THAT (0,0) SIGNIFIES THE POLE.

      IMPLICIT none

      INTEGER IT, JT, IHEMI
      REAL RLAT, RLONG, DLAT, DLONG, COSZ, STIME, TIME, TSFLUX
      REAL XLAT(42,31), XLON(42,31)
      INTEGER NSTEP

      REAL B1, X, Y, A, B, SINLAT, COSLON
      INTEGER I, J

      COMMON /C6/ RLAT,RLONG,DLAT,DLONG,NSTEP,
     1COSZ,STIME,TIME,TSFLUX,XLAT,XLON

      DATA B1/0.016569/

      DO 60 I=1,IT
      DO 60 J=1,JT
      IF(IHEMI.EQ.2) GO TO 5
      X= FLOAT(I)-24.0
      Y= FLOAT(J)-19.0
      GO TO 8
    5 CONTINUE
      Y= FLOAT(I)-21.0
      X= FLOAT(J)-21.0
    8 CONTINUE
      IF (X.EQ.0.0) GO TO 19
      IF (Y.LT.0.0) GO TO 10
      IF (X.LT.0.0) GO TO 11
      A=Y/X
      RLONG=ATAN(A)
      GO TO 20
   10 IF (X.LT.0.0) GO TO 12
      A=-Y/X
      RLONG=6.283185  -ATAN(A)
      GO TO 20
   12 A=Y/X
      RLONG=3.141593  +ATAN(A)
      GO TO 20
   11 A=-Y/X
      RLONG=3.141593   -ATAN(A)
   20 CONTINUE
      COSLON=COS(RLONG)
      B=X*B1/COSLON
      GO TO 50
   19 IF (Y.LT.0.0) GO TO 18
      RLONG=1.570796
      B=Y*B1
      GO TO 50
   18 RLONG=4.712389
      B=-Y*B1
   50 SINLAT=(1.0-B*B)/(1.0+B*B)
CBG      RLAT=ARSIN(SINLAT)
      RLAT=ASIN(SINLAT)
      IF(IHEMI.EQ.2) RLAT=-RLAT
      XLAT(I,J)=RLAT
      XLON(I,J)=RLONG
   60 CONTINUE
      RETURN
      END