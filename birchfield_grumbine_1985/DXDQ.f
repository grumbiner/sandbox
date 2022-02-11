      FUNCTION DXDQ(LAT)
  
C     DXDQ IS DELTAQ/DELTAX ^-1, LINEARLY INTERPOLATED FOR THE
C       DESIRED LATITUDE. 
C     LAT IS THE LATITUDE TO COMPUTE DXDQ FOR 
C     TABLE IS A TABLE OF THE LATITUDES AND DXDQ VALUES READ IN.
C        TABLE(1, X) ARE THE LATITUDES, TABLE(2, X) ARE THE DXDQ. 
C     DX1 AND DX2 ARE USED IN MAKING A LINEAR INTERPOLATION FOR 
C       VALUES OF LAT. NOT IN THE TABLE.
C     I IS A DUMMY COUNTER
C     N IS THE NUMBER OF DATA POINTS. 
  
      REAL DXDQ, LAT, TABLE(2,100)
      REAL DX1, DX2 
      INTEGER I, N
  
C     FIND THE TWO POINTS IN THE TABLE THAT BRACKET THE LATITUDE
C       INPUT.  IF THE LAT. INPUT EQUALS ONE THAT IS IN THE TABLE,
C       USE THAT POINT, AND THE NEXT ONE (HIGHER LAT) IN THE
C       TABLE.
      I = 0 
  100 CONTINUE
        I = I + 1 
        IF (LAT .GE. TABLE(1,I) .AND. I.LT.N) GOTO 100
  
C     INTERPOLATE DXDQ LINEARLY FOR THE DESIRED LATITUDE. 
      DX1  = TABLE(1,I) - TABLE(1,I-1)
      DX2  = LAT - TABLE(1,I-1) 
      DXDQ = TABLE(2, I-1) + (TABLE(2,I) - TABLE(2,I-1))*DX2/DX1
  
      RETURN
  
      ENTRY DXDQSET
 
      READ (*,9001) N
      DO 200 I= 1, N
        READ (*,9002) TABLE(1,I), TABLE(2,I)
  200 CONTINUE

 9001 FORMAT (I5)
 9002 FORMAT (2F14.4)
  
      RETURN
      END