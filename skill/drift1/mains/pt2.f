      PROGRAM skpoints
C     Compute the latitude and longitude of all the Skiles points.
C     Bob Grumbine 2 June 1994.

      INTEGER npoint
      PARAMETER (npoint = 207)

      REAL*4  XML(npoint) 
C
      INTEGER*4  IJSEL(2*npoint)
      INTEGER*4  IJA(138),IJB(138),IJC(138)
C
      REAL lat, long, degprd
      PARAMETER (degprd = 180./3.141592654)
      REAL FJD, FID, RSQ, SINL

      EQUIVALENCE (IJA(1),IJSEL(1)), (IJB(1),IJSEL(139)),
     1                               (IJC(1),IJSEL(277))
C
C     Location of the Skiles points.
      DATA IJA/
     A  28,15,30,15,24,16,29,16,30,16,31,16,23,17,24,17,27,17,28,17,    00013300
     B  29,17,30,17,31,17,22,18,23,18,24,18,25,18,26,18,27,18,28,18,    00013400
     C  29,18,30,18,23,19,24,19,25,19,26,19,27,19,28,19,29,19,30,19,    00013500
     D  22,20,23,20,24,20,26,20,27,20,28,20,29,20,30,20,21,21,22,21,    00013600
     E  23,21,24,21,25,21,26,21,29,21,30,21,20,22,21,22,22,22,23,22,    00013700
     F  24,22,25,22,26,22,29,22,30,22,19,23,20,23,21,23,22,23,23,23,    00013800
     G  24,23,28,23,29,23,30,23,31,23,19,24,20,24,21,24,22,24/
      DATA IJB/
     A  23,24,24,24,25,24,27,24,28,24,29,24,30,24,14,25,15,25,16,25,    00014100
     B  17,25,18,25,19,25,20,25,21,25,22,25,23,25,24,25,25,25,26,25,    00014200
     C  27,25,28,25,29,25,14,26,15,26,16,26,17,26,18,26,19,26,20,26,    00014300
     D  21,26,22,26,23,26,24,26,25,26,26,26,27,26,28,26,29,26,33,26,    00014400
     E  34,26,14,27,15,27,16,27,17,27,18,27,19,27,20,27,21,27,22,27,    00014500
     F  23,27,24,27,25,27,26,27,27,27,28,27,29,27,32,27,33,27,34,27,    00014600
     G  14,28,15,28,16,28,19,28,20,28,21,28,22,28,23,28,24,28/
      DATA IJC/
     A  25,28,26,28,27,28,28,28,29,28,31,28,32,28,33,28,15,29,16,29,    00014900
     B  20,29,21,29,22,29,23,29,24,29,25,29,26,29,27,29,28,29,29,29,    00015000
     C  30,29,32,29,15,30,16,30,17,30,21,30,22,30,23,30,25,30,26,30,    00015100
     D  27,30,28,30,29,30,36,30,37,30,15,31,17,31,26,31,27,31,28,31,    00015200
     E  36,31,37,31,14,32,15,32,16,32,17,32,18,32,35,32,36,32,14,33,    00015300
     F  15,33,16,33,17,33,18,33,14,34,15,34,16,34,17,34,18,34,14,35,    00015400
     G  15,35,16,35,16,36,16,37,16,38,17,38,17,39,18,40,19,40/

      DATA IPOLE/24/, JPOLE/26/
C
C...................................................................... 00017400
C
C FORM THE MAP FACTOR IN 'XMF' FOR THE SELECTED POINTS.
C
        K=0
        DO 20  M=1,2*npoint,2
          K=K+1
          I=IJSEL(M)
          J=IJSEL(M+1)
          FID=IPOLE-I
          FJD=JPOLE-J
          RSQ=FID**2 + FJD**2
          SINL=(973.71 - RSQ)/(973.71 + RSQ)
          XML(K)=1.35985/SINL
C         XML is the combination of the parameters other than
C          delta P which are used to compute the geostrophic
C          wind. (! BG)
          lat = degprd*ASIN(SINL)
          IF (FID .EQ. 0. .AND. FJD .EQ. 0 ) THEN
            long = -80.
          ELSE
            long = -80.-degprd*ATAN2(FID, FJD)
          ENDIF

          IF (long .GT. 0.0) THEN     
            WRITE (*,9001) K, lat, long
           ELSE
            WRITE (*,9001) K, lat, 360.+long 
          ENDIF
   20   CONTINUE

C       Test southern hemisphere points.
        ipole = 25
        jpole = 25
        DO 1000 j = 14, 36
          DO 1100 i = 14, 36
            fid = FLOAT(ipole - i)
            fjd = FLOAT(jpole - j)
            rsq = fid**2 + fjd**2
            SINL=(973.71 - RSQ)/(973.71 + RSQ)
            lat =  - degprd*ASIN(SINL)
            IF (FID .EQ. 0. .AND. FJD .EQ. 0 ) THEN
              long = -80.
            ELSE
              long = -80.-degprd*ATAN2( fid, fjd )
            ENDIF
            IF (lat .LT. -55. .AND. lat .GT. -81.) THEN
              k = k + 1
              IF (long .GT. 0.0) THEN     
                WRITE (*,9001) K, lat, long
               ELSE
                WRITE (*,9001) K, lat, 360.+long 
              ENDIF
            ENDIF
 1100   CONTINUE 
 1000 CONTINUE



 9001 FORMAT (I3, 3x, F8.4, 7x, F8.4)
C
      STOP
      END
