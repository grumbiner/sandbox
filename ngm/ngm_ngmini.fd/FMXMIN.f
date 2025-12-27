       SUBROUTINE FMXMIN(F,N,MESAGE)
C--------
         CHARACTER*1 MESAGE(80),DOLLAR
         DATA DOLLAR/'$'/
C--------
         DIMENSION F(N)
C--------
         IFMAX=MIN(5,N)
         FMAX=-1.E15
         FMIN=1.E15
         FBAR=0.
         FRMS=0.
         RN=MAX(1,N)
         DO 100 I=1,N
           FMAX=MAX(F(I),FMAX)
           FMIN=MIN(F(I),FMIN)
           FBAR=FBAR+F(I)
           FRMS=FRMS+F(I)**2
100      CONTINUE
         FBAR=FBAR/RN
         FRMS=SQRT(FRMS/RN)
         IP=0
         DO 200 I=1,80
           IP=IP+1
           IF(MESAGE(I).EQ.DOLLAR) GO TO 210
200      CONTINUE
210      CONTINUE
         IP=IP-1
         IP=MAX(IP,1)
         WRITE(6,300)(MESAGE(I),I=1,IP)
300      FORMAT(1H ,80A1)
         WRITE(6,400)FMAX,FMIN,FBAR,FRMS,N
400      FORMAT(' FMX,MN,BR,RMS=',4E12.4, 'NUM PTS=',I6)
         IF(IFMAX.GT.0)
     *       WRITE(6,500)(F(I),I=1,IFMAX)
500      FORMAT(1H ,5E15.5)
       RETURN
       END
