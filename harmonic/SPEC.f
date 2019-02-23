       SUBROUTINE SPEC(X, LEN, LAG, DELTA, IU, IER) 
  
C      X     THE INPUT ARRAY TO FIND THE POWER SPECTRUM OF. 
C      LEN   THE NUMBER OF DATA POINTS
C      LAG   IS THE NUMBER OF FREQUENCIES THE DATA IS TO BE DIVIDED INTO
C              IT IS FOUND BY ????????????????????????????? 
C      DELTA THE INTERVAL BETWEEN POINTS
C      IU    THE UNIT FOR I/O 
C      IER   ERROR MESSAGE NUMBER.  IT IS 0 WHEN THERE IS NO ERROR
C      Robert Grumbine 1985
  
       REAL X(LEN), XIND(2) 
       INTEGER IND(6)
       REAL y(20000)
 
C      THE NUMBER PRECEDING EACH ARRAY IS ITS ACTUAL LENGTH 
C      THE VARIABLES IN PSCOM DO NOT NEED TO BE IN A COMMON BLOCK 
       REAL XNA, PS(1001), XNB, FREQ(1001), 
     1  XNC, ACVZ, ACV(2000), XYMV(6) 
 
       CHARACTER*60 fname
       LOGICAL yes
 
       DATA MAXLAG/1000/
  
       IRN(Z)=INT(Z+60.)-60 
       IER=0
       IF(LAG.LE.MAXLAG.AND.LAG.GT.2.AND.LAG.LT.LEN)GO TO 10
       IER=1
       RETURN 
  
 10    XNA = LAG+1
       XNB = LAG+1
       XNC = LAG+1
       LAGP= LAG+1
  
C      IND   SELECTS OPTIONS IN THE IMSL ROUTINES 
       IND(1) = 0 
       IND(2) = LEN 
       IND(3) = 0 
       IND(4) = LAG 
       IND(5) = 0 
       IND(6) = 0 
  
C      XIND IS PASSED TO FTFREQ.  XIND(1)=TIME STEP  XIND(2)
C             IS USED IN PREWHITENING (IF THAT IS CHOSEN) AND 
C             MUST BE IN THE RANGE (-1.0 TO 1.0) EXCLUSIVE. 
       XIND(1)=DELTA
       XIND(2)=.5 
  
C      DTRND  WAS WRITTEN BY ALBERT LUNDE TO DO SIMPLE LINEAR 
C               DETRENDING. 
C      A0, A1, AND XMEAN ARE NOT USED HERE, MAY DROP FROM CALL
C      Replaced with a call to detrnd, written by BG, 1-20-88.
       DO 3000 i = 1, len
         y(i) = FLOAT(i)
 3000  CONTINUE 
       CALL detrnd(y, x, len, bnot, bwon, avg, .TRUE.)
  
C      FTFREQ IS THE IMSL ROUTINE WHICH COMPUTES THE SPECTRUM 
C             IT ALSO REQUIRES IMSL ROUTINE FTAUTO
       CALL FTFREQ(X,IND,XIND,XYMV,ACV,FREQ,PS, 
     1  DUM,DUM,DUM,DUM,DUM,IER)
 
      IF (IER.EQ.0) GO TO 21 
      IF (IER.NE.67) GO TO 22
      IER=0
      GO TO 21 
 22   CONTINUE 
      IER=IER*100
      RETURN 
  
 21   ACVZ=XYMV(2) 
 
      I=LAGP+1 
 510  XNA = I-1
      XNB = I-1
      LAGP= I-1
      IF(IU.EQ.0) GO TO 100

      PRINT *,'What would you like to call the output file?'
      READ (*,9001) fname
      PRINT *,'Do you want unformatted output?'
      IF (yes(.TRUE.)) THEN
        OPEN (IU, FILE=fname, FORM='UNFORMATTED', STATUS='NEW')
       ELSE
        OPEN (IU, FILE=fname, FORM='FORMATTED', STATUS='NEW')
      ENDIF

 400  FORMAT(1X,3E15.8)
      WRITE (IU, 400) FREQ(1), PS(1), FREQ(1) 
      DO 402 I=2, LAGP 
        PER = 1./FREQ(I) 
        WRITE (IU, 400)  FREQ(I), PS(I), PER
 402  CONTINUE 
 
 9001 FORMAT(A60)
       
 100  RETURN 
      END
