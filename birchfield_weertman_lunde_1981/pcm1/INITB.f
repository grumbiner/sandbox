      SUBROUTINE INITB
C     MODIFIED FOR FTN5 MACHINE INDEPENDANCE 8/10/83.  BOB GRUMBINE 
C     READS IN INITIAL VALUES 
C     MODIFIED TO READ IN FROM 4 DEGREE SPACING 8/17/83.
C     FINE GRID NOT USED NOW.  8-24-83
      COMMON/CONST/M,MM,MP,MPP,N, 
     1 DELPHI,DELT,PI,DUM(39) 
      COMMON/LARGE/NFLD,JFLD(12,2)
      COMMON W(46,50) 
C     WSPACE REMOVED 8/30/83, NO LONGER USED. 
      CHARACTER FORIN*10,FOR*10,FORD*10 
      DOUBLE PRECISION ICFLG,ISFLG,NUM
      DATA FORD/'(4G20.12) ' /
C     SF TO GET THE NTH DIGIT FROM NUM. 
      IDIGIT(L,NUM)=DINT(10.*((NUM/1.D1**L)-DINT(NUM/1.D1**L))) 
 10   FORMAT(1X,8G15.8) 
 11   FORMAT(1X,A10)
 15   FORMAT(1X,D35.26) 
      READ (*,11,END=9000)FORIN 
 9000 WRITE(6,11)FORIN
      IF (FORIN.NE.'          ' ) THEN
       FOR=FORIN
       ELSE 
C     DEFAULT 
       FOR=FORD 
      ENDIF 
C     READ THE DOUBLE PRECISION NUMBERS THAT INDICATE WHICH 
C     FIELDS TO READ AS A SINGLE CONSTANT OR TO SKIP
      READ (*,20,END=9001 )ICFLG,ISFLG
 9001 WRITE(6,15)ICFLG,ISFLG
 20   FORMAT(BZ,D26.0)
      DO 30 L=1,NFLD
      JJSUB2=JFLD(L,2)
      IF (IDIGIT(L,ISFLG).NE.0) GO TO 30
      IF (IDIGIT(L,ICFLG).NE.0) GO TO 35
      READ (*,987,END=988)(W(I,JJSUB2),I=1,46)
 987  FORMAT (1X,4G20.12) 
 988  WRITE(6,10) (W(K,JJSUB2),K=1,46)
      GO TO 30
 35   READ (*,FOR,END=9002 )W(1,JJSUB2) 
 9002 WRITE(6,10)W(1,JJSUB2)
      CALL PRESET(W(1,JJSUB2),W(2,JJSUB2),MP) 
 30   CONTINUE
      RETURN
      END 
