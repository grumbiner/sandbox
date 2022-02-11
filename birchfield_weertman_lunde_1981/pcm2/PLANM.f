      SUBROUTINE PLANM(NS,NSZ)
C     ANNUAL MEAN PLANETARY ALBEDO  10/28/80
C     MEAN SWIN,SWOUT,LWOUT  2/4/81 
C     TEST TO SEE IF AVERAGING SHOULD BE DONE THIS TIME MOVED TO CALL IN
C       TWOB 10/3/83. BG
      COMMON/CONST/M,MM,MP,MPP,N,DUMMM(42)
      COMMON/MINOR/NMINOR,JFM,JRSN,JPS,JSP,JSPP,JCZB,JCZBP,JQ1P,JQ2P
     1,JEXHT,JFSM(3,2),JTEQ,JXIEQ,JRSNL,JRSOUT,JRLOUT,
     2 JDHLZ,JHELV,JALV,JALBL,JALBO 
      COMMON W(46,50) 
      COMMON/CFCOM/NCG,NSC
      REAL SWIN(3),SWOUT(3),PA(3),PAAM(3) 
      REAL SWINAM(3),SWOUTAM(3),LWOUT(3),LWOUTAM(3) 

      MMOD(I,J)=MOD(J+MOD(I,J),J) 
C     AVERAGE SWIN,SWOUT WITH SPHERE TO SAVE TIME 
      CALL SPHERE(JSPP,SWIN(1),SWIN(2),SWIN(3)) 
      CALL SPHERE(JRSOUT,SWOUT(1),SWOUT(2),SWOUT(3))
      CALL SPHERE(JRLOUT,LWOUT(1),LWOUT(2),LWOUT(3))
      DO 10 L=1,3 
      PA(L)=SWOUT(L)/SWIN(L)
      PAAM(L)=PAAM(L)+PA(L) 
      SWINAM(L)=SWINAM(L)+SWIN(L) 
      SWOUTAM(L)=SWOUTAM(L)+SWOUT(L)
      LWOUTAM(L)=LWOUTAM(L)+LWOUT(L)
 10   CONTINUE
      IF(MMOD(NS-NSZ,NCG).NE.0)RETURN 
      DO 30 L=1,3 
      SWINAM(L)=SWINAM(L)/N 
      SWOUTAM(L)=SWOUTAM(L)/N 
      LWOUTAM(L)=LWOUTAM(L)/N 
 30   PAAM(L)=PAAM(L)/N 
      NSM=NS-1
      WRITE(6,40) NSM,PAAM,SWINAM,SWOUTAM,LWOUTAM 
 40   FORMAT(' MEAN ANNUAL ',I5,
     1' PA G=',G15.8,' N=',G15.8,' S=',G15.8/16X, 
     1' SWIN G=',G15.8,' N=',G15.8,' S=',G15.8/15X, 
     1' SWOUT G=',G15.8,' N=',G15.8,' S=',G15.8/15X,
     1' LWOUT G=',G15.8,' N=',G15.8,' S=',G15.8)
      ENTRY PMSET 
      DO 50 L=1,3 
      PAAM(L)=0.0 
      SWINAM(L)=0.0 
      SWOUTAM(L)=0.0
 50   LWOUTAM(L)=0.0
      RETURN
      END 