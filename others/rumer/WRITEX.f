      SUBROUTINE WRITEX ( TT )
      INCLUDE "glgrid.inc"

      DIMENSION U(NX,NY), V(NX,NY), P(NX,NY), JO(NX,NY), JOFN(NX,NY),
     *       CN(NX,NY), DIV(NX,NY), TICE(NX,NY), WIN(3), WAN(3), TEMP(3)
      COMMON / SPEEDY / U , V
      COMMON / JOINT / JO , JOFN
      COMMON / DIVER / DIV
      COMMON / THICK / TICE
      COMMON / MASS / CN
      COMMON / PRESUR / P
      COMMON / GENERAL / N , M , NM1 , MM1
      COMMON / WIND / WIN, WAN, TEMP
      COMMON / UVCONST / GAMMA , EPS , RHOICE , FK2MIN , FK2MAX ,
     *                   RRDLL , RRDLL4
      COMMON / WRITES / IDISK, KOUNT, ICONT, OVERWRT, YIDO
      COMMON / WINDEX / PRINTM, WINDINT, DISKINT,  IRATIO, IW,
     *                  IPR, MELT
      COMMON / CONSTAN / DL, DT, DUM2
      COMMON / PCONSTS /  PST, T0, DIVLIM, K
      IHOUR = TT / 3600
      IMIN = ( TT - IHOUR * 3600 ) / 60
      ISEC = TT - IHOUR * 3600 - IMIN * 60
      WRITE( 6 , 333 ) IHOUR, IMIN, ISEC, ((WIN(I), WAN(I)),I = 1,3), DT
      WRITE ( 6 , 400 )
      DO 100 I = 1 , NM1
100   WRITE ( 6 , 500 ) ( P(I,J), J = 1 , MM1 )
      WRITE ( 6 , 430 )
      DO 140 I = 1 , NM1
140   WRITE ( 6 , 530 ) ( DIV(I,J), J = 1 , MM1 )
C
      CALL PRINTER ( IHOUR, IMIN, ISEC, 1, 4 )
C
      IF ( IDISK .NE. 3HYES ) GO TO 3000
      IF (TT .NE. 0.) GO TO 160
      WRITE ( 9 , 300 ) N, M, NM1, MM1
      WRITE ( 9 , 310 ) DT,DL,YIDO,FK2MIN,FK2MAX,PST,T0,DIVLIM,WINDINT,K
      WRITE ( 9 , 300 ) ( ( JO(I,J), J = 1 , M ) , I = 1 , N )
      WRITE ( 9 , 300 ) ( ( JOFN ( I , J ), J = 1 , MM1 ), I = 1 , NM1 )
160   IF ( ICONT .EQ. 3HYES .AND. OVERWRT .NE. 3HYES .AND.
     *     KOUNT .EQ. 0 ) GO TO  3000
      IF ( KOUNT / IRATIO * IRATIO .NE. KOUNT ) GO TO 3000
      WRITE ( 9 , 520 ) TT, (( WIN(I), WAN(I)), I = 1, 3 )
      WRITE ( 9 , 550 ) ( ( U(I,J), J = 1 , M ) , I = 1 , N )
      WRITE ( 9 , 550 ) ( ( V(I,J), J = 1 , M ) , I = 1 , N )
      WRITE ( 9 , 550 ) ( ( CN(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
      WRITE ( 9 , 570 ) ( ( P(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
      WRITE ( 9 , 550 ) ( ( TICE(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
      WRITE ( 9 , 560 ) ( ( DIV(I,J), J = 1 , MM1 ) , I = 1 , NM1 )
3000  KOUNT = KOUNT + 1
C
      CALL CHECK
C
300   FORMAT ( 30I4 )
310   FORMAT ( 9F10.1 , I2 )
333   FORMAT ( 1H1, / , 15X, I4,' HOURS',I4,' MINUTES',I4,' SECONDS',
     *    6X,'WIND AT TOLEDO    =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.'/
     *   55X,'WIND AT CLEVELAND =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.'/
     *   55X,'WIND AT BUFFALO   =',F5.1,1X,'M/SEC',2X,F6.1,1X,'DEG.'/
     *   55X,'DT =',F6.1,' SEC.',// )
400   FORMAT ( // , 10X , ' P ( I , J ) ( NEWTON/M/M ) ' / )
430   FORMAT ( //,10X,'DIVERGENCE ( I , J ) ( KG/M/M - SEC ) ' / )
500   FORMAT ( 1X , 21F5.0 )
510   FORMAT ( 1X , 21F5.3 )
520   FORMAT ( F8.0, 6F8.2 )
530   FORMAT ( 1X , 21E6.0 )
540   FORMAT ( / 10X, 20F5.2 )
550   FORMAT ( 15F8.3 )
560   FORMAT ( 15E8.1 )
570   FORMAT ( 15F8.0 )
      RETURN
      END
