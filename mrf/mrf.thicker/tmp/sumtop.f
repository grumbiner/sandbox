CFPP$ NOCONCUR R
      SUBROUTINE SUMTOP(AP,TOP,QVV,KLIL,LEN0,LENH)
CC
      PARAMETER ( LEV2P1 = 2* 28 +1 )
CC
      DIMENSION     AP(2,0:LEN0,KLIL)
CC
      DIMENSION    TOP(2,0: 62 ,KLIL)
      DIMENSION     EV(2,0: 62 ,LEV2P1)
      DIMENSION     OD(2,0: 62 ,LEV2P1)
CC
      DIMENSION   QTOP(2,0: 62 )
CC
      DIMENSION    QVV( 4158 )
CC
CC
CC
      LEN=2* 63
      J=LEN+1
      DO 10 L=0, 62
      QTOP(1,L) = QVV(J)
      QTOP(2,L) = QVV(J+1)
      J=LEN+J
      LEN=LEN-2
   10 CONTINUE
CC
CC
CC
      DO 24 K=1,KLIL
      DO 22 L=0, 62
            EV(1,L,K) = 0.0
            EV(2,L,K) = 0.0
            OD(1,L,K) = 0.0
            OD(2,L,K) = 0.0
   22 CONTINUE
   24 CONTINUE
CC
CC
CC
CC    ODD
      DO 50 L=0, 63 ,2
CC
CC    REAL
      DO 30 K=1,KLIL
            OD(1,L,K) = TOP(1,L,K) * QTOP(1,L)
   30 CONTINUE
CC
CC    IMAGINARY
      DO 40 K=1,KLIL
            OD(2,L,K) = TOP(2,L,K) * QTOP(2,L)
   40 CONTINUE
CC
   50 CONTINUE
CC
CC
CC
CC    EVEN
      DO 150 L=1, 62 ,2
CC
CC    REAL
      DO 130 K=1,KLIL
            EV(1,L,K) = TOP(1,L,K) * QTOP(1,L)
  130 CONTINUE
CC
CC    IMAGINARY
      DO 140 K=1,KLIL
            EV(2,L,K) = TOP(2,L,K) * QTOP(2,L)
  140 CONTINUE
CC
  150 CONTINUE
CC
CC
CC
      DO 250 K=1,KLIL
      DO 240 L=0, 62
            AP(1,L,K) = AP(1,L,K) + EV(1,L,K) + OD(1,L,K)
            AP(2,L,K) = AP(2,L,K) + EV(2,L,K) + OD(2,L,K)
CC
            AP(1,L+LENH,K) = AP(1,L+LENH,K) + EV(1,L,K) - OD(1,L,K)
            AP(2,L+LENH,K) = AP(2,L+LENH,K) + EV(2,L,K) - OD(2,L,K)
  240 CONTINUE
  250 CONTINUE
CC
      RETURN
      END
