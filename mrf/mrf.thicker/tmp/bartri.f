      SUBROUTINE BARTRI (F, G, FGBAR)
      DIMENSION F( 4033 ),G( 4033 )
C
      JOFF(N,L)=( 63 )*( 64 )-( 63 -L)*( 64 -L)+2*(N-L)
C
      L=0
      FGBAR = 0.
      DO 1 N=0,  62
      FGBAR = FGBAR + F(JOFF(N,L)+1)*G(JOFF(N,L)+1)
1     CONTINUE
      DO 11 N=0,  62
      FGBAR = FGBAR + F(JOFF(N,L)+2)*G(JOFF(N,L)+2)
11    CONTINUE
      FGBAR=FGBAR*0.5
      DO 3 L=1,  62
      DO 2 N=L,  62
      FGBAR = FGBAR + F(JOFF(N,L)+1)*G(JOFF(N,L)+1)
2     CONTINUE
      DO 22 N=L,  62
      FGBAR = FGBAR + F(JOFF(N,L)+2)*G(JOFF(N,L)+2)
22    CONTINUE
3     CONTINUE
      FGBAR = SQRT(FGBAR)
      RETURN
      END
