      SUBROUTINE DZUVLE(D,Z,U,V,UTOP,VTOP)
      PARAMETER (JCAP= 62 )
      PARAMETER (LNT22=(JCAP+1)*(JCAP+2)+1)
      PARAMETER (LNEP=(JCAP+2)*(JCAP+3)/2)
      DIMENSION  D(LNT22, 28 ),Z(LNT22, 28 )
      DIMENSION  U(LNT22, 28 ),V(LNT22, 28 )
      DIMENSION  UTOP(2,JCAP+1, 28 )
      DIMENSION  VTOP(2,JCAP+1, 28 )
      DIMENSION  E(LNEP)
C
      SAVE E, IFIRST
C
C   ARRAY E =EPS/N
C   ARRAY E =EPS/N
C            EPS/N=0. FOR N=L
C   ARRAY E =EPS/N
C   ARRAY E =EPS/N
C
      JE(N,L) =((JCAP+2)*(JCAP+3)-(JCAP+2-L)*(JCAP+3-L))/2+N-L
C
      JC(N,L) = (JCAP+1)*(JCAP+2)-(JCAP+1-L)*(JCAP+2-L)+2*(N-L)
C
      DATA IFIRST/1/
      IF(IFIRST.NE.1)GO TO 280
C
      DO 220 L=0,JCAP
             N=L
      E(JE(N,L)+1)=0.
220   CONTINUE
      DO 260 L=  0,JCAP
      DO 240 N=L+1,JCAP+1
      RN=N
      RL=L
      A=(RN*RN-RL*RL)/(4.*RN*RN-1.)
      E(JE(N,L)+1)=SQRT(A) / RN
240   CONTINUE
260   CONTINUE
      IFIRST=0
280   CONTINUE
C
CMIC$ DO ALL
CMIC$1 SHARED(D,Z,U,V,UTOP,VTOP,E)
CMIC$1 PRIVATE(K,L,N,RL,RN,J)
      DO 10000 K=1, 28
             L=0
      DO 320 N=0,JCAP
C     U(L,N)=-I*L*D(L,N)/(N*(N+1))
C
      U(JC(N,L)+1,K)=0.0
      U(JC(N,L)+2,K)=0.0
C
      V(JC(N,L)+1,K)=0.0
      V(JC(N,L)+2,K)=0.0
320   CONTINUE
C
      DO 440 L=1,JCAP
          RL=L
      DO 420 N=L,JCAP
          RN=N
C     U(L,N)=-I*L*D(L,N)/(N*(N+1))
C
      U(JC(N,L)+2,K)=-RL*D(JC(N,L)+1,K)/(RN*(RN+1.))
      U(JC(N,L)+1,K)= RL*D(JC(N,L)+2,K)/(RN*(RN+1.))
C
      V(JC(N,L)+2,K)=-RL*Z(JC(N,L)+1,K)/(RN*(RN+1.))
      V(JC(N,L)+1,K)= RL*Z(JC(N,L)+2,K)/(RN*(RN+1.))
420   CONTINUE
440   CONTINUE
C
      DO 540 L=  0,JCAP-1
      DO 520 N=L+1,JCAP
      U(JC(N,L)+1,K)=U(JC(N,L)+1,K)-E(JE(N,L)+1)*Z(JC(N,L)-1,K)
      U(JC(N,L)+2,K)=U(JC(N,L)+2,K)-E(JE(N,L)+1)*Z(JC(N,L)  ,K)
C
      V(JC(N,L)+1,K)=V(JC(N,L)+1,K)+E(JE(N,L)+1)*D(JC(N,L)-1,K)
      V(JC(N,L)+2,K)=V(JC(N,L)+2,K)+E(JE(N,L)+1)*D(JC(N,L)  ,K)
520   CONTINUE
540   CONTINUE
C
      DO 640 L=0,JCAP-1
      DO 620 N=L,JCAP-1
      U(JC(N,L)+1,K)=U(JC(N,L)+1,K)+E(JE(N+1,L)+1)*Z(JC(N,L)+3,K)
      U(JC(N,L)+2,K)=U(JC(N,L)+2,K)+E(JE(N+1,L)+1)*Z(JC(N,L)+4,K)
C
      V(JC(N,L)+1,K)=V(JC(N,L)+1,K)-E(JE(N+1,L)+1)*D(JC(N,L)+3,K)
      V(JC(N,L)+2,K)=V(JC(N,L)+2,K)-E(JE(N+1,L)+1)*D(JC(N,L)+4,K)
620   CONTINUE
640   CONTINUE
C
      N=JCAP+1
      DO 740 L=0,JCAP
      UTOP(1,L+1,K)=-E(JE(N,L)+1)*Z(JC(N,L)-1,K)
      UTOP(2,L+1,K)=-E(JE(N,L)+1)*Z(JC(N,L)  ,K)
C
      VTOP(1,L+1,K)= E(JE(N,L)+1)*D(JC(N,L)-1,K)
      VTOP(2,L+1,K)= E(JE(N,L)+1)*D(JC(N,L)  ,K)
740   CONTINUE
C
      DO 820 J=1, 4032
      U(J,K)=U(J,K)* 6.3712E+6
      V(J,K)=V(J,K)* 6.3712E+6
820   CONTINUE
C
      DO 840 J=1, 63
      UTOP(1,J,K)=UTOP(1,J,K)* 6.3712E+6
      UTOP(2,J,K)=UTOP(2,J,K)* 6.3712E+6
      VTOP(1,J,K)=VTOP(1,J,K)* 6.3712E+6
      VTOP(2,J,K)=VTOP(2,J,K)* 6.3712E+6
840   CONTINUE
10000 CONTINUE
      RETURN
      END
