      SUBROUTINE DELLNP(Q,DPDPHS,DPDTOP,DPDLA)
C
      PARAMETER (JCAP= 62 )
      PARAMETER (LNEP=(JCAP+2)*(JCAP+3)/2)
C
C INPUT Q IS IN IBM TRIANG. ORDER
C OUTPUT  IS IN IBM TRIANG. ORDER
C
      DIMENSION         Q( 4033 )
      DIMENSION    DPDPHS( 4033 )
      DIMENSION    DPDTOP(2, 63 )
      DIMENSION     DPDLA( 4033 )
      DIMENSION         E(LNEP)
C
      SAVE E, IFIRST
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
        IE=JE(N,L)+1
      E(IE)=0.
220   CONTINUE
      DO 260 L=  0,JCAP
      DO 240 N=L+1,JCAP+1
      RN=N
      RL=L
      A=(RN*RN-RL*RL)/(4.*RN*RN-1.)
        IE=JE(N,L)+1
      E(IE)=SQRT(A)
240   CONTINUE
260   CONTINUE
      IFIRST=0
280   CONTINUE
C
      DO 340 L=0,JCAP
          RL=L
      DO 320 N=L,JCAP
        ICR=JC(N,L)+1
        ICI=JC(N,L)+2
C     DPDLA(L,N)= I*L*Q(L,N)
C
      DPDLA(ICI)= RL*Q(ICR)
      DPDLA(ICR)=-RL*Q(ICI)
320   CONTINUE
340   CONTINUE
C
      DO 440 L=0,JCAP-1
      DO 420 N=L,JCAP-1
         IE=JE(N+1,L)+1
        ICR=JC(N,L)+1
        ICI=JC(N,L)+2
         RN=N
      DPDPHS(ICR)=(RN+2.)*E(IE)*Q(ICR+2)
      DPDPHS(ICI)=(RN+2.)*E(IE)*Q(ICI+2)
420   CONTINUE
440   CONTINUE
C
             N=  JCAP
      DO 540 L=0,JCAP
        ICR=JC(N,L)+1
        ICI=JC(N,L)+2
      DPDPHS(ICR)=0.0
      DPDPHS(ICI)=0.0
540   CONTINUE
C
      DO 640 L=  0,JCAP-1
      DO 620 N=L+1,JCAP
         IE=JE(N,L)+1
        ICR=JC(N,L)+1
        ICI=JC(N,L)+2
         RN=N
      DPDPHS(ICR)=DPDPHS(ICR)+(1.-RN)*E(IE)*Q(ICR-2)
      DPDPHS(ICI)=DPDPHS(ICI)+(1.-RN)*E(IE)*Q(ICI-2)
620   CONTINUE
640   CONTINUE
C
          N=JCAP+1
         RN=N
      DO 740 L=0,JCAP
         IE=JE(N,L)+1
        ICR=JC(N,L)+1
        ICI=JC(N,L)+2
      DPDTOP(1,L+1)=(1.-RN)*E(IE)*Q(ICR-2)
      DPDTOP(2,L+1)=(1.-RN)*E(IE)*Q(ICI-2)
740   CONTINUE
C
      AA=1./ 6.3712E+6
      DO 820 J=1, 4032
       DPDLA(J)= DPDLA(J)*AA
      DPDPHS(J)=DPDPHS(J)*AA
820   CONTINUE
C
      DO 840 J=1, 63
      DPDTOP(1,J)=DPDTOP(1,J)*AA
      DPDTOP(2,J)=DPDTOP(2,J)*AA
840   CONTINUE
C
      RETURN
      END
