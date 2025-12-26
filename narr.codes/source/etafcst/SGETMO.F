      SUBROUTINE SGETMO(A,NHRZ1,NHRZ2,LM1,ACOL_T,LM2)
C------------------------------------------------------
                   R E A L
     1 A(NHRZ1,LM1),ACOL_T(LM1,NHRZ1)
C------------------------------------------------------
      DO N=1,NHRZ1
      DO L=1,LM1
        ACOL_T(L,N)=A(N,L)
      ENDDO
      ENDDO
C
      RETURN
      END
