C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
      SUBROUTINE HPBLCAL(I,J,LM,LMHK,LPBL,HPBL,Q2,Z)
C **********************************************************************
C *      PBL HEIGHT (HPBL) CALCULATION BASED ON LEVEL 2.5 MIXING       *
C   03-01-23 - M EK LIFTED HPBL COMPUTATION FROM ETA MODEL
C **********************************************************************
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(EPSQ2=0.2,FH=1.01)
      
      REAL Q2(LM),Z(LM+1)
      LMHP=LMHK+1
C--------------FIND THE HEIGHT OF THE PBL-------------------------------
      LPBL=LMHK
          DO 100 IVI=1,LMHK
      L=LMHK-IVI
      IF(Q2(L).LE.EPSQ2*FH)THEN
CVVVVVVVVVVVVVV NOT NECESSARY IF DRIVEN BY TURBL VVVVVVVVVVVVVVVVVVVVVVV
C        Q2(L)=EPSQ2
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
        LPBL=L
        GO TO 110
      ENDIF
 100  CONTINUE
      LPBL=1
C--------------THE DEPTH OF THE PBL------------------------------------
 110  HPBL=Z(LPBL)-Z(LMHP)
      if(i.eq.550.and.j.eq.144)print*
     +, 'Debug:sample HPBLCAL in HPBLCAL'
     +,I,J,LM,LMHK,LPBL,HPBL,(Q2(L),l=1,lm)
     +,(Z(L),l=1,lm+1) 
C-----------------------------------------------------------------------
      RETURN
      END
