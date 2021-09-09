      SUBROUTINE SMFUN(II,MP,KFF) 
      COMMON W(46,50) 
      REAL F(46,3,2)
      DATA J,JF,JFP,JFPP/0,1,2,3/ 
      IF (J.LE.0) CALL ENDIT
      IF (J.LE.1) THEN
        DO 10 MZ=1,MP 
 10     F(MZ,JF,II)=W(MZ,KFF) 
       ELSEIF (J.LE.2) THEN 
        DO 20 MZ=1,MP 
        W(MZ,KFF)=0.5*(W(MZ,KFF)+F(MZ,JFP,II))
 20     F(MZ,JF,II)=0.5*(W(MZ,KFF)+F(MZ,JFP,II))
        ELSE
          DO 30 MZ=1,MP 
          W(MZ,KFF)=.75*W(MZ,KFF)+.5*F(MZ,JFP,II)-.25*F(MZ,JFPP,II) 
 30       F(MZ,JF,II)=.75*W(MZ,KFF)+.5*F(MZ,JFP,II)-.25*F(MZ,JFPP,II) 
      ENDIF 
      RETURN
      ENTRY SMSET 
      J=0 
      RETURN
      ENTRY SMSTEP
      J=J+1 
C     CALL THIS PRIOR TO ENTERING THE CURRENT STEP FIELDS 
      JX=JFPP 
      JFPP=JFP
      JFP=JF
      JF=JX 
      RETURN
      END 
