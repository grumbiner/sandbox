       SUBROUTINE PLGFLD(MPREF,JSUB,MZERO,LEN,FLD,NIX)
C      ADDITIONAL SUBS FOR INTEGRATION OF MODELS  8/17/79 
C      PLUG A SUBFIELD IN THE SHORT TS MODEL
       REAL FLD(46) 
      COMMON W(46,50) 
       COMMON/LARGE/NFLD,JFLD(12,2) 
       COMMON/CONST/M,MM,MP,MPP,N,IDUM(42)
       NIX=0
       IF (MP.NE.MPREF) THEN
         NIX=1
        ELSE
         MZ=MZERO 
         JP=JFLD(JSUB,2)
         DO 20   L=1,LEN
         W(MZ,JP)=FLD(L)
   20    MZ=MZ+1
      ENDIF 
       RETURN 
       END
