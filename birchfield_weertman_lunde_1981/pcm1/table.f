       FUNCTION TABLE(X,F,IMAX,XMIN,DELX) 
       COMMON/NOTE/NT(3)
       REAL F(IMAX) 
C      LINEARY INTERPOLATES A TABULATED FUNCTION F
       RI = 1. + (X - XMIN)/DELX
       IA=RI
       IF ((IA.LT.1).OR.(RI.GT.IMAX)) NT(1)=NT(1)+1 
       IA = MIN0(MAX0(IA,1),IMAX-1) 
       WB = RI-IA 
       TABLE = (1.-WB) * F(IA) + WB * F(IA+1) 
       RETURN 
       END
