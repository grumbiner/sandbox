       SUBROUTINE PRESET(VAL,FWA,LEN) 
       REAL FWA(*)
       V=VAL
       DO 10 I=1,LEN
 10    FWA(I)=V 
       RETURN 
       END
