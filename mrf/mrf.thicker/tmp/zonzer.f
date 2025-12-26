      SUBROUTINE ZONZER(FLN)
        DIMENSION FLN(2, 2016 )
        INC= 63
        I=1
        DO 1 LL=1, 63
C       PRINT 100,I,INC
C100    FORMAT(1H ,'I INC ',I4,2X,I4)
        FLN(2,I)=0.
        I=I+INC
        INC=INC-1
1       CONTINUE
        RETURN
        END
