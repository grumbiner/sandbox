CFPP$ NOCONCUR R
      SUBROUTINE GFT 192
C
      COMMON /COM 192 /ITEST,JUMP,IFAX(20),TRIGS( 192 ,2)
C
CCC    DATA ITEST/0/
CCC     IF (ITEST.EQ.0)  THEN
            ITEST=1
            JUMP = 192 +3
C           CALL SET777 (TRIGS, IFAX,  192 )
            CALL FFTFAX ( 192 ,IFAX,TRIGS)
            IF (IFAX(1) .EQ. -99)  PRINT 120
            IF (IFAX(1) .EQ. -99)  STOP
120         FORMAT (' ERROR IN GFT 192 .   192  NOT FACTORABLE. ')
            PRINT 140, JUMP
140         FORMAT (' FFTFAX CALLED IN GFT 192 .  LONF =  192  ',
     X                                          ' JUMP =', I5 )
CCC   ENDIF
C
      RETURN
      END
