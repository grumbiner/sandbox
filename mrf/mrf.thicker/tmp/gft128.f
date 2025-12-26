CFPP$ NOCONCUR R
      SUBROUTINE GFT 128
C
      COMMON /COM 128 /ITEST,JUMP,IFAX(20),TRIGS( 128 ,2)
C
CCC    DATA ITEST/0/
CCC     IF (ITEST.EQ.0)  THEN
            ITEST=1
            JUMP = 128 +3
C           CALL SET777 (TRIGS, IFAX,  128 )
            CALL FFTFAX ( 128 ,IFAX,TRIGS)
            IF (IFAX(1) .EQ. -99)  PRINT 120
            IF (IFAX(1) .EQ. -99)  STOP
120         FORMAT (' ERROR IN GFT 128 .   128  NOT FACTORABLE. ')
            PRINT 140, JUMP
140         FORMAT (' FFTFAX CALLED IN GFT 128 .  LONF =  128  ',
     X                                          ' JUMP =', I5 )
CCC   ENDIF
C
      RETURN
      END
