      SUBROUTINE PRINTIMS ( SCVH, MSKSCVH )
C
      REAL      SCVH    (1024,1024)
      INTEGER   MSKSCVH (1024,1024)
C
      DIMENSION ILABEL(64), ILABEL1(64)
C
      DO 10 I=1,64
        ILABEL(I) = I*16
   10 CONTINUE                                                                
C
      WRITE(6,20)                                                          
  20  FORMAT (/' LAND-SEA MASK FOR NESDIS-IMS SNOW-ICE ANAL'/
     &         ' ','   ( LAND=1, SEA/LAKE=0 )')               
C                                                                           
      WRITE(6,*) 
C                                                                 
C   NEXT 64 COLUMNS
      DO 30 J=1024,16,-16
        WRITE(6,40) J, (MSKSCVH(I,J), I=16,1024,16)
  30  CONTINUE
  40  FORMAT( ' ', ' J=', I4, 1X, 64I1)
C
      WRITE(6,50)
  50  FORMAT (//' NESDIS-IMS SNOW-ICE ANAL: '/
     &  ' ',' (ICE-FREE SEA=0, SNOW-FREE LAND=1, ICE=2, SNOW=3)'/) 
C       
C   NEXT 72 COLUMNS                     
      WRITE(6,*)
C
      DO 60 J=1024,16,-16
        WRITE(6,40) J, (MSKSCVH(I,J)+NINT(SCVH(I,J))*2, I=16,1024,16) 
  60  CONTINUE                                                            
C                                                                              
      RETURN
      END
