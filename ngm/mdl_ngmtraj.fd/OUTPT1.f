      SUBROUTINE OUTPT1(A,JLVL,JTYPE)                                   
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    OUTPT1      PRINTS DATA                                
C   PRGMMR: R. M. REAP       ORG: W/OSD21          DATE: 96-02-29       
C                                                                       
C ABSTRACT:  PREPARES DATA AND PRINTS SAMPLE VALUES               
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   66-09-01  R. M. REAP                                                
C   74-02-01  CONVERT TO IBM SYSTEM                                     
C   92-09-25  CONVERT TO FORTRAN 77                                     
C   95-11-01  CONVERT TO CRAY                                           
C   96-02-29  ELIMINATE GRDPRT CALL
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR TDL STANDARDS                                         
C     PROGRAM OUTPT1                                                    
C        FEB 1996     R. M. REAP         TDL          CRAY              
C        PURPOSE                                                        
C        PREPARES DATA FIELD AND PRINTS SAMPLE VALUES          
C                                                                       
C        LIBRARY:                                                       
C              COMMON,W3LIB,TDLLIB                                       
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      DIMENSION DATA(26,33),A(26,33)	           
C     ***************************************************************** 
      DO 5 I=1,26                                                       
      DO 5 J=1,33                                                       
      IF(JTYPE.EQ.48) GO TO 3  
      IF(JTYPE.EQ.49) GO TO 3  
      IF(JTYPE.EQ.5) DATA(I,J)=A(I,J)*10.0  
      GO TO 5
  3   DATA(I,J)=A(I,J)*1000.0                                           
  5   CONTINUE                                                          
      DO 8 I=1,13  
      II=14-I
      WRITE(6,6) (DATA(II,J),J=8,17)
  6   FORMAT(3X,10F7.2)
  8   CONTINUE  
      PRINT 10,JLVL,JTYPE                                               
 10   FORMAT(///,20X,'LEVEL= ',I3,' WIND COMPONENT= ',I2)           
      RETURN                                                            
      END                                                               
