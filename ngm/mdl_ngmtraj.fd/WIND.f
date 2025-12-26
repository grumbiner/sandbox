      SUBROUTINE WIND(A)                                                
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    WIND        CONVERTS U,V TO GRID INT/HR                
C   PRGMMR: R. M. REAP       ORG: W/OSD21           DATE: 95-11-01      
C                                                                       
C ABSTRACT:  CONVERTS MODEL WINDS FROM KNOTS TO GRID U,V WINDS          
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   66-08-01  R. M. REAP                                                
C   74-02-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-01  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM WIND                                                      
C        NOV 1995     R. M. REAP         MDL         CRAY               
C        PURPOSE                                                        
C        CONVERTS NCEP U AND V WINDS FROM KNOTS TO MDL GRID U AND V WIND
C        IN GRID INTERVALS PER HOUR.  F3 INCLUDES MAP SCALE FACTOR.     
C        POLE IS AT I=26, J=24                                          
C ATTRIBUTES:                                                           
C  LANGUAGE: FORTRAN 90                                                 
C  MACHINE:  CRAY                                                       
C$$$                                                                    
C                                                                       
      DIMENSION A(26,33)                                                
C     ***************************************************************** 
      DO 20 I=1,26                                                      
      XI=(I-26)*(I-26)                                                  
      DO 20 J=1,33                                                      
      XJ=(J-24)*(J-24)                                                  
      F2=XI+XJ                                                          
      F3=0.9076E-2/(1.+(973.71-F2)/(973.71+F2))                         
      A(I,J)=A(I,J)*F3                                                  
 20   CONTINUE                                                          
      RETURN                                                            
      END                                                               
