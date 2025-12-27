      SUBROUTINE RDPE(DK,JTAU,JPP,JII,NFILE)                            
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    RDPE       READS NCEP FIELDS FROM DISK                 
C   PRGMMR: R. M. REAP      ORG: W/OSD21         DATE: 95-11-01         
C                                                                       
C ABSTRACT:  READS 858 WORD RECORDS FROM DISK THAT WERE STORED BY NTRNS  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   68-07-01  R. M. REAP                                                
C   74-02-01  CONVERT TO IBM SYSTEM                                     
C   94-09-14  INCREASE JTAB & JTABX DIMENSIONS TO 340                   
C   95-03-15  INCREASE JTAB & JTABX DIMENSIONS TO 400                   
C   95-11-01  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM RDPE                                                      
C        NOV 1995       R. M. REAP           MDL         CRAY           
C        PURPOSE                                                        
C        READS 858 WORD RECORDS FROM DISK WHICH HAVE BEEN STORED        
C          BY NTRNS                                                      
C        DATA SET USE                                                   
C          FT84 (INPUT)                                                 
C        VARIABLES                                                      
C          DK(858)=ARRAY TO READ RECORD INTO                            
C              JTAU=FORECAST HOUR IN BINARY                             
C               JPP=PRESSURE LEVEL IN CENTIBARS, 4-BIT BCD              
C               JII=TYPE OF DATA, 4-BIT BCD                             
C             NFILE=DISK FILE NUMBER                                    
C         JTAB(M,5)=PRESSURE LEVEL, DATA TYPE, TAU, DISK RECORD NUMBER, 
C                   AND MOS VARIABLE INDICATOR                          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      DIMENSION DK(858),JTAB(400,5),JTABX(25,400)                       
      COMMON/BLOCK3/JTAB,JTABX,KEY                                      
C     ***************************************************************** 
      DO 126 J=1,400                                                    
      IF(JTAB(J,1))127,127,120                                          
 120  IF(JPP-JTAB(J,1))126,122,126                                      
 122  IF(JII-JTAB(J,2))126,123,126                                      
 123  IF(JTAU-JTAB(J,3))126,124,126                                     
 124  KEY=J                                                             
      GO TO 130                                                         
 126  CONTINUE                                                          
 127  PRINT 129,JTAU,JPP,JII                                            
 129  FORMAT(10X,'RDPE -- DATA NOT STORED ON DISK',3I4)                 
      GO TO 140                                                         
 130  READ(NFILE,REC=KEY,ERR=134) (DK(K),K=1,858)                       
      GO TO 140                                                         
 134  PRINT 136                                                         
 136  FORMAT(///,' DISK READ PROBLEM IN SUBROUTINE RDPE, RETRY PROGRAM')
 140  RETURN                                                            
      END                                                               
