      SUBROUTINE OUTPUT(JDTH)                                           
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    OUTPUT      PRINTS TEMP/DEW POINT DATA                 
C   PRGMMR: R. M. REAP       ORG: W/OSD21          DATE: 96-05-14       
C                                                                       
C ABSTRACT:  PREPARES TEMP/DEW POINT DATA AND PRINTS SAMPLE VALUES      
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-05-14  R. M. REAP                                                
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM OUTPUT                                                    
C        MAY 1996     R. M. REAP         MDL          CRAY              
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
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCKG/TEMP(13,17,4),DEWPT(13,17,4)                        
C     ***************************************************************** 
      DO 40 KK=1,3 
      DO 8 I=1,13  
      II=14-I
      WRITE(6,6) (TEMP(II,J,KK),J=8,17)
  6   FORMAT(3X,10F7.2)
  8   CONTINUE  
      PRINT 10,JDTH,KK                                                  
 10   FORMAT(/,15X,'TEMPERATURE FCST VALID AT ',I10,' LEVEL = ',I1,///) 
      DO 20 I=1,13  
      II=14-I
      WRITE(6,6) (DEWPT(II,J,KK),J=8,17)
 20   CONTINUE  
      PRINT 30,JDTH,KK                                                  
 30   FORMAT(/,15X,'DEW POINT FCST VALID AT ',I10,'   LEVEL = ',I1,///) 
      DO 34 I=1,13  
      II=14-I
      WRITE(6,6) (RHUM(II,J,KK),J=8,17)
 34   CONTINUE  
      PRINT 36,JDTH,KK                                                  
 36   FORMAT(/,13X,'REL HUMID FCST VALID AT ',I10,'   LEVEL = ',I1,///) 
 40   CONTINUE
      RETURN                                                            
      END                                                               
