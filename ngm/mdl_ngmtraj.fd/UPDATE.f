      SUBROUTINE UPDATE(JT,JDTH,INC)                                    
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    UPDATE      UPDATES DATE/TIME GROUP                    
C   PRGMMR:  R. M. REAP      ORG: W/OSD21           DATE: 98-05-06      
C                                                                       
C ABSTRACT:  UPDATES DATE/TIME GROUP BY INCR (HOURS)                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   71-03-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C   96-03-08  CONVERT TO GRIB INPUT DATE-TIME GROUP 
C   98-05-06  CONVERT TO YEAR 2000 COMPLIANT
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM UPDATE                                                    
C        MAY 1998      R. M. REAP         MDL           CRAY            
C         PURPOSE                                                       
C         UPDATES DATE/TIME GROUP BY INCR HOURS                         
C         SUBROUTINE ARGUMENTS                                          
C            JT - DATE-TIME GROUP IN 10-DIGIT INTEGER FORM (YYYYMMDDHH) 
C          JDTH - UPDATED DATE-TIME GROUP, 10-DIGIT INTEGER FORM        
C           INC - AMOUNT TO INCREMENT JT TO ARRIVE AT JDTH              
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
C     ***************************************************************** 
      JY=JT/1000000                                                     
      JM=(JT/10000)-((JT/1000000)*100)   
      JD=(JT/100)-((JT/10000)*100)                                      
      JH=JT-((JT/100)*100)                                              
C        JH=HOUR, JY=YEAR, JM=MONTH, AND JD=DAY IN INTEGER FORM         
      IF(INC)170,170,140                                                
 140  DO 164 J=1,INC                                                    
      JH=JH+1                                                           
      IF(JH-24)164,145,145                                              
 145  JH=0                                                              
      JD=JD+1                                                           
      GO TO(158,150,158,157,158,157,158,158,157,158,157,158),JM         
 150  IF(MOD(JY,4))156,155,156                                          
 155  IF(JD-29)164,164,159                                              
 156  IF(JD-28)164,164,159                                              
 157  IF(JD-30)164,164,159                                              
 158  IF(JD-31)164,164,159                                              
 159  JD=1                                                              
      JM=JM+1                                                           
      IF(JM-12)164,164,162                                              
 162  JM=1                                                              
      JY=JY+1                                                           
 164  CONTINUE                                                          
C        JH,JY,JM, AND JD ARE NOW UPDATED IN 10-DIGIT INTEGER FORM      
 170  JDTH=JH+JD*100+JM*10000+JY*1000000                                
      RETURN                                                            
      END                                                               
