      SUBROUTINE RDADP(XLAT,XLON,KHR,KGO)                               
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    RDADP       CHECKS RAOB/SFC REPORTS                    
C   PRGMMR: R. M. REAP       ORG: W/OSD21            DATE: 96-03-11     
C                                                                       
C ABSTRACT:  SELECTS RAOB AND SURFACE REPORTS WITHIN 26X33 GRID         
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   71-04-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C   96-03-11  CONVERT TO BUFR INPUT 
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM RDADP                                                     
C        NOV 1996        W/OSD21          MDL         CRAY              
C        PURPOSE                                                        
C        CHECKS REPORT ID FROM NCEP KUPA, KSFC, & KSHP DATA SETS        
C        CHECKS FOR RAOB (SURFACE) STATIONS WITHIN MDL 26X33 GRID.      
C        ACCEPTS OFF-TIME REPORTS WITHIN TWO HOURS OF INITIAL TIME.     
C        VARIABLES                                                      
C             AI--STATION I COORDINATE  (MDL GRID)                      
C             AJ--STATION J COORDINATE  (MDL GRID)                      
C             KHR=HOUR OF OBSERVATION
C             KGO=0,,,,USABLE REPORT      KGO=1,,,,REPORT REJECTED      
C        COMMON BLOCKS                                                  
C            KDMY,BLOCKH                                                
C        SUBPROGRAMS CALLED:                                            
C            W3FB00                                                     
C         LIBRARY                                                       
C            COMMON,W3LIB,MDLLIB                                         
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR,KSFC,KSHP,KUPA           
      COMMON/BLOCKH/JFL                                            
C     *************************************************************
      KGO=1                                                             
C        CONVERT LAT/LONG TO NCEP I,J COORDINATES                       
      XLON=-XLON
      IF(XLAT)140,100,100                                               
  100 CALL W3FB00(XLAT,XLON,381.0,XI,XJ)                                
C        CONVERT NCEP GRID COORDINATES TO MDL 26X33 GRID COORDINATES    
      AI=26.0+XJ                                                        
      AJ=24.0+XI                                                        
C        TEST IF REPORT WITHIN SUBSET OF 26X33 MDL GRID                 
      IF(AI-24.)104,104,140                                             
  104 IF(AI-2.)140,106,106                                              
  106 IF(AJ-5.)140,108,108                                              
  108 IF(AJ-31.)116,116,110                                             
  110 IF(AJ-33.)112,112,140                                             
  112 IF(AI-19.)114,140,140                                             
  114 IF(AI-7.)140,140,116                                              
C        CHECK REPORT TO INSURE WITHIN TWO HOURS OF INITIAL TIME        
  116 IF(JFL)124,122,124                                                
  122 IF(KHR-2)126,126,123                                              
  123 IF(KHR-22)140,126,126                                             
  124 IF(KHR-10)140,125,125                                             
  125 IF(KHR-14)126,126,140                                             
  126 KGO=0                                                             
  140 RETURN                                                            
      END                                                               
