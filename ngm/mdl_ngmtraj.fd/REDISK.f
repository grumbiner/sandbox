      SUBROUTINE REDISK(KTAU,AP,APP,KU,KV,KW,LU,LV,LW,MU,MV,MW,NPROJ)   
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    REDISK      READS U,V,W FOR 12 H INT FROM DISK         
C   PRGMMR: R. M. REAP       ORG: W/OSD21            DATE: 95-11-01     
C                                                                       
C ABSTRACT: READS WIND DATA FOR 12 HR PERIOD FROM LOCAL DISK 80         
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   68-09-01  R. M. REAP                                                
C   74-02-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-01  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM REDISK                                                    
C        NOV 1995        R. M. REAP         MDL          CRAY           
C        PURPOSE                                                        
C        WIND DATA (U,V,W) FOR A 12-HOUR FORECAST PERIOD ARE READ       
C        FROM DISK AND ASSIGNED TO ARRAYS IN CORE STORAGE.    
C        DATA SET USE:                                                  
C             FT80 (INPUT)                                              
C        VARIABLES                                                      
C        WIND COMPONENT ARRAYS ARE AS FOLLOWS:                          
C             KU,KV,KW=WIND COMPONENTS AT HOUR  T0                      
C             LU,LV,LW=WIND COMPONENTS AT HOUR (T0 + 6)                 
C             MU,MV,MW=WIND COMPONENTS AT HOUR (T0 + 12)                
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      DIMENSION AP(858),APP(26,33),KU(26,33,5),KV(26,33,5),KW(26,33,5), 
     1LU(26,33,5),LV(26,33,5),LW(26,33,5),MU(26,33,5),MV(26,33,5),      
     2MW(26,33,5)                                                       
C     ******************************************************************
      ND=80                                                             
      IF(NPROJ.EQ.1) ND=89                                              
      NRCD=((KTAU/12)*30)                                               
      DO 60 K=1,3                                                       
      DO 60 M=1,3                                                       
      DO 60 N=1,5                                                       
      NRCD=NRCD+1                                                       
      READ(ND,REC=NRCD,ERR=17) (AP(KK),KK=1,858)                        
      GO TO 20                                                          
 17   KHOUR=KTAU+(K-1)*6                                                
      PRINT 18,KHOUR,N,M                                                
 18   FORMAT(10X,'REDISK DISK READ PROBLEM ',/,10X,'HOUR= ',I2,' LEVEL='
     1,I4,' WIND COMPONENT= ',I2)                                       
 20   DO 60 I=1,26                                                      
      DO 60 J=1,33                                                      
      GO TO(34,42,50),M                                                 
 34   IKX=APP(I,J)*10000.                                               
      GO TO(36,38,40),K                                                 
 36   KU(I,J,N)=IKX                                                     
      GO TO 60                                                          
 38   LU(I,J,N)=IKX                                                     
      GO TO 60                                                          
 40   MU(I,J,N)=IKX                                                     
      GO TO 60                                                          
 42   IKX=APP(I,J)*10000.                                               
      GO TO(44,46,48),K                                                 
 44   KV(I,J,N)=IKX                                                     
      GO TO 60                                                          
 46   LV(I,J,N)=IKX                                                     
      GO TO 60                                                          
 48   MV(I,J,N)=IKX                                                     
      GO TO 60                                                          
 50   IKX=APP(I,J)*100.                                                 
      GO TO(52,54,56),K                                                 
 52   KW(I,J,N)=IKX                                                     
      GO TO 60                                                          
 54   LW(I,J,N)=IKX                                                     
      GO TO 60                                                          
 56   MW(I,J,N)=IKX                                                     
 60   CONTINUE                                                          
      RETURN                                                            
      END                                                               
