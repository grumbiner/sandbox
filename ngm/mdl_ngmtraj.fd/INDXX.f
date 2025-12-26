      SUBROUTINE INDXX(XPOS,YPOS,ISTA,ITAB)                             
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    INDXX       LOCATES 7 CLOSEST STATIONS                 
C   PRGMMR:  R. M. REAP      ORG: W/OSD21            DATE: 95-11-02     
C                                                                       
C ABSTRACT:  LOCATES 7 CLOSEST STATIONS TO SPECIFIED POINT              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   68-09-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM INDXX                                                     
C        NOV 1995       R. M. REAP       MDL        CRAY                
C        PURPOSE                                                        
C        LOCATES 7 CLOSEST STATIONS TO ANY SPECIFIED POINT AND STORES   
C        IN ARRAY ISTA(L)                                               
C        VARIABLES                                                      
C             ITAB=0 ..... LOCATES RAOB STATIONS                        
C             ITAB=1 ..... LOCATES SURFACE STATIONS                     
C        COMMON BLOCKS                                                  
C            KDMY,BLOCKB                                                
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR,KSFC,KSHP,KUPA           
      COMMON/BLOCKB/SFC(900,4),STAX(900),STAY(900),JSTA(900),JBLK(900), 
     1 KSTAP                                                         
      DIMENSION DS(900),ISTA(7),KSTA(7),DIS(7)                          
C     ****************************************************************  
      IF(ITAB) 8,8,4                                                    
   4  DO 6 K=1,KSTAP                                                    
      XSQ=(STAX(K)-XPOS)*(STAX(K)-XPOS)                                 
      YSQ=(STAY(K)-YPOS)*(STAY(K)-YPOS)                                 
C        DS(K)=SQUARE OF DISTANCE FROM SURFACE STATION                  
C        TO TRAJECTORY ORIGIN POINT.                                    
      DS(K)=XSQ+YSQ                                                     
   6  CONTINUE                                                          
      MSTA=KSTAP                                                        
      GO TO 14                                                          
   8  DO 10 K=1,NSTAP                                                   
      XSQ=(XCORD(K)-XPOS)*(XCORD(K)-XPOS)                               
      YSQ=(YCORD(K)-YPOS)*(YCORD(K)-YPOS)                               
C        DS(K)=SQUARE OF DISTANCE FROM RAOB TO TRAJECTORY ORIGIN POINT  
      DS(K)=XSQ+YSQ                                                     
  10  CONTINUE                                                          
      MSTA=NSTAP                                                        
  14  DO 20 I=1,7                                                       
      DIS(I)=9999.                                                      
  20  CONTINUE                                                          
      DO 34 J=1,MSTA                                                    
      M=1                                                               
      DO 30 K=1,7                                                       
      IF(DS(J)-DIS(K))30,22,22                                          
  30  CONTINUE                                                          
      K=8                                                               
  22  M=M+1                                                             
      IF(M-K)24,26,34                                                   
  24  DIS(M-1)=DIS(M)                                                   
      KSTA(M-1)=KSTA(M)                                                 
      GO TO 22                                                          
  26  DIS(M-1)=DS(J)                                                    
      KSTA(M-1)=J                                                       
  34  CONTINUE                                                          
      DO 36 I=1,7                                                       
      J=8-I                                                             
      ISTA(I)=KSTA(J)                                                   
  36  CONTINUE                                                          
      RETURN                                                            
      END                                                               
