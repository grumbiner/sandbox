      SUBROUTINE OVERT(KU,KV,KW,LU,LV,LW,MU,MV,MW)                      
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    OVERT       GENERATES VERTICAL VELOCITIES AT OO HR     
C   PRGMMR: R. M. REAP       ORG: W/OSD21             DATE: 95-11-01    
C                                                                       
C ABSTRACT:  GENERATES VERTICAL VELOCITIES AT 00 HR                     
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C    67-01-01 R. M. REAP                                                
C    74-02-01 CONVERT TO IBM SYSTEM                                     
C    92-09-18 CONVERT TO FORTRAN 77                                     
C    95-11-01 CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM OVERT                                                     
C        NOV 1995       R. M. REAP        MDL         CRAY              
C        PURPOSE                                                        
C        GENERATES VERTICAL VELOCITIES AT FORECAST HOUR ZERO BY BACKWARD
C        ADVECTION OF THE SIX-HOUR VERTICAL VELOCITY FIELDS.  THE       
C        ADVECTING WIND AT AN INDIVIDUAL GRID POINT IS GIVEN BY 0.55    
C        TIMES THE 500 MB WIND AVERAGED OVER NINE ADJACENT GRID POINTS  
C        VARIABLES                                                      
C             WDEL--LOCAL TIME RATE OF CHANGE OF VERTICAL VELOCITY      
C             KW=00-HR VERTICAL VELOCITY                                
C             LW=06-HR VERTICAL VELOCITY                                
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      INTEGER X,Y                                                       
      DIMENSION WDEL(26,33),KU(26,33,5),KV(26,33,5),KW(26,33,5),        
     1LU(26,33,5),LV(26,33,5),LW(26,33,5),MU(26,33,5),MV(26,33,5),      
     2MW(26,33,5),KWX(26,33,5)                                          
C     ******************************************************************
      DO 10 I=1,26                                                      
      DO 10 J=1,33                                                      
      WDEL(I,J)=0.                                                      
      DO 10 N=1,5                                                       
      KW(I,J,N)=LW(I,J,N)                                               
      KWX(I,J,N)=0                                                      
  10  CONTINUE                                                          
      N=4                                                               
      DO 24 M=1,6                                                       
      P=(6.-M)/5.                                                       
      DO 20 I=2,25                                                      
      DO 20 J=2,32                                                      
      IM1=I-1                                                           
      IP1=I+1                                                           
      JM1=J-1                                                           
      JP1=J+1                                                           
      UA=0                                                              
      VA=0                                                              
      UB=0                                                              
      VB=0                                                              
C        SUM U,V WINDS OVER NINE GRID POINTS CENTERED AT I,J            
      DO 18 X=IM1,IP1                                                   
      DO 18 Y=JM1,JP1                                                   
      BUA=KU(I,J,N)*0.0001                                              
      BVA=KV(I,J,N)*0.0001                                              
      BUB=LU(I,J,N)*0.0001                                              
      BVB=LV(I,J,N)*0.0001                                              
      UA=UA+BUA                                                         
      VA=VA+BVA                                                         
      UB=UB+BUB                                                         
      VB=VB+BVB                                                         
  18  CONTINUE                                                          
C        COMPUTE AVERAGE U,V OVER NINE GRID-POINT ARRAY WHERE           
C        0.55/NO. GRID POINTS (9) = 0.06111                             
      UAAA=0.06111*UA                                                   
      UBBB=0.06111*UB                                                   
      VAAA=0.06111*VA                                                   
      VBBB=0.06111*VB                                                   
      UBAR=((1.-P)*UAAA)+(P*UBBB)                                       
      VBAR=((1.-P)*VAAA)+(P*VBBB)                                       
C        COMPUTE WDEL AND NEW VERTICAL VELOCITY AT I,J                  
      DO 20 K=1,5                                                       
      WJP1=KW(I,J+1,K)*0.01                                             
      WJM1=KW(I,J-1,K)*0.01                                             
      WIP1=KW(I+1,J,K)*0.01                                             
      WIM1=KW(I-1,J,K)*0.01                                             
      AA=WJP1-WJM1                                                      
      BB=WIP1-WIM1                                                      
      WDEL(I,J)=-0.5*((UBAR*AA)+(VBAR*BB))                              
      WX=KW(I,J,K)*0.01                                                 
      WX=WX-WDEL(I,J)                                                   
      KWX(I,J,K)=WX*100.                                                
  20  CONTINUE                                                          
      DO 24 I=1,26                                                      
      DO 24 J=1,33                                                      
      DO 24 K=1,5                                                       
      KW(I,J,K)=KWX(I,J,K)                                              
  24  CONTINUE                                                          
      RETURN                                                            
      END                                                               
