      SUBROUTINE INTPL1(KVER,LHRZ,MHOUR,LTAU,KU,KV,KW,LU,LV,LW,MU,MV,MW)
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    INTPL1      INTERPOLATE U,V,W                          
C   PRGMMR:  R. M. REAP      ORG: W/OSD21            DATE: 95-11-01     
C                                                                       
C ABSTRACT:  INTERPOLATE U,V,W AT 5 LEVELS FOR 5X5 ARRAY                
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   66-11-01  R. M. REAP                                                
C   74-02-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-01  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM INTPL1                                                    
C        NOV 1995          R. M. REAP         MDL         CRAY          
C        PURPOSE                                                        
C        INTERPOLATES U,V,W WINDS AT 5 STANDARD PRESSURE LEVELS         
C        FOR 5X5 ARRAY CENTERED AT GRID POINT (KVER,LHRZ).  VALUES ARE  
C        INTERPOLATED FOR HOURS H AND (H-2).                            
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      INTEGER A,B                                                       
      COMMON/BLOCK4/U(5,5,5),V(5,5,5),W(5,5,5),UP(5,5,5),VP(5,5,5),     
     1WP(5,5,5)                                                         
      DIMENSION AK(3),AA(3),KU(26,33,5),KV(26,33,5),KW(26,33,5),        
     1LU(26,33,5),LV(26,33,5),LW(26,33,5),MU(26,33,5),MV(26,33,5),      
     2MW(26,33,5)                                                       
C     ******************************************************************
      H=MHOUR                                                           
      P=(13.-H-6.)/6.                                                   
      G=(11.-H-6.)/6.                                                   
      AK(1)=P*(P-1.)*0.5                                                
      AK(2)=(1.-P)*(1.+P)                                               
      AK(3)=P*(P+1.)*0.5                                                
      AA(1)=G*(G-1.)*0.5                                                
      AA(2)=(1.-G)*(1.+G)                                               
      AA(3)=G*(G+1.)*0.5                                                
      KM2=KVER-2                                                        
      KP2=KVER+2                                                        
      LM2=LHRZ-2                                                        
      LP2=LHRZ+2                                                        
      A=0                                                               
      DO 30 I=KM2,KP2                                                   
      A=A+1                                                             
      B=0                                                               
      DO 30 J=LM2,LP2                                                   
      B=B+1                                                             
      DO 30 N=1,5                                                       
C        SCALE AND FLOAT WIND COMPONENTS                                
      UA=KU(I,J,N)*0.0001                                               
      VA=KV(I,J,N)*0.0001                                               
      WA=KW(I,J,N)*0.01                                                 
      UB=LU(I,J,N)*0.0001                                               
      VB=LV(I,J,N)*0.0001                                               
      WB=LW(I,J,N)*0.01                                                 
      UC=MU(I,J,N)*0.0001                                               
      VC=MV(I,J,N)*0.0001                                               
      WC=MW(I,J,N)*0.01                                                 
      U(A,B,N)=AK(1)*UA+AK(2)*UB+AK(3)*UC                               
      V(A,B,N)=AK(1)*VA+AK(2)*VB+AK(3)*VC                               
      W(A,B,N)=AK(1)*WA+AK(2)*WB+AK(3)*WC                               
      UP(A,B,N)=AA(1)*UA+AA(2)*UB+AA(3)*UC                              
      VP(A,B,N)=AA(1)*VA+AA(2)*VB+AA(3)*VC                              
      WP(A,B,N)=AA(1)*WA+AA(2)*WB+AA(3)*WC                              
 30   CONTINUE                                                          
      RETURN                                                            
      END                                                               
