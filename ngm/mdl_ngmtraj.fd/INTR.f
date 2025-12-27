      SUBROUTINE INTR(DTM,XC,YC,VAL)                                    
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    INTR        DOES BI-QUADRATIC INTERPOLATION ON INPUT   
C   PRGMMR:  R. M. REAP      ORG: W/OSD21             DATE: 95-11-02    
C                                                                       
C ABSTRACT:  PERFORMS BI-QUAD INTERPOLATION ON DTM AT POINT XC,YC       
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   70-06-01 R. M. REAP                                                 
C   74-03-01 CONVERT TO IBM SYSTEM                                      
C   92-09-18 CONVERT TO FORTRAN 77                                      
C   95-11-02 CONVERT TO CRAY                                            
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM INTR                                                      
C        NOV 1995          R. M. REAP          MDL           CRAY       
C        PURPOSE                                                        
C        PERFORMS BI-QUADRATIC INTERPOLATION IN FIELD DTM AT POINT      
C        XC,YC AND RETURNS VALUE IN VAL.  BI-LINEAR INTERPOLATION       
C        IS PERFORMED IN OUTSIDE GRID INTERVAL.                         
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      DIMENSION DTM(26,33),B(4)                                         
C     ***************************************************************** 
      KXC=XC                                                            
      KYC=YC                                                            
      DX=XC-KXC                                                         
      DY=YC-KYC                                                         
      KXP1=KXC+1                                                        
      KYP1=KYC+1                                                        
      KYP2=KYC+2                                                        
      KYM1=KYC-1                                                        
      IF(KXC-1)120,120,100                                              
  100 IF(KXC-32)102,120,120                                             
  102 IF(KYC-1)120,120,104                                              
  104 IF(KYC-25)106,120,120                                             
  106 FAC=((DY*DY)-DY)/4.                                               
      FAD=((DX*DX)-DX)/4.                                               
      DO 110 K=1,4                                                      
      N=KXC-2+K                                                         
      B(K)=DTM(KYC,N)+(DTM(KYP1,N)-DTM(KYC,N))*DY+(DTM(KYM1,N)+         
     1DTM(KYP2,N)-DTM(KYC,N)-DTM(KYP1,N))*FAC                           
  110 CONTINUE                                                          
      VAL=B(2)+(B(3)-B(2))*DX+(B(1)+B(4)-B(2)-B(3))*FAD                 
      RETURN                                                            
  120 CNR=DTM(KYC,KXC)                                                  
      VAL=CNR+(DTM(KYC,KXP1)-CNR)*DX+(DTM(KYP1,KXC)-CNR)*DY+            
     1(CNR+DTM(KYP1,KXP1)-DTM(KYP1,KXC)-DTM(KYC,KXP1))*DX*DY            
      RETURN                                                            
      END                                                               
