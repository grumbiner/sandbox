      SUBROUTINE EXTRA(JND,PSA,TM,DP,RPRES,RTEMP,RDEWPT,STA,BLK,KSP,    
     1TAB,HT,KB)                                                        
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    EXTRA       EXTRACTS TEMP/DEWPT FROM RAOB              
C   PRGMMR: R. M. REAP       ORG: W/OSD21          DATE: 95-11-02       
C                                                                       
C ABSTRACT:  EXTRACTS TEMPERATURE AND DEW POINT FROM RAOB               
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   67-07-01  R. M. REAP                                                
C   74-04-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM EXTRA                                                     
C        NOV 1995         R. M. REAP          MDL           CRAY        
C        PURPOSE                                                        
C        EXTRACTS TEMPERATURE AND DEWPOINT AT SPECIFIED HEIGHT FROM     
C        STATION SOUNDING.                                              
C        VARIABLES                                                      
C             JND--CLOSEST STATION INDEX                                
C             PSA--PARCEL HEIGHT (MB)                                   
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR,KSFC,KSHP,KUPA           
      DIMENSION KSP(150),TAB(50),HT(50),RPRES(150,40),RTEMP(150,40),    
     1RDEWPT(150,40),STA(150),BLK(150)                            
C     ****************************************************************  
      TM=9999.                                                          
      DP=9999.                                                          
      I=JND                                                             
      ALT=PSA                                                           
      IF(ALT-RPRES(I,1))102,102,100                                     
  100 ALT=RPRES(I,1)                                                    
  102 IF(RTEMP(I,1)-9999.)103,10,103                                    
  10  IF(ALT-RPRES(I,2))103,103,12                                      
  12  IF(RPRES(I,2)-850.)14,160,14                                      
  14  IF(RPRES(I,2)-(ALT-50.))160,16,16                                 
  16  ALT=RPRES(I,2)                                                    
      GO TO 106                                                         
  103 IF(ALT-300.)104,106,106                                           
  104 ALT=300.                                                          
  106 DO 110 J=2,40                                                     
      IF(ALT-RPRES(I,J))110,110,108                                     
  108 IF(RPRES(I,J))109,160,109                                         
  109 N=J                                                               
      GO TO 112                                                         
  110 CONTINUE                                                          
      GO TO 160                                                         
  112 M=N-1                                                             
C        MISSING DATA CHECK AND LINEAR INTERPOLATION                    
      MT=M                                                              
      MD=M                                                              
      NT=N                                                              
      ND=N                                                              
      JTAB=0                                                            
      KTAB=0                                                            
      IF(RTEMP(I,M)-9999.)120,114,120                                   
  114 MT=M-1                                                            
      IF(MT)160,160,116                                                 
  116 IF(RTEMP(I,MT)-9999.)118,160,118                                  
  118 JTAB=1                                                            
  120 IF(RTEMP(I,N)-9999.)128,122,128                                   
  122 IF(JTAB-1)124,160,160                                             
  124 NT=N+1                                                            
      IF(NT-40)126,126,160                                              
  126 IF(RTEMP(I,NT)-9999.)128,160,128                                  
C        GROSS ERROR CHECK (TEMPERATURE ONLY) FOR LARGE INVERSION       
  128 IF(RTEMP(I,MT)-RTEMP(I,NT))200,129,129                            
  200 DELP=RPRES(I,MT)-RPRES(I,NT)                                      
      DELT=RTEMP(I,NT)-RTEMP(I,MT)                                      
      CHKT=10.5-0.05*DELP                                               
      IF(DELT-CHKT)129,129,160                                          
  129 H=RPRES(I,MT)-RPRES(I,NT)                                         
      XO=RPRES(I,MT)                                                    
      G=(XO-ALT)/H                                                      
      TM=(1.-G)*RTEMP(I,MT)+G*RTEMP(I,NT)                               
      IF(RDEWPT(I,M)-9999.)138,132,138                                  
  132 MD=M-1                                                            
      IF(MD)147,147,134                                                 
  134 IF(RDEWPT(I,MD)-9999.)136,147,136                                 
  136 KTAB=1                                                            
  138 IF(RDEWPT(I,N)-9999.)145,140,145                                  
  140 IF(KTAB-1)142,147,147                                             
  142 ND=N+1                                                            
      IF(ND-40)144,144,147                                              
  144 IF(RDEWPT(I,ND)-9999.)145,147,145                                 
  145 H=RPRES(I,MD)-RPRES(I,ND)                                         
      XO=RPRES(I,MD)                                                    
      G=(XO-ALT)/H                                                      
      DP=(1.-G)*RDEWPT(I,MD)+G*RDEWPT(I,ND)                             
      IF(DP-(TM+0.5))147,147,146                                        
  146 DP=9999.                                                          
C        CHECK FOR SUPER-ADIABATIC LAPSE RATE                           
  147 TEMP1=RTEMP(I,MT)+273.16                                          
      TEMP2=RTEMP(I,NT)+273.16                                          
      PRES1=RPRES(I,MT)*0.1                                             
      PRES2=RPRES(I,NT)*0.1                                             
      THETA1=TEMP1*((100./PRES1)**0.286)                                
      THETA2=TEMP2*((100./PRES2)**0.286)                                
      IF(THETA1-(THETA2+2.))160,160,148                                 
  148 TM=9999.                                                          
      DP=9999.                                                          
      DO 151 K=1,KB                                                     
      IF(KB-49)149,149,160                                              
  149 IF(I-TAB(K))151,150,151                                           
  150 IF(THETA1-HT(K))151,160,151                                       
  151 CONTINUE                                                          
      KB=KB+1                                                           
      TAB(KB)=I                                                         
      HT(KB)=THETA1                                                     
      ISTA=STA(I)                                                       
      IBLK=BLK(I)                                                       
C     PRINT 154,ISTA,IBLK,ALT,THETA1,THETA2                             
C 154 FORMAT(//,5X,'SUPER-ADIABATIC LAYER.     STATION',I5,4X,'BLOCK',  
C    1I4,5X,F6.1,2F10.2)                                                
  160 RETURN                                                            
      END                                                               
