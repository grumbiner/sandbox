      SUBROUTINE MARIAH(I,J,N,KVER,LHRZ,ITER,ERR,ERRA,MHOUR,TERRA)      
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    MARIAH      COMPUTES BACKWARD 3D TRAJECTORIES          
C   PRGMMR:  R. M. REAP      ORG: W/OSD21             DATE: 95-11-01    
C                                                                       
C ABSTRACT:  COMPUTES BACKWARD 3D TRAJECTORIES FOR GRID PTS/STATIONS    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C    66-12-01  R. M. REAP                                               
C    74-02-01  CONVERT TO IBM SYSTEM                                    
C    92-09-18  CONVERT TO FORTRAN 77                                    
C    95-11-01  CONVERT TO CRAY                                          
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM MARIAH                                                    
C        NOV 1995        R. M. REAP        MDL         CRAY             
C        PURPOSE                                                        
C        COMPUTES BACKWARD 3-D TRAJECTORIES FOR GRID POINTS OR STATIONS 
C        SPECIFIED BY MAIN PROGRAM. THE INITIAL COORDINATES ARE         
C        DEFINED TO BE INDIVIDUALLY CONSERVED.  TWO-HOURLY DISPLACEMENTS
C        ARE COMPUTED. A FOUR-POINT BI-LINEAR FORMULA IS USED FOR       
C        HORIZONTAL INTERPOLATIONS ON THE STANDARD PRESSURE LEVELS.     
C        VERTICAL INTERPOLATIONS ARE ACCOMPLISHED BY THE LAGRANGE       
C        THREE-POINT QUADRATIC FORMULA.                                 
C        VARIABLES                                                      
C             ITER - NUMBER OF ITERATIONS ALLOWED FOR TRAJECTORY        
C             CONVERGENCE. IF EXCEEDED, AVERAGE OF LAST TWO COMPUTED    
C             POSITIONS IS STORED.                                      
C             ERR,ERRA - HORIZONTAL AND VERTICAL CONVERGENCE CRITERIA.  
C             KP,LP - ESTABLISH A CORRESPONDENCE BETWEEN THE 26X33X5    
C             AND 5X5X5 DATA ARRAYS                                     
C        COMMON BLOCKS                                                  
C             KDMY,BLOCK4,BLOCK5,BLOCKF                                 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCK4/U(5,5,5),V(5,5,5),W(5,5,5),UP(5,5,5),VP(5,5,5),     
     1WP(5,5,5)                                                         
      COMMON/BLOCK5/XXX(13,17,4),YYY(13,17,4),ZZZ(13,17,4)              
      COMMON/BLOCKF/ZDF(13,17,4),JSW                                    
      DIMENSION XPOS(10),YPOS(10),ZPOS(10),UI(5),VI(5),WI(5),AL(6),     
     1AM(6),AN(6),TERRA(51,65)                                          
C      **************************************************************   
      NK=0                                                              
      JJ=0                                                              
      IHOUR=11-MHOUR                                                    
      XPOS(1)=PLX(I,J,N)                                                
      YPOS(1)=PLY(I,J,N)                                                
      ZPOS(1)=PLP(I,J,N)                                                
  19  NK=NK+1                                                           
  191 LL=XPOS(NK)                                                       
      KK=YPOS(NK)                                                       
      KP=KK-KVER+3                                                      
      LP=LL-LHRZ+3                                                      
      IF(KP-5)193,192,192                                               
  192 YPOS(NK)=KK-.01                                                   
      GO TO 191                                                         
  193 IF(LP-5)195,194,194                                               
  194 XPOS(NK)=LL-.01                                                   
      GO TO 191                                                         
  195 IF(KP)196,196,197                                                 
  196 YPOS(NK)=KVER-2.                                                  
      GO TO 191                                                         
  197 IF(LP)198,198,199                                                 
  198 XPOS(NK)=LHRZ-2.                                                  
      GO TO 191                                                         
  199 DX=XPOS(NK)-LL                                                    
      DY=YPOS(NK)-KK                                                    
      G=ZPOS(NK)-700.                                                   
      IF(G)20,22,22                                                     
  20  XO=500.                                                           
      H=200.                                                            
      KM=2                                                              
      GO TO 23                                                          
  22  XO=850.                                                           
      H=150.                                                            
      KM=0                                                              
  23  IF(JJ)24,24,27                                                    
  24  DO 25 M=1,3                                                       
      II=M+KM                                                           
      LXP1=LP+1                                                         
      KYP1=KP+1                                                         
      UINL=U(KP,LP,II)                                                  
      VINL=V(KP,LP,II)                                                  
      WINL=W(KP,LP,II)                                                  
      UI(II)=UINL+(U(KP,LXP1,II)-UINL)*DX+(U(KYP1,LP,II)-UINL)*DY+      
     1(UINL+U(KYP1,LXP1,II)-U(KYP1,LP,II)-U(KP,LXP1,II))*DX*DY          
      VI(II)=VINL+(V(KP,LXP1,II)-VINL)*DX+(V(KYP1,LP,II)-VINL)*DY+      
     1(VINL+V(KYP1,LXP1,II)-V(KYP1,LP,II)-V(KP,LXP1,II))*DX*DY          
      WI(II)=WINL+(W(KP,LXP1,II)-WINL)*DX+(W(KYP1,LP,II)-WINL)*DY+      
     1(WINL+W(KYP1,LXP1,II)-W(KYP1,LP,II)-W(KP,LXP1,II))*DX*DY          
  25  CONTINUE                                                          
      P=(ZPOS(NK)-XO)/H                                                 
      IF(P-1.)262,262,260                                               
  260 P=1.0                                                             
  262 IF(P+1.)264,266,266                                               
  264 P=-1.0                                                            
  266 UIP=(P*(P-1.)*.5)*UI(KM+3)+(1.-P)*(1.+P)*UI(KM+2)+(P*(P+1.)*.5)   
     1*UI(KM+1)                                                         
      VIP=(P*(P-1.)*.5)*VI(KM+3)+(1.-P)*(1.+P)*VI(KM+2)+(P*(P+1.)*.5)   
     1*VI(KM+1)                                                         
      WIP=(P*(P-1.)*.5)*WI(KM+3)+(1.-P)*(1.+P)*WI(KM+2)+(P*(P+1.)*.5)   
     1*WI(KM+1)                                                         
      XPOS(NK+1)=XPOS(1)-(UIP+UIP)                                      
      YPOS(NK+1)=YPOS(1)-(VIP+VIP)                                      
      ZPOS(NK+1)=ZPOS(1)-(WIP+WIP)                                      
      JJ=1                                                              
      GO TO 19                                                          
  27  DO 29 M=1,3                                                       
      II=M+KM                                                           
      LXP1=LP+1                                                         
      KYP1=KP+1                                                         
      UPHI=UP(KP,LP,II)                                                 
      VPHI=VP(KP,LP,II)                                                 
      WPHI=WP(KP,LP,II)                                                 
      UI(II)=UPHI+(UP(KP,LXP1,II)-UPHI)*DX+(UP(KYP1,LP,II)-UPHI)*DY+    
     1(UPHI+UP(KYP1,LXP1,II)-UP(KYP1,LP,II)-UP(KP,LXP1,II))*DX*DY       
      VI(II)=VPHI+(VP(KP,LXP1,II)-VPHI)*DX+(VP(KYP1,LP,II)-VPHI)*DY+    
     1(VPHI+VP(KYP1,LXP1,II)-VP(KYP1,LP,II)-VP(KP,LXP1,II))*DX*DY       
      WI(II)=WPHI+(WP(KP,LXP1,II)-WPHI)*DX+(WP(KYP1,LP,II)-WPHI)*DY+    
     1(WPHI+WP(KYP1,LXP1,II)-WP(KYP1,LP,II)-WP(KP,LXP1,II))*DX*DY       
  29  CONTINUE                                                          
      P=(ZPOS(NK)-XO)/H                                                 
      IF(P-1.)294,294,292                                               
  292 P=1.0                                                             
  294 IF(P+1.)296,298,298                                               
  296 P=-1.0                                                            
  298 UPP=(P*(P-1.)*.5)*UI(KM+3)+(1.-P)*(1.+P)*UI(KM+2)+(P*(P+1.)*.5)   
     1*UI(KM+1)                                                         
      VPP=(P*(P-1.)*.5)*VI(KM+3)+(1.-P)*(1.+P)*VI(KM+2)+(P*(P+1.)*.5)   
     1*VI(KM+1)                                                         
      WPP=(P*(P-1.)*.5)*WI(KM+3)+(1.-P)*(1.+P)*WI(KM+2)+(P*(P+1.)*.5)   
     1*WI(KM+1)                                                         
  30  UBAR=(UIP+UPP)                                                    
      VBAR=(VIP+VPP)                                                    
      WBAR=(WIP+WPP)                                                    
      XPOS(NK+1)=XPOS(1)-UBAR                                           
      YPOS(NK+1)=YPOS(1)-VBAR                                           
      ZPOS(NK+1)=ZPOS(1)-WBAR                                           
      AL(NK)=XPOS(NK)-XPOS(NK+1)                                        
      AM(NK)=YPOS(NK)-YPOS(NK+1)                                        
      AN(NK)=ZPOS(NK)-ZPOS(NK+1)                                        
      AI= ABS(AL(NK))                                                   
      AJ= ABS(AM(NK))                                                   
      AK= ABS(AN(NK))                                                   
      IF(AI-ERR)31,31,35                                                
  31  IF(AJ-ERR)33,33,35                                                
  33  IF(AK-ERRA)50,50,35                                               
  35  IF((NK+1)-ITER)19,40,40                                           
  40  PRINT 42,IHOUR,AI,AJ,AK,I,J,N                                     
  42  FORMAT(5X,'TRAJECTORY TRUNCATED',I3,5X,3F8.4,5X,3I3)              
C     XPOS(NK+1)=0.5*(XPOS(NK)+XPOS(NK+1))                              
C     YPOS(NK+1)=0.5*(YPOS(NK)+YPOS(NK+1))                              
C     ZPOS(NK+1)=0.5*(ZPOS(NK)+ZPOS(NK+1))                              
      XPOS(NK+1)=XPOS(NK)                                      
      YPOS(NK+1)=YPOS(NK)                                      
      ZPOS(NK+1)=ZPOS(NK)                                      
      PRINT 44,XPOS(NK+1),YPOS(NK+1),ZPOS(NK+1)                         
  44  FORMAT(16H PARCEL POSITION,2F11.3,F13.3/)                         
  50  XXX(I,J,N)=XPOS(NK+1)                                             
      YYY(I,J,N)=YPOS(NK+1)                                             
      ZZZ(I,J,N)=ZPOS(NK+1)                                             
C        CORRECTION FOR TERRAIN SLOPE                                   
      GX=PLX(I,J,N)+PLX(I,J,N)-1.                                       
      GY=PLY(I,J,N)+PLY(I,J,N)-1.                                       
      LX=GX                                                             
      LY=GY                                                             
      DX=GX-LX                                                          
      DY=GY-LY                                                          
      LXP1=LX+1                                                         
      KYP1=LY+1                                                         
      LOC=TERRA(LY,LX)                                                  
      TERA1=LOC+(TERRA(LY,LXP1)-LOC)*DX+(TERRA(KYP1,LX)-LOC)*DY         
     1+(LOC+TERRA(KYP1,LXP1)-TERRA(KYP1,LX)-TERRA(LY,LXP1))*DX*DY       
      GP=XXX(I,J,N)+XXX(I,J,N)-1.                                       
      RP=YYY(I,J,N)+YYY(I,J,N)-1.                                       
      LP=GP                                                             
      KP=RP                                                             
      DX=GP-LP                                                          
      DY=RP-KP                                                          
      LXP1=LP+1                                                         
      KYP1=KP+1                                                         
      LOC=TERRA(KP,LP)                                                  
      TERA2=LOC+(TERRA(KP,LXP1)-LOC)*DX+(TERRA(KYP1,LP)-LOC)*DY         
     1+(LOC+TERRA(KYP1,LXP1)-TERRA(KYP1,LP)-TERRA(KP,LXP1))*DX*DY       
      IF(PLP(I,J,N)-600.)60,60,508                                      
  508 IF(PLX(I,J,N)-17.)510,510,51                                      
  510 IF(TERA1-1000.)52,57,57                                           
  51  IF(TERA1-950.)52,57,57                                            
  52  D5T1=TERA1-500.                                                   
      D5T2=TERA2-500.                                                   
      DPT1=TERA1-PLP(I,J,N)                                             
      DPT2=(D5T2*DPT1)/D5T1                                             
      ZBUF=TERA2-DPT2                                                   
      IF(TERA2-TERA1)53,57,55                                           
  53  IF(ZZZ(I,J,N)-ZBUF)57,57,54                                       
  54  IF(JSW-1)540,540,542                                              
 540  ZDF(I,J,N)=ZDF(I,J,N)+ZZZ(I,J,N)-ZBUF                             
 542  ZZZ(I,J,N)=ZBUF                                                   
      GO TO 57                                                          
  55  IF(ZZZ(I,J,N)-ZBUF)56,57,57                                       
  56  RHUM(I,J,N)=RHUM(I,J,N)+PLP(I,J,N)-ZBUF                           
      IF(JSW-1)560,560,562                                              
 560  ZDF(I,J,N)=ZDF(I,J,N)+ZZZ(I,J,N)-ZBUF                             
 562  ZZZ(I,J,N)=ZBUF                                                   
  57  IF(ZZZ(I,J,N)-TERA2)60,60,58                                      
  58  ZZZ(I,J,N)=TERA2                                                  
  60  RETURN                                                            
      END                                                               
