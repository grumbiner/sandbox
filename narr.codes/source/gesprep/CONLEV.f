      SUBROUTINE CONLEV (ICT,ZX,ZN)                                     
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    CONLEV      DETERMINES NUMBER OF CONTOURS FOR PLOT     
C   PRGMMR: DIMEGO           ORG: W/NMC22    DATE: 86-07-18             
C                                                                       
C ABSTRACT: DETERMINES NUMBER OF CONTOURS FOR PLOT                      
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   86-07-18  G DIMEGO                                                  
C   88-09-22  B SCHMIDT ADDED THE DOCBLOCK                              
C                                                                       
C USAGE:    CALL CONLEV (ICT, ZX, ZN)                                   
C   INPUT ARGUMENT LIST:                                                
C     ICT      - LOGICAL VARIABLE                                       
C     ZX       - MAXIMUM VALUE                                          
C     ZN       - MINIMUM VALUE                                          
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     LIBRARY:                                                          
C       COMMON   - BLK3                                                 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: STANDARD FORTRAN                                          
C   MACHINE:                                                            
C                                                                       
C$$$                                                                    
      IMPLICIT REAL   (A-H,O-Z)                                         
      DIMENSION CON(24),XLIM(4),YLIM(4)                                 
      CHARACTER*4 YTITLE(20)                                            
      CHARACTER*1 XTITLE(30)                                            
      LOGICAL IACROS,XREV,YREV,TICK,ALLCON                              
      COMMON /BLK3/ LI,LJ,LID,IACROS,XREV,YREV,TICK,ALLCON,XLIM,YLIM,   
     + XRG,YRG,IXD,IYD,NPRL,NPRC,NLV,CON,XTITLE,YTITLE                  
      DIF = ABS(ZX-ZN)                                                  
C WAS XX = HMAX1(ABS(ZX),ABS(ZN))                                       
      XX = AMAX1(ABS(ZX),ABS(ZN))                                       
      CONT = CON(2)                                                     
      IEVODD = 0                                                        
      NLV2 = NLV/2                                                      
      ENLV = NLV                                                        
      ENMN = 0.5 * ENLV                                                 
      ENMX = 1.25 * ENLV                                                
      GO TO (10,25,30,40,50,60),ICT                                     
   10 IF(IEVODD .EQ. 1) GO TO 41                                        
      CON(1) = -CON(2) * 10. * ENLV                                     
      RETURN                                                            
   20 CON(2) = CON(2) + CONT                                            
   25 IF(DIF/CON(2) .GT. ENMX - 2.) GO TO 20                            
      GO TO 10                                                          
   30 IF(DIF/CON(2) - ENLV) 36,10,33                                    
   33 IF(DIF/CON(2) .LE. ENMX - 2.) GO TO 10                            
      CON(2) = CON(2) * 2.                                              
      IF(DIF/CON(2) .LE.  ENMX   ) GO TO 10                             
      CON(2) = CON(2) * 2.5                                             
      IF(DIF/CON(2) .LE. ENMX-2.) GO TO 10                              
      CON(2) = CON(2) * 2.                                              
      GO TO 33                                                          
   36 IF(DIF/CON(2) .GE.ENMN + 1.) GO TO 10                             
      CON(2) = CON(2) * .5                                              
      IF(DIF/CON(2) .GE. ENMN) GO TO 10                                 
      CON(2) = CON(2) * .4                                              
      IF(DIF/CON(2) .GE.ENMN + 1.) GO TO 10                             
      CON(2) = CON(2) * .5                                              
      GO TO 36                                                          
   40 NLV = NLV + 4                                                     
      NLV2 = NLV2 + 2                                                   
      CON(NLV2 + 1) = 1. E-21                                           
      CON(NLV2 + 2) = .1 * CONT                                         
      CON(NLV2 + 3) =.25 * CONT                                         
      CON(NLV2 + 4) = .5 * CONT                                         
      CON(NLV2 + 5) =      CONT                                         
      NST = NLV2 + 6                                                    
      GO TO 43                                                          
   41 CONT = CON(2)                                                     
      CON(NLV2 + 1) = 1. E-21                                           
      CON(NLV2 + 2) = .5 * CONT                                         
      CON(NLV2 + 3) =      CONT                                         
      NST = NLV2 + 4                                                    
   43 DO 45 II = NST,NLV                                                
      CON(II) = CON(II-1) + CONT                                        
   45 CONTINUE                                                          
      DO 47 II = 1,NLV2                                                 
      III = NLV - II + 1                                                
      CON(II) = -CON(III)                                               
   47 CONTINUE                                                          
      ALLCON = .TRUE.                                                   
      RETURN                                                            
   50 RAT = ABS( ZX/ZN )                                                
      IF( RAT .GT. 100.  .OR.  RAT .LT. .01 )  GO TO 30                 
   60 IEVODD = 1                                                        
      DIF = 2. * XX                                                     
      ENMX = ENLV - 1.                                                  
      ENMN = FLOAT(NLV2 - 3)                                            
      GO TO 30                                                          
      END                                                               
