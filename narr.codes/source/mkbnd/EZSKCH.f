      SUBROUTINE EZSKCH(ZZ,ICT,XL,YC)                                   
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    EZSKCH      PRODUCES PRINTER PLOTS                     
C   PRGMMR: DIMEGO           ORG: W/NMC22    DATE: 86-07-18             
C                                                                       
C ABSTRACT: DISPLAYS ONLY THE right half PORTION
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   86-07-18  G DIMEGO                                                  
C   88-09-23  B SCHMIDT ADDED THE DOCBLOCK                              
C                                                                       
C USAGE:    CALL EZSKCH (ZZ, ICT, XL, YC)                               
C   INPUT ARGUMENT LIST:                                                
C     ZZ       - ARRAY TO BE PLOTTED                                    
C     ICT      - VARIABLE controlling type of contour intervals
C     XL       - XL = NPRL / (# of J points in N/S direction)
C     YC       - YC = NPRC / (# of I points in E/W direction)
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE:    - CONLEV                                               
C     LIBRARY:                                                          
C       COMMON   - BLK3                                                 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: STANDARD FORTRAN                                          
C   MACHINE:                                                            
C                                                                       
C$$$                                                                    
C                                                                       
      IMPLICIT REAL   (A-H,O-Z)                                         
      DIMENSION ZZ(1,1),CON(24),XLIM(4),YLIM(4),XAXIS(30)               
      CHARACTER*1 CSYMB(12),CSYM(12),IQ(121),C1,C2,C3                   
      CHARACTER*1 FSYMB(9),XFOR(52)                                     
      EQUIVALENCE (XFOR,YFOR)                                           
      CHARACTER*2 YFOR(26),FFSYMB(30)                                   
      CHARACTER*4 YTITLE(20)                                            
      LOGICAL   IACROS, XREV, YREV, TICK, ALLCON                      
      CHARACTER*1 XTIT, XBLK, XTITLE(30)                                
      CHARACTER*1 HTIT, BLK, EYE, PLS, MIN, FIL                         
      EQUIVALENCE (XTIT,HTIT),(XBLK,BLK)                                
      LOGICAL   AXIS, END                                             
      COMMON /BLK3/ LI,LJ,LID,IACROS,XREV,YREV,TICK,ALLCON,XLIM,YLIM,   
     + XRG,YRG,IXD,IYD,NPRL,NPRC,NLV,CON,XTITLE,YTITLE                  
      CHARACTER*1 IOVER, IUNDR                                          
      CHARACTER*2 CM3, EFF                                              
      DATA        IOVER/'+'/, IUNDR/'-'/                                
      DATA        CM3/',3'/, EFF/' F'/                                  
      DATA  BLK,EYE,PLS,MIN /' ','I','+','-'/                           
      DATA CSYMB/'N','9','7','5','3','1','0','2','4','6','8','X'/       
      DATA FSYMB/'1','2','3','4','5','6','7','8','9'/                   
      DATA FFSYMB/'01','02','03','04','05','06','07','08','09','10',    
     +  '11','12','13','14','15','16','17','18','19','20',              
     +  '21','22','23','24','25','26','27','28','29','30'/              
      ZMIN = +1. E+15                                                   
      ZMAX = -1. E+15                                                   
C                                                                       
C MAX-MIN OBTAINED FROM right half of grid
C                                                                       
      IJ = 0                                                            
      IHALF = (LI/2) + 1
c     JHALF = (LJ/2) + 1
      DO 1 J=1,LJ                                                       
      DO 1 I=1,LI                                                     
      IJ = IJ + 1                                                       
c     IF(I.GE.IHALF.AND.J.GE.JHALF) THEN
      IF(I.GE.IHALF) THEN
        ZMIN = AMIN1(ZMIN,ZZ(IJ,1))                             
        ZMAX = AMAX1(ZMAX,ZZ(IJ,1))               
      END IF
    1 CONTINUE                                                          
      WRITE(6,7)ZMAX,ZMIN                                               
    7 FORMAT(2G15.5)                                                    
      YLIM(1) = 1.0                                                     
      YLIM(2) = FLOAT(LI)                                               
C     YLIM(3) = 1.0
      YLIM(3) = FLOAT(IHALF)
      YLIM(4) = FLOAT(LI)
      NPRC = ((YLIM(4)-YLIM(3))*(YC*2.) + 1.00001)                
      XLIM(1) = FLOAT(LJ)                                               
      XLIM(2) = 1.0                                                     
      XLIM(3) = FLOAT(LJ)
      XLIM(4) = 1.0                                                     
c     XLIM(4) = FLOAT(JHALF)
c     NPRL = ((XLIM(3)-XLIM(4))*(XL*2.) + 1.00001)                      
      NPRL = ((XLIM(3)-XLIM(4))*(XL) + 1.00001)                      
      YRG = YLIM(3)                                                     
      XRG = XLIM(3)                                                     
      IYD = 0                                                           
    8 CONTINUE                                                          
C     IYD = IYD + AMAX1(1. E+0,(YC + YC + .00001 E+0))             
      IYD = IYD + AMAX1(1. E+0,(YC * 4. + .00001 E+0))             
      IF(IYD.LE.4.OR.(NPRC/IYD).GT.20) GO TO 8                          
C     IXD = AMAX1(1. E+0,(XL + XL + .00001 E+0))                        
      IXD = AMAX1(1. E+0,(XL * 4. + .00001 E+0))                        
      NLV = 20                                                          
      IACROS = .TRUE.                                                   
      XREV  = .TRUE.                                                    
      YREV  = .FALSE.                                                   
      TICK  = .TRUE.                                                    
      ALLCON = .FALSE.                                                  
      IF( ZMIN.EQ.ZMAX )  RETURN                                        
      IF( ICT.GT.2 )  CON(2) = 1.0                                      
      CALL CONLEV(ICT,ZMAX,ZMIN)                                        
C     DO 3 I=1,20                                                       
C     YTITLE(I) = BLANK                                                 
C   3 CONTINUE                                                          
    4 CONTINUE                                                          
      NLV2   = NLV/2                                                    
      NLV2M1 = NLV2 - 1                                                 
C INTERNAL WRITE NOW FORTRAN STANDARD ???                            
      WRITE(UNIT=XFOR,FMT=5)
C     ENCODE(52,5,YFOR)                                                 
    5 FORMAT(52H( 21X,6A4,/,5X,3A4,5X,4A4,5X,7A4,//,  X,   30F  .1) )
      IF( .NOT. ALLCON )  GO TO 10                                      
      INC = ( 24 - NLV ) / 4                                            
      DO 6 I = 1,NLV2                                                   
      II = I + INC                                                      
      CSYM(I) = CSYMB(II)                                               
    6 CONTINUE                                                          
      GO TO 15                                                          
   10 CSYM(1)=CSYMB(7)                                                  
      DO 11 I=1,NLV2M1                                                  
      CSYM(I+1) = FSYMB(I)                                              
   11 CONTINUE                                                          
   15 XXXRG=XRG                                                         
      LZZ   = LID*LJ                                                    
      IZ1   = 1                                                         
      JZ1   = 1                                                         
      IF (IACROS) GO TO 20                                              
      IZD   = LID                                                       
      LIZ   = LJ                                                        
      IZM   = LZZ - LID + 1                                             
      JZD   = 1                                                         
      LJZ   = LI                                                        
      JZM   = LI                                                        
      GO TO 30                                                          
 20   IZD   = 1                                                         
      LIZ   = LI                                                        
      IZM   = LI                                                        
      JZD   = LID                                                       
      LJZ   = LJ                                                        
      JZM   = LZZ - LID + 1                                             
 30   IF (.NOT.YREV) GO TO 40                                           
      IZ1   = IZM                                                       
      IZD   =-IZD                                                       
 40   IF (.NOT.XREV) GO TO 50                                           
      JZ1   = JZM                                                       
      JZD   =-JZD                                                       
 50   CONTINUE                                                          
      UNLIN = FLOAT(NPRL-1)                                             
      XLO   =  XLIM(3)                                                  
      XDL   = (XLIM(4) - XLO) / UNLIN                                   
      XLOA  =  XLIM(1)                                                  
      XDLA  = (XLIM(2)  - XLOA) / FLOAT(LJZ-1)                          
      XLOB = AMIN1(XLIM(1),XLIM(2))                                     
      XHIB = AMAX1(XLIM(1),XLIM(2))                                     
      VNCOL = FLOAT(NPRC-1)                                             
      YLO   =  YLIM(3)                                                  
      YDL   = (YLIM(4) - YLIM(3)) / VNCOL                               
      YLOA  =  YLIM(1)                                                  
      YDLA  = (YLIM(2) - YLOA) / FLOAT(LIZ-1)                           
      YLOB = AMIN1(YLIM(1),YLIM(2))                                     
      YHIB = AMAX1(YLIM(1),YLIM(2))                                     
      NXAXIS = MIN0((NPRC+IYD-1)/IYD,20)                                
      ZLIM = ABS(YDL)*1. E-4                                            
      DO 60  J=1,NXAXIS                                                 
      XAXIS(J)= YRG + YDL*FLOAT(IYD*(J-1))                              
      IF(XAXIS(J).GT.YHIB)GO TO 61                                      
      IF(XAXIS(J).LT.YLOB) GO TO 61                                     
      IF ( ABS(XAXIS(J)) .LT. ZLIM)  XAXIS(J) = 0.                      
   60 CONTINUE                                                          
      GO TO 62                                                          
   61 NXAXIS=J-1                                                        
   62 YFOR(24) = FFSYMB(IYD)                                            
      YD=IYD                                                            
      YRGG=MOD(MOD((YRG-YLO)/YDL,YD)+YD+.5 E+0,YD)                      
      IYA=YRGG                                                          
      NSKS=IYA+1                                                        
      NSPACE = NSKS + 12 - IYD                                          
      IF(NSPACE.LE.0)GO TO 65                                           
      YFOR(19) = FFSYMB(NSPACE)                                         
      GO TO 66                                                          
   65 CONTINUE                                                          
      I=IYD+NSPACE                                                      
      YFOR(22) = CM3                                                    
      YFOR(21) = YFOR(25)                                               
      YFOR(20) = FFSYMB(I)                                              
      YFOR(19) = EFF                                                    
   66 WRITE(6,YFOR)(YTITLE(I),I=1,20),(XAXIS(J),J=1,NXAXIS)             
      NSK=IYD                                                           
      XD    = IXD                                                       
      XRG   = MOD(MOD((XRG-XLO)/XDL,XD)+XD+.5 E+0,XD)                   
      IXA   = XRG                                                       
      X  = XLO                                                          
      ZLIM = ABS(XDL)*1. E-4                                            
      ZMAX = -1. E+15                                                   
      ZMIN = +1. E+15                                                   
      DO 300 IL=1,NPRL                                                  
      XTIT = XBLK                                                       
      IF (IL.LE.30) XTIT = XTITLE(IL)                                   
      AXIS =  MOD(IL-1,IXD).EQ.IXA                                      
      FIL  = BLK                                                        
      END  = TICK.AND.(IL.EQ.1.OR.IL.EQ.NPRL)                           
      IF (AXIS.AND.(.NOT.TICK).OR.END) FIL = MIN                        
      IF(IL.EQ.1.OR.IL.EQ.NPRL)FIL=MIN                                  
      DO 80 IC=1,NPRC                                                   
      IQ(IC) = FIL                                                      
   80 CONTINUE                                                          
      FIL  = EYE                                                        
      IF (AXIS.OR.END)  FIL = PLS                                       
      IQ(1)=FIL                                                         
      IQ(NPRC)=FIL                                                      
      IF (TICK.AND.IL.GT.3.AND.IL.LT.NPRL-2) GO TO 91                   
      DO 90 IC=NSKS,NPRC,NSK                                            
      IQ(IC) = FIL                                                      
   90 CONTINUE                                                          
91    IF (.NOT.(TICK.AND.AXIS)) GO TO 110                               
      DO 100 IC=1,2                                                     
      JC = NPRC-IC                                                      
      IQ(IC+1) = MIN                                                    
      IQ(JC) = MIN                                                      
  100 CONTINUE                                                          
  110 IF (X.LT.XLOB.OR.X.GT.XHIB)    GO TO 250                          
      XA  = (X-XLOA)/XDLA                                               
      JZA = JZ1 + JZD*INT(XA)                                           
      JZB = MAX0(1,JZA+JZD)                                             
      XAD = MOD(XA,1. E+0)                                              
      Y  = YLO                                                          
C     WRITE(6,12345)IL,JZA,JZB,JZD,XA,XAD                               
      DO 200  IC=1,NPRC                                                 
      IF (Y.LT.YLOB.OR.Y.GT.YHIB) GO TO 199                             
      YA  = (Y-YLOA)/YDLA                                               
      IZA = IZ1 + IZD*INT(YA)                                           
      IZB = IZA + IZD                                                   
      YAD = MOD(YA,1. E+0)                                              
      ZAA = ZZ(IZA,JZA)                                                 
      ZAB = ZZ(IZA,JZB)                                                 
      ZBA = ZZ(IZB,JZA)                                                 
      ZBB = ZZ(IZB,JZB)                                                 
      Z1  = ZAA + (ZAB-ZAA)*XAD                                         
      Z2  = ZBA + (ZBB-ZBA)*XAD                                         
      Z   = Z1 + (Z2-Z1)*YAD                                            
C     IF(IL.EQ.NPRL)WRITE(6,12345)IZA,IZB,YA,YAD                        
C2345 FORMAT(6G15.5)                                                    
C     IF(IL.EQ.NPRL)WRITE(6,12345)ZAA,ZAB,ZBA,ZBB                       
C     IF(IL.EQ.NPRL)WRITE(6,12345)Z,Z1,Z2,ZMIN                          
      ZMAX = AMAX1(ZMAX,Z)                                              
      ZMIN = AMIN1(ZMIN,Z)                                              
      IF (.NOT.ALLCON) GO TO 160                                        
      IF(Z.LT.CON(1))GO TO 180                                          
      IF(Z.GT.CON(NLV))GO TO 190                                        
      DO 150 I=1,NLV,2                                                  
      IF (Z.LT.CON(I)) GO TO 199                                        
      IF (Z.GT.CON(I+1)) GO TO 150                                      
      IS = (I+1)/2                                                      
      GO TO 170                                                         
 150  CONTINUE                                                          
      GO TO 199                                                         
 160  IF (Z.LT.CON(1)) GO TO 180                                        
      I  = (Z-CON(1))/CON(2)                                            
      IF (MOD(I,2).EQ.1) GO TO 199                                      
      IS = MOD(I/2,NLV2)+1                                              
 170  IQ(IC) = CSYM(IS)                                                 
      GO TO 199                                                         
  180 IQ(IC) = IUNDR                                                    
      GO TO 199                                                         
  190 IQ(IC) = IOVER                                                    
  199 Y = Y + YDL                                                       
  200 CONTINUE                                                          
 250  IF (.NOT.AXIS) WRITE (6,260) HTIT,    (IQ(I),I=1,NPRC)            
  260 FORMAT(1X, A1, 10X, 121A1 )                                       
      ZX = X                                                            
      IF (ABS(ZX).LT.ZLIM) ZX = 0.                                      
      IF (AXIS)      WRITE (6,270) HTIT,ZX, (IQ(I),I=1,NPRC)            
  270 FORMAT(1X, A1, G10.4, 121A1 )                                     
      X = X + XDL                                                       
  300 CONTINUE                                                          
      IF( NPRL .LE. 75 )  WRITE(6,29) ZMIN, ZMAX                        
      LPRL=NPRL+7                                                       
      IF (LPRL.GT.78) LPRL = 3                                          
      IF(LPRL.EQ.3)WRITE(6,28)ZMIN,ZMAX                                 
   28 FORMAT(1H1,/,7X,'MIN=',E11.4,4X,'MAX=',E11.4,/)                   
   29 FORMAT(/,7X,'MIN=',E11.4,4X,'MAX=',E11.4,/)                       
      IF(ALLCON)GO TO 310                                               
      CON(11)=CON(1)                                                    
      IF(ZMIN.LE.CON(1))GO TO 47                                        
   46 CON(13)=CON(11)+2.*CON(2)                                         
      IF(ZMIN.LE.CON(13).AND.ZMIN.GE.CON(11))GO TO 47                   
      CON(11)=CON(13)                                                   
      GO TO 46                                                          
   47 Z=CON(11)+0.5*CON(2)                                              
      DO 55 M = 1,4                                                     
      DO 45 L=12,16                                                     
      CON(L) = CON(L-1) + CON(2)                                        
   45 CONTINUE                                                          
      I  = (Z-CON(1))/CON(2)                                            
      IS = MOD(I/2,NLV2)+1                                              
      C1=CSYM(IS)                                                       
      Z=Z+CON(2)+CON(2)                                                 
      I  = (Z-CON(1))/CON(2)                                            
      IS = MOD(I/2,NLV2)+1                                              
      C2=CSYM(IS)                                                       
      Z=Z+CON(2)+CON(2)                                                 
      I  = (Z-CON(1))/CON(2)                                            
      IS = MOD(I/2,NLV2)+1                                              
      C3=CSYM(IS)                                                       
      Z=Z+CON(2)+CON(2)                                                 
      WRITE(6,34)                                                       
     + CON(11),C1,CON(12),CON(13),C2,CON(14),CON(15),C3,CON(16)         
      LPRL=LPRL+1                                                       
   34 FORMAT(    E12.4,4('  *',A1,'*',2E12.4) )                         
   35 FORMAT(1H0,E12.4,4('  *',A1,'*',2E12.4) )                         
      IF(CON(16).GT.ZMAX)GO TO 320                                      
      CON(11) = CON(16) + CON(2)                                        
   55 CONTINUE                                                          
      GO TO 320                                                         
  310 IC = NLV / 4 - 2                                                  
      GO TO (311,312,313,314), IC                                       
  311 WRITE(6,34)CON(1),CSYM(1),CON(2),CON(3),CSYM(2),CON(4),CON(5),    
     + CSYM(3),CON(6)                                                   
      WRITE(6,35)CON(7),CSYM(4),CON(8),CON(9),CSYM(5),CON(10),CON(11),  
     + CSYM(6),CON(12)                                                  
      GO TO 315                                                         
  312 WRITE(6,34)CON(1),CSYM(1),CON(2),CON(3),CSYM(2),CON(4),CON(5),    
     + CSYM(3),CON(6),CON(7),CSYM(4),CON(8)                             
      WRITE(6,35)CON(9),CSYM(5),CON(10),CON(11),CSYM(6),CON(12),CON(13),
     + CSYM(7),CON(14),CON(15),CSYM(8),CON(16)                          
      GO TO 315                                                         
  313 WRITE(6,34)CON(1),CSYM(1),CON(2),CON(3),CSYM(2),CON(4),CON(5),    
     + CSYM(3),CON(6),CON(7),CSYM(4),CON(8)                             
      WRITE(6,34)CON(9),CSYM(5),CON(10),CON(11),CSYM(6),CON(12),CON(13),
     + CSYM(7),CON(14),CON(15),CSYM(8),CON(16)                          
      WRITE(6,34)CON(17),CSYM(9),CON(18),CON(19),CSYM(10),CON(20)       
      GO TO 315                                                         
  314 WRITE(6,34)CON(1),CSYM(1),CON(2),CON(3),CSYM(2),CON(4),CON(5),    
     + CSYM(3),CON(6)                                                   
      WRITE(6,34)CON(7),CSYM(4),CON(8),CON(9),CSYM(5),CON(10),CON(11),  
     + CSYM(6),CON(12)                                                  
      WRITE(6,34)CON(13),CSYM(7),CON(14),CON(15),CSYM(8),CON(16),       
     + CON(17),CSYM(9),CON(18)                                          
      WRITE(6,34)CON(19),CSYM(10),CON(20),CON(21),CSYM(11),CON(22),     
     + CON(23),CSYM(12),CON(24)                                         
      LPRL = LPRL + 4                                                   
      GO TO 320                                                         
  315 LPRL = LPRL + 3                                                   
  320 XRG=XXXRG                                                         
      ALLCON=.FALSE.                                                    
      NLV=20                                                            
      IF( LPRL - 82 )  323,325,324                                      
  323 LP = 5                                                            
      LJJ = XDL * UNLIN / XDLA + 1.49999                                
      IF( LPRL .GE. 82 - LJJ - LJJ - 9 )  LP = 82 - LPRL                
  324 DO 322 L = 1,LP                                                   
      WRITE(6,321)                                                      
  322 CONTINUE                                                          
  321 FORMAT(1H )                                                       
  325 RETURN                                                            
      END                                                               
