      SUBROUTINE TJANAL                                                 
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    TJANAL      DECODE OBS DATA/OBJ ANALYSIS               
C   PRGMMR:  R. M. REAP      ORG: W/OSD21             DATE: 96-03-06    
C                                                                       
C ABSTRACT:  DECODES UPPER-AIR AND SURFACE REPORTS/ DOES OBJ ANALYSIS   
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   67-07-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   96-03-06  CONVERT TO CRAY BUFR FORMAT                               
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM TJANAL                                                    
C        MAR 1996         R. M. REAP          MDL           CRAY        
C        PURPOSE                                                        
C        DECODES UPPER-AIR AND SURFACE REPORTS FROM NCEP DATA SETS.     
C        PERFORMS OBJECTIVE ANALYSIS OF INITIAL TEMPERATURE AND MOISTURE
C        FIELDS AT TRAJECTORY ORIGIN POINTS BY A COMBINATION WEIGHTED-  
C        AVERAGING AND SURFACE-FITTING METHOD USING ALL AVAILABLE UPPER-
C        AIR AND SURFACE DATA.                                          
C        DATA SET USE                                                   
C           FT81 (INPUT)                                                
C        COMMON BLOCKS                                                  
C           KDMY,BLOCKA,BLOCKB,BLOCKD,BLOCKE                            
C        SUBPROGRAMS CALLED                                             
C           RADATA,DATA,SFLAND,OBTEMP,OBDEWP                            
C         LIBRARY                                                       
C           COMMON,W3LIB,TDLLIB                                          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCKA/TEMP(13,17,4),DEWPT(13,17,4)                        
      COMMON/BLOCKB/SFC(900,4),STAX(900),STAY(900),JSTA(900),JBLK(900), 
     1 KSTAP                                                         
      COMMON/BLOCKD/KU(26,33,5),KV(26,33,5)                             
      COMMON/BLOCKE/TERRA(51,65),SFTM(7),SFDP(7),DLPS(7),DIST(7),       
     1KSFC(7),PSFC(7)                                                   
      DIMENSION RPRES(150,40),RTEMP(150,40),RDEWPT(150,40),             
     1 STA(150),BLK(150),KSP(150),BUF(13,17,4)                       
C     ****************************************************************  
      MXNR=NSTAP                                                        
C        READ INITIAL (00 HR) U,V,W WINDS FROM DISK                     
      KST=0                                                             
      NRCD=1                                                            
      READ(81,REC=NRCD,ERR=52)(((KU(I,J,N),N=1,5),J=1,33),I=1,26)       
      GO TO 54                                                          
  52  KST=1                                                             
  54  NRCD=2                                                            
      READ(81,REC=NRCD,ERR=60)(((KV(I,J,N),N=1,5),J=1,33),I=1,26)       
      IF(KST)60,64,60                                                   
  60  PRINT 62                                                          
  62  FORMAT(10X,'DISK READ PROBLEM -- INITIAL U,V,W WINDS',//)         
  64  CALL RADATA(RPRES,RTEMP,RDEWPT,STA,BLK)             
      CALL SFLAND                                                       
      CALL SFSHIP 
      CALL OBTEMP(RPRES,RTEMP,RDEWPT,STA,BLK,KSP)                       
      CALL OBDEWP(RPRES,RTEMP,RDEWPT,STA,BLK,KSP)                       
      DO 150 K=1,2                                                      
      DO 96 I=1,13                                                      
      DO 96 J=1,17                                                      
      DO 96 N=1,3                                                       
      GO TO(90,92),K                                                    
  90  BUF(I,J,N)=TEMP(I,J,N)                                            
      GO TO 96                                                          
  92  BUF(I,J,N)=DEWPT(I,J,N)                                           
  96  CONTINUE                                                          
      DO 150 I=1,13                                                     
      DO 150 J=1,17                                                     
      DO 150 N=1,3                                                      
      ITAB=4                                                            
      KNT=1                                                             
      TOT=0.0                                                           
      IF(BUF(I,J,N)-9999.)150,100,150                                   
  100 PRINT 101,K,I,J,N                                                 
  101 FORMAT(//,5X,'DATA MISSING TYPE',I2,'   GRID POINT ',3I3)         
      KYP1=I+KNT                                                        
      IF(KYP1-13)104,104,102                                            
  102 ITAB=ITAB-1                                                       
      GO TO 110                                                         
  104 IF(BUF(KYP1,J,N)-9999.)106,108,106                                
  106 TOT=TOT+BUF(KYP1,J,N)                                             
      GO TO 110                                                         
  108 ITAB=ITAB-1                                                       
  110 KYM1=I-KNT                                                        
      IF(KYM1-1)112,114,114                                             
  112 ITAB=ITAB-1                                                       
      GO TO 120                                                         
  114 IF(BUF(KYM1,J,N)-9999.)116,118,116                                
  116 TOT=TOT+BUF(KYM1,J,N)                                             
      GO TO 120                                                         
  118 ITAB=ITAB-1                                                       
  120 KXP1=J+KNT                                                        
      IF(KXP1-17)124,124,122                                            
  122 ITAB=ITAB-1                                                       
      GO TO 130                                                         
  124 IF(BUF(I,KXP1,N)-9999.)126,128,126                                
  126 TOT=TOT+BUF(I,KXP1,N)                                             
      GO TO 130                                                         
  128 ITAB=ITAB-1                                                       
  130 KXM1=J-KNT                                                        
      IF(KXM1-1)132,134,134                                             
  132 ITAB=ITAB-1                                                       
      GO TO 140                                                         
  134 IF(BUF(I,KXM1,N)-9999.)136,138,136                                
  136 TOT=TOT+BUF(I,KXM1,N)                                             
      GO TO 140                                                         
  138 ITAB=ITAB-1                                                       
  140 IF(ITAB)142,142,144                                               
  142 KNT=KNT+1                                                         
      ITAB=4                                                            
      GO TO 100                                                         
  144 TAB=ITAB                                                          
      AVG=TOT/TAB                                                       
      GO TO(146,148),K                                                  
  146 TEMP(I,J,N)=AVG                                                   
      GO TO 150                                                         
  148 DEWPT(I,J,N)=AVG                                                  
  150 CONTINUE                                                          
      DO 160 I=1,13                                                     
      DO 160 J=1,17                                                     
      DO 160 N=1,3                                                      
      PLX(I,J,N)=TEMP(I,J,N)                                            
      PLY(I,J,N)=DEWPT(I,J,N)                                           
  160 CONTINUE                                                          
      AI=MXNR                                                           
      WRITE(6,170)
  170 FORMAT(/,1X,'END PROGRAM TJANAL')
      RETURN                                                            
      END                                                               
