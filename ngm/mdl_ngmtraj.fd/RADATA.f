      SUBROUTINE RADATA(RPRES,RTEMP,RDEWPT,STA,BLK)                     
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    RADATA      DECODES UPPER-AIR/SFC DATA                 
C   PRGMMR:  R. M. REAP      ORG: W/OSD21           DATE: 96-03-06      
C                                                                       
C ABSTRACT:  DECODES UPPER-AIR DATA FROM NCEP DATA SETS                
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   71-02-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   96-03-06  CONVERT TO CRAY BUFR FORMAT                               
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM RADATA                                                    
C        MAR 1996       R. M. REAP          MDL            CRAY         
C        PURPOSE                                                        
C        DECODES UPPER-AIR REPORTS FROM NCEP ADPUPA DATA SET FORMATTED  
C        IN WMO BUFR FORMAT. MANDATORY AND SIGNIFICANT LEVEL ARE MESHED 
C        INTO A CONTINUOUS SOUNDING.                                 
C                                                                       
C        DATA SET USE                                                   
C           KUPA                                                        
C        COMMON BLOCKS                                                  
C           BLANK,BLOCKH,BLOCKP                                      
C        SUBPROGRAMS CALLED:                                            
C           OPENBF,DATELEN,READMG,UPDATE,READSB,UFBINT,RDADP,
C           SORT1,CLOSBF                                                
C         LIBRARY                                                       
C           COMMON,W3LIB,TLDLIB                                         
C        PROGRAM STOPS                                                  
C           #320  MODEL & UPPER-AIR DATE/TIME GROUPS DO NOT AGREE       
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR,KSFC,KSHP,KUPA           
      COMMON/BLOCKP/KPRINT,KERR                                         
      COMMON/BLOCKH/JFL                                            
      DIMENSION RPRES(150,40),RTEMP(150,40),RDEWPT(150,40),STA(150),    
     1 BLK(150)        
      DIMENSION LIST(40),JPOS(40),XPRES(40),XTEMP(40),XDEWP(40)       
      CHARACTER*8 SUBSET,INOUT
      CHARACTER*80 STRING 
      REAL*8 ARR(5,255),BARR(4,255)  
C     ****************************************************************  
C        INITIALIZE SOUNDING ARRAYS
      DO 30 I=1,150                                                     
      DO 30 J=1,40                                                      
      RPRES(I,J)=0.0                                                    
      RTEMP(I,J)=0.0                                                    
      RDEWPT(I,J)=0.0                                                   
   30 CONTINUE                                                          
      NSTAP=0                                                           
      NSTOT=0
C        OPEN/INITIAL READ UPPER-AIR BUFR FILE  
      INOUT='IN'
      CALL OPENBF(KUPA,INOUT,KUPA)
      CALL DATELEN(10)
      CALL READMG(KUPA,SUBSET,IDATE,IEOF)
C        FORM INITIAL DATE/TIME GROUP
      JDT=(IDATE/100)*100
      JDF=IDATE-JDT 
      IF(JDF.GE.8.AND.JDF.LE.16) GO TO 32
      IF(JDF.GE.20) GO TO 34
      IF(JDF.LE.4) JDATE=JDT
      GO TO 36 
   32 JDATE=JDT+12   
      GO TO 36
   34 JDD=24-JDF
      CALL UPDATE(IDATE,JDATE,JDD)
C        CHECK MODEL AND UPPER-AIR DATE-TIME GROUP FOR AGREEMENT        
C        JFL = 00Z OR 12Z FROM NCEP DATA                                
C      JDATE = INITIAL DATE/TIME OF UPPER-AIR REPORTS                   
C       JDTH = INITIAL DATE/TIME OF NCEP MODEL DATA
   36 JTAU=0                                                            
      CALL UPDATE(KDMY,JDTH,JTAU)                                       
      JFL=JDTH-(JDTH/100)*100                                           
      IF(JDTH-JDATE)40,80,40                                            
   40 WRITE(6,50) JDTH,JDATE                                            
   50 FORMAT(//,'MODEL/UPPER-AIR DATE-TIME DONT AGREE, EXIT 50',    
     1 /,5X,'MODEL = ',I10,5X,'UPPER-AIR = ',I10,/)
      KERR=50 
      CALL EXIT(KERR)
C        START MAIN LOOP TO PROCESS UPPER-AIR REPORTS  
      IRET=0 
   80 CALL READSB(KUPA,IRET)
      IF(IRET.EQ.0) GO TO 84      
      CALL READMG(KUPA,SUBSET,IDATE,IEOF)
      IF(IEOF.EQ.0) GO TO 80 
      GO TO 200
   84 NSTOT=NSTOT+1
      STRING=' WMOB WMOS CLAT CLON HOUR '
      CALL UFBINT(KUPA,ARR,5,255,ISEQ,STRING)
      STRING=' PRLC TMDB TMDP VSIG ' 
      CALL UFBINT(KUPA,BARR,4,255,ISEQ,STRING)
C        VSIG=1**2 (2) FOR WIND DIR & SPEED   
C            =2**2 (4) FOR SIG LEVEL PRESSURE, TEMP, & DEW POINT
C            =3**2 (8) FOR LEVEL OF MAX WIND     
C            =4**2 (16) FOR TROPOPAUSE LEVEL
C            =5**2 (32) FOR MANDATORY LEVEL DATA
C            =6**2 (64) FOR SURFACE DATA    
C            =10 RAISED TO 12TH POWER FOR END OF DATA
C     CHECK FOR REPORT WITHIN 26X33 MDL GRID AND 2 HOURS OF CYCLE TIME  
      CLAT=ARR(3,1) 
      CLON=ARR(4,1)
      IHR=ARR(5,1) 
      CALL RDADP(CLAT,CLON,IHR,KGO)                                     
      IF(KGO)80,86,80                                                   
   86 NSTAP=NSTAP+1
      YCORD(NSTAP)=AI                                                   
      XCORD(NSTAP)=AJ                                                   
      BLK(NSTAP)=ARR(1,1)                                               
      STA(NSTAP)=ARR(2,1)                                               
C        READ PRESSURE, TEMP, AND DEW POINT FOR EACH SOUNDING 
      J=1
      DO 96 KK=1,ISEQ  
      IF(J.GT.40) GO TO 96
      IVSIG=BARR(4,KK)
      IF(IVSIG.EQ.2) GO TO 96
      IF(IVSIG.EQ.8) GO TO 96
      IF(IVSIG.EQ.16) GO TO 96
      IF(IVSIG.GT.100000000) GO TO 96
      PRS=BARR(1,KK)*0.01
      IF(PRS.LT.100.0) GO TO 96 
      IF(PRS.GT.100000000) GO TO 96     
      RPRES(NSTAP,J)=PRS          
      TEM=BARR(2,KK)-273.15
      IF(TEM.GT.100000000) GO TO 96      
      RTEMP(NSTAP,J)=TEM 
      DWP=BARR(3,KK)-273.15 
      IF(DWP.GT.100000000) DWP=9999.   
      RDEWPT(NSTAP,J)=DWP    
      J=J+1 
   96 CONTINUE  
C        CHECK FOR ALL MISSING DATA OR PARTIAL SOUNDING 
C      IF(RPRES(NSTAP,1).GE.700.0) GO TO 98 
C      NSTAP=NSTAP-1
C      GO TO 80
C        SORT SOUNDING BY DECREASING PRESSURE
   98 DO 100 J=1,40 
      LIST(J)=RPRES(NSTAP,J) 	
  100 CONTINUE
      KNM=40  
      CALL SORT1(LIST,JPOS,KNM)     
      DO 180 K=1,40                            
      KK=41-K 
      IDX=JPOS(K) 
      XPRES(KK)=RPRES(NSTAP,IDX)                                      
      XTEMP(KK)=RTEMP(NSTAP,IDX) 
      XDEWP(KK)=RDEWPT(NSTAP,IDX)   
  180 CONTINUE
C        ELIMINATE REDUNDANT LEVELS
      KK=0
      DO 190 K=1,40
      KK=KK+1 
      IF(K.EQ.1) GO TO 183
      IF(XPRES(K-1).EQ.XPRES(K)) KK=KK-1       
  183 RPRES(NSTAP,KK)=XPRES(K)
      RTEMP(NSTAP,KK)=XTEMP(K)
      RDEWPT(NSTAP,KK)=XDEWP(K)
  190 CONTINUE
      DO 192 K=KK+1,40
      RPRES(NSTAP,K)=0.0
      RTEMP(NSTAP,K)=0.0 
      RDEWPT(NSTAP,K)=0.0 
  192 CONTINUE
      GO TO 80   
C        END SORT
  200 IF(NSTAP-75)210,210,220                                           
  210 PRINT 212,NSTAP,JDATE,NSTOT                                       
  212 FORMAT(1X,'KUPA END-OF-FILE,  ONLY ',I3,' STATIONS PROCESSED FOR '
     1 ,I10,' OUT OF ',I3,' STATIONS',//)   
      GO TO 230    
  220 PRINT 222,NSTAP,JDATE                                             
  222 FORMAT(//,1X,'KUPA END-OF-FILE.',I5,1X,'STATIONS PROCESSED FOR ', 
     1 I10,//)
  230 CALL CLOSBF(KUPA) 
      RETURN   
      END
