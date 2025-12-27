      SUBROUTINE SFSHIP                                                 
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    SFSHIP      DECODES SURFACE SYNOPTIC (SHIP) REPORTS    
C   PRGMMR: R. M. REAP       ORG: W/OSD21         DATE: 96-03-19        
C                                                                       
C ABSTRACT:  DECODES SURFACE SHIP OBSERVATIONS FROM KSHP DATA SET       
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   71-04-01  R. M. REAP                                                
C   74-03-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   96-03-19  CONVERT TO CRAY BUFR FORMAT                               
C   99-09-30  J. P. DALLAVALLE
C                - MODIFIED TWO WRITE STATEMENTS FOR CONVERSION
C                  TO IBM SP 
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM SFSHIP                                                    
C        MAR 1996        R. M. REAP         MDL         CRAY            
C        PURPOSE                                                        
C        DECODES SURFACE SHIP REPORTS FROM KSHP DATA SET                
C        DATA SET USE                                                   
C           KSHP                                                        
C        VARIABLES                                                      
C        ....OBSERVED VARIABLES...............                          
C        SFC(I,1)--STATION PRESSURE (MB)                                
C        SFC(I,2)--TEMPERATURE (DEG C)                                  
C        SFC(I,3)--DEW POINT (DEG C)                                    
C        SFC(I,4)--PRESENT WEATHER (WW)                                 
C        COMMON BLOCKS                                                  
C           KDMY,BLOCKB,BLOCKH                                          
C        SUBPROGRAMS CALLED:                                            
C           OPENBF,READMG,UPDATE,READSB,UFBINT,RDADP,CLOSBF                                         
C         LIBRARY                                                       
C           COMMON,W3LIB,TDLLIB                                          
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
      COMMON/BLOCKH/JFL                                            
      COMMON/BLOCKP/KPRINT,KERR                                         
      CHARACTER*8 SUBSET
      CHARACTER*80 STRING 
      REAL*8 ARR(3,255),BARR(4,255) 
C     ****************************************************************  
      JSTAP=KSTAP+1
      MPRINT=0
      NSTOT=0                                                           
C        OPEN/INITIAL READ SFC SHIP BUFR FILE  
      CALL OPENBF(KSHP,'IN',KSHP)
      CALL READMG(KSHP,SUBSET,IDATE,IRET)
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
C        CHECK MODEL AND SFC SHIP DATE-TIME GROUP FOR AGREEMENT        
C        JFL = 00Z OR 12Z FROM NCEP DATA                                
C      JDATE = INITIAL DATE/TIME OF SFC SHIP REPORTS                    
C       JDTH = INITIAL DATE/TIME OF NCEP MODEL DATA
   36 JTAU=0                                                            
      CALL UPDATE(KDMY,JDTH,JTAU)                                       
      JFL=JDTH-(JDTH/100)*100                                           
      IF(JDTH-JDATE)40,80,40                                            
   40 WRITE(6,70) JDTH,JDATE                                            
   70 FORMAT(//,1X,'MODEL AND SFC SHIP DATE/TIME DONT AGREE--CONTINUE', 
     1 2I11,/)
      KERR=70
      RETURN 
C        START MAIN LOOP TO PROCESS SFC SHIP REPORTS  
   80 CALL READSB(KSHP,IRET)
      IF(IRET.EQ.0) GO TO 84      
      CALL READMG(KSHP,SUBSET,IDATE,IEOF)
      IF(IEOF.EQ.0) GO TO 80 
      GO TO 200
   84 NSTOT=NSTOT+1
      STRING=' CLAT CLON HOUR ' 
      CALL UFBINT(KSHP,ARR,3,255,IRET1,STRING)
      STRING=' PMSL TMDB TMDP PRWE ' 
      CALL UFBINT(KSHP,BARR,4,255,IRET2,STRING) 
C        CHECK FOR REPORT WITHIN 26X33 MDL GRID AND 2 HOURS OF CYCLE TIM
      CLAT=ARR(1,1) 
      CLON=ARR(2,1)
      IHR=ARR(3,1) 
      IF(CLAT.LT.0.OR.CLAT.GT.88) GO TO 80
      IF(CLON.LT.-180.OR.CLON.GT.180) GO TO 80 
      CALL RDADP(CLAT,CLON,IHR,KGO)                                     
      IF(KGO)80,92,80                                                   
C        TEST FOR SURFACE SHIP REPORTS WITHIN SUBSET OF 26X33 GRID      
   92 IF(AI-4.0)2226,2226,2220                                          
 2220 IF(AI-22.0)2222,2226,2226                                         
 2222 IF(AJ-9.0)2226,2226,2224                                          
 2224 IF(AJ-21.0)2240,2226,2226                                         
 2226 IF(AI-2.0)80,80,2228                                              
 2228 IF(AI-19.0)2230,80,80                                             
 2230 IF(AJ-21.0)80,80,2232                                             
 2232 IF(AJ-32.0)2240,80,80                                             
 2240 KSTAP=KSTAP+1
      IF(KSTAP.LE.899) GO TO 2244
      WRITE(6,2242)
 2242 FORMAT(5X,'SFSHIP RETURN--SFC LAND AND SHIP REPORTS EXCEED 899')
      GO TO 200
 2244 STAY(KSTAP)=AI                                                    
      STAX(KSTAP)=AJ                                                    
      IF(MPRINT.LE.1) GO TO 100 
      WRITE(6,86) NSTOT,(ARR(I,1),I=1,3)
   86 FORMAT(1X,I4,2X,2F9.2,1X,F6.2)   
      WRITE(6,88) (BARR(I,1),I=1,4)
   88 FORMAT(21X,'CHECK = ',F9.2,2X,F7.2,2X,F7.2,F5.0)   
      WRITE(6,89) STAX(KSTAP),STAY(KSTAP) 
   89 FORMAT(3X,'STAX,STAY, = ',2F15.2) 
C        READ PRESSURE, TEMPERATURE, DEW POINT, & PRESENT WEATHER
  100 AMAX=100000000 
      PRS=BARR(1,1)*0.01
      IF(PRS.GT.AMAX) PRS=9999.
      SFC(KSTAP,1)=PRS          
      TEM=BARR(2,1)-273.15
      IF(TEM.GT.AMAX) TEM=9999.     
      SFC(KSTAP,2)=TEM    
      DWP=BARR(3,1)-273.15 
      IF(DWP.GT.AMAX) DWP=9999.   
      SFC(KSTAP,3)=DWP    
      PRW=BARR(4,1)
      IF(PRW.GT.AMAX) PRW=9999.
      SFC(KSTAP,4)=PRW	   
      GO TO 80 
  200 KTS=KSTAP-JSTAP 
      PRINT 210,KTS,JDATE,NSTOT                                         
  210 FORMAT(//,1X,'KSHP END OF FILE.',I5,1X,'STATIONS PROCESSED FOR ', 
     1 I10,' OUT OF ',I3,//)
      DO 220 I=JSTAP,KSTAP
      DO 220 J=1,4  
      IF(SFC(I,J).GT.100000000) SFC(I,J)=9999.
  220 CONTINUE
      IF(MPRINT.EQ.0) GO TO 270
      WRITE(6,230) 
  230 FORMAT(12X,'I',5X,'J',4X,'PRES',5X,'TEMP',
     1 4X,'DEWP',3X,'PRWX',/)
      DO 260 I=JSTAP,KSTAP                                              
      WRITE(6,240) STAY(I),STAX(I),(SFC(I,J),J=1,4)      
  240 FORMAT(9X,F5.1,1X,F5.1,4F8.1)                                
  260 CONTINUE                                                          
  270 CALL CLOSBF(KSHP)
      WRITE(6,300)
  300 FORMAT(/,1X,'END PROGRAM SFSHIP')  
      RETURN   
      END
