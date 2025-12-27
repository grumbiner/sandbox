      SUBROUTINE NTRNS                                                  
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    NTRNS       WRITES NGM FIELDS TO DISK 84               
C   PRGMMR:  R. M. REAP      ORG: W/OSD21          DATE: 98-05-29    
C                                                                       
C ABSTRACT:  SELECTS AND WRITES NGM FORECAST FIELDS TO DISKS 84 & 85    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   87-11-01  R. M. REAP                                                
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   94-09-14  INCREASE RECORD COUNT TO 340 FOR JTAB, JTABX ARRAYS       
C             AND 125 DO LOOP                                           
C   95-03-15  INCREASE RECORD COUNT TO 400 FOR JTAB, JTABX ARRAYS       
C             AND 125 DO LOOP                                           
C   95-11-01  CONVERT TO CRAY                                           
C   96-02-26  CONVERT TO GRIB FORMAT FOR NCEP INPUT FIELDS
C   98-05-29  CONVERT TO YEAR 2000 COMPLIANT
C   00-03-10  J.P. DALLAVALLE
C             CONVERTED TO RUN ON THE IBM-SP; MODIFIED ARRAYS TO CONFORM
C             TO GETGB CONVENTIONS; ADDED CALLS TO BAOPEN TO OPEN GRIB
C             INDEX AND DATA FILES
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C    PROGRAM NTRNS                                                      
C          MAY 1998        R. M. REAP          MDL          CRAY        
C        PURPOSE                                                        
C        SUBROUTINE TO SELECT, MODIFY, AND TRANSFER DESIRED RECORDS     
C        FROM NGM FORECAST FIELDS ON DISK TO LOCAL DISK STORAGE         
C        BLOCK COMMON AND VARIABLES.............................        
C         JTAB(N,1)=THE PRESSURE LEVEL IN 4 BIT BCD                     
C         JTAB(N,2)=THE DATA TYPE IN 4 BIT BCD                          
C         JTAB(N,3)=TAU IN BINARY                                       
C         JTAB(N,4)=THE LOGICAL RECORD NUMBER ON LOCAL DISK STORAGE     
C                   WHERE THIS RECORD IS STORED                         
C         JTAB(N,5)= 0 IF MOS VARIABLE TO BE ARCHIVED                   
C             "    = 1 OTHERWISE                                        
C        JTABX(25,N)=HOLDS 25-WORD INTEGER PDS ARRAY         
C        INCR=TOTAL NUMBER OF FIELDS READ FROM NGM DATA SET             
C        BUF2(2385)=UNPACKED RECORD (BY GETGB) FROM NGM DATA SET        
C        GBUF(65160)=ARRAY IDENTICAL IN FUNCTION TO BUF2 BUT            
C                    USED ONLY FOR SEA-SFC TEMPS           
C
C          FIELDS MUST BE ORDERED BY HOUR, I.E. 00-,06-,12-HR.          
C          RECORDS ARE STORED ON DISC IN SAME ORDER AS ENTRIES          
C          IN JTAB.                                                     
C        COMMON BLOCKS                                                  
C              KDMY,BLOCK3                                              
C        SUBPROGRAMS CALLED:                                            
C              GETDATE,GETGB,INSST,NGMGRD                        
C          LIBRARY:                                                     
C              COMMON,W3LIB,TDLLIB                                       
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE:  FORTRAN 90                                               
C   MACHINE:   CRAY                                                     
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCK3/JTAB,JTABX,KEY                                      
      COMMON/BLOCKP/KPRINT,KERR                                         
      DIMENSION BUF(9999),BUF2(2385),JTAB(400,5),JTABX(25,400),
     1 IPDS(200),
     2 IGDS(200),KPDS(200),KGDS(200),A(400),C(400),AA(26,33),AP(858),      
     3 D(45,53),MAP(2385),SST(65,65),IDAT(8)              
      EQUIVALENCE (AA(1,1),AP(1)),(BUF(1),BUF2(1))
      CHARACTER*80 FILEINDX,FILEGRIB
      CHARACTER*11 ENVVAR              
      LOGICAL*1 BMP(9999) 
      DATA IGDS/200*0/  
C        *************************************************************  
      INCR=0                                                            
      MAXPTS=9999
      KNUM=0 
C        INITIALIZE JTABX ARRAY 
      DO 90 K=1,25
      DO 90 J=1,400
      JTABX(K,J)=-1
  90  CONTINUE
C        READ CYCLE DATE/TIME FROM NCEP DATE FILE 
      CALL GETDATE(49,IYR,IMO,IDA,IHR,NDATE,IERR)
      KDMY=NDATE 
      IF(IERR.EQ.0) GO TO 96
      WRITE(6,94) IERR
  94  FORMAT(5X,'PROBLEM READING NCEP DATE FILE--NTRNS, IERR = ',I1)
  96  DO 500 J=1,400                                                    
      READ (5,100) JTAB(J,1),JTAB(J,2),JTAB(J,3),JTAB(J,5),       
     1 (JTABX(K,J),K=1,14),JTABX(21,J),A(J),C(J)                     
 100  FORMAT(3I4,I2,(3I3,I4,I3,I4,I6,7I3),I3,F7.2,F9.3)                 
C        INSERT CURRENT DATE/TIME IN PDS ARRAY
C        NOTE THAT THE CENTURY VALUE IN THE PDS IS NOT SET FOR
C        INPUT
      KYR=IYR-((IYR/100)*100)   
      IF(KYR.EQ.00) KYR=100
      JTABX(8,J)=KYR
      JTABX(9,J)=IMO
      JTABX(10,J)=IDA
      JTABX(11,J)=IHR
C     WRITE(6,91) IYR,IMO,IDA,IHR,KDMY,JTABX(8,J)
C 91  FORMAT(2X,'IYR,IMO,IDA,IHR,KDMY,JTABX(8,J)=',/,2X
C    1 4I6,I10,I6)
      JTAB(J,4)=0                                                       
      IF(JTAB(J,1))510,510,104                                          
 104  INCR=INCR+1                                                       
      KHR=JTAB(J,3)                                                     
      JFIL=(KHR/6)+11                                                   
      INDX=(KHR/6)+21 
      DO 110 I=1,25
      IPDS(I)=JTABX(I,J) 
 110  CONTINUE
C        ADDED CALLS TO GETENV, BAOPEN TO ALLOW READING
C        FILENAMES AND TO ACCESS GRIB FILES PROPERLY
C        FIRST OPEN THE GRIB FILE - NOTE THAT THE GRIB FILE AND 
C        INDEX FILE ARE OPENED FOR EVERY FIELD READ - WHICH IS 
C        LIKELY VERY INEFFICIENT. 
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') JFIL
      CALL GETENV(ENVVAR,FILEGRIB)
      CALL BAOPEN(JFIL,FILEGRIB,IRET)
C      IF(IRET.NE.0)THEN
C        WRITE(6,112) IRET
C 112    FORMAT(' TROUBLE WITH BAOPEN FOR GRIB FILE IN NTRNS, IRET=',I5)
C        KNUM=KNUM+1
C        GO TO 400
C      ENDIF
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') INDX
      CALL GETENV(ENVVAR,FILEINDX)
      CALL BAOPEN(INDX,FILEINDX,IRET)
C      IF(IRET.NE.0)THEN
C        WRITE(6,114) IRET
C  98    FORMAT(' TROUBLE WITH BAOPEN FOR INDEX FILE IN NTRNS, IRET=',I5)
C        KNUM=KNUM+1
C        GO TO 400
C      ENDIF
      ICND=0  
      JSR=0 
      IF(J.EQ.1) GO TO 130 	 
      CALL GETGB(JFIL,INDX,MAXPTS,JSR,IPDS,IGDS,NPNTS,MESG,             
     1 KPDS,KGDS,BMP,BUF,ICND)
      IF(ICND.EQ.0) GO TO 400 
      KNUM=KNUM+1
      WRITE(6,120) ICND,(IPDS(K),K=1,25),NPNTS,INCR  
 120  FORMAT(2X,'GETGB PROBLEM-ICND,IPDS,NPNTS=',I4,6I4,I6,3I4, 
     1 /,15I4,3X,2I6)
      GO TO 400                                                      
C        INTERPOLATE SEA-SFC TEMPS FROM 1 DEG LAT/LON GRID TO 65X65 GRID
 130  CALL INSST(IPDS,SST)
C        TRANSFER SEA-SFC TEMPS FROM 65X65 GRID TO MDL 26X33 GRID
      DO 310 M=1,26                                                     
      MP=7+M 
      DO 310 N=1,33                                                     
      NP=9+N 
      AA(M,N)=SST(MP,NP) 
 310  CONTINUE                                                          
      GO TO 410                                                         
C        POSITION DATA FROM NGM 45X53 GRID TO MDL 26X33 GRID            
 400  CALL NGMGRD(BUF2,D,AA)                                            
 410  IF(A(J).EQ.0.AND.C(J).EQ.1) GO TO 430                             
C        SET TEMPS AND DEW PTS TO ZERO AT NO-DATA POINTS OUTSIDE        
C        NGM GRID BOUNDARIES                                            
      IF(A(J)-273.15)418,412,418                                        
 412  DO 416 M=1,858                                                    
      IF(AP(M))414,414,416                                              
 414  AP(M)=AP(M)+273.15                                                
 416  CONTINUE                                                          
 418  DO 420 M=1,858                                                    
      AP(M)=(AP(M)-A(J))*C(J)                                           
 420  CONTINUE                                                          
      DO 424 M=1,2385                                                   
      BUF2(M)=(BUF2(M)-A(J))*C(J)                                       
 424  CONTINUE                                                          
 430  JTAB(J,4)=J                                                       
      WRITE(84,REC=J) (AP(K),K=1,858)                                   
      IF(J.EQ.1) GO TO 500                                              
C        WRITE NGM 45X53 DATA ARRAY TO DISK 85                          
      WRITE(85,REC=J) (BUF2(K),K=1,2385)                                
 500  CONTINUE
 510  WRITE(6,512) INCR,KDMY 
 512  FORMAT(5X,'TOTAL FIELDS PROCESSED = ',I3,' FOR ',I12)
      IF(KNUM.EQ.0) GO TO 520
      KERR=80
 520  RETURN                                                            
      END                                                               
