      SUBROUTINE INSST(JPDS,HO)
C                                         
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    INSST       INTERP SEA-SFC TEMPS FROM LATLON TO 65X65 G
C   PRGMMR: R. M. REAP       ORG: W/OSD21           DATE: 96-05-07      
C                                                                       
C ABSTRACT:  INTERPOLATES SEA-SFC TEMPS ON 1 DEG LATLON GRID TO 65X65   
C            POLAR STEREOGRAPHIC GRID ORIENTED ALONG 80 DEG WEST.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-05-07 R. M. REAP
C   00-03-10 J. P. DALLAVALLE
C            REVISED CODE TO RUN ON THE IBM-SP WITH THE NEW GETGB                                             
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C     SUBPROGRAM INSST                                                  
C        MAY 1996         R. M. REAP           MDL            CRAY      
C     ABSTRACT:
C        INTERPOLATES SEA-SURFACE TEMPS ON 1 DEG LATLON GRID (360X181)  
C        TO 65X65 POLAR STEREOGRAPHIC GRID ORIENTED ALONG 80 DEG WEST.
C
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      INTEGER IPOPT(20)
      INTEGER JPDS(200),JGDS(200),KPDSI(200),KGDSI(200),
     *     KPDSO(200),KGDSO(200)
      PARAMETER(JI=360*181,IG=27,JO=65*65,KM=1)
      CHARACTER*80 FILEINDX,FILEGRIB
      CHARACTER*11 ENVVAR
      REAL RLAT(JO),RLON(JO),CROT(JO),SROT(JO)
      INTEGER IBI(KM),IBO(KM)
      LOGICAL*1 LI(JI),LO(JO,KM) 
      REAL HI(JI,KM),RI(JI),UI(JI),VI(JI) 
      REAL HO(JO,KM),RO(JO),UO(JO),VO(JO)
      CHARACTER GDSO(400)  
      COMMON/BLOCKP/KPRINT,KERR                                         
C     ******************************************************************
C        DEFINE 65x65 GRID  
      CALL MAKGDS(IG,KGDSO,GDSO,LENGDS,IRET)
      IF(IRET.NE.0) WRITE(6,90) IRET
  90  FORMAT(2X,'MAKGDS IRET = ',I4)
      KGDSO(4)=-20826! fix w3fi71 error
C     WRITE(6,92) (KGDSO(J),J=1,22) 
C 92  FORMAT(2X,'KGDSO = ',3I4,2I8,I4,3I8,/,3I4,I12,3I4,I12,5I4)
      IPOPT=0
C        DEFINE THE UNIT NUMBER FOR THE GRIB INPUT FILE (30) AND THE 
C        ASSOCIATED INDEX FILE (31).      
      NFIL=30
      NFILI=31
C        ADDED CALLS TO GETENV, BAOPEN TO ALLOW READING
C        FILENAMES AND TO ACCESS GRIB FILES PROPERLY
C        FIRST OPEN THE GRIB FILE
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') NFIL
      CALL GETENV(ENVVAR,FILEGRIB)
      CALL BAOPEN(NFIL,FILEGRIB,IRET)
      IF(IRET.NE.0)THEN
        WRITE(6,95) IRET
  95    FORMAT(' UNABLE TO OPEN SST GRIB FILE IN INSST, IRET=',I5)
        KERR=100
        RETURN
      ENDIF
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') NFILI
      CALL GETENV(ENVVAR,FILEINDX)
      CALL BAOPEN(NFILI,FILEINDX,IRET)
      IF(IRET.NE.0)THEN
        WRITE(6,98) IRET
  98    FORMAT(' UNABLE TO OPEN SST INDEX FILE IN INSST, IRET=',I5)
        KERR=100
        RETURN
      ENDIF
      ISRCH=0
C        RETRIEVE SEA-SURFACE TEMPERATURES FOR 1 DEG LATLON GRID  
      JPDS(8)=-1
      JPDS(9)=-1
      JPDS(10)=-1
      JPDS(11)=-1
C        NOTE WELL - THE SIZE OF THE JPDS, JGDS, KPDSI, AND KGDSI ARRAYS
C        WERE MODIFIED TO CONFORM WITH THE GETGB STANDARDS.        
      CALL GETGB(NFIL,NFILI,JI,ISRCH,JPDS,JGDS,KI,KG,KPDSI,KGDSI,
     1  LI,HI(1,1),IRET)
      IF(IRET.EQ.0) GO TO 110
      WRITE(6,100) IRET,(JPDS(J),J=1,25)   
  100 FORMAT(2X,'INSST GETGB PROBLEM, IRET = ',I4,/,2X,'JPDS = ',25I3)
      KERR=100
      RETURN
C        INTERPOLATE (BILINEAR) SEA-SFC TEMPS TO 65X65 GRID (HO) 
  110 IBI(1)=0 
      CALL IPOLATES(0,IPOPT,KGDSI,KGDSO,JI,JO,KM,IBI,LI,HI,
     1  KO,RLAT,RLON,IBO,LO,HO,IRET)  
      IF(IRET.NE.0) WRITE(6,120) IRET 
  120 FORMAT(2X,'INSST IPOLATES PROBLEM, IRET = ',I4) 
      RETURN
      END  
