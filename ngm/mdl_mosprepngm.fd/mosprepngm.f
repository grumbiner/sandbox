C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: MOSPREPNGM
C   PRGMMR: ERICKSON         ORG: W/OSD211    DATE: 2000-03-03
C                                                                       
C ABSTRACT: THIS PROGRAM COPIES SELECTED NGM MODEL FIELDS FROM 
C   NCEP'S MODEL FILES TO ONE FILE TO BE USED LATER BY MDL'S MOS  
C   FORECAST PROGRAM.  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-02-22  GILBERT                                                   
C   98-07-15  ERICKSON   CHANGED GTDATE TO Y2K GETDATE, 
C                        CHANGED ID63(8) (1-100 YR OF CEN) DEFINITION.
C                        FROM MOD(IYR,100) ==> TO MOD(IYR-1,100)+1
C   99-12-27  ERICKSON   MODIFIED TO USE NEW GRIB CONVENTIONS IBM
C   00-12-29  ERICKSON   CORRECTED SPECIFICATION OF CENTURY 
C                                                                       
C USAGE:                                                                
C                                                                       
C   PROGRAM MOSPREP                                                     
C      MARCH 1996     GILBERT   MDL   CRAY                          
C      PURPOSE                                                          
C          THIS PROGRAM COPIES SELECTED NGM MODEL FIELDS FROM NMC'S MODE
C          FILES TO ONE FILE TO BE USED LATER BY MDL'S MOS FORECAST     
C          PROGRAM.
C                                                                       
C      VARIABLES                                                        
C            CIN = DESCRIPTION OF INPUT DATASET   (CHAR*8)
C          CTERM = INPUT RECORD TERMINATOR   (CHAR*8)
C         GRIB() = GRIB FIELD   (CHAR*1)
C           ID() = 25-WORD GRIB PDS IDENTIFIER                          
C         ID63() = 25-WORD GRIB PDS IDENTIFIER AS DESCRIBED IN W3FI63
C         IPDS() = 28-BYTE GRIB PDS READ FROM CONTROL RECORDS
C         KPDS() = 25-WORD GRIB PDS RETURNED FROM GETGB1.
C         KGDS() = GDS RETURNED FROM GETGB1
C          BMP() = DUMMY ARRAY TO HOLD PLACE FOR BIT MAP.
C          FLD() = GRIDDED DATA
C            IYR = CURRENT YEAR (4 DIGITS)                              
C            IDA = CURRENT DAY                                          
C            IMO = CURRENT MONTH                                        
C            IHR = CURRENT CYCLE                                        
C          NDATE = CURRENT DATE IN MDL FORMAT YYYYMMDDHH                
C            NUM = NUMBER OF RECORDS COPIED                             
C           INUM = NUMBER OF RECORDS PREVIOUSLY COPIED                  
C           NREC = NUMBER OF RECORDS TO COPY                            
C          INTOT = TOTAL NUMBER OF RECORDS TO COPY PER MODEL            
C           ITOT = ACTUAL NUMBER OF RECORDS COPIED PER MODEL.           
C         CTITLE = TEXT DESCRIPTION OF GRIB FIELD.  (CHAR*86)
C             II = X DIMENSION OF GRID
C             JJ = Y DIMENSION OF GRID
C           NFIL = UNIT NUMBER OF GRIB DATA FILE BEING READ
C          NFILI = UNIT NUMBER OF GRIB INDEX FILE BEING READ
C                                                                       
C   INPUT FILES:                                                        
C     FT05F001 - INPUT CONTROL RECORDS SPECIFYING WHICH MODELS 
C              - FIELDS TO PROCESS.      
C                                                                       
C   OUTPUT FILES:                                                       
C     FT06F001 - INCLUDE IF ANY PRINTOUT                                
C                                                                       
C   SUBPROGRAMS CALLED:                                                
C     LIBRARY:                                                          
C       W3LIB    - GETGB1, PUTGB, W3FI69, W3FP11,         
C       TDLLIB   - GETDATE                                              
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 - SUCCESSFUL RUN                                       
C          =  10 - TROUBLE READING NMC DATE FILE.                       
C                                                                       
C REMARKS: 
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN90
C   MACHINE:  IBM                                                      
C                                                                       
C$$$                                                                    
      PROGRAM MOSPREPNGM
      PARAMETER (II=53,JJ=45)
      CHARACTER*86 CTITLE
      CHARACTER*8 CTERM,CIN
      CHARACTER*11 ENVVAR
      CHARACTER*80 FILEI,FILEB,FILEO
      CHARACTER*1 GRIB(100000)
      INTEGER IPDS(4),ID(200),ID63(200),KPDS(200),KGDS(200),IGDS(200)
      INTEGER JPDS(200),JGDS(200)
      REAL FLD(II,JJ)
      LOGICAL*1 BMP(II*JJ)
      DATA CTERM/'99999999'/,IOUT/6/,INPT/5/
      DATA ID63/200*-1/
      DATA NFILO/75/,JG/100000/
      DATA MAXPTS/2385/
       CALL W3TAGB('MOSPREPNGM',2000,0063,0073,'W/OSD211')                 
C        GET DATE FROM NMC'S DATE FILE    
      CALL GETDATE(49,IYR,IMO,IDA,IHR,NDATE,IERR)
      IF (IERR.NE.0) THEN
        WRITE(IOUT,10)
 10     FORMAT(' PROBLEM READING NMC DATE FILE.')
        CALL W3TAGE('MOSPREPNGM')                                         
        CALL EXIT(10)
      ENDIF
      MAXPTS=II*JJ
      NPTS=II*JJ
      NFIL=10
      NFILI=20
      INTOT=0
      ITOT=0

      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') NFILO
      CALL GETENV(ENVVAR,FILEO)
      CALL BAOPEN(NFILO,FILEO,IRET)
C
C        READ INPUT FILE NAME AND NUMBER OF RECORDS TO COPY             
 100  READ(INPT,200)CIN,NREC
 200  FORMAT(5X,A8,7X,I3)
      INTOT=INTOT+NREC
      IF (CIN.EQ.CTERM) GOTO 900
      NUM=0
      NFIL=NFIL+1
      NFILI=NFILI+1
C        ADDED CALLS TO GETENV, BAOPEN TO ALLOW READING
C        FILENAMES, ACCESS GRIB FILES PROPERLY
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') NFIL
      CALL GETENV(ENVVAR,FILEB)
      CALL BAOPEN(NFIL,FILEB,IRET)
C     WRITE(6,12) IRET,NFIL,FILEB
      ENVVAR='XLFUNIT_   '
      WRITE(ENVVAR(9:10),FMT='(I2)') NFILI
      CALL GETENV(ENVVAR,FILEI)
      CALL BAOPEN(NFILI,FILEI,IRET)
C     WRITE(6,12) IRET,NFILI,FILEI

      DO 500 N=1,NREC
C        READ PDS OF FIELD TO BE COPIED.
        READ(INPT,300)(IPDS(I),I=1,4),IOPT
 300    FORMAT(2Z16,1X,2Z16,I4)
C        CONVERT PDS TO 25 WORD INTEGER ARRAY
        CALL W3FI69(IPDS,ID)
C        REORDER 25 WORD INTEGER ARRAY TO BE THE VERSION
C        NEEDED FOR GETGB1.
        ID63(1)=ID(3)
        ID63(2)=ID(4)
        ID63(3)=ID(5)
        ID63(5)=ID(8)
        ID63(6)=ID(9)
        ID63(7)=(ID(10)*256)+ID(11)
        ID63(8)=MOD(IYR-1,100)+1
        ID63(9)=IMO
        ID63(10)=IDA
        ID63(11)=IHR
        ID63(13)=ID(17)
        ID63(14)=ID(18)
        ID63(15)=ID(19)
        ID63(16)=ID(20)
        ID63(17)=ID(21)
        ID63(20)=ID(22)
        ID63(21)=((IYR-1)/100)+1  
C        READ FIELD TO BE COPIED      
        ISRCH=0
        CALL GETGB(NFIL,NFILI,MAXPTS,ISRCH,ID63,KGDS,NUMPTS,
     *       INUM,KPDS,KGDS,BMP,FLD,IRET)
C       WRITE(6,390) IRET, NFIL,(ID63(KK),KPDS(KK),KK=1,25)
C390    FORMAT(' IRET=',I6,' AFTER CALL TO GETGB FOR NFIL:',I3,
C    *        /,(' ',5(2I8)))
        CALL GETGBP(NFIL,NFILI,JG,ISRCH,ID63,KGDS,KG,INUM,KPDS,
     *       KGDS,GRIB,IRET2)
C       WRITE(6,391) IRET2, NFIL
C391    FORMAT(' IRET2=',I6,' AFTER CALL TO GETGBP FOR NFIL:',I3)

C       CALL GETGB1(NFIL,NFILI,MAXPTS,ISRCH,ID63,KGDS,GRIB,NUMPTS,
C    *       INUM,KPDS,KGDS,BMP,FLD,IRET)
        IF (IRET.NE.0) THEN
          WRITE(IOUT,400)CIN,IRET,IPDS
 400      FORMAT(' ERROR READING FILE ',A8,' IERR = ',I4/' IPDS: ',
     *           2Z16,1X,2Z16)
          GOTO 500
        ENDIF
C        CONVERT GRIB PDS TO TEXT DESCRIPTION
        CALL W3FP11(GRIB,GRIB(9),CTITLE,IER)
        WRITE(6,415) CTITLE(1:80)
 415    FORMAT(A80)
        NUM=NUM+1
C        FOR PRESSURE FIELDS, CONVERT PA TO HPA (MB).
        IF (IOPT.EQ.1) THEN
          DO 421 I=1,II
            DO 420 J=1,JJ
              FLD(I,J)=FLD(I,J)/100.0
 420        CONTINUE
 421      CONTINUE
          KPDS(22)=3
        ELSEIF (IOPT.EQ.2) THEN
C        FOR PRECIP WATER AND PRECIP AMT, CONVERT KG/M**2 (MM) TO METERS
          DO 431 I=1,II
            DO 430 J=1,JJ
              FLD(I,J)=FLD(I,J)/1000.0
 430        CONTINUE
 431      CONTINUE
          KPDS(22)=4
        ELSEIF (IOPT.NE.0) THEN
          WRITE(6,*) ' INVALID IOPT = ',IOPT
	  GOTO 500
        ENDIF
C        WRITE FIELD TO NEW FILE.
        CALL PUTGB(NFILO,NPTS,KPDS,KGDS,BMP,FLD,NERR)
        IF (NERR.NE.0) THEN
          WRITE(IOUT,450)CIN,NERR,IPDS
 450      FORMAT(' ERROR WRITING FILE ',A8,' IERR = ',I4/' IPDS: ',
     *           2Z16,1X,2Z16)
        ENDIF
 500  CONTINUE
      WRITE(IOUT,600)NUM,NREC,CIN
 600  FORMAT(I4,' OUT OF ',I4,' RECORDS COPIED FROM FILE ',A8)
      ITOT=ITOT+NUM
      GOTO 100
C        CLOSE OUTPUT FILE                                              
 900  WRITE(IOUT,902)ITOT,INTOT
 902  FORMAT(/' A TOTAL OF ',I5,' OUT OF ',I5,' RECORDS WERE COPIED.')
      CALL W3TAGE('MOSPREPNGM')                                           
      STOP
      END
