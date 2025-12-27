C$$$  MAIN PROGRAM DOCUMENTATION BLOCK                                  
C                .      .    .                                       .  
C MAIN PROGRAM: BUFRMOS      GENERATES A TDL MESSAGE IN BUFR FORMAT     
C   PRGMMR: ERICKSON         ORG: OSD211      DATE: 1998-09-28
C                                                                       
C ABSTRACT: THIS PROGRAM GENERATES AND POSTS A BUFR MESSAGE             
C           CONTAINING TDL MOS FORECASTS FOR SPECIFIED STATIONS.        
C           THIS BUFR MESSAGE TREATS EACH PROJECTION OF EACH STATION    
C           AS A SUBSET, AND THE DATA ARE STORED IN COMPRESSED          
C           FORMAT.  WMO'S BUFR EDITION 2 IS CURRENTLY BEING USED.      
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-12-03  GILBERT                                                   
C   96-02-20  GILBERT  - CONVERTED TO CRAY
C   96-04-15  GILBERT  - ADDED SUBROUTINE LATLON FOR PROCESSING
C                        OF STATION LATITUDES AND LONGITUDES.
C   96-08-23  GILBERT  - FIXED PROBLEM WITH THE WMO HEADER GENERATED
C                        IN SUBROUTINE BUFRGN.
C   98-07-07  SHIREY   - CHANGED GTDATE TO GETDATE (2X) FOR Y2K.
C   98-07-23  ERICKSON - CHANGED SUBRT BUFRGN FOR Y2K
C   00-03-07  ALLEN    - CONVERTED TO RUN ON THE IBM SP.  CHANGES WERE
C                        MADE TO BUFRGN TO WRITE THE OUTPUT FILE AS A
C                        DIRECT ACCESS FILE.
C   04-03-24  COSGROVE - TOC CHANGED THE WAY DATA IS SENT, SO THESE
C                        BUFR MESSAGES CAN NOT BE BROKEN UP WITHIN A
C                        HEADER ANYMORE. TOOK PXX'S OUT OF THE HEADERS.
C                        IT NOW READS THE KWBC FROM THE STATION LIST.
C                        UPPED THE MAX LENGTH OF THE BULLETIN FROM
C                        20000 TO 300000 BYTES.
C                                                                       
C USAGE:                                                                
C                                                                       
C     PROGRAM BUFRMOS                                                   
C                                                                       
C        JULY 1992    GILBERT    TDL   NAS9000                          
C                                                                       
C        PURPOSE                                                        
C            THIS PROGRAM GENERATES AND POSTS A BUFR MESSAGE            
C            CONTAINING TDL MOS FORECASTS FOR SPECIFIED STATIONS.       
C            THIS BUFR MESSAGE TREATS EACH PROJECTION OF EACH STATION   
C            AS A SUBSET, AND THE DATA ARE STORED IN COMPRESSED         
C            FORMAT.  WMO'S BUFR EDITION 2 IS CURRENTLY BEING USED.     
C                                                                       
C        DATA SET USE                                                   
C            FT05F001 - DATA CONTROL RECORDS (INPUT)                    
C            FT06F001 - PRINT FILE. (OUTPUT)                            
C            FT07F001 - STATION LIST & WMO HEADERS (INPUT)              
C            FT40F001 - TRANSMISSION FILE (OUTPUT)                      
C            FT90F001 - NMC DATE FILE (INPUT)                           
C            MOSMATXX - MOSMAT FILE (XX = 00 OR 12)  (INPUT)            
C            MOSCONST - MOS CONSTANT FILE (INPUT)                       
C                                                                       
C        VARIABLES                                                      
C               MAXSTA = MAXIMUM NUMBER OF STATIONS                     
C               MAXPRJ = MAXIMUM NUMBER OF PROJECTIONS                  
C               MAXDSC = MAXIMUM NUMBER OF DESCRIPTORS                  
C               MAXMES = MAXIMUM NUMBER OF BYTES IN A BUFR MESSAGE      
C               MAXBUL = MAXIMUM NUMBER OF WMO HEADERS                  
C               CSTA() = LIST OF STATION CALL LETTERS                   
C                WMO() = LIST OF WMO HEADERS                            
C              NWMO(I) = NUMBER OF STATIONS FOR HEADER WMO(I)           
C              LPROJ() = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE    
C                        ENCODED                                        
C             KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH     
C                        STATION AND EACH PROJECTION. 1ST DIMENSION IS  
C                        # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF  
C                        PROJECTIONS.  CONTAINS MISSING "9999" FOR      
C                        PROJECTION FOR WHICH FORECASTS ARE NOT VALID.  
C              MBUF(,) = HOLDS FORECASTS AFTER THEY ARE PACKED INTO BUFR
C                        FORMAT.  1ST DIMENSION IS # OF BYTES IN BUFR   
C                        MESSAGE, AND 2ND DIMENSION IS # OF BULLETINS   
C                        (WMO HEADERS).                                 
C              IVPRJ() = CONTAINS VALID PROJECTIONS FOR THE CURRENT     
C                        FORECAST ELEMENT.                              
C               NBPB() = # OF BITS IN EACH BULLETIN.                    
C               NBUL = NUMBER OF BUFR BULLETINS (WMO HEADERS)           
C                  IYR = CURRENT YEAR (4 DIGITS)                        
C                  IMO = CURRENT MONTH                                  
C                  IDA = CURRENT DAY                                    
C                  IHR = CURRENT CYCLE ( = 00 OR 12)                    
C                NDATE = CURRENT DATE                                   
C                IUNIT = DATA SET REFERNCE NUMBER FOR INPUT HEADERS     
C                        AND STATION LIST.                              
C                NUNIT = DATA SET REFERENCE NUMBER FOR INPUT CONTROL    
C                        RECORDS.                                       
C                 IEND = FLAG INDICATING END OF INPUT CONTROL RECORDS.  
C                ITERM = TERMINATOR FOR LIST OF PROJRCTIONS.            
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE               
C                                                                       
C   SUBPROGRAMS CALLED:                                                 
C     UNIQUE:    - BDRIVR, BPACK, BUFOPT, BUFRGN, RDWMO, BCATGR,        
C                  BPROB, BPROJ, BSPEC, BTMPDP, BWDIR, BWSPD, 
C                  BCLDMR, BDATE, BGENER, BNMLFQ, BCMAP, GETCON         
C     LIBRARY:                                                          
C       W3LIB    - W3TAG, W3FK40, W3AI15, W3FI62, GBYTE,        
C                  SBYTE, W3UTCDAT                                        
C       TDLLIB   - GETDATE, GTMOS, RDLSTA, RDLSTI
C                                                                       
C   EXIT STATES:                                                        
C     COND =   0 - SUCCESSFUL RUN                                       
C          =  10 - PROBLEM READING NMC DATE FILE                        
C          = 200 - PROBLEM POSTING TRANSMISSION FILE                    
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                               
C   MACHINE:  CRAY (4,5)                                                       
C                                                                       
C$$$                                                                    
      PROGRAM BUFRMOS
      PARAMETER (MAXSTA=2000,MAXPRJ=20,MAXDSC=100,MAXMES=300000)
      PARAMETER (MAXBUL=150)
      CHARACTER*1 CFLAG,MBUF(MAXMES,MAXBUL)
      CHARACTER*5 CTYPE1,CTYPE2
      CHARACTER*6 CDESC1,CDESC2
      CHARACTER*8 CSTA(MAXSTA),CALLS1(MAXSTA),CALLS2(MAXSTA)
      CHARACTER*8 CDDNM,CKEYW,CFMT
      CHARACTER*11 WMO(MAXBUL)
      CHARACTER*20 CNAME1(MAXSTA),CNAME2(MAXSTA)
      CHARACTER*21 CSEC2
      CHARACTER*80 CFILE1,CFILE2
      INTEGER NWMO(MAXBUL),KDATA(MAXSTA,MAXPRJ),
     *        LPROJ(MAXPRJ),LTEMP(18),NBPB(MAXBUL),IVPRJ(MAXPRJ)
      INTEGER LDESC(MAXDSC),JWBAN1(MAXSTA),JWBAN2(MAXSTA)
      INTEGER JBLCK1(MAXSTA),JBLCK2(MAXSTA),ID(4)
      DATA IUNIT/5/,NUNIT/7/,CFMT/'(18I4)  '/,ITERM/9999/,IEND/0/,
     *     NDESC/0/,NBPB/MAXBUL*0/,ICND/0/,IMOS/20/,ICON/30/,
     *     NOPT/0/
      CALL W3TAGB('BUFRMOS ',1998,0271,0057,'OSD211 ')                  


C        GET TRANSMISSION FILE INFO FOR W3AG15                          
      READ(NUNIT,1)CDDNM,CKEYW
 1    FORMAT(2A8)
C        READ FILE NAMES OF MOS FORECAST AND CONSTANT FILE,
C        IF APPROPRIATE.
      READ(NUNIT,2) CFILE1
      READ(NUNIT,2) CFILE2
 2    FORMAT(A80)
C        GET DESCRIPTION FOR SECTION 2                                  
      READ(NUNIT,3)CSEC2
 3    FORMAT(A21)
C        GET CURRENT DATE FROM NMC'S DATE FILE.                         
      CALL GETDATE(49,IYR,IMO,IDA,IHR,NDATE,IERR)
      IF (IERR.NE.0) THEN
        WRITE(6,10) IERR
 10     FORMAT(' PROBLEM READING DATE FILE.  IERR = ',I4)
      CALL W3TAGE('BUFRMOS ') 
        CALL EXIT(10)
      ENDIF
C        OPEN MOS FORECAST FILE
      CALL OPMOS(IMOS,CFILE1,NOPT,CDESC1,IDATE1,CTYPE1,NSTA1,
     *     CALLS1,JWBAN1,JBLCK1,CNAME1,NERR)
      IF (NERR.NE.0) THEN
	WRITE(6,20) NERR
 20	FORMAT(' ERROR OPENING MOS FORECAST FILE = ',I4)
      CALL W3TAGE('BUFRMOS ') 
        CALL EXIT(20)
      ENDIF
C        OPEN MOS CONSTANT FILE
      CALL OPMOS(ICON,CFILE2,NOPT,CDESC2,IDATE2,CTYPE2,NSTA2,
     *     CALLS2,JWBAN2,JBLCK2,CNAME2,NERR)
      IF (NERR.NE.0) THEN
	WRITE(6,30) NERR
 30	FORMAT(' ERROR OPENING MOS CONSTANT FILE = ',I4)
      CALL W3TAGE('BUFRMOS ') 
        CALL EXIT(30)
      ENDIF
      IDATE=(IMO*100)+IDA
C        READ WMO HEADERS AND ASSOCIATED STATION LISTS.                 
      CALL RDWMO(NUNIT,NSTA,CSTA,WMO,NWMO,NBUL,MAXSTA,MAXBUL)
C        READ LIST OF PROJECTIONS                                       
      CALL RDLSTI(IUNIT,LPROJ,MAXPRJ,LTEMP,18,CFMT,NUMPRJ,ITERM)
C        READ A SET OF CONTROL RECORDS FROM THE INPUT CONTROL FILE.     
 100  CALL BDRIVR(ID,MAXDSC,LDESC,NDESC,SCALE,REF,NBITS,
     *            IVPRJ,MAXPRJ,NVPRJ,CFLAG,IEND,IOPT,IUNIT)
C        IEND = 1 INDICATES THE END OF THE INPUT CONTROL RECORDS.       
      IF (IEND.EQ.0) THEN
C        GET FORECASTS DATA AND DO NECESSARY CONVERSIONS TO PREPARE     
C        THE DATA FOR PACKING.                                          
        CALL BUFOPT(IMOS,ICON,NSTA,CSTA,KDATA,ID,LPROJ,SCALE,REF,NDATE,
     *       MAXSTA,IVPRJ,NVPRJ,NUMPRJ,IOPT,ICND)
C        CHECK TO SEE IF SNOW FORECASTS SHOULD BE CHECKED FOR AN        
C        INVALID SEASON.                                                
        IF (CFLAG.EQ.'*') THEN
          CALL SEACHK(IDATE,KDATA,MAXSTA,NUMPRJ,WMO,NWMO,NBUL)
        ENDIF
C        PACK CONVERTED AND SCALED DATA INTO MBUF.                      
        CALL BPACK(NSTA,KDATA,CSTA,NBITS,MBUF,NWMO,NBPB,NBUL,MAXSTA,
     *       NUMPRJ,MAXMES,IOPT)
        GOTO 100
      ENDIF
C        GENERATE ALL BUFR BULLETINS.                                   
      CALL BUFRGN(NSTA,WMO,NWMO,NBUL,LDESC,NDESC,MBUF,NBPB,IYR,IMO,
     *     IDA,IHR,MAXMES,NUMPRJ,CSTA,CSEC2)
      CALL W3TAGE('BUFRMOS ') 
      STOP
      END
