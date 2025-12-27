      SUBROUTINE BUFOPT(IMOS,ICON,NSTA,CSTA,KDATA,ID,LPROJ,SCALE,REF,
     *           NDATE,MAXSTA,IVPRJ,NVPRJ,NUMPRJ,IOPT,ICND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BUFOPT      CALLS APPROPRIATE DATA PROCESSOR.          
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-13             
C                                                                       
C ABSTRACT: CALLS THE APPROPRIATE SUBROUTINE TO                         
C           READ THE REQUESTED FORECAST DATA AND DO ANY NECESSARY       
C           CONVERSIONS AND/OR SCALING TO PREPARE THE DATA FOR PACKING. 
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-13  GILBERT                                                   
C   96-04-15  GILBERT  - ADDED IOPT 13 FOR PROCESSING OF
C                        STATION LATITUDES AND LONGITUDES.
C                                                                       
C USAGE:                                                                
C                                                                       
C        JULY 1992   GILBERT   TDL    NAS9000                           
C                                                                       
C        PURPOSE                                                        
C            CALLS THE APPROPRIATE SUBROUTINE TO                        
C            READ THE REQUESTED FORECAST DATA AND DO ANY NECESSARY      
C            CONVERSIONS AND/OR SCALING TO PREPARE THE DATA FOR PACKING.
C                                                                       
C        DATA SET USE                                                   
C              FT06 - PRINT FILE.  (OUTPUT)                             
C                                                                       
C        VARIABLES                                                      
C             CSTA() = LIST OF STATION CALL LETTERS                     
C               NSTA = NUMBER OF STATIONS                               
C            LPROJ() = LIST OF PROJECTIONS IN MESSAGE                   
C             NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE                 
C            IVPRJ() = LIST OF VALID PROJECTIONS FOR CURRENT FORECAST   
C                      ELEMENT.                                         
C              NVPRJ = NUMBER OF VALID PROJECTIONS                      
C           KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH       
C                      STATION AND EACH PROJECTION. 1ST DIMENSION IS    
C                      # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF    
C                      PROJECTIONS.  CONTAINS MISSING "9999"'S FOR      
C                      PROJECTION FOR WHICH FORECASTS ARE NOT VALID.    
C            RDATA() = WORK ARRAY, HOLDING FORECASTS IN PROCESSING      
C                      ROUTINES.                                        
C             MAXSTA = MAXIMUM # OF STATIONS (USED FOR DIMENSIONING)    
C              ID(4) = MOS FORECAST IDENTIFIER
C              SCALE = BUFR TABLE B; SCALING VALUE                      
C                REF = BUFR TABLE B; REFERENCE VALUE                    
C              NDATE = DATE IN TDL FORMAT                               
C               ICND = ERROR FLAG                                       
C                                                                       
C REMARKS: THE PROCESS PICKED BY BUFOPT IS DETERMINED BY IOPT.          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      CHARACTER*8 CSTA(NSTA)
      INTEGER KDATA(MAXSTA,NUMPRJ),LPROJ(NUMPRJ),IVPRJ(NVPRJ),ID(4)
      REAL RDATA(2000)
      ICND=0
C        INSERT PROJECTION LIST.                                        
      IF (IOPT.EQ.0) THEN
        CALL BPROJ(NSTA,KDATA,LPROJ,SCALE,REF,MAXSTA,NUMPRJ)
        GOTO 900
      ENDIF
C        INSERT SPECIFIED USER VALUE = ID(4)
      IF (IOPT.EQ.1) THEN
        CALL BSPEC(NSTA,KDATA,ID(4),SCALE,REF,MAXSTA,NUMPRJ,LPROJ,
     *       IVPRJ,NVPRJ)
        GOTO 900
      ENDIF
C        FOR CALL LETTERS...CONTINUE ON
      IF (IOPT.EQ.2) THEN
        GOTO 900
      ENDIF
C        GET TEMPS OR DEW POINTS AND PROCESS                            
      IF (IOPT.EQ.3) THEN
        CALL BTMPDP(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        GET WIND SPEEDS AND PROCESS                                    
      IF (IOPT.EQ.4) THEN
        CALL BWSPD(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        GET WIND DIRECTIONS AND PROCESS                                
      IF (IOPT.EQ.5) THEN
        CALL BWDIR(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        GET CATEGORICAL FORECASTS AND PROCESS                          
      IF (IOPT.EQ.6) THEN
        CALL BCATGR(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,NDATE,
     *       MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        GET PROBABILITY FORECASTS AND PROCESS                          
      IF (IOPT.EQ.7) THEN
        CALL BPROB(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        GET "GENERIC" FORECASTS AND PROCESS                            
      IF (IOPT.EQ.8) THEN
        CALL BGENER(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        GET M.R. CLOUD FORECASTS AND PROCESS                           
      IF (IOPT.EQ.9) THEN
        CALL BCLDMR(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        GET M.R. NORMALS AND/OR RELATIVE FRQUENCIES                    
      IF (IOPT.EQ.10) THEN
        CALL BNMLFQ(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,ICON,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        INSERT INITIAL DATE/TIME: YEAR, MONTH, DAY OR HOUR DETERMINED  
C        BY VALUE IN ID(4) 
      IF (IOPT.EQ.11) THEN
        CALL BDATE(NSTA,KDATA,ID(4),SCALE,REF,MAXSTA,NUMPRJ,LPROJ,
     *       IVPRJ,NVPRJ,NDATE)
        GOTO 900
      ENDIF
C        INSERT SPECIFIED VALUES THAT ARE DIFFERENT FOR EACH PROJECTION.
      IF (IOPT.EQ.12) THEN
        CALL BSPECS(NSTA,KDATA,SCALE,REF,MAXSTA,NUMPRJ,LPROJ,
     *       IVPRJ,NVPRJ)
        GOTO 900
      ENDIF
C        GET STATION LONGITUDES AND PROCESS                            
      IF (IOPT.EQ.13) THEN
        CALL LATLON(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,REF,
     *       NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
        GOTO 900
      ENDIF
C        SET ICND = 1 IF NO PROCESSING DONE                             
      ICND=1
      WRITE(6,800) IOPT
 800  FORMAT(' IOPT =',I4,'  NOT FOUND IN BUFR OPTION LIST.')
 900  IF (ICND.NE.0) THEN
        WRITE(6,1000) (ID(I),I=1,4)
 1000   FORMAT(' NO FORECASTS FOUND FOR ID = ',4I10)
      ENDIF
      RETURN
      END
