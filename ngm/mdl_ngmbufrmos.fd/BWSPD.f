      SUBROUTINE BWSPD(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,
     *           REF,NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BWSPD       READS WIND SPEED FORECASTS AND STORES      
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-20             
C                                                                       
C ABSTRACT: READS WIND SPEED FORECASTS AND STORES IN KDATA(,) FOR       
C           LATER PACKING USE.   THE FORECASTS ARE CONVERTED TO METERS  
C           PER SECOND AND SCALED APPROPRIATELY.  MISSING VALUES (9999) 
C           ARE INSERTED FOR PROJECTIONS FOR WHICH FORECASTS ARE NOT    
C           VALID OR FOUND.                                             
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-20  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        JULY 1992   GILBERT     TDL     NAS9000                        
C                                                                       
C        PURPOSE                                                        
C            READS WIND SPEED FORECASTS AND STORES IN KDATA(,) FOR      
C            LATER PACKING USE.   THE FORECASTS ARE CONVERTED TO METERS 
C            PER SECOND AND SCALED APPROPRIATELY.  MISSING VALUES (9999)
C            ARE INSERTED FOR PROJECTIONS FOR WHICH FORECASTS ARE NOT   
C            VALID OR FOUND.                                            
C                                                                       
C        DATA SET USE                                                   
C            FT06     - PRINT FILE  (OUTPUT)                            
C            MOSMATXX - MOS FORECAST FILE (XX=00 OR 12)  (INPUT)        
C                                                                       
C        VARIABLES                                                      
C               MAXSTA = MAXIMUM NUMBER OF STATIONS                     
C               CSTA() = LIST OF STATION CALL LETTERS                   
C              LPROJ() = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE    
C                        ENCODED                                        
C             KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH     
C                        STATION AND EACH PROJECTION. 1ST DIMENSION IS  
C                        # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF  
C                        PROJECTIONS.  CONTAINS MISSING "9999" FOR      
C                        PROJECTION FOR WHICH FORECASTS ARE NOT VALID.  
C              IVPRJ() = CONTAINS VALID PROJECTIONS FOR THE CURRENT     
C                        FORECAST ELEMENT.                              
C                NDATE = CURRENT DATE                                   
C                MDATE = MOS FORECAST DATE                              
C                 NSTA = NUMBER OF STATIONS                             
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE               
C                NVPRJ = NUMBER OF VALID PROJECTIONS                    
C              RDATA() = WORK ARRAY, HOLDING FORECASTS IN PROCESSING    
C                        ROUTINES.                                      
C                ID(4) = MOS FORECAST IDENTIFIER
C                SCALE = BUFR TABLE B; SCALING VALUE                    
C                  REF = BUFR TABLE B; REFERENCE VALUE                  
C                 ICND = ERROR FLAG                                     
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      INTEGER KDATA(MAXSTA,NUMPRJ),LPROJ(NUMPRJ),IVPRJ(NVPRJ),ID(4)
      REAL RDATA(NSTA)
      CHARACTER*8 CSTA(NSTA)
      ICYC=MOD(NDATE,100)
      N=1
      NUM=0
      RMULT=10.0**SCALE
C        DO FOR EACH PROJECTION                                         
      DO 400 L=1,NUMPRJ
C        IF PROJECTION IS NOT VALID, INSERT MISSING VALUES (9999).      
        IF (N.GT.NVPRJ) GOTO 250
        IF (LPROJ(L).NE.IVPRJ(N)) GOTO 250
        N=N+1
C        READ WIND SPEED FORECASTS.                                     
	ID(3)=LPROJ(L)
        CALL GTMOS(IMOS,ID,MDATE,NSTA,RDATA,1,CSTA,X,NERR)
        IF (NERR.EQ.0) THEN
C        CHECK MOS FORECAST DATE WITH RUN DATE.                         
          IF (MDATE.NE.NDATE) THEN
            WRITE(6,10)NDATE,MDATE
 10         FORMAT(' NMC DATE: ',I8,' DOES NOT MATCH MOSMAT: ',I8)
            DO 100 I=1,NSTA
              KDATA(I,L)=9999
 100        CONTINUE
            GOTO 400
          ENDIF
          NUM=NUM+1
C        FOR EACH STATION, CONVERT TO METERS PER SECOND AND DO          
C        APPROPRIATE SCALING.                                           
          DO 200 I=1,NSTA
            IF ((RDATA(I).NE.9999.).AND.(RDATA(I).NE.9997.)) THEN
              RDATA(I)=0.51479*RDATA(I)
              RDATA(I)=(RDATA(I)*RMULT)-REF
              IF (RDATA(I).LT.0.0) RDATA(I)=0.0
              KDATA(I,L)=NINT(RDATA(I))
            ELSE
              KDATA(I,L)=9999
            ENDIF
 200      CONTINUE
          GOTO 400
        ELSE
          WRITE(6,225) (ID(II),II=1,4)
 225      FORMAT(' ERROR READING FORECAST: ID = ',4I10)
        ENDIF
 250    DO 300 I=1,NSTA
          KDATA(I,L)=9999
 300    CONTINUE
 400  CONTINUE
C        ICND = 1, IF NO FORECASTS WERE FOUND                           
      IF(NUM.EQ.0)ICND=1
 900  RETURN
      END
