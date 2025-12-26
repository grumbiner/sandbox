      SUBROUTINE BCATGR(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,NDATE,
     *           MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BCATGR      GET CATEGORICAL FORECASTS AND STORE.       
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-15             
C                                                                       
C ABSTRACT: READS CATEGORICAL FORECASTS FOR ALL PROJECTIONS.            
C           ALSO, STORES DATA IN KDATA(,) TO BE PACKED LATER.  MISSING  
C           VALUES (9999) ARE INSERTED FOR PROJECTIONS AT WHICH THE     
C           FORECASTS ARE NOT VALID.                                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-15  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        JULY 1992   GILBERT   TDL  NAS9000                             
C                                                                       
C        PURPOSE                                                        
C             READS CATEGORICAL FORECASTS FOR ALL PROJECTIONS.          
C             ALSO, STORES DATA IN KDATA(,) TO BE PACKED LATER.  MISSING
C             VALUES (9999) ARE INSERTED FOR PROJECTIONS AT WHICH THE   
C             FORECASTS ARE NOT VALID.                                  
C                                                                       
C        DATA SET USE                                                   
C             FT06 - PRINT FILE (OUTPUT)                                
C         MOSMATXX - MOSMAT FILE (XX=00 OR 12) (INPUT)                  
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
C                 NSTA = NUMBER OF STATIONS                             
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE               
C                NVPRJ = NUMBER OF VALID PROJECTIONS                    
C              RDATA() = WORK ARRAY, HOLDING FORECASTS IN PROCESSING    
C                        ROUTINES.                                      
C                ID(4) = MOS FORECAST IDENTIFIER
C                MDATE = DATE IN MDL MOSMAT FILE                        
C                 ICYC = CURRENT CYCLE (00 OR 12)                       
C                 ICND = ERROR FLAG                                     
C                                                                       
C REMARKS: NO SCALING OR CONVERSIONS ARE DONE FOR CATEGORICAL           
C          FORECASTS.                                                   
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      INTEGER KDATA(MAXSTA,NUMPRJ),LPROJ(NUMPRJ),IVPRJ(NVPRJ),ID(4)
      REAL RDATA(NSTA)
      CHARACTER*8 CSTA(NSTA)
      ICND=0
      ICYC=MOD(NDATE,100)
      N=1
      NUM=0
C        DO FOR EACH PROJECTION                                         
      DO 400 L=1,NUMPRJ
C        IF NOT A VALID PROJECTION, FILL WITH MISSINGS                  
        IF (N.GT.NVPRJ) GOTO 250
        IF (LPROJ(L).NE.IVPRJ(N)) GOTO 250
        N=N+1
C        READ CATEGORICAL FORECASTS                                     
	ID(3)=LPROJ(L)
        CALL GTMOS(IMOS,ID,MDATE,NSTA,RDATA,1,CSTA,X,NERR)
        IF (NERR.EQ.0) THEN
C        CHECK MOSMAT DATE WITH RUN DATE                                
          IF (MDATE.NE.NDATE) THEN
            WRITE(6,10)NDATE,MDATE
 10         FORMAT(' NMC DATE: ',I8,' DOES NOT MATCH MOSMAT: ',I8)
            DO 100 I=1,NSTA
              KDATA(I,L)=9999
 100        CONTINUE
            GOTO 400
          ENDIF
          NUM=NUM+1
C        ADD FORECASTS TO KDATA(,)                                      
          DO 200 I=1,NSTA
            IF (RDATA(I).EQ.9997.) RDATA(I)=9999.
            KDATA(I,L)=INT(RDATA(I))
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
C        SET ICND = 1 IF NO FORECASTS WERE FOUND                        
      IF (NUM.EQ.0) ICND=1
      IF (ICND.EQ.0) CALL BCMAP(KDATA,MAXSTA,NUMPRJ,NSTA,ID)
 900  RETURN
      END
