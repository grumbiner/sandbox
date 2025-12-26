      SUBROUTINE LATLON(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,IMOS,SCALE,
     *           REF,NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BLONG       GETS STATION LATITUDES OR LONGITUDES  
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 96-04-15             
C                                                                       
C ABSTRACT: READS STATION LATITUDES OR LONGITUDES, CONVERTS 
C           DEGREES WEST TO NEGATIVE VALUES (LONGITUDES) AND THEN
C           PERFORMS THE APPROPRIATE SCALING.  THE DATA ARE ADDED TO    
C           KDATA(,) FOR LATER USE IN PACKING.  MISSING VALUES (9999)   
C           ARE INSERTED FOR PROJECTIONS AT WHICH THE DATA ARE NOT
C           FOUND OR VALID.                                             
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   96-04-15  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        APRIL 1996   GILBERT    TDL
C                                                                       
C        PURPOSE                                                        
C            READS STATION LONGITUDES, CONVERTS DEGREES WEST TO
C            NEGATIVE VALUES AND THEN
C            PERFORMS THE APPROPRIATE SCALING.  THE DATA ARE ADDED TO   
C            KDATA(,) FOR LATER USE IN PACKING.  MISSING VALUES (9999)  
C            ARE INSERTED FOR PROJECTIONS AT WHICH THE DATA ARE NOT
C            FOUND OR VALID.                                            
C                                                                       
C        DATA SET USE                                                   
C            FT06     - PRINT FILE (OUTPUT)                             
C            MOSMATXX - MOSMAT FILE  (XX = 00 OR 12)   (INPUT)          
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
C                MDATE = DATE OF FORECASTS ON MOSMAT FILE.              
C                 NSTA = NUMBER OF STATIONS                             
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE               
C                NVPRJ = NUMBER OF VALID PROJECTIONS                    
C              RDATA() = WORK ARRAY, HOLDING FORECASTS IN PROCESSING    
C                        ROUTINES.                                      
C                ID(4) = MOS FORECAST IDENTIFIER
C                SCALE = BUFR TABLE B; SCALING VALUE                    
C                  REF = BUFR TABLE B; REFERENCE VALUE                  
C                 ICND = ERROR FLAG                                     
C                 ICYC = CURRENT CYCLE  (00 OR 12)                      
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  CRAY
C                                                                       
C$$$                                                                    
      INTEGER KDATA(MAXSTA,NUMPRJ),LPROJ(NUMPRJ),IVPRJ(NVPRJ),ID(4)
      REAL RDATA(NSTA)
      CHARACTER*8 CSTA(NSTA)
      ICYC=MOD(NDATE,100)
      N=1
      NUM=0
      RMULT=10.0**SCALE
C        READ STATION LATITUDES OR LONGITUDES
      CALL GTMOS(IMOS,ID,MDATE,NSTA,RDATA,1,CSTA,X,NERR)
      IF (NERR.EQ.0) THEN
        NUM=NUM+1
C        MULTIPLY SCALING FACTOR AND SUBTRACT REFERENCE VALUE FROM EACH 
C        DATA VALUE                                                     
        DO 100 I=1,NSTA
          IF ((RDATA(I).NE.9999.).AND.(RDATA(I).NE.9997.)) THEN
            IF (ID(1).EQ.400007000) RDATA(I)=-1.0*RDATA(I)
            RDATA(I)=(RDATA(I)*RMULT)-REF
          ENDIF
 100    CONTINUE
      ELSE
        WRITE(6,125) (ID(II),II=1,4)
 125    FORMAT(' ERROR READING LONGITUDES: ID = ',4I10)
      ENDIF
C        DO FOR EACH PROJECTION                                         
      DO 400 L=1,NUMPRJ
C        IF NOT A VALID PROJECTION, FILL KDATA(,) WITH MISSING (9999)   
C        VALUES.                                                        
        IF (N.GT.NVPRJ) GOTO 250
        IF (LPROJ(L).NE.IVPRJ(N)) GOTO 250
        N=N+1
        DO 200 I=1,NSTA
          IF ((RDATA(I).NE.9999.).AND.(RDATA(I).NE.9997.)) THEN
            KDATA(I,L)=NINT(RDATA(I))
          ELSE
            KDATA(I,L)=9999
          ENDIF
 200    CONTINUE
        GOTO 400
 250    DO 300 I=1,NSTA
          KDATA(I,L)=9999
 300    CONTINUE
 400  CONTINUE
C        IF NO LATITUDES OR LONGITUDES WERE FOUND SET ICND = 1.      
      IF(NUM.EQ.0)ICND=1
 900  RETURN
      END
