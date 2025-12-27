      SUBROUTINE BCMAP(KDATA,MAXSTA,NUMPRJ,NSTA,ID)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BCMAP       MAPS MOS CATEGORIES TO BUFR CATEGORIES     
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 93-12-03             
C                                                                       
C ABSTRACT: MAPS MOS CATEGORICAL FORECAST VALUES TO THE CORRESPONDING   
C   BUFR CODE TABLE VALUES.                                             
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-12-03  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        DEC 1993   GILBERT   TDL  NAS9000                              
C                                                                       
C        PURPOSE                                                        
C             MAPS MOS CATEGORICAL FORECAST VALUES TO THE CORRESPONDING 
C             BUFR CODE TABLE VALUES.                                   
C                                                                       
C        DATA SET USE                                                   
C             NONE                                                      
C                                                                       
C        VARIABLES                                                      
C               MAXSTA = MAXIMUM NUMBER OF STATIONS                     
C             KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH     
C                        STATION AND EACH PROJECTION. 1ST DIMENSION IS  
C                        # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF  
C                        PROJECTIONS.  CONTAINS MISSING "9999" FOR      
C                        PROJECTION FOR WHICH FORECASTS ARE NOT VALID.  
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE               
C                 NSTA = NUMBER OF STATIONS                             
C                ID(4) = MOS FORECAST IDENTIFIER
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS                     
C   MACHINE:  NAS, CYBER, WHATEVER                                      
C                                                                       
C$$$                                                                    
      INTEGER KDATA(MAXSTA,NUMPRJ),ID(4)
C        MAP CATEGORICAL VALUES FOR 6-H SNOW AMT                        
      IF (ID(1).EQ.208401006) THEN
        DO 110 I=1,NSTA
          DO 100 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=2
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=0
            ENDIF
 100      CONTINUE
 110    CONTINUE
C        MAP CATEGORICAL VALUES FOR 12-H SNOW AMT.                      
      ELSEIF (ID(1).EQ.208402006) THEN
        DO 130 I=1,NSTA
          DO 120 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=5
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=0
            ENDIF
 120      CONTINUE
 130    CONTINUE
C        MAP CATEGORICAL VALUES FOR 6-H QPF                             
      ELSEIF (ID(1).EQ.203210006) THEN
        DO 150 I=1,NSTA
          DO 140 J=1,NUMPRJ
            IF (KDATA(I,J).NE.9999) KDATA(I,J)=6-KDATA(I,J)
 140      CONTINUE
 150    CONTINUE
C        MAP CATEGORICAL VALUES FOR 12-H QPF                            
      ELSEIF (ID(1).EQ.203220006) THEN
        DO 170 I=1,NSTA
          DO 160 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=7
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=6
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=4
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=3
            ELSEIF (KDATA(I,J).EQ.5) THEN
              KDATA(I,J)=2
            ELSEIF (KDATA(I,J).EQ.6) THEN
              KDATA(I,J)=1
            ELSEIF (KDATA(I,J).EQ.7) THEN
              KDATA(I,J)=0
            ENDIF
 160      CONTINUE
 170    CONTINUE
C        MAP CATEGORICAL VALUES CLOUD AMT                               
      ELSEIF (ID(1).EQ.208300006) THEN
        DO 190 I=1,NSTA
          DO 180 J=1,NUMPRJ
            IF (KDATA(I,J).EQ.1) THEN
              KDATA(I,J)=0
            ELSEIF (KDATA(I,J).EQ.2) THEN
              KDATA(I,J)=11
            ELSEIF (KDATA(I,J).EQ.3) THEN
              KDATA(I,J)=12
            ELSEIF (KDATA(I,J).EQ.4) THEN
              KDATA(I,J)=8
            ENDIF
 180      CONTINUE
 190    CONTINUE
      ENDIF
      RETURN
      END
