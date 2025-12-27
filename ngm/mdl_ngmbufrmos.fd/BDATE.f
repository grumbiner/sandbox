      SUBROUTINE BDATE(NSTA,KDATA,NVAL,SCALE,REF,MAXSTA,NUMPRJ,LPROJ,
     *           IVPRJ,NVPRJ,NDATE)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BDATE       STORES INITIAL DATE/TIME                   
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 93-10-22             
C                                                                       
C ABSTRACT: STORES INITIAL DATE/TIME INTO KDATA(,).  THE VALUE IN NVAL  
C           INDICATES WHETHER TO STORE THE YEAR, MONTH, DAY, OR         
C           CYCLE.  THE VALUE IS SCALED ACCORDINGLY.                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-10-22  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        JULY 1992    GILBERT    TDL    NAS9000                         
C                                                                       
C        PURPOSE                                                        
C           STORES INITIAL DATE/TIME INTO KDATA(,).  THE VALUE IN NVAL  
C           INDICATES WHETHER TO STORE THE YEAR, MONTH, DAY, OR         
C           CYCLE.  THE VALUE IS SCALED ACCORDINGLY.                    
C                                                                       
C        DATA SET USE                                                   
C            NONE                                                       
C                                                                       
C        VARIABLES                                                      
C               MAXSTA = MAXIMUM NUMBER OF STATIONS                     
C              LPROJ() = LIST OF PROJECTIONS FOR WHICH FORECASTS ARE    
C                        ENCODED                                        
C             KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH     
C                        STATION AND EACH PROJECTION. 1ST DIMENSION IS  
C                        # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF  
C                        PROJECTIONS.  CONTAINS MISSING "9999" FOR      
C                        PROJECTION FOR WHICH FORECASTS ARE NOT VALID.  
C              IVPRJ() = CONTAINS VALID PROJECTIONS FOR THE CURRENT     
C                        FORECAST ELEMENT.                              
C               NSTA = NUMBER OF STATIONS                               
C             NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE                 
C            IVPRJ() = LIST OF VALID PROJECTIONS FOR CURRENT FORECAST   
C                      ELEMENT.                                         
C              NVPRJ = NUMBER OF VALID PROJECTIONS                      
C               NVAL = VALUE INDICATING WHETHER TO STORE YEAR, MONTH,   
C                      DAY OR CYCLE.                                    
C              SCALE = BUFR TABLE B; SCALING VALUE                      
C                REF = BUFR TABLE B; REFERENCE VALUE                    
C              NDATE = INITIAL DATE OF FORECAST RUN                     
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      INTEGER KDATA(MAXSTA,NUMPRJ),LPROJ(NUMPRJ),IVPRJ(NVPRJ)
      IMULT=10**INT(SCALE)
      IREF=INT(REF)
      N=1
      IF (NVAL.EQ.1) THEN
        NTEMP=NDATE/1000000
      ELSEIF (NVAL.EQ.2) THEN
        NTEMP=MOD(NDATE,1000000)/10000
      ELSEIF (NVAL.EQ.3) THEN
        NTEMP=MOD(NDATE,10000)/100
      ELSEIF (NVAL.EQ.4) THEN
        NTEMP=MOD(NDATE,100)
      ENDIF
      ITEMP=(NTEMP*IMULT)-IREF
C        DO FOR EACH PROJECTION                                         
      DO 200 L=1,NUMPRJ
C        IF NOT A SPECIFIED PROJECTION, INSERT MISSING (9999).          
        IF (N.GT.NVPRJ) N=1
        IF (LPROJ(L).EQ.IVPRJ(N)) THEN
C        ADD SCALED SPECIFIED VALUE TO KDATA(,) FOR EACH STATION        
          DO 100 I=1,NSTA
            KDATA(I,L)=ITEMP
 100      CONTINUE
          N=N+1
        ELSE
          DO 150 I=1,NSTA
            KDATA(I,L)=9999
 150      CONTINUE
        ENDIF
 200  CONTINUE
      RETURN
      END
