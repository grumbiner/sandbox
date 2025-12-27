      SUBROUTINE BSPECS(NSTA,KDATA,SCALE,REF,MAXSTA,NUMPRJ,LPROJ,
     *           IVPRJ,NVPRJ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BSPECS      STORES DIFFERENT USER SPECIFIED VALUES     
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-17             
C                                                                       
C ABSTRACT: STORES USER SPECIFIED VALUES FOR SPECIFIED PROJECTIONS      
C           INTO KDATA(,) TO BE PACKED LATER.  ALSO, THE SPECIFIED      
C           VALUES ARE SCALED ACCORDINGLY.                              
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-17  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        JULY 1992    GILBERT    TDL    NAS9000                         
C                                                                       
C        PURPOSE                                                        
C            STORES USER SPECIFIED VALUES FOR SPECIFIED PROJECTIONS     
C            INTO KDATA(,) TO BE PACKED LATER.  ALSO, THE SPECIFIED     
C            VALUES ARE SCALED ACCORDINGLY.                             
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
C            IVALS() = USER SPECIFIED VALUE TO BE INSERTED IN THE       
C                      BUFR MESSAGE.                                    
C              SCALE = BUFR TABLE B; SCALING VALUE                      
C                REF = BUFR TABLE B; REFERENCE VALUE                    
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      INTEGER KDATA(MAXSTA,NUMPRJ),LPROJ(NUMPRJ),IVPRJ(NVPRJ),IVALS(20)
      IMULT=10**INT(SCALE)
      IREF=INT(REF)
      N=1
C        READ SPECIFIED VALUES FROM INPUT CARDS.                        
      READ(5,50) (IVALS(I),I=1,NVPRJ)
 50   FORMAT(20I3)
C        DO FOR EACH PROJECTION                                         
      DO 200 L=1,NUMPRJ
C        IF NOT A SPECIFIED PROJECTION, INSERT MISSING (9999).          
        IF (N.GT.NVPRJ) N=1
        IF (LPROJ(L).EQ.IVPRJ(N)) THEN
C        ADD SCALED SPECIFIED VALUE TO KDATA(,) FOR EACH STATION        
          ITEMP=(IVALS(N)*IMULT)-IREF
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
