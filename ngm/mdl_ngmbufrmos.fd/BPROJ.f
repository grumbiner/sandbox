      SUBROUTINE BPROJ(NSTA,KDATA,LPROJ,SCALE,REF,MAXSTA,NUMPRJ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BPROJ       STORES PROJECTIONS                         
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-17             
C                                                                       
C ABSTRACT: STORES THE LIST OF PROJECTIONS IN KDATA(,) FOR EACH         
C   STATION.  SCALING IS DONE BEFORE ADDING TO KDATA(,).                
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-17  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        JULY 1992     GILBERT     TDL    NAS9000                       
C                                                                       
C        PURPOSE                                                        
C            STORES THE LIST OF PROJECTIONS IN KDATA(,) FOR EACH        
C            STATION.  SCALING IS DONE BEFORE ADDING TO KDATA(,).       
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
C               NSTA = NUMBER OF STATIONS                               
C             NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE                 
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
      INTEGER KDATA(MAXSTA,NUMPRJ),LPROJ(NUMPRJ)
      IMULT=10**INT(SCALE)
      IREF=INT(REF)
      DO 200 L=1,NUMPRJ
        ITEMP=(LPROJ(L)*IMULT)-IREF
        DO 100 I=1,NSTA
          KDATA(I,L)=ITEMP
 100    CONTINUE
 200  CONTINUE
      RETURN
      END
