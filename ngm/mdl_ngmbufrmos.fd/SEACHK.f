C                                                                       
      SUBROUTINE SEACHK(IDATE,KDATA,MAXSTA,NUMPRJ,WMO,NWMO,NBUL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    SEACHK      CHECKS FOR VALID SNOW SEASON               
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 95-08-10             
C                                                                       
C ABSTRACT: THIS SUBROUTINE SETS FORECAST VALUES TO MISSING IF          
C   THE CURRENT DATE IS OUTSIDE THE VALID SEASON.  THIS IS USED         
C   FOR PRECIP TYPE, SNOW AMOUNT, AND CPOS.                             
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   95-08-10  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR TDL STANDARDS                                         
C                                                                       
C     SUBROUTINE SEACHK                                                 
C                                                                       
C     AUG  1995   GILBERT    TDL   NAS9000                              
C                                                                       
C        PURPOSE                                                        
C           THIS SUBROUTINE SETS FORECAST VALUES TO MISSING IF          
C   THE CURRENT DATE IS OUTSIDE THE VALID SEASON.  THIS IS USED         
C   FOR PRECIP TYPE, SNOW AMOUNT, AND CPOS.                             
C                                                                       
C        VARIABLES                                                      
C           IDATE = CURRENT DATE (MONTH*100)+DAY                        
C        KDATA(,) = HOLDS DATA FOR A FORECAST ELEMENT FOR EACH          
C                   STATION AND EACH PROJECTION. 1ST DIMENSION IS       
C                   # OF STATIONS, AND 2ND DIMENSION IS NUMBER OF       
C                   PROJECTIONS.  CONTAINS MISSING "9999" FOR           
C                   PROJECTION FOR WHICH FORECASTS ARE NOT VALID.       
C          MAXSTA = MAXIMUM NUMBER OF STATIONS                          
C          NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE                    
C          WMO(I) = LIST OF WMO HEADERS                                 
C         NWMO(I) = NUMBER OF STATIONS IN WMO HEADER WMO(I).            
C            NBUL = NUMBER OF BUFR BULLETINS (WMO HEADERS)              
C          IDATE1 = END DATE OF VALID PERIOD                            
C          IDATE2 = START DATE OF THE VALID PERIOD                      
C          NUMSTA = COUNTER FOR THE NUMBER OF STATIONS                  
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      INTEGER KDATA(MAXSTA,NUMPRJ),NWMO(NBUL)
      CHARACTER*10 WMO(NBUL)
      NUMSTA=0
      DO 900 N=1,NBUL
        IF (WMO(N)(6:6).LT.'7') THEN
          IDATE1=515
          IDATE2=916
        ELSE
          IDATE1=531
          IDATE2=901
        ENDIF
        IF ((IDATE.GT.IDATE1).AND.(IDATE.LT.IDATE2)) THEN
          DO 400 I=1,NWMO(N)
            NUMSTA=NUMSTA+1
            DO 300 J=1,NUMPRJ
              KDATA(NUMSTA,J)=9999
 300        CONTINUE
 400      CONTINUE
        ELSE
          NUMSTA=NUMSTA+NWMO(N)
        ENDIF
 900  CONTINUE
      RETURN
      END
