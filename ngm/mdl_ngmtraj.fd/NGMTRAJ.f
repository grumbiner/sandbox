C$$$  MAIN PROGRAM DOCUMENTATION BLOCK  ***                             
C 
C MAIN PROGRAM: MDL_NGMTRAJ
C   PRGMMR: MERICKSON        ORG: OSD211      DATE: 2000-03-17
C                                                                       
C ABSTRACT: PROGRAM GENERATES THREE-DIMENSIONAL TRAJECTORY MODEL        
C   FORECASTS.  INDIVIDUAL SUBPROGRAMS AND THEIR FUNCTIONS ARE          
C   DESCRIBED BELOW:                                                    
C                                                                       
C     TJREAD - READS SELECTED NGM FORECAST FIELDS AND TRANSFERS TO      
C              DISK FOR LATER USE.                                      
C     TJTRAJ - COMPUTES THREE-DIMENSIONAL PARCEL TRAJECTORIES FROM NGM  
C              WINDS AND OUTPUTS 6-HOURLY X,Y,P POSITIONS TO DISK.      
C     TJANAL - DECODES UPPER-AIR AND SFC REPORTS FROM NCEP DATA SETS.   
C              PERFORMS OBJECTIVE ANALYSIS OF INITIAL TEMPERATURE AND   
C              MOISTURE FIELDS AT TRAJECTORY ORIGIN POINTS.             
C     TJFCST - COMPUTES 6-HOURLY VARIATIONS OF TEMPERATURE AND MOISTURE 
C              FOR AIR PARCELS MOVING ALONG 3D TRAJECTORIES TO OBTAIN   
C              PROGNOSTIC SOUNDINGS AT SPECIFIED GRID POINTS. COMPUTES  
C              AIR MASS TRANSFORMATION RESULTING FROM AIR-SEA HEAT AND  
C              MOISTURE FLUX.                                           
C     TJPROJ - COMPUTES 3D PARCEL TRAJECTORIES FOR 36-HR AND 48-HR      
C              PROJECTIONS FROM NCEP WINDS AND OUTPUTS 6-HR X,Y,P PARCEL
C              POSITIONS TO DISK                                        
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   87-11-20  R. M. REAP                                                
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   93-02-24  REORGANIZE, RENAME, AND STREAMLINE MODEL CODE             
C   94-09-14  INCREASE NTRNS RECORDS TO 340, UPDATE DATA CARDS,         
C             INCREASE FT85 SPACE TO SIX CYLINDERS TO ACCOMODATE        
C             FIELDS NEEDED FOR NGM-BASED PROBS, AND CHANGE FT80        
C             RECORD NUMBERS FOR JTAB AND JTABX ARRAYS                  
C   95-05-02  INCREASE NTRNS RECORDS TO 400, UPDATE DATA CARDS          
C   96-03-11  CONVERT TO CRAY BUFR FORMAT
C   96-09-17  CONVERT TO NEW BUFR DATASET
C   98-05-06  FORTRAN 90 AND YEAR-2000 COMPATIBILITY	
C   00-03-10  JPD -- MODIFIED TO USE BAOPEN AND CORRECT CALL TO 
C             GETGB FOR GRIB FILES
C                                                                       
C USAGE:                                                                
C                                                                       
C   SEE BELOW FOR MDL STANDARDS                                         
C                                                                       
C     PROGRAM NGMTRAJ                                                   
C        MAY 1998   R. M. REAP         MDL          CRAY                
C        PURPOSE                                                        
C            PROGRAM GENERATES 3D TRAJECTORY MODEL FORECASTS FROM       
C            NGM WIND INPUT AND OBSERVED SURFACE AND UPPER-AIR DATA     
C        DATA SET USE                                                   
C            GES, RF00, RF12, .... RF48 (INPUT)                         
C            ADPUPA, ADPSFC, SFCSHP (INPUT)                             
C            JCDATA DATA CARDS (INPUT)                                  
C            FT80-89 - UTILITY DATA SETS (INTERNAL)                     
C        VARIABLES                                                      
C            DEFINED IN SUBPROGRAMS TJREAD,TJTRAJ,TJANAL,TJFCST, AND    
C            TJPROJ                                                     
C        COMMON BLOCKS                                                  
C            KDMY                                                       
C        SUBPROGRAMS CALLED:                                            
C            TJREAD,TJTRAJ,TJANAL,TJFCST,TJPROJ                         
C        PROGRAM STOPS                                                  
C            SEE INDIVIDUAL PROGRAMS                                    
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C                                                                       
C$$$                                                                    
C                                                                       
      PROGRAM NGMTRAJ
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR,KSFC,KSHP,KUPA           
      COMMON/BLOCKP/KPRINT,KERR                                         
C     ******************************************************************
      CALL W3TAGB('MDL_NGMTRAJ',2000,0077,0064,'OSD211')
C        DEFINE ON/OFF INDICATOR FOR PRINT DIAGNOSTICS                  
C          (KPRINT > 0 FOR PRINT)                                       
      KPRINT=0                                                          
C        SET ERROR INDICATOR 
      KERR=0
C        DEFINE FILE NUMBER FOR SURFACE LAND STATION OBSERVATIONS       
      KSFC=40                                                           
C        DEFINE FILE NUMBER FOR SURFACE SHIP OBSERVATIONS               
      KSHP=41                                                           
C        DEFINE FILE NUMBER FOR UPPER-AIR (RAOB) OBSERVATIONS           
      KUPA=42                                                           
      OPEN(80,STATUS='UNKNOWN',ACCESS='DIRECT',recl=6864)
      OPEN(81,STATUS='UNKNOWN',ACCESS='DIRECT',recl=34320)
      OPEN(82,STATUS='UNKNOWN',ACCESS='DIRECT',recl=7072)
      OPEN(83,STATUS='UNKNOWN',ACCESS='DIRECT',recl=26520)
      OPEN(84,STATUS='UNKNOWN',ACCESS='DIRECT',recl=6864)
      OPEN(85,STATUS='UNKNOWN',ACCESS='DIRECT',recl=19080)   
      OPEN(87,STATUS='UNKNOWN',ACCESS='DIRECT',recl=7072)
      OPEN(88,STATUS='UNKNOWN',ACCESS='DIRECT',recl=7072)
      OPEN(89,STATUS='UNKNOWN',ACCESS='DIRECT',recl=6864)
      CALL TJREAD                                                       
      CALL TJTRAJ                                                       
      CALL TJANAL                                                       
      KPR=0                                                             
      CALL TJFCST(KPR)                                                  
      CALL TJPROJ                                                       
      CALL W3TAGE('MDL_NGMTRAJ')
      IF(KERR.NE.0) CALL EXIT(KERR)  
      STOP                                                              
      END                                                               
