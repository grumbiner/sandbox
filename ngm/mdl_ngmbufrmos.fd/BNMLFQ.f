      SUBROUTINE BNMLFQ(NSTA,CSTA,RDATA,KDATA,ID,LPROJ,ICON,SCALE,
     *           REF,NDATE,MAXSTA,NUMPRJ,IVPRJ,NVPRJ,ICND)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BNMLFQ      GETS NORMALS/REL. FREQS. FROM MOSCONST     
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 93-09-09             
C                                                                       
C ABSTRACT: READS NORMALS/REL. FREQUENCIES PERFORMS THE APPROPRIATE     
C           SCALING AND CONVERSION.  THE DATA ARE ADDED TO KDATA(,)     
C           FOR LATER USE IN PACKING.  MISSING VALUES (9999) ARE        
C           INSERTED FOR PROJECTIONS AT WHICH THE FORECASTS ARE NOT     
C           FOUND OR VALID.  THE NORMALS/REL. FREQUENCIES ARE READ      
C           FROM THE MOSCONST FILE.                                     
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-09-09  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C        SEPT 1993   GILBERT    TDL    NAS9000                          
C                                                                       
C        PURPOSE                                                        
C            READS NORMALS/REL. FREQUENCIES PERFORMS THE APPROPRIATE    
C            SCALING AND CONVERSION.  THE DATA ARE ADDED TO KDATA(,)    
C            FOR LATER USE IN PACKING.  MISSING VALUES (9999) ARE       
C            INSERTED FOR PROJECTIONS AT WHICH THE FORECASTS ARE NOT    
C            FOUND OR VALID.  THE NORMALS/REL. FREQUENCIES ARE READ     
C            FROM THE MOSCONST FILE.                                    
C                                                                       
C        DATA SET USE                                                   
C            FT06     - PRINT FILE (OUTPUT)                             
C            MOSCONST - OSD.PROD.TDL.USCONST                            
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
C                SCALE = BUFR TABLE B; SCALING VALUE                    
C                  REF = BUFR TABLE B; REFERENCE VALUE                  
C                 ICND = ERROR FLAG                                     
C                 ICYC = CURRENT CYCLE  (00 OR 12)                      
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
      ICODE=ID(1)
      DO 400 L=1,NUMPRJ
C        IF NOT A VALID PROJECTION, FILL KDATA(,) WITH MISSING (9999)   
C        VALUES.                                                        
        IF (N.GT.NVPRJ) GOTO 250
        IF (LPROJ(L).NE.IVPRJ(N)) GOTO 250
        N=N+1
C        DETERMINES APPROPRIATE CONSTANT IDS, READS CONSTANTS, AND      
C        PERFORMS ANY NECESSARY CONVERSIONS.                            
        ID(3)=LPROJ(L)
        CALL GETCON(NSTA,CSTA,ICODE,ID,RDATA,ICON,NDATE,NERR)
        IF (NERR.EQ.0) THEN
          NUM=NUM+1
C        MULTIPLY SCALING FACTOR AND SUBTRACT REFERENCE VALUE FROM EACH 
C        DATA VALUE                                                     
          DO 200 I=1,NSTA
            IF (RDATA(I).NE.9999.) THEN
              RDATA(I)=(RDATA(I)*RMULT)-REF
              KDATA(I,L)=INT(RDATA(I)+SIGN(.5,RDATA(I)))
            ELSE
              KDATA(I,L)=9999
            ENDIF
 200      CONTINUE
          GOTO 400
        ENDIF
 250    DO 300 I=1,NSTA
          KDATA(I,L)=9999
 300    CONTINUE
 400  CONTINUE
C        IF NO FORECASTS WERE FOUND SET ICND = 1.                       
      IF(NUM.EQ.0)ICND=1
 900  RETURN
      END
