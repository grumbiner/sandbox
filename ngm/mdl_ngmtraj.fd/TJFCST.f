      SUBROUTINE TJFCST(NPR)                                            
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    TJFCST      COMPUTES PARCEL TEMP/DEW POINT             
C   PRGMMR: R. M. REAP       ORG: W/OSD21           DATE: 95-11-02      
C                                                                       
C ABSTRACT:  COMPUTES 6-H TEMPS/DEW POINTS OF PARCELS ALONG 3D TRAJS    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   70-11-01 R. M. REAP                                                 
C   71-07-01 AIR-SEA HEAT FLUX MODIFICATION                             
C   74-03-01 CONVERT TO IBM SYSTEM                                      
C   89-11-20 CONVERT TO 36 AND 48 HR FORECAST PROJECTIONS               
C   92-09-18 CONVERT TO FORTRAN 77                                      
C   95-11-02 CONVERT TO CRAY                                            
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM TJFCST                                                    
C        NOV 1995     R. M. REAP         MDL           CRAY             
C        PURPOSE                                                        
C        SIX-HOURLY VARIATIONS OF TEMPERATURE AND MOISTURE ARE          
C        COMPUTED FOR PARCELS MOVING ALONG 3-D TRAJECTORIES.  THE       
C        PARCELS ARE LIFTED DRY-ADIABATICALLY UNTIL SATURATED AND       
C        PSEUDO-ADIABATICALLY THEREAFTER TO OBTAIN PROGNOSTIC SOUNDINGS 
C        OF TEMPERATURE AND MOISTURE OVER SPECIFIED POINTS.  COMPUTES   
C        AIR MASS TRANSFORMATION RESULTING FROM AIR-SEA HEAT AND        
C        MOISTURE FLUX. OUTPUTS 24-HR FORECAST FIELDS ON CONTOURED MAPS.
C        DATA SET USE                                                   
C          FT82, FT87, FT88 (INPUT,OUTPUT)                              
C        COMMON BLOCKS                                                  
C          KDMY,BLOCKG                                                  
C        SUBPROGRAMS CALLED:                                            
C          TRACE,SEAMOD,UPDATE,OUTPUT                                   
C         LIBRARY:                                                      
C          COMMON,W3LIB,TDLLIB                                           
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCKG/TEMP(13,17,4),DEWPT(13,17,4)                        
      COMMON/BLOCKP/KPRINT                                              
      DIMENSION SAV(13,17,4)                                            
C     ***************************************************************** 
      DO 180 I=1,13                                                     
      DO 180 J=1,17                                                     
      DO 180 N=1,3                                                      
      TEMP(I,J,N)=PLX(I,J,N)                                            
      DEWPT(I,J,N)=PLY(I,J,N)                                           
  180 CONTINUE                                                          
      MXNR=AI                                                           
      CALL TRACE(MXNR)                                                  
      CALL SEAMOD(MXNR)                                                 
C        SMOOTH TEMPERATURE FORECASTS                                   
      DO 200 I=2,12                                                     
      DO 200 J=2,16                                                     
      DO 200 N=1,3                                                      
      IF(N-1)194,190,194                                                
  190 IF(J-8)192,192,194                                                
  192 SMF=1.0                                                           
      GO TO 196                                                         
  194 SMF=0.5                                                           
  196 TBAR=(TEMP(I,J+1,N)+TEMP(I,J-1,N)+TEMP(I+1,J,N)+TEMP(I-1,J,N))*.25
      PLP(I,J,N)=(TEMP(I,J,N)+SMF*TBAR)/(1.+SMF)                        
  200 CONTINUE                                                          
C        RECOMPUTE RELATIVE HUMIDITIES USING SMOOTHED TEMPERATURES AND  
C        UNSMOOTHED DEWPOINTS                                           
      NRCD=3                                                            
      READ(82,REC=NRCD,ERR=202) (((PLX(I,J,N),N=1,4),J=1,17),I=1,13)    
      GO TO 204                                                         
  202 PRINT 203                                                         
  203 FORMAT(10X,'TJFCST DISK READ PROBLEM/PARCEL TERMINAL PRESSURE',//)
  204 DO 210 I=2,12                                                     
      DO 210 J=2,16                                                     
      DO 210 N=1,3                                                      
      TEMP(I,J,N)=PLP(I,J,N)                                            
      IF(DEWPT(I,J,N)-TEMP(I,J,N))208,208,206                           
  206 DEWPT(I,J,N)=TEMP(I,J,N)                                          
  208 TEMP1=TEMP(I,J,N)+273.16                                          
      DEWPT1=DEWPT(I,J,N)+273.16                                        
      VAP1=0.61078*2.71828**((17.269*(DEWPT1-273.16))/(DEWPT1-35.86))   
      PRST=PLX(I,J,N)                                                   
      XXR1=0.622*(VAP1/(PRST-VAP1))                                     
      SVAP1=0.61078*2.71828**((17.269*(TEMP1-273.16))/(TEMP1-35.86))    
      SMXR1=0.622*(SVAP1/(PRST-SVAP1))                                  
      RHUM(I,J,N)=(XXR1/SMXR1)*100.                                     
  210 CONTINUE                                                          
      JTAU=INCR                                                         
      CALL UPDATE(KDMY,JDTH,JTAU)                                       
      IF(KPRINT.EQ.0) GO TO 226                                         
      CALL OUTPUT(JDTH)                                                 
  226 TEMP(13,17,4)=KDMY                                                
      ND=82                                                             
      IF(NPR.EQ.1) ND=87                                                
      IF(NPR.EQ.2) ND=88                                                
      NRCD=49                                                           
      IF(NPR.EQ.1) NRCD=22                                              
      IF(NPR.EQ.2) NRCD=28                                              
      WRITE(ND,REC=NRCD) (((TEMP(I,J,N),N=1,4),J=1,17),I=1,13)          
      NRCD=50                                                           
      IF(NPR.EQ.1) NRCD=23                                              
      IF(NPR.EQ.2) NRCD=29                                              
      WRITE(ND,REC=NRCD) (((DEWPT(I,J,N),N=1,4),J=1,17),I=1,13)         
      NRCD=35                                                           
      IF(NPR.EQ.1) NRCD=24                                              
      IF(NPR.EQ.2) NRCD=30                                              
      WRITE(ND,REC=NRCD) (((RHUM(I,J,N),N=1,4),J=1,17),I=1,13)          
      RETURN                                                            
      END                                                               
