      SUBROUTINE DATA(RPRES,RTEMP,RDEWPT,STA,BLK)                       
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    DATA        PRINTS RAOB DATA                           
C   PRGMMR: R. M. REAP       ORG: W/OSD21         DATE: 96-03-12        
C                                                                       
C ABSTRACT:  PRINTS RAOB DATA FOR ALL AVAILABLE LEVELS                  
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   67-07-01  R. M. REAP                                                
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-02  CONVERT TO CRAY                                           
C   96-03-12  CONVERT TO BUFR FORMAT 
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM DATA                                                      
C        MAR 1996       R. M. REAP        MDL         CRAY              
C        PURPOSE                                                        
C        PRINTS RAOB TEMPS AND DEWPTS FOR ALL LEVELS                    
C        COMMON BLOCKS                                                  
C          KDMY                                                         
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90                                                
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR,KSFC,KSHP,KUPA           
      DIMENSION RPRES(150,40),RTEMP(150,40),RDEWPT(150,40),             
     1 STA(150),BLK(150),LIST(150),JPOS(150)                            
C     ****************************************************************  
C        SORT PRINT BY BLOCK/STATION NUMBER  
      DO 80 J=1,NSTAP 
      ISTA=STA(J)
      IBLK=BLK(J)
      LIST(J)=(IBLK*1000)+ISTA
   80 CONTINUE
      CALL SORT1(LIST,JPOS,NSTAP)     
C        PRINT SOUNDING BY INCREASING BLOCK/STATION NUMBER
      WRITE(6,84)                                                       
   84 FORMAT(///23X,14HUPPER-AIR DATA,//,2X,5HYCORD,5X,5HXCORD,5X,      
     18HPRESSURE,5X,11HTEMPERATURE,5X,9HDEW POINT)                      
      DO 100 I=1,NSTAP
      K=JPOS(I) 
      ISTA=STA(K)                                                       
      IBLK=BLK(K)                                                       
      WRITE(6,93) ISTA,IBLK                                             
   93 FORMAT(//,2X,'STATION',I5,5X,'BLOCK',I5,/)                        
      DO 96 J=1,40                                                      
      WRITE(6,95) YCORD(K),XCORD(K),RPRES(K,J),RTEMP(K,J),RDEWPT(K,J)   
   95 FORMAT(2X,F5.2,5X,F5.2,7X,F5.0,8X,F6.1,9X,F6.1)                   
   96 CONTINUE                                                          
  100 CONTINUE                                                          
      RETURN                                                            
      END                                                               
