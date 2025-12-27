      SUBROUTINE OUTPT3(NDSK)                                           
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    OUTPT3      PRINTS NET VERTICAL DISPLACEMENTS          
C
C
C   PRGMMR: R. M. REAP       ORG: W/OSD21         DATE: 96-02-29        
C ABSTRACT:  PREPARES DATA AND PRINTS SAMPLE NVD'S                
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   67-07-01  R. M. REAP                                                
C   74-02-01  CONVERT TO IBM SYSTEM                                     
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   95-11-01  CONVERT TO CRAY                                           
C   96-02-29  ELIMINATE GRDPRT CALL                                     
C USAGE:                                                                
C   SEE BELOW FOR MDL STANDARDS                                         
C     PROGRAM OUTPT3                                                    
C        FEB 1996      R. M. REAP        MDL         CRAY               
C        PURPOSE                                                        
C        NET VERTICAL DISPLACEMENTS ARE OUTPUT AT END OF          
C        FORECAST PERIOD.                                               
C        DATA SET USE                                                   
C           NDSK (INPUT FROM DISK)                                      
C        COMMON BLOCKS                                                  
C           KDMY,BLOCK5,BLOCKF                                          
C        LIBRARY:                                                       
C           COMMON,W3LIB,TDLLIB                                          
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: FORTRAN 90
C   MACHINE:  CRAY                                                      
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCK5/XXX(13,17,4),YYY(13,17,4),ZZZ(13,17,4)              
      COMMON/BLOCKF/ZDF(13,17,4),JSW                                    
      DIMENSION DATA(13,17)                     
C     ******************************************************************
      ND=NDSK                                                           
      JTAU=12                                                           
C        READ 00-HR AND 12-HR PARCEL PRESSURES FROM NDSK                
      KST=0                                                             
      NR1=3                                                             
      READ(ND,REC=NR1,ERR=2) (((XXX(I,J,N),N=1,4),J=1,17),I=1,13)       
      GO TO 4                                                           
  2   KST=1                                                             
  4   NR2=9                                                             
      READ(ND,REC=NR2,ERR=5) (((YYY(I,J,N),N=1,4),J=1,17),I=1,13)       
      IF(KST)5,8,5                                                      
  5   PRINT 6                                                           
  6   FORMAT(10X,'OUTPT3 DISK READ PROBLEM')                            
  8   DO 10 I=1,13                                                      
      DO 10 J=1,17                                                      
      DO 10 N=1,3                                                       
      ZZZ(I,J,N)=YYY(I,J,N)-XXX(I,J,N)+ZDF(I,J,N)                       
  10  CONTINUE                                                          
      DO 18 N=1,3                                                       
      DO 12 I=1,13                                                      
      DO 12 J=1,17                                                      
      DATA(I,J)=ZZZ(I,J,N)                                              
  12  CONTINUE                                                          
      DO 15 I=1,13 
      II=14-I  
      WRITE(6,14) (DATA(II,J),J=8,17)
  14  FORMAT(3X,10F7.1)
  15  CONTINUE 
      WRITE(6,16) JTAU,N                                                
  16  FORMAT(16X,'NET VERT DISP (MB/',I2,'-HR)   LEVEL = ',I4)      
  18  CONTINUE                                                          
      RETURN                                                            
      END                                                               
