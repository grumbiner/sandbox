      SUBROUTINE TJREAD                                                 
C                                                                       
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***                               
C                                                                       
C SUBPROGRAM:    TJREAD      WRITES FIELDS TO DISK FILE 80              
C   PRGMMR:  R. M. REAP      ORG: W/OSD21           DATE: 95-11-01      
C                                                                       
C ABSTRACT:  READS WIND COMPONENTS, SEA-SURFACE TEMPERATURES, AND MOS   
C   VARIABLES AND TRANSFERS TO DISK FILE 80                             
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   87-11-01  R. M. REAP                                                
C   92-09-18  CONVERT TO FORTRAN 77                                     
C   94-09-14  CHANGE FT80 RECORD NUMBERS FOR JTAB & JTABX ARRAYS        
C   95-11-01  CONVERT TO CRAY                                           
C   99-09-30  J. P. DALLAVALLE
C                - MODIFIED 2ND CALL TO OUTPT1 TO ADD A MISSING
C                  ARGUMENT IN THE CALL STATEMENT.  THIS WAS DONE
C                  FOR THE CONVERSION TO THE IBM SP AND MAY BE A
C                  POTENTIAL PROBLEM.
C                    
C                                                                       
C USAGE:                                                                
C                                                                       
C  SEE BELOW FOR MDL STANDARDS                                          
C                                                                       
C   SUBPROGRAM TJREAD                                                   
C         NOV 1995          R. M. REAP            MDL           CRAY    
C         PURPOSE                                                       
C           READS 1000,850,700,500 AND 300-MB WIND DATA (U,V,W) FROM    
C           NCEP NGM DATA SET. WIND DATA FOR A 24-HR FORECAST PERIOD    
C           ARE REFERENCED TO A 26X33 MDL GRID, CONVERTED TO GRID       
C           INTERVALS PER HOUR(HORIZONTAL),MILLIBARS PER HOUR(VERTICAL),
C           AND STORED ON DISK. ALSO READS SEA-SURFACE TEMPERATURES AND 
C           SELECTED NGM FIELDS AND TRANSFER TO DISK FOR MDL 26X33 GRID.
C           READS MOS VARIABLES FROM NGM DATA SET AND TRANSFERS TO DISK 
C           ALONG WITH TRAJECTORY PREDICTORS FOR MDL 26X33 GRID.        
C         DATA SET USE                                                  
C              FT80 (OUTPUT)                                            
C         COMMON BLOCKS                                                 
C              KDMY,BLOCK3                                              
C         SUBPROGRAMS CALLED:                                           
C              NTRNS,NGMWND,RDPE,WIND,OUTPT1,SMTR9               
C          LIBRARY:                                                     
C              COMMON,W3LIB,TDLLIB                                       
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE:  FORTRAN 90                                               
C   MACHINE:   CRAY                                                     
C$$$                                                                    
C                                                                       
      COMMON KDMY,PLX(13,17,4),PLY(13,17,4),PLP(13,17,4),NSTAP,AI,AJ,   
     1XCORD(150),YCORD(150),RHUM(13,17,4),INCR                          
      COMMON/BLOCK3/JTAB,JTABX,KEY                                      
      DIMENSION JPP(5),JII(3),A(26,33),AP(858),F3(26,33),               
     1IB(7),JB(15),JTAB(400,5),JTABX(25,400),KP(858)                    
      DIMENSION B(26,33),BP(858),P(5)                                   
      EQUIVALENCE(A(1,1),AP(1),KP(1))                                   
      EQUIVALENCE(B(1,1),BP(1))                                         
      DATA JPP/160,133,112,80,48/,JII/48,49,5/                          
      DATA P/1000.,850.,700.,500.,300./                                 
C    *******************************************************************
      READ (5,6) (IB(K),K=1,7)                                       
  6   FORMAT(7I1)                                                       
      READ (5,10) (JB(K),K=1,15)                                        
 10   FORMAT(15I1)                                                      
C           TRANSFER FNWF SEA-SURFACE TEMPS, NGM U,V,W WIND COMPONENTS, 
C           TEMPS, SURFACE PRESSURES, AND MOS VARIABLES TO LOCAL DISK   
C           FILE 84                                                     
      CALL NTRNS                                                        
      CALL NGMWND                                                       
      NFILE=84                                                          
C           TRANSFER SEA-SURFACE TEMPS TO DISK FILE 80                  
      CALL RDPE(AP,0,153,71,NFILE)   
      DO 18 I=1,26                                                      
      DO 18 J=1,33                                                      
      IF(A(I,J))18,14,14                                                
  14  IF(A(I,J)-1.0)16,18,18                                            
  16  A(I,J)=-1.0                                                       
  18  CONTINUE                                                          
      NRCD=76                                                           
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
C           CONVERT U,V,W WIND COMPS AND TRANSFER TO DISK FILE 80       
      NRCD=0                                                            
      DO 39 K=1,25,6                                                    
      J=0                                                               
      JTAU=K-1                                                          
      M=(JTAU/6)+1                                                      
      DO 21 L=1,2                                                       
      DO 21 N=1,5                                                       
      CALL RDPE(AP,JTAU,JPP(N),JII(L),NFILE)                            
      CALL WIND(A)                                                      
      NRCD=NRCD+1                                                       
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
      J=J+1                                                             
      IF(IB(M)-JB(J))21,20,21                                           
 20   CALL OUTPT1(A,JPP(N),JII(L))                                      
 21   CONTINUE                                                          
      DO 37 N=1,5                                                       
      CALL RDPE(AP,JTAU,JPP(N),JII(3),NFILE)                            
      DO 24 II=1,26                                                     
      DO 24 JJ=1,33                                                     
      F3(II,JJ)=A(II,JJ)*3.6                                            
 24   CONTINUE                                                          
      CALL SMTR9(F3,A)                                                  
      NRCD=NRCD+1                                                       
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
      J=J+1                                                             
      IF(IB(M)-JB(J))37,36,37                                           
 36   CALL OUTPT1(A,JPP(N),JII(L))                                             
 37   CONTINUE                                                          
 39   CONTINUE                                                          
C           TRANSFER NCEP SURFACE PRESSURES TO DISK FILE 80          
      NRCD=95                                                           
      DO 45 K=1,49,6                                                    
      JTAU=K-1                                                          
      CALL RDPE(AP,JTAU,153,72,NFILE)                                   
      NRCD=NRCD+1                                                       
      IF(NRCD.EQ.101) NRCD=110                                          
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
  45  CONTINUE                                                          
C           TRANSFER NCEP TEMPERATURES TO DISK FILE 80                  
      NRCD=76                                                           
      DO 46 K=1,49,6                                                    
      JTAU=K-1                                                          
      DO 46 N=1,3                                                       
      CALL RDPE(AP,JTAU,JPP(N),16,NFILE)                                
      NRCD=NRCD+1                                                       
      IF(NRCD.EQ.92) NRCD=115                                           
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
 46   CONTINUE                                                          
C           TRANSFER 24-HR 500 MB NGM TEMPERATURES TO DISK FILE 80      
      CALL RDPE(AP,24,80,16,NFILE)                                      
      NRCD=92                                                           
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
C           TRANSFER 00-HR 500,300-MB TEMPERATURES TO DISK FILE 80      
      CALL RDPE(AP,0,80,16,NFILE)                                       
      NRCD=106                                                          
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
      CALL RDPE(AP,0,48,16,NFILE)                                       
      NRCD=107                                                          
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
C           READ 00-HR 1000, 850, 700, 500, AND 300-MB RELATIVE HUMIDITY
C           AND TEMPERATURE FROM DISK 84.  CONVERT RELATIVE HUMIDITY TO 
C           DEW POINT TEMPERATURE AND WRITE TO DISK 80.                 
      NRCD=100                                                          
      DO 52 K=1,5                                                       
      CALL RDPE(AP,0,JPP(K),17,NFILE)                                   
      CALL RDPE(BP,0,JPP(K),16,NFILE)                                   
      DO 50 I=1,26                                                      
      DO 50 J=1,33                                                      
      TEMP1=B(I,J)+273.16                                               
      PRES1=P(K)*0.1                                                    
      SVAP1=0.61078*2.71828**((17.269*(TEMP1-273.16))/(TEMP1-35.86))    
      SMXR1=0.622*(SVAP1/(PRES1-SVAP1))                                 
      IF(A(I,J).LE.0) A(I,J)=1.0                                        
      XXR1=(A(I,J)/100.)*SMXR1                                          
      AVAP=(XXR1*PRES1)/(XXR1+0.622)                                    
      DDD=ALOG(AVAP)+0.49299                                            
      A(I,J)=((4717.20-(35.86*DDD))/(17.269-DDD))-273.16                
 50   CONTINUE                                                          
      NRCD=NRCD+1                                                       
      WRITE(80,REC=NRCD) (AP(KK),KK=1,858)                              
 52   CONTINUE                                                          
C           WRITE JTAB ON DISK FILE 80                                  
C           KCN=TOTAL NUMBER OF NGM FIELDS READ FROM DISK               
      KCN=INCR                                                          
      IND=1                                                             
      NRCD=129                                                          
      DO 64 I=1,KCN                                                     
      DO 64 J=1,5                                                       
      KP(IND)=JTAB(I,J)                                                 
      IF(IND-858)60,54,60                                               
 54   NRCD=NRCD+1                                                       
      WRITE(80,REC=NRCD) (KP(KK),KK=1,858)                              
      IND=0                                                             
 60   IND=IND+1                                                         
 64   CONTINUE                                                          
      DO 66 I=IND,858                                                   
      KP(I)=0                                                           
 66   CONTINUE                                                          
      NRCD=NRCD+1                                                       
      WRITE(80,REC=NRCD) (KP(KK),KK=1,858)                              
      IF(NRCD-130)680,660,680                                           
 660  DO 670 K=1,858                                                    
      KP(K)=0                                                           
 670  CONTINUE                                                          
      NRCD=131                                                          
      WRITE(80,REC=NRCD) (KP(K),K=1,858)                                
      NRCD=132                                                          
      WRITE(80,REC=NRCD) (KP(K),K=1,858)                                
C           WRITE JTABX ON DISK FILE 80                                 
 680  IND=1                                                             
      NRCD=132                                                          
      DO 70 I=1,KCN                                                     
      DO 70 J=1,25                                                      
      KP(IND)=JTABX(J,I)                                                
      IF(IND-858)68,67,68                                               
 67   NRCD=NRCD+1                                                       
      WRITE(80,REC=NRCD) (KP(KK),KK=1,858)                              
      IND=0                                                             
 68   IND=IND+1                                                         
 70   CONTINUE                                                          
      DO 74 I=IND,858                                                   
      KP(I)=0                                                           
 74   CONTINUE                                                          
      NRCD=NRCD+1                                                       
      WRITE(80,REC=NRCD) (KP(KK),KK=1,858)                              
      IF(NRCD-133)80,76,80                                              
 76   DO 78 K=1,858                                                     
      KP(K)=0                                                           
 78   CONTINUE                                                          
      NRCD=134                                                          
      WRITE(80,REC=NRCD) (KP(K),K=1,858)                                
      NRCD=135                                                          
      WRITE(80,REC=NRCD) (KP(K),K=1,858)                                
 80   WRITE(6,82)
 82   FORMAT(5X,'END PROGRAM TJREAD')
      RETURN                                                            
      END                                                               
