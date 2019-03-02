      SUBROUTINE TITLE                                                  
     1 (LELEM,NCELM,IY,IM,ID,IH,IFT,LEVEL,LABEL,NCHLB,MTITLE,NUMCHM)    
C                                                                       
      DIMENSION MONTH(12)                                               
      DIMENSION SEASON(4)                                               
C                                                                       
      CHARACTER*5 YEAR                                                  
      CHARACTER*7 SEASON                                                
      CHARACTER*5 MONTH                                                 
      CHARACTER*3 DAY                                                   
      CHARACTER*4 HOUR                                                  
      CHARACTER*8 KLEV                                                  
      CHARACTER*16 KT                                                   
      CHARACTER   FMT*26                                                
C                                                                       
      CHARACTER*16 LELEM                                                
C                                                                       
      CHARACTER*5 MEAN                                                  
      CHARACTER*20 DATES                                                
C                                                                       
      CHARACTER*80 LABEL                                                
      CHARACTER*80 MTITLE                                               
      DATA MAXCHA/80/                                                   
C                                                                       
      MONTH( 1)=' JAN.'                                                 
      MONTH( 2)=' FEB.'                                                 
      MONTH( 3)=' MAR.'                                                 
      MONTH( 4)=' APR.'                                                 
      MONTH( 5)=' MAY '                                                 
      MONTH( 6)=' JUN.'                                                 
      MONTH( 7)=' JUL.'                                                 
      MONTH( 8)=' AUG.'                                                 
      MONTH( 9)=' SEP.'                                                 
      MONTH(10)=' OCT.'                                                 
      MONTH(11)=' NOV.'                                                 
      MONTH(12)=' DEC.'                                                 
C                                                                       
      SEASON(1)=' WINTER'                                               
      SEASON(2)=' SPRING'                                               
      SEASON(3)=' SUMMER'                                               
      SEASON(4)='   FALL'                                               
C                                                                       
      MEAN=' MEAN'                                                      
C                                                                       
      NCHAL=NCELM+1                                                     
C                                                                       
      IF(IY.GE.9999.OR.IY.LE.0) THEN                                    
        YEAR=' '                                                        
        NCHD=1                                                          
        IF(IY.GT.9999.OR.IY.EQ.0) GOTO 100                              
      ELSE                                                              
        WRITE(YEAR,902) IY                                              
  902   FORMAT(1X,I4)                                                   
      ENDIF                                                             
C                                                                       
      IF(IM.EQ.9999) THEN                                               
      DATES=YEAR                                                        
      NCHD=5                                                            
      GO TO 100                                                         
      ENDIF                                                             
C                                                                       
      IF(IM.LE.0.OR.IM.GE.17) THEN                                      
      DATES=MEAN//YEAR                                                  
      NCHD=10                                                           
      GO TO 100                                                         
      ENDIF                                                             
C                                                                       
      IF(ID.EQ.9999.AND.IM.GE.1.AND.IM.LE.12) THEN                      
      DATES=MONTH(IM)//YEAR                                             
      NCHD=10                                                           
      GO TO 100                                                         
      ENDIF                                                             
C                                                                       
      IF(ID.EQ.0.AND.IM.GE.1.AND.IM.LE.12) THEN                         
      DATES=MEAN//MONTH(IM)//YEAR                                       
      NCHD=15                                                           
      GO TO 100                                                         
      ENDIF                                                             
C                                                                       
      IF(ID.EQ.9999.AND.IM.GE.13.AND.IM.LE.16) THEN                     
      DATES=SEASON(IM-12)//YEAR                                         
      NCHD=12                                                           
      GO TO 100                                                         
      ENDIF                                                             
C                                                                       
      IF(ID.EQ.0.AND.IM.GE.13.AND.IM.LE.16) THEN                        
      DATES=MEAN//SEASON(IM-12)//YEAR                                   
      NCHD=17                                                           
      GO TO 100                                                         
      ENDIF                                                             
C                                                                       
      IF(IH.EQ.9999.AND.IM.GE.1.AND.IM.LE.12) THEN                      
      DATES=YEAR//DAY//MONTH(IM)                                        
      NCHD=13                                                           
      GO TO 100                                                         
      ENDIF                                                             
C                                                                       
      WRITE(HOUR,900) IH                                                
  900 FORMAT(1X,I2,1HZ)                                                 
      WRITE(DAY,901) ID                                                 
  901 FORMAT(1X,I2)                                                     
      DATES=YEAR//DAY//MONTH(IM)//HOUR                                  
      NCHD=17                                                           
C                                                                       
  100 CONTINUE                                                          
C                                                                       
      IF(IFT.EQ.9999) THEN                                              
      KT=' '                                                            
      NCHK=1                                                            
      GO TO 200                                                         
      ENDIF                                                             
      IF(IFT.EQ.-9999) THEN                                             
      KT='ANALYSIS'                                                     
      NCHK=8                                                            
      GO TO 200                                                         
      ENDIF                                                             
      IF(IFT.EQ.-9998) THEN                                             
      KT='INITIALIZED'                                                  
      NCHK=11                                                           
      GO TO 200                                                         
      ENDIF                                                             
      IF(IFT.GT.1000000) THEN                                           
      IFTS = (IFT-1000000) / 1000                                       
      IFTE = IFT - (IFTS * 1000 + 1000000)                              
      FTS = IFTS
      FTE = IFTE
      IF(FTS.GT.0.) THEN                                                
        I1 = 1 + IFIX(ALOG10(FTS) + .000001)                            
      ELSE                                                              
        I1 = 1                                                          
      ENDIF                                                             
      IF(FTE.GT.0.) THEN                                                
        I2 = 1 + IFIX(ALOG10(FTE) + .000001)                            
      ELSE                                                              
        I2 = 1                                                          
      ENDIF                                                             
      WRITE(FMT,6000) I1, I2                                            
      WRITE(KT,FMT) IFTS, IFTE                                          
 6000 FORMAT(9H('DAY ',I,I1,6H,'-',I,I1,9H,' MEAN'))                    
      NCHK = 10 + I1 + I2                                               
      GO TO 200                                                         
      ELSEIF(IFT.GT.90000) THEN                                         
      IFTS=(IFT-90000)/100                                              
      IFTF=IFT-(IFTS*100+90000)                                         
      WRITE(KT,958) IFTS,IFTF                                           
  958 FORMAT('DAY ',I2,'-',I2,' MEAN')                                  
      NCHK=14                                                           
      GO TO 200                                                         
      ENDIF                                                             
C                                                                       
      IF(IFT.GT.120) THEN                                               
      RIFT=IFT/24                                                       
      WRITE(KT,933) RIFT                                                
  933 FORMAT('FD=',F5.1)                                                
      NCHK=8                                                            
      ELSE                                                              
      WRITE(KT,903) IFT                                                 
  903 FORMAT('FT=',I4)                                                  
      NCHK=7                                                            
      ENDIF                                                             
C                                                                       
  200 CONTINUE                                                          
C                                                                       
      IF(LEVEL.LE.0) THEN                                               
      KLEV=' '                                                          
      NCHL=1                                                            
      GO TO 300                                                         
      ENDIF                                                             
C                                                                       
      IF(LEVEL.GT.2000.AND.LEVEL.LT.2100) THEN                          
      LL=LEVEL-2000                                                     
      WRITE(KLEV,908) LL                                                
  908 FORMAT(6HLEVEL=,I2)                                               
      NCHL=8                                                            
      GO TO 300                                                         
      ENDIF                                                             
C                                                                       
      IF(LEVEL.GT.3000.AND.LEVEL.LT.4000) THEN                          
      LL=LEVEL-3000                                                     
      WRITE(KLEV,909) LL                                                
  909 FORMAT(I3,4HK   )                                                 
      NCHL=8                                                            
      GO TO 300                                                         
      ENDIF                                                             
C                                                                       
      IF(LEVEL.EQ.1013) THEN                                            
      KLEV='SEALEVEL'                                                   
      NCHL=8                                                            
      GO TO 300                                                         
      ENDIF                                                             
C                                                                       
      IF(LEVEL.EQ.9999) THEN                                            
      KLEV='GROUND  '                                                   
      NCHL=8                                                            
      GO TO 300                                                         
      ENDIF                                                             
C                                                                       
      WRITE(KLEV,904) LEVEL                                             
  904 FORMAT(I4,4HMB  )                                                 
      NCHL=8                                                            
C                                                                       
  300 CONTINUE                                                          
      NCHALM=NCHAL-1                                                    
      NUMCHM=NCHAL+NCHL+NCHK+NCHD+2                                     
      NUMLFT=MAXCHA-NUMCHM                                              
      NCHLBX=NCHLB                                                      
      IF(NCHLBX.GT.NUMLFT) NCHLBX=NUMLFT                                
      NUMCHM=NUMCHM+NCHLBX                                              
      IF(NCHLBX.GT.0) THEN                                              
      MTITLE=LELEM(1:NCHALM)//' '//KLEV(1:NCHL)//DATES(1:NCHD)//' '     
     1       //KT(1:NCHK)//' '//LABEL(1:NCHLBX)                         
      ELSE                                                              
      MTITLE=LELEM(1:NCHALM)//' '//KLEV(1:NCHL)//DATES(1:NCHD)//' '     
     1       //KT(1:NCHK)                                               
      ENDIF                                                             
C                                                                       
      RETURN                                                            
      END                                                               
