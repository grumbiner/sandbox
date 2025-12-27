      SUBROUTINE SORT1(LIST,JPOS,KK)                          
C                                                             
C        GLAHN   SEPTEMBER 1976   IBM 360/195               
C                ADAPTED FROM NESS ISORT (M. WHITNEY)   (SHELL SORT?)   
C        JENSENIUS  OCT. 1989  COMPILED UNDER FORTRAN 77               
C        REAP    MAR 1996  CONVERTED TO CRAY 
C                                                                    
C     PURPOSE:                                                      
C                                                                    
C        SORTS POSITIVE OR NEGATIVE INTEGERS OR POSITIVE REAL (FLOATING 
C        POINT) NUMBERS IN LIST( ) AND PUTS INDICES OF ORIGINAL         
C        POSITIONS IN JPOS( ).                                          
C                                                                       
C     VARIABLES:                                                        
C                                                                       
C         LIST( ) = INTEGERS GIVEN TO SUBROUTINE UNORDERED AND GIVEN    
C                   BACK TO CALLING PROGRAM ORDERED SMALL TO LARGE.     
C         JPOS( ) = LOCATIONS IN JPOS( ) CORRESPOND TO LOCATIONS        
C                   IN LIST( ), AND CONTAIN INDICES GIVING ORIGINAL     
C                   POSITION IN LIST OF THE INTEGERS IN LIST( ).        
C              KK = NUMBER OF ITEMS IN LIST( ).                         
C                                                                       
C                                                                       
      DIMENSION LIST(KK),JPOS(KK)                        
C     **************************************************************
      DO 104 M=1,KK                                                     
      JPOS(M)=M                                                         
 104  CONTINUE                                                          
      M=KK                                                              
 105  M=M/2                                                             
      IF(M)110,150,110                                                  
 110  KKK=KK-M                                                          
      J=1                                                               
 115  N=J                                                              
      L=N+M                                                          
 120  IF(LIST(N)-LIST(L))140,140,121                                  
 121  KEEP=LIST(N)                                                   
      LIST(N)=LIST(L)                                               
      LIST(L)=KEEP                                                    
      KEEP=JPOS(N)                                                     
      JPOS(N)=JPOS(L)                                                  
      JPOS(L)=KEEP                                                    
      N=N-M                                                        
      L=N+M                                                          
      IF(N)140,140,120                                          
 140  J=J+1                                                            
      IF(J-KKK)115,115,105                                              
 150  CONTINUE                                                          
      RETURN                                                            
      END                                                               
