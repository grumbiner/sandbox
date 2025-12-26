      SUBROUTINE RDWMO(NUNIT,NSTA,CSTA,WMO,NWMO,NHEAD,MAXSTA,MAXBUL)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    RDWMO       READS WMO HEADERS AND ASSOCIATED STATIONS  
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-08             
C                                                                       
C ABSTRACT: READS A LIST OF WMO HEADERS AND THEIR ASSOCIATED STATION    
C           LISTS.  IT KEEPS TRACK OF THE NUMBER OF STATIONS IN EACH    
C           WMO HEADER, AND KEEPS 1 LONG STATION LIST IN THE ORDER      
C           THAT THE WMO HEADERS ARE READ IN.                           
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-08  GILBERT                                                   
C   04-03-24  COSGROVE - TOC CHANGED THE WAY DATA IS SENT, SO THESE
C                        BUFR MESSAGES CAN NOT BE BROKEN UP WITHIN A
C                        HEADER ANYMORE. TOOK PXX'S OUT OF THE HEADERS.
C                        IT NOW READS THE KWBC FROM THE STATION LIST.
C                                                                       
C USAGE:                                                                
C   SEE BELOW FOR TDL STANDARDS                                         
C                                                                       
C     SUBROUTINE RDWMO                                                  
C                                                                       
C     JULY 1992   GILBERT    TDL   NAS9000                              
C                                                                       
C        PURPOSE                                                        
C           READS A LIST OF WMO HEADERS AND THEIR ASSOCIATED STATION    
C           LISTS.  IT KEEPS TRACK OF THE NUMBER OF STATIONS IN EACH    
C           WMO HEADER, AND KEEPS 1 LONG STATION LIST IN THE ORDER      
C           THAT THE WMO HEADERS ARE READ IN.                           
C                                                                       
C        DATA SET USE                                                   
C           FT(NUNIT) - CONTROL DATA (INPUT)                            
C                                                                       
C        VARIABLES                                                      
C            NSTA = TOTAL NUMBER OF STATIONS                            
C          CSTA() = LIST OF CALL LETTERS FOR ALL STATIONS               
C          WMO(I) = LIST OF WMO HEADERS                                 
C         NWMO(I) = NUMBER OF STATIONS IN WMO HEADER WMO(I).            
C           NHEAD = NUMBER OF WMO HEADERS                               
C         CTEMP() = TEMPORARY ARRAY FOR ROUTINE RDLSTA.                 
C           CTERM = TERMINATOR FOR EACH STATION LIST.                   
C            CFMT = FORMAT OF EACH STATION LIST.                        
C                                                                       
C REMARKS:                                                              
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      CHARACTER*8 CSTA(MAXSTA),CTERM,CTEMP(9)
      CHARACTER*8 CFMT
      CHARACTER*11 WMO(MAXBUL)
      INTEGER NWMO(MAXBUL)
      DATA CFMT/'(9A8)  '/,CTERM/'ZZZZZZZZ'/
      NSTA=0
C        READ IN NUMBER OF DIFFERENT WMO HEADERS                        
      READ(NUNIT,10)NHEAD
 10   FORMAT(I4)
      IF (NHEAD.GT.MAXBUL) THEN
        WRITE(6,15) NHEAD,MAXBUL
 15     FORMAT(' NUMBER OF INPUT BULLETINS = ',I4,' EXCEEDS ',I4/
     *         ' STOP IN SUBROUTINE RDWMO.')
        CALL EXIT(15)
      ENDIF
C        READ IN WMO HEADER AND STATION LIST ASSOCIATED WITH THAT       
C        WMO HEADER.                                                    
      DO 100 I=1,NHEAD
        READ(NUNIT,20)WMO(I)
 20     FORMAT(A11)
        LEFT=MAXSTA-NSTA
        CALL RDLSTA(NUNIT,CSTA(NSTA+1),LEFT,CTEMP,9,CFMT,NWMO(I),CTERM,
     *       8)
        NSTA=NSTA+NWMO(I)
        IF (NSTA.GT.MAXSTA) THEN
          WRITE(6,30) NSTA,MAXSTA
 30       FORMAT(' NUMBER OF INPUT STATIONS = ',I4,' EXCEEDS ',I4/
     *           ' STOP IN SUBROUTINE RDWMO.')
          CALL EXIT(30)
        ENDIF
 100  CONTINUE
      RETURN
      END
