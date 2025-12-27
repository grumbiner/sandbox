      SUBROUTINE BDRIVR(ID,MAXDSC,LDESC,NDESC,SCALE,REF,NBITS,
     *           IVPRJ,MAXPRJ,NVPRJ,CFLAG,IEND,IOPT,IUNIT)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BDRIVR      READS A SET OF INPUT CONTROL RECORDS       
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-13             
C                                                                       
C ABSTRACT: READS A SET OF INPUT CONTROL RECORDS.  THE SET CONSISTS OF  
C           AN ELEMENT DESCRIPTOR DESCRIBING A FORECAST ELEMENT, THE    
C           ASSOCIATED FORECAST IDENTIFIER, THE SCALING AND REFERENCE
C           VALUES, AND A LIST OF VALID PROJECTIONS FOR THE FORECAST.   
C           AN OPTION VALUE IS ALSO READ IN TO DETERMINE THE TYPE OF    
C           PROCESSING THAT NEEDS TO BE DONE.                           
C           ALSO, A LIST OF THE DESCRIPTORS IS KEPT.                    
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-13  GILBERT                                                   
C                                                                       
C USAGE:                                                                
C                                                                       
C     JULY  1992   GILBERT    TDL   NAS9000                             
C                                                                       
C     PURPOSE                                                           
C         READS A SET OF INPUT CONTROL RECORDS.  THE SET CONSISTS OF    
C         AN ELEMENT DESCRIPTOR DESCRIBING A FORECAST ELEMENT, THE      
C         ASSOCIATED FORECAST IDENTIFIER, THE SCALING AND REFERENCE
C         VALUES, AND A LIST OF VALID PROJECTIONS FOR THE FORECAST.     
C         ALSO, A LIST OF THE DESCRIPTORS IS KEPT.                      
C                                                                       
C     DATA SET USE                                                      
C         FT(IUNIT) - INPUT CONTROL DATA SET.                           
C                                                                       
C     VARIABLES                                                         
C          IVPRJ() = LIST OF VALID PROJECTIONS FOR THE FORECAST ELEMENT 
C          LDESC() = LIST OF ELEMENT DESCRIPTORS READ.
C           MAXPRJ = MAXIMUM NUMBER OF PROJECTIONS.                     
C           MAXDSC = MAXIMUM NUMBER OF DESCRIPTORS.                     
C            ID(4) = MOS FORECAST IDENTIFIER
C            SCALE = BUFR TABLE B; SCALE VALUE                          
C              REF = BUFR TABLE B; REFERENCE VALUE                      
C            NBITS = BUFR TABLE B; NUMBER OF BITS                       
C               IF = 1ST PART OF DESCRIPTOR (F|X|Y)                     
C               IX = 2ND PART OF DESCRIPTOR (F|X|Y)                     
C               IY = 3RD PART OF DESCRIPTOR (F|X|Y)                     
C             IEND = FLAG INDICATING THE END OF THE INPUT CONTROL LIST  
C            ITERM = TERMINATOR FOR THE LIST OF VALID PROJECTIONS       
C             CFMT = FORMAT OF VSLID PROJECTION LIST  (CHAR*8)          
C            IUNIT = DATA SET REFERENCE NUMBER FOR THE INPUT CONTROL    
C                    LIST                                               
C                                                                       
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      INTEGER IVPRJ(MAXPRJ),ITEMP(24),ID(4)
      INTEGER LDESC(MAXDSC)
      REAL SCALE,REF
      CHARACTER*1 CFLAG
      CHARACTER*8 CFMT
      CHARACTER*12 CDESC
      DATA ITERM/999/,CFMT/'(24I3)  '/,IMAX/24/
      SAVE ITERM,CFMT,IMAX
      READ(IUNIT,200) IF,IX,IY,(ID(I),I=1,4),SCALE,REF,NBITS,IOPT,
     *                CFLAG,CDESC
 200  FORMAT(I2,1X,I2,1X,I3,4I10,1X,F2.0,1X,F6.0,1X,I2,1X,I2,1X,
     *       A1,1X,A12)
      WRITE(6,400) IF,IX,IY,(ID(I),I=1,4),NINT(SCALE),NINT(REF),
     *             NBITS,IOPT,CFLAG,CDESC
 400  FORMAT(I2,1X,I2,1X,I3,4I10,1X,I2,1X,I6,1X,I2,1X,I2,1X,
     *       A1,1X,A12)
      IF (IF.EQ.9) THEN
        IEND=1
        GOTO 900
      ELSE
        CALL RDLSTI(IUNIT,IVPRJ,MAXPRJ,ITEMP,IMAX,CFMT,NVPRJ,ITERM)
      ENDIF
      NDESC=NDESC+1
      LDESC(NDESC)=(IF*16384)+(IX*256)+IY
 900  RETURN
      END
