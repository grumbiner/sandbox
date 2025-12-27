      SUBROUTINE BUFRGN(NSTA,WMO,NWMO,NBUL,LDESC,NDESC,MBUF,NBPB,IYR,
     *           IMO,IDA,IHR,MAXMES,NUMPRJ,CSTA,CSEC2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    BUFRGN      GENERATES BUFR MESSAGES                    
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 92-07-15             
C                                                                       
C ABSTRACT: GENERATES BUFR MESSAGES CONTAINING COMPRESSED DATA AND      
C           USING PROJECTIONS AS BUFR SUBSETS.  ALSO, THE ROUTINE       
C           ATTACHES A QUEUE DESCRIPTOR TO EACH MESSAGE, AND            
C           WRITES OUT THE BUFR MESSAGE TO A TRANSMISSION FILE.
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   92-07-15  GILBERT                                                   
C   96-08-23  GILBERT - ADDED PROPER CR CR LF AT END OF WMO HEADER.
C                       CHANGED INCORRECT '<<@' TO ASCII 13,13,10.
C   98-07-23  ERICKSON - CHANGED W3FQ02 CALL TO W3UTCDAT, CHANGED
C                        YEAR IN SECTION 1 TO PROPER YR OF CENTURY
C                        CHANGES MADE FOR Y2K PURPOSES
C   98-08-28  ERICKSON - ADDED CENTURY IN BYTE 18 OF SECTION 1.
C   00-03-07  ALLEN    - CONVERTED TO RUN ON IBM SP.  A CHANGE WAS MADE
C                        SO THAT THE OUTPUT FILE IS WRITTEN OUT AS A
C                        DIRECT ACCESS FILE.
C   04-03-24  COSGROVE - TOC CHANGED THE WAY DATA IS SENT, SO THESE
C                        BUFR MESSAGES CAN NOT BE BROKEN UP WITHIN A
C                        HEADER ANYMORE. TOOK PXX'S OUT OF THE HEADERS.
C                        IT NOW READS THE KWBC FROM THE STATION LIST.
C                        UPPED THE MAX LENGTH OF THE BULLETIN FROM 
C                        20000 TO 300000 BYTES. 
C   04-11-08  MALONEY  - CHANGED W3FI62 CALL TO NOW USE MKFLDSEP.
C                        80 BYTE QUEUE DESCRIPTOR IS BEING REPLACED
C                        BY A 19 BYTE FLAG FIELD SEPARATOR.  REMOVED
C                        END OF TRANSMISSION MARKER GENERATION.  
C                        REMOVED LEGACY 'XTRN END' MARKER.  CHANGED
C                        FORTRAN FILE I/O TO USE BACIO LIBRARY.
C                                                                       
C USAGE:                                                                
C        PURPOSE                                                        
C            GENERATES BUFR MESSAGES CONTAINING COMPRESSED DATA AND     
C            USING PROJECTIONS AS BUFR SUBSETS.  ALSO, THE ROUTINE      
C            ATTACHES A QUEUE DESCRIPTOR TO EACH MESSAGE, AND           
C            WRITES OUT THE BUFR MESSAGE TO A TRANSMISSION FILE.
C                                                                       
C        DATA SET USE                                                   
C            FT06   -  PRINT FILE (OUTPUT)                              
C         FT(MUNIT) -  TRANSMISSION FILE (OUTPUT)                       
C         FT(JUNIT) -  COPY OF FILE (OUTPUT)                            
C                                                                       
C        VARIABLES                                                      
C               MAXMES = MAXIMUM NUMBER OF BYTES IN A BUFR MESSAGE      
C                WMO() = LIST OF WMO HEADERS                            
C              NWMO(I) = NUMBER OF STATIONS FOR HEADER WMO(I)           
C              MBUF(,) = HOLDS FORECASTS AFTER THEY ARE PACKED INTO BUFR
C                        FORMAT.  1ST DIMENSION IS # OF WORDS IN BUFR   
C                        MESSAGE, AND 2ND DIMENSION IS # OF BULLETINS   
C                        (WMO HEADERS).                                 
C               NBPB() = # OF BITS IN EACH BULLETIN.                    
C              LDESC() = LIST OF DESCRIPTORS FOR SECTION 3              
C                NDESC = NUMBER OF DESCRIPTORS IN LDESC                 
C              MBUFR() = CONTAINS THE BUFR MESSAGE AND QUEUE            
C                        DESCRIPTOR  (MAX OF 10,000 BYTES)              
C                CHEAD = BULLETIN HEADER                                
C                 NBUL = NUMBER OF BULLETINS (WMO HEADERS)              
C             LENGTH() = HOLDS LENGTH OF EACH SECTION IN BYTES          
C               KARY() = ARRAY USED BY W3FI62                           
C                        1 - DAY OF MONTH
C                        2 - HOUR OF DAY
C                        3 - HOUR*100 + MIN
C               IDAT() = NCEP ABSOLUTE DATE + TIME FROM SYSTEM CLOCK
C                        RETURNED FROM W3UTCDAT IN UTC TIME
C                        1 - 4 DIGIT YEAR    2 - MONTH OF YEAR
C                        3 - DAY OF MONTH    4 - HHMM TIME ZONE DIFF
C                        5 - HR OF DAY IN 24H CLOCK
C                        6 - MINUTES OF HOUR        
C                        7 - SECONDS OF MINUTE   8 - MILLISECS OF SECS  
C                IBUFR = INTEGER FORM OF THE ASCII CHARS 'BUFR'         
C                I7777 = INTEGER FORM OF THE ASCII CHARS '7777'         
C                 CETX = END OF TRANSMISIION CHARACTER                  
C                  IYR = CURRENT YEAR (4 DIGITS)                        
C                  IYC = YEAR OF CENTURY(1,100) YR2000=100TH YR OF CEN
C                 ICEN = CENTURY (20TH STARTS IN YR 2001)
C                  IMO = CURRENT MONTH                                  
C                  IDA = CURRENT DAY                                    
C                  IHR = CURRENT CYCLE ( = 00 OR 12)                    
C                MUNIT = DATA SET REFERNCE NUMBER FOR TRANSMISSION      
C                        FILE                                           
C               NUMPRJ = NUMBER OF PROJECTIONS IN MESSAGE               
C                 LREC = RECORD LENGTH OF TRANSMISSION FILE             
C                IOFST = OFSET IN BUFR MESSAGE     
C                 NREC = NUMBER OF RECORDS FOR WRITING TO TRANSMISSION FILE     
C               LENOUT = OUTPUT FROM MKFLDSEP
C               ENVVAR = XLF "NAME" OF OUTPUT FILE
C                FILEO = ACTUAL FILE OUTPUT NAME LINKED TO XLF UNIT NO.
C                 IRET = RETURN STATUS FROM BAOPEN.
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: VS FORTRAN                                                
C   MACHINE:  NAS                                                       
C                                                                       
C$$$                                                                    
      PARAMETER (MAXM=300000)
      CHARACTER*80 ENVVAR,FILEO
      CHARACTER*21 CHEAD
      CHARACTER*21 CSEC2
      CHARACTER*11 WMO(NBUL)
      CHARACTER*8 CSTA(NSTA)
      CHARACTER*4 CTEMP
      CHARACTER*1 MBUF(MAXMES,NBUL),MBUFR(MAXM),CR,LF
      INTEGER LDESC(NDESC)
      INTEGER NWMO(NBUL),LENGTH(0:5),IYC,ICEN,IRET
     *        IDAT(8),IHEAD(7),NBPB(NBUL),KARY(7)
      DATA NCHEAD/21/,IPOS/1/,NUMSTA/0/,
     *     LREC/1280/,MUNIT/60/ 
      DATA NREC/0/
C
C     OPEN THE OUTPUT FILE MUNIT AS DIRECT ACCESS
CCC      OPEN(UNIT=MUNIT,RECL=1280,ACCESS='DIRECT',FORM='UNFORMATTED')
CCC      INSTEAD OF THIS, USE BAOPENW TO OPEN THE FILE
C
      ENVVAR='XLFUNIT_  '
      WRITE(ENVVAR(9:10),FMT='(I2)')MUNIT
      CALL GETENV(ENVVAR,FILEO)
      CALL BAOPEN(MUNIT,FILEO,IRET)
      IF(IRET.NE.0)THEN
         WRITE(KFILDO,30)
   30    FORMAT(/'****  ERROR OPENING THE BUFR OUTPUT FILE.  STOP IN',
     &           ' BUFRGN AT 30')
         STOP 100
      ENDIF
C

      CR=CHAR(13)
      LF=CHAR(10)
C        GENERATE A BUFR MESSAGE FOR EACH WMO HEADER                    
      DO 900 N=1,NBUL
        DO 10 I=1,MAXM
          MBUFR(I)=CHAR(0)
 10     CONTINUE
C        GET SYSTEM TIME AND PUT IN HEADER                              
        CHEAD='XXXXNN YYYY DDHHMM   '       
        CALL W3AI15(IDA,CTEMP,1,2,'-')
        CHEAD(13:14)=CTEMP(1:2)
        CALL W3AI15(IHR,CTEMP,1,2,'-')
        CHEAD(15:16)=CTEMP(1:2)
        CHEAD(17:18)='00'
C  ****  GENERATE WMO HEADER                                            
        CHEAD(1:6)=WMO(N)(1:6)
        CHEAD(8:11)=WMO(N)(8:11)
        CHEAD(19:21)=CR//CR//LF
        WRITE(6,20)CHEAD
 20     FORMAT('0',A18)
CCCCC   CALL W3AI38(CHEAD,28)                                           
        DO 100 I=1,NCHEAD
CCC         MBUFR(80+I)=CHEAD(I:I)
CCC         CHANGED FROM 80+I TO 19+I...FLAG FIELD DESC.
           MBUFR(19+I)=CHEAD(I:I)
 100    CONTINUE
C*************************************************                      
C *****  GENERATE SECTION 0                                             
C           CHANGED FROM 80+N TO 19+N...FLAG FIELD DESC.
        JPOS=(19+NCHEAD)
        IOFST=(JPOS)*8
        MBUFR(JPOS+1)='B'
        MBUFR(JPOS+2)='U'
        MBUFR(JPOS+3)='F'
        MBUFR(JPOS+4)='R'
        IOFST=IOFST+32
C        SAVE POSITION FOR TOTAL MESSAGE LENGTH                         
        LENPOS=IOFST
        IOFST=IOFST+24
C        BUFR EDITION NUMBER                                            
        ITEMP=2
        CALL SBYTE(MBUFR,ITEMP,IOFST,8)
        IOFST=IOFST+8
        LENGTH(0)=8
C*************************************************                      
C *****  GENERATE SECTION 1  *******                                    
C       WRITE(6,*)' START SECTION 1 AT OFFSET',IOFST                    
        LENGTH(1)=18
        CALL SBYTE(MBUFR,LENGTH(1),IOFST,24)
        IOFST=IOFST+24
C        MASTER TABLE: 0, IF STANDARD TABLES USED.                      
        IOFST=IOFST+8
C        ORIGINATING CENTER (=7 FOR NWS)                                
        ITEMP=7
        CALL SBYTE(MBUFR,ITEMP,IOFST,16)
        IOFST=IOFST+16
C        UPDATE SEQUENCE # = 0                                          
        IOFST=IOFST+8
C        OPTIONAL SECTION 2 EXISTS IN MESSAGE (BIT 1 SET TO 0)          
        ITEMP=128
        CALL SBYTE(MBUFR,ITEMP,IOFST,8)
        IOFST=IOFST+8
C        MESSAGE TYPE (TABLE A) = 0                                     
        IOFST=IOFST+8
C        MESSAGE SUBTYPE = 0                                            
        IOFST=IOFST+8
C        VERSION OF MASTER TABLE = 2                                    
        ITEMP=2
        CALL SBYTE(MBUFR,ITEMP,IOFST,8)
        IOFST=IOFST+8
C        VERSION OF LOCAL TABLE = 0                                     
        IOFST=IOFST+8
C        YEAR, MONTH, DAY AND HOUR                                      
C        YEAR = YEAR OF CENTURY
        IYC=MOD(IYR-1,100) +1
        CALL SBYTE(MBUFR,IYC,IOFST,8)
        IOFST=IOFST+8
        CALL SBYTE(MBUFR,IMO,IOFST,8)
        IOFST=IOFST+8
        CALL SBYTE(MBUFR,IDA,IOFST,8)
        IOFST=IOFST+8
        CALL SBYTE(MBUFR,IHR,IOFST,8)
        IOFST=IOFST+8
C        NO MINUTES 
        IOFST=IOFST+8 
C        INDICATE Y2K FILE BY CENTURY IN OCTET 18
        ICEN=((IYR-1)/100)+1
        CALL SBYTE(MBUFR,ICEN,IOFST,8)
        IOFST=IOFST+8
C       WRITE(6,*)' LENGTH OF SECTION 1:',LENGTH(1),' BYTES.'           
C*************************************************                      
C *****  GENERATE SECTION 2  *******                                    
C       WRITE(6,*)' START SECTION 2 AT OFFSET',IOFST                    
        LENGTH(2)=4+21+(8*NWMO(N))+1
        CALL SBYTE(MBUFR,LENGTH(2),IOFST,24)
        IOFST=IOFST+24
C        SET FOURTH BYTE TO 0                                           
        IOFST=IOFST+8
C          ADD FIRST 21 BYTES OF CSEC2 TO MBUFR                         
        JPOS=IOFST/8
        DO 180 I=1,21
          MBUFR(JPOS+I)=CSEC2(I:I)
 180    CONTINUE
        IOFST=IOFST+(21*8)
	JPOS=JPOS+21
        DO 200 I=1,NWMO(N)
	  DO 190 K=1,8
            MBUFR(JPOS+K)=CSTA(IPOS)(K:K)
 190      CONTINUE
          IPOS=IPOS+1
          JPOS=JPOS+8
          IOFST=IOFST+64
 200    CONTINUE
C          LENGTH OF SECTION MUST CONTAIN EVEN NUMBER OF BYTES          
        IOFST=IOFST+8
C       WRITE(6,*)' LENGTH OF SECTION 2:',LENGTH(2),' BYTES.'           
C*************************************************                      
C *****  GENERATE SECTION 3  *******                                    
C       WRITE(6,*)' START SECTION 3 AT OFFSET',IOFST                    
C        LENGTH OF SECTION 3.  NOTE: LENGTH MUST BE EVEN NUMBER         
C        OF BYTES.                                                      
        LENGTH(3)=7+(2*NDESC)+1
        CALL SBYTE(MBUFR,LENGTH(3),IOFST,24)
        IOFST=IOFST+24
C        SET FOURTH BYTE TO 0                                           
        IOFST=IOFST+8
C        NUMBER OF SUBSETS = NUMBER OF STATIONS IN MESSAGE TIMES        
C        THE NUMBER OF PROJECTIONS                                      
        ITEMP=NUMPRJ*NWMO(N)
        CALL SBYTE(MBUFR,ITEMP,IOFST,16)
        IOFST=IOFST+16
C        NOT OBSERVED DATA; BUT COMPRESSED DATA                         
        ITEMP=2**6
        CALL SBYTE(MBUFR,ITEMP,IOFST,8)
        IOFST=IOFST+8
C        ADD DESCRIPTOR LIST TO BUFR MESSAGE.  
        CALL SBYTES(MBUFR,LDESC,IOFST,16,0,NDESC)
        IOFST=IOFST+(16*NDESC)
C        ADD ONE BYTE TO IOFST TO GET EVEN NUMBER OF
C        BYTES IN SECTION 3.
        IOFST=IOFST+8
C       WRITE(6,*)' LENGTH OF SECTION 3:',LENGTH(3),' BYTES.'           
C*************************************************                      
C *****  GENERATE SECTION 4  *******                                    
C       WRITE(6,*)' START SECTION 4 AT OFFSET',IOFST                    
C        SAVE POSITION FOR LENGTH OF SECTION 4, TO BE CALCULATED LATER. 
        LPOS4=IOFST
        IOFST=IOFST+24
C        SET FOURTH BYTE TO ZERO.                                       
        IOFST=IOFST+8
C        FIND NUMBER OF BYTES NEEDED FOR DATA 
        NBYTES=NBPB(N)/8
        IF (MOD(NBPB(N),8).NE.0) NBYTES=NBYTES+1
C        CHECK IF MESSAGE IS GREATER THAN 20,000 BYTES 
C        BEFORE PACKING                                                 
        IF (NBYTES.GT.MAXM) THEN
          WRITE(6,350) NBYTES
 350      FORMAT(' NUMBER OF BYTES IN MESSAGE = ',I8,' EXCEEDS ',
     *    ' MAXIMUM OF ',I8,' BYTES')
          GOTO 900
        ENDIF
	JPOS=IOFST/8
	DO 375 I=1,NBYTES
	  MBUFR(JPOS+I)=MBUF(I,N)
 375    CONTINUE
        IOFST=IOFST+(8*NBYTES)
C        CALCULATE LENGTH OF SECTION 4 AND INSERT AT BEGINNING OF       
C        SECTION 4.  MAKE SURE THERE ARE AN EVEN NUMBER OF BYTES        
C        IN SECTION 4.                                                  
        ITBIT=IOFST-LPOS4
        ITBYTE=ITBIT/8
        LEFT=MOD(ITBIT,8)
        IF (MOD(ITBYTE,2).EQ.0) THEN
          IF (LEFT.EQ.0) THEN
            LENGTH(4)=ITBYTE
          ELSE
            LENGTH(4)=ITBYTE+2
            IOFST=IOFST-LEFT+16
          ENDIF
        ELSE
          LENGTH(4)=ITBYTE+1
          IOFST=IOFST-LEFT+8
        ENDIF
        CALL SBYTE(MBUFR,LENGTH(4),LPOS4,24)
C       WRITE(6,*)' LENGTH OF SECTION 4:',LENGTH(4),' BYTES.'           
C*************************************************                      
C *****  GENERATE SECTION 5  *******                                    
C       WRITE(6,*)' START SECTION 5 AT OFFSET',IOFST                    
	JPOS=IOFST/8
	DO 500 I=1,4
	  MBUFR(JPOS+I)='7'
 500    CONTINUE
        IOFST=IOFST+32
        LENGTH(5)=4
C       WRITE(6,*)' LENGTH OF SECTION 5:',LENGTH(5),' BYTES.'           
C       WRITE(6,*)' BULLETIN ENDED AT OFFSET',IOFST                     
C*************************************************                      
C *****  PLACE TOTAL LENGTH OF MESSAGE BACK IN SECTION 0                
        LENTOT=0
        DO 400 I=0,5
          LENTOT=LENTOT+LENGTH(I)
 400    CONTINUE
        CALL SBYTE(MBUFR,LENTOT,LENPOS,24)
C       WRITE(6,*)' LENGTH OF BULLETIN:',LENTOT,' BYTES.'               
C*************************************************                      
C *****  PLACE END OF TRANSMISSION MARKER                               
CCC      REMOVED:  NO LONGER NECESSARY
CCC      JPOS=(IOFST/8)+1
CCC      MBUFR(JPOS)=CETX
CCC      IOFST=IOFST+8
C*************************************************                      
C *****  GENERATE QUEUE DESCRIPTOR                                      
CCC      NOTE:  THIS SECTION HAS BEEN REPLACED BY
CCC      THE 19BYTE FLAG FIELD DESCRIPTOR
CCC        CALL W3UTCDAT(IDAT)
CCC        KARY(1)=IDAT(3)
CCC        KARY(2)=IDAT(5)
CCC        KARY(3)=IDAT(5)*100 + IDAT(6)
CCC        KARY(4)=0
CCC        KARY(5)=0
CCC        KARY(6)=0
CCC        KARY(7)=LENTOT+NCHEAD
CCC        CALL W3FI62(MBUFR,WMO(N)(1:6),KARY,IERR)
C*************************************************
C *****  GENERATE FLAG FIELD DESCRIPTOR
C
         CALL MKFLDSEP(MBUFR,2,0,LENTOT+NCHEAD,LENOUT)
C
C*************************************************                      
C        WRITE BUFR MESSAGE TO TRANSMISSION FILE                        
        NBYTES=(IOFST/8)
         CALL WRYTE(MUNIT,NBYTES,MBUFR)
CC        NUMREC=(NBYTES/LREC)
CC	IF (MOD(NBYTES,LREC).NE.0) NUMREC=NUMREC+1
CC        ISTART=1
CC	IEND=LREC
CC        DO 600 I=1,NUMREC
CC        NREC=NREC+1
CC          WRITE(MUNIT,REC=NREC) (MBUFR(L),L=ISTART,IEND)
CC          ISTART=ISTART+LREC
CC	  IEND=IEND+LREC
CC 600    CONTINUE
        WRITE(6,700)N,NBYTES,NWMO(N)
 700    FORMAT(' MESSAGE ',I3,' HAS A TOTAL LENGTH OF ',I6,' BYTES ',
     *         'CONTAINING ',I4,' STATIONS.')
        NUMSTA=NUMSTA+NWMO(N)
 900  CONTINUE
      WRITE(6,988) NUMSTA
 988  FORMAT(' A TOTAL OF ',I5,' STATIONS HAVE BEEN PROCESSED.')
      RETURN
      END
