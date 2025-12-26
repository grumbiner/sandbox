      SUBROUTINE WRMSG(MSG,N1,N2,NBSTA,NESTA,LINES,NCAT,KOUNT,M1,M2,    
     * NAME,JA,MDG,MHG,NOHEAD,LUNIT,BFILE,NHEAD)                        
C                                                                       
C        GLAHN   OCTOBER 1975   - IBM 360/195                           
C        DALLAVALLE  MAY 1988   - NAS 9000 - ADDED DOCBLOCK             
C        GILBERT   MARCH 1991   - CONVERTED TO FORTRAN 77, AND          
C                                 ADDED SOME PARAMETERS TO MAKE THE     
C                                 SUBROUTINE FOR MORE GENERAL USE.      
C                               - ADDED ALGORITHM TO DETERMINE THE      
C                                 NUMBER OF STATIONS IN EACH BULLETIN   
C        GILBERT   MAY 1991     - REMOVED A LARGE ARRAY TO MAKE SUBR.   
C                                 MORE SPACE EFFICIENT.                 
C        GILBERT   APR 1992     - DECREASED MAXIMUM BULLETIN SIZE TO    
C                                 3840 BYTES.                           
C        GILBERT   NOV 1992     - CORRECTED AN ERROR IN THE CALCULATION 
C                                 OF IST AND LST, WHICH CAUSED THE      
C                                 SUBROUTINE TO WRITE 1281 BYTE RECORDS 
C                                 INSTEAD OF 1280 BYTES.                
C        GILBERT   DEC 1992     - CORRECTED ERROR THAT CHECKS THE NO.   
C                                 OF STATIONS TO PUT IN EACH BULLETIN   
C                                 (ADDED KK COUNTER).                   
C        GILBERT   OCT 1993     - CHANGED TO ADD AS MANY STATIONS       
C                                 AS POSSIBLE TO EACH BULLETIN.
C          WEISS   OCT 1995     - CONVERTED FOR CRAY C90 AND HP (HOBBS)
C                                 OPERATIONS. CONTROL CHARACTERS
C                                 (TRANSMISSION COMPONENTS) AND BULLETIN
C                                 ARE NOW IN ASCII

C        PURPOSE                                                        
C            FINALIZE PREPARATION OF MESSAGE AND WRITE TO TRANS-     
C            MISSION FILE.

C        VARIABLES
C               BFILE=OUTPUT FILE NAME
C              ICNT()=NUMBER OF CHARACTERS PER STATION
C        KOUNT(M1,M2)=FOR EACH LINE (M1) AND EACH STATION (M2),
C                     KOUNT( , ) = 0 UNLESS NON-MISSING FORECASTS HAVE
C                     BEEN INSERTED IN THAT LINE.  USED TO ELIMINATE
C                     LINES FROM BULLETIN.  KOUNT( , ) BEING NEGATIVE
C                     INDICATES A BLANK LINE TO BE TRANSMITTED UNLESS NO
C                     FORECASTS EXIST, IN WHICH CASE NOTHING WILL BE
C                     TRANSMITTED.
C                  JA=NUMBER OF CHARACTERS IN THE BULLETIN WMO HEADER
C                     TYPICALLY 11.
C              LENBUL=MAXIMUM LENGTH OF BULLETIN (INT. MULT. OF LRECL)
C               LINES=NUMBER OF LINES PER STATION
C               LRECL=RECORD LENGTH (BYTES) OF TRANS FILE. TRANSMISSION
C                     FILE MUST BE BLOCKED IN RECORDS OF 1280 BYTES.
C               LUNIT=DATA SET REFERENCE NUMBER FOR TRANSMISSION FILE.
C              MAXSTA=MAXIMUM NUMBER OF STATIONS
C                 MDG=DAY, 2 DIGITS
C                 MHG=HOUR, 2 DIGITS
C          MSG(N1,N2)=CONTAINS MESSAGE BEFORE FINAL PREPARATION
C                MT()=HOLDS THE WHOLE MESSAGE FOR A BULLETIN INCLUDING
C                     TRANSMISSION COMPONENTS.
C            NAME(JA)=HOLDS BULLETIN NAME, UP TO 20 CHARACTERS
C               NBSTA=NUMBER OF FIRST STATION FOR BULLETIN
C                NCAT=CATELOGUE NUMBER, 5 CHARACTERS
C               NCHHD=NUMBER OF CHARACETERS IN THE HEADER
C               NESTA=NUMBER OF LAST STATION FOR BULLETIN
C               NHEAD=NUMBER OF BULLETIN HEADERS (IF APPLICABLE)
C                     OTHERWISE NHEAD IS EQUAL TO ONE
C              NOHEAD=NUMBER OF LINES IN HEADER, INCLUDING BLANKS
C              NSTAPB=NUMBER OF STATIONS PER BULLETIN
C              NUMREC=NUMBER OF RECORDS PER BULLETIN
C            TMPBUL()=HOLDS ONE BULLETIN (MAY INCLUDE SEVERAL STATIONS)

C        FUNCTIONS:
C                CHAR=INTRINSIC FUNCTION WHICH RETURNS THE ASCII CHARACT
C                     OF AN ASCII CHARACTER REPRESENTATION.
                                                                       
C        COMMENTS                                                       
C            THIS PROGRAM CONFORMS TO NMC'S OFFICE NOTE 100, EXCEPT     
C            FOR THE LENGTH OF THE BULLETIN CRITERIA.  O.N. 100 STATES  
C            THAT EACH BULLETIN SHOULD NOT EXCEED 2000 BYTES.  OSO      
C            STATED THAT THE MAXIMUM LENGTH OF A BULLETIN SHOULD BE     
C            3800 BYTES.  WE CHOSE A MAXIMUM OF 3840 BYTES, WHICH IS    
C            THREE TIMES THE RECORD LENGTH OF 1280 BYTES, FOR EASE OF   
C            COMPUTATION AND PROGRAMMING.  THE MAXIMUM NUMBER OF        
C            STATIONS THAT WILL FIT IN A BULLETIN IS CALCULATED AND     
C            THE ROUTINE ADDS AS MANY STATIONS AS POSSIBLE TO EACH      
C            BULLETIN.                                                  
C                                                                       
      PARAMETER (MAXSTA=2500,LRECL=1280)                                
      DIMENSION KOUNT(M1,M2),ICNT(MAXSTA)
      INTEGER IREC,NSTART,NHEAD 
      CHARACTER*18 NAME
      CHARACTER*80 BFILE
      CHARACTER*9  CTEMP
      CHARACTER*5  NCAT
      CHARACTER*1 MSG(N1,N2),MSOH,NETB,NFIL,           
     *            MT(3*LRECL),TMPBUL(MAXSTA)

      DATA TMPBUL/2500*' '/
      DATA NSTART/0/,IREC/0/

C        NSTART IS TREATED AS AN INTERNAL VARIABLE
      SAVE NSTART
      SAVE IREC

C        ASCII CONTROL CHARACTER DEFINITION
      MSOH=CHAR(39)
      NETB=CHAR(62)
      NFIL=CHAR(94)

      DO 100 I=1,3*LRECL 
        MT(I)=NFIL
 100  CONTINUE
      DO 101 I=1,MAXSTA
	ICNT(I)=0
 101  CONTINUE

C       OPEN A DIRECT ACCESS DATA FILE TO HOLD THE TRANSMISSION FILE:
C       NOTE: THE TRANSMISSION FILE MUST BE DIRECT ACCESS DUE TO LENGTH
C             OF THE REQUIRED RECORD LENGTH OF 1280 BYTES
C
C       FOR PORTABILITY THE OPTION MUST EXIST TO OPEN A DIRECT 
C       ACCESS DATASET EITHER BY SPECIFYING A FILENAME
C       DIRECTLY (HOBBS) WITHIN THE OPEN STATEMENT OR OUTSIDE
C       THE OPEN STATEMENT (CRAY C90) 
C

       IF (NSTART.EQ.0)THEN
       IF (BFILE(1:1).EQ.' ')THEN
	  OPEN(LUNIT,IOSTAT=IOS,ACCESS='DIRECT',RECL=LRECL,
     *	  ERR=99,FORM='FORMATTED')
       ELSE
          OPEN(LUNIT,IOSTAT=IOS,ACCESS='DIRECT',RECL=LRECL,
     *    FILE=BFILE,STATUS='UNKNOWN',ERR=99,FORM='FORMATTED')
       ENDIF
       ENDIF

C        INCREMENT NSTART TO EVENTUALLY EQUAL NHEAD
       NSTART=NSTART+1

C       FIRST 40 BYTES OF THE ARRAY MT
      CALL PUTCHAR(' 1                                      ',MT(1),40)

C        PLACES BEGINNING SOH CHARACTER IN BYTE NUMBER ONE OF MT
      MT(1)=MSOH

C        PLACES CATELOGUE NUMBER
      CALL PUTCHAR(NCAT,MT(3),5)

C        PLACES BULLETIN NAME
      CALL PUTCHAR(NAME(1:11),MT(41),JA)

C        PLACES BLANK
      CALL PUTCHAR(' ',MT(JA+41),1)

C        PLACES DAY
      CALL PUTMOS('DAY',FLOAT(MDG),0.,1.,1,31,MT(JA+42),2,2,'99',KT)

C        PLACES HOUR
      CALL PUTMOS('HR',FLOAT(MHG),0.,1.,0,24,MT(JA+44),2,2,'99',KT)

C        PLACES MINUTES
      CALL PUTCHAR('00',MT(JA+46),2)

C        INSERT CR CR LINE-FEED CONTROL CHARACTERS
      CALL PUTCHAR('<<@',MT(JA+48),3)

C        INSERT PIL
      CTEMP=NAME(13:18)//'<<@'
      CALL PUTCHAR(CTEMP,MT(JA+51),9)
      NOC=JA+60                                                         
C        NOC IS PLACE TO PUT NEXT CHARACTER (62)                        
C        WRITE HEADER RECORDS TO MT( )
C        IF HEADER CONTAINS MORE THAN ONE RECORD
      DO 200 I=2,NOHEAD                                                 
        CALL PUTQ(MSG(1,I),MT(NOC),NOC,N1)                              
 200  CONTINUE

      NCHHD=NOC                                                         
C        COPY EACH LINE FOR STATION K TO TMPBUL (IF FORECASTS WERE      
C        WRITTEN) TO DETERMINE STATION WITH LARGEST # OF BYTES (IMAX)
      IMAX=0
C      NESTA=10

      DO 400 K=NBSTA,NESTA                                              
        NOC=1                                                           
        DO 300 J=1,LINES                                                
          IF (KOUNT(J,K).NE.0) THEN                                     
            CALL PUTQ(MSG(1,(K-1)*LINES+J+NOHEAD),TMPBUL(NOC),NOC,N1)   
          ENDIF                                                         
 300    CONTINUE                                                        
        ICNT(K)=NOC                                                     
        IF (ICNT(K).GT.IMAX) IMAX=ICNT(K)                               
 400  CONTINUE

C        DETERMINE THE MAXIMUM NUMBER OF STATIONS THAT WILL FIT IN A    
C        BULLETIN.  ANY ADDITIONAL STATIONS WILL BE PUT IN ANOTHER      
C        BULLETIN WITH THE SAME WMO HEADER. (NSTAPB IS TYPICALLY 2)
      LENBUL=3*LRECL                                                    
      NSTAPB=(LENBUL-NCHHD)/IMAX                                        


C        START FILLING BULLETIN
C******************************
C

C        WRMSG GENERATES A TX BULLETIN FOR EVERY NSTAPB STATIONS
C        NOC=62 "AGAIN" NOTE: IF NOHEAD=1(FOUS14TX) NOC=62 
C        OTHERWISE, NOC IS LARGER THAN 62
      NOC=NCHHD                                                         
      KK=0
C*******************
C*******************IREC = DIRECT ACCESS DATA RECORD #
C      IREC=0

      DO 1000 K=NBSTA,NESTA                                             
        KK=KK+1

C        WRITE A STATION MESSAGE TO THE BULLETIN ARRAY                  
        DO 650 J=1,LINES                                                
          IF (KOUNT(J,K).NE.0) THEN                                     
            CALL PUTQ(MSG(1,(K-1)*LINES+J+NOHEAD),MT(NOC),NOC,N1)       
          ENDIF                                                         
 650    CONTINUE

C       NOC=NOC+ICNT(K)-1

C        INSERT END-OF-BULLETIN CHARACTER AND DETERMINE HOW MANY RECORDS
C        ARE NEEDED TO HOLD THE BULLETIN
        IF ((MOD(KK,NSTAPB).EQ.0).OR.(K.EQ.NESTA)) THEN                 
           CALL PUTCHAR('%',MT(NOC),1)                                  
           WRITE(6,651) NAME,NOC                            
 651       FORMAT(' BULLETIN ',A11,' CONTAINS ',I6,' BYTES.')

C        NUMREC IS (3100/1280)+1 OR APROX. 3 
	   NUMREC=(NOC/LRECL)+1                                          
           IF (MOD(NOC,LRECL).EQ.0) NUMREC=NUMREC-1

C        WRITE THE BULLETIN OUT TO THE TX FILE IN NUMREC RECORDS AS
C        A DIRECT ACCESS FILE. EQUIVALENT TO A IBM MAINFRAME SEQUENTIAL
C        FILE. NOTE: THE 1280 BYTE RECORDS ARE NOT COMPLETELY FILLED
C        ON EITHER THE HOBBS(CRAY) OR IBM MAINFRAME.

           DO 800 I=1,NUMREC
	     IREC=IREC+1
             IST=((I-1)*LRECL)-(I-2)                                    
             IF (I.EQ.1) IST=1                                          
             LST=(I*LRECL)-I                                            
             IF (I.NE.NUMREC) THEN
C               WRITE(10,30,REC=1,IOSTAT=IOS) (CREC(J),J=1,1280)
                WRITE(LUNIT,1100,REC=IREC,IOSTAT=IOS) 
     *		(MT(N),N=IST,LST),NETB
             ELSE                                                       
                WRITE(LUNIT,1100,REC=IREC,IOSTAT=IOS) 
     *		(MT(N),N=IST,LST)
             ENDIF
 1100           FORMAT(1280A1)
 800      CONTINUE

C        RESET MT() TO START THE NEXT BULLETIN                          
           NOC=JA+60                                                    
           DO 900 I=NOC,3*LRECL                                         
             MT(I)=NFIL                                                 
 900       CONTINUE                                                     
        ENDIF

 1000 CONTINUE

C       MUST NOW CLOSE THE DIRECT ACCESS DATA FILE
C301  CONTINUE
      GO TO 600

 99    WRITE(6,305)IOS
 305   FORMAT(1X,'OPEN IN WRMSG FAILED: IOS=',I5)
       STOP 3115

 600  IF(NSTART.EQ.NHEAD)THEN 
	 CLOSE(LUNIT,IOSTAT=IOS)
      ENDIF

C501  CONTINUE

      RETURN                                                            
      END                                                               
