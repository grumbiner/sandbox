      SUBROUTINE GETCON(NSTA,CSTA,ICODE,ID,RDATA,ICON,MDATE,NERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
C                .      .    .                                       .  
C SUBPROGRAM:    GETCON      READS CLIMATIC VALUES FROM CONSTANT FILE   
C   PRGMMR: GILBERT          ORG: W/OSD211   DATE: 93-12-03             
C                                                                       
C ABSTRACT: READS THE CLIMATIC VALUES FROM THE CONSTANT 
C   FILE, AND THEN PERFORMS THE NECESSARY CONVERSIONS TO THE            
C   VALUES.                                                             
C                                                                       
C PROGRAM HISTORY LOG:                                                  
C   93-12-03  GILBERT                                                   
C   98-07-29  shirey  changed gtnorm/getmon to getnorm/getmonth for y2k   
C USAGE:                                                                
C        DEC  1993   GILBERT    TDL    NAS9000                          
C                                                                       
C        PURPOSE                                                        
C            READS THE CLIMATIC VALUES FROM THE
C            CONSTANT FILE, AND THEN PERFORMS THE NECESSARY CONVERSIONS 
C            TO THE VALUES.                                             
C                                                                       
C        DATA SET USE                                                   
C            FT06     - PRINT FILE (OUTPUT)                             
C            MOSCONST - OSD.PROD.TDL.USCONST  (MOS CONSTANT FILE)       
C                                                                       
C        VARIABLES                                                      
C                MDATE = CURRENT DATE                                   
C                 NSTA = NUMBER OF STATIONS                             
C              RDATA() = CONTAINS CLIMATIC VALUES READ                  
C                ID(4) = MOS FORECAST IDENTIFIER
C                 ICYC = CURRENT CYCLE  (00 OR 12)                      
C              IWBAN() = LIST OF STATION WBAN NUMBERS                   
C                 NERR = ERROR RETURNED FROM GTNMFQ                     
C                 JOPT = INDICATES TYPE OF CLIMATIC VALUE               
C                                                                       
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION             
C                                                                       
C ATTRIBUTES:                                                           
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS                     
C   MACHINE:  NAS, CYBER, WHATEVER                                      
C                                                                       
C$$$                                                                    
      INTEGER ID(4)
      REAL RDATA(NSTA)
      CHARACTER*8 CSTA(NSTA)
      ISW=1
      NVCYC=MOD((MDATE+MOD(ID(3),24)),100)
      IF ((ICODE.EQ.213505009.OR.ICODE.EQ.213505008).AND.
     *    (NVCYC.EQ.00)) THEN
                  ID(1)=403626000
                  JOPT=1
         ELSE IF((ICODE.EQ.213505009.OR.ICODE.EQ.213505008).AND.
     *           (NVCYC.EQ.12)) THEN
                  ID(1)=403624000
                  JOPT=1
         ELSE IF((ICODE.EQ.213510009).AND.(NVCYC.EQ.00)) THEN
                  ID(1)=403628000
                  JOPT=1
         ELSE IF((ICODE.EQ.214215009.OR.ICODE.EQ.214215008).AND.
     *           (NVCYC.EQ.00)) THEN
                  ID(1)=404217000
                  JOPT=2
         ELSE IF((ICODE.EQ.214215009.OR.ICODE.EQ.214215008).AND.
     *           (NVCYC.EQ.12)) THEN
                  ID(1)=404216000
                  JOPT=2
         ELSE IF((ICODE.EQ.218305009.OR.ICODE.EQ.218305008).AND.
     *           (NVCYC.EQ.00)) THEN
                  ID(1)=408307000
                  JOPT=3
         ELSE IF((ICODE.EQ.218305009.OR.ICODE.EQ.218305008).AND.
     *           (NVCYC.EQ.12)) THEN
                  ID(1)=408306000
                  JOPT=3
         ELSE IF((ICODE.EQ.218505009.OR.ICODE.EQ.218505008).AND.
     *           (NVCYC.EQ.00)) THEN
                  ID(1)=408507000
                  JOPT=1
         ELSE IF((ICODE.EQ.218505009.OR.ICODE.EQ.218505008).AND.
     *           (NVCYC.EQ.12)) THEN
                  ID(1)=408506000
                  JOPT=1
         ELSE IF((ICODE.EQ.212001009.OR.ICODE.EQ.212001008)) THEN
                  ID(1)=402006000
                  JOPT=4
         ELSE IF((ICODE.EQ.212011009.OR.ICODE.EQ.212011008)) THEN
                  ID(1)=402016000
                  JOPT=4
         ELSE
C        ICODE IS NOT RECOGNIZED BY THIS SUBROUTINE       
           WRITE(6,230) ICODE,NVCYC
  230      FORMAT(' ','ICODE =',I4,I3,' NOT RECOGNIZED BY SUBROUTINE ',
     *            ' CALIBR IN STATEMENT 230. NERR SET TO 1.')
           NERR=1
           GO TO 900
      ENDIF
C        READ NORMALS (IF ID INDICATES A TEMPERATURE NORMAL)
      IF ((ID(1).EQ.402006000).OR.(ID(1).EQ.402016000)) THEN
	CALL GETNORM(ICON,ID,MDATE,NSTA,RDATA,ISW,CSTA,X,NERR)
      ELSE
C        READ RELATIVE FREQUENCIES
	CALL GETMONTH(ICON,ID,MDATE,NSTA,RDATA,ISW,CSTA,X,NERR)
      ENDIF
      IF (NERR.NE.0) THEN
        WRITE(6,300) ID,NERR
 300    FORMAT(' ERROR READING CONSTANT FILE FOR ID = ',4I10,
     *  ',  NERR = ',I4)
        GOTO 900
      ENDIF
      IF (JOPT.EQ.1) THEN
C        CHANGE RELATIVE FREQUENCIES TO PROBABILITIES
        DO 400 I=1,NSTA
          IF (RDATA(I).NE.9999.) THEN
            RDATA(I)=RDATA(I)*100.0
          ENDIF
 400    CONTINUE
      ELSEIF (JOPT.EQ.2) THEN
C        CHANGE KNOTS TO M/S FOR WIND SPEEDS
        DO 500 I=1,NSTA
          IF (RDATA(I).NE.9999.) THEN
            RDATA(I)=0.51479*RDATA(I)
          ENDIF
 500    CONTINUE
      ELSEIF (JOPT.EQ.3) THEN
C        CHANGE MEAN CLOUDINESS (TENTHS) TO PROBABILITIES
        DO 600 I=1,NSTA
          IF (RDATA(I).NE.9999.) THEN
            RDATA(I)=RDATA(I)*10.0
          ENDIF
 600    CONTINUE
      ELSEIF (JOPT.EQ.4) THEN
C        CHANGE FAHRENHEIT TO CELSIUS FOR TEMPS
        DO 700 I=1,NSTA
          IF (RDATA(I).NE.9999.) THEN
            RDATA(I)=273.16+((RDATA(I)-32.0)*5.0/9.0)
          ENDIF
 700    CONTINUE
      ENDIF
 900  RETURN
      END
