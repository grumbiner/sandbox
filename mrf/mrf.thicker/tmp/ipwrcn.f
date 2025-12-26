      FUNCTION IPWRCN(CN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    IPWRCN      GET EXPECTED ORDER OF MAGNITUDE OF A FIELD.
C   PRGMMR: MARK IREDELL     ORG: W/NMC23    DATE: 91-03-15
C
C ABSTRACT: INTEGER FUNCTION TO RETURN THE EXPECTED
C           ORDER OF MAGNITUDE OF A FIELD GIVEN ITS NAME.
C
C PROGRAM HISTORY LOG:
C   91-03-15  MARK IREDELL
C
C USAGE:    I = IPWRCN (CN)
C   INPUT ARGUMENT LIST:
C     CN       - CHARACTER*8 FIELD NAME
C
C   OUTPUT ARGUMENT LIST:
C     IPWRCN   - EXPECTED ORDER OF MAGNITUDE
C
C   SUBPROGRAMS CALLED:
C     (ISC8)   - CHARACTER*8 STRING SEARCH
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77.
C   MACHINE:  CRAY YMP.
C
C$$$
      PARAMETER(NL=56)
      CHARACTER*8 CN
      CHARACTER*8 CNL(NL)
      INTEGER IPL(0:NL)
      SAVE CNL,IPL
      DATA IPL(0)/0/
      DATA CNL( 1)/'U       '/, IPL( 1)/0/
      DATA CNL( 2)/'V       '/, IPL( 2)/0/
      DATA CNL( 3)/'TV      '/, IPL( 3)/0/
      DATA CNL( 4)/'Q       '/, IPL( 4)/-3/
      DATA CNL( 5)/'VOT**2  '/, IPL( 5)/-12/
      DATA CNL( 6)/'DIV**2  '/, IPL( 6)/-12/
      DATA CNL( 7)/'OMEGA   '/, IPL( 7)/-5/
      DATA CNL( 8)/'T       '/, IPL( 8)/0/
      DATA CNL( 9)/'RH      '/, IPL( 9)/0/
      DATA CNL(10)/'KE      '/, IPL(10)/0/
      DATA CNL(11)/'PD      '/, IPL(11)/-1/
      DATA CNL(12)/'DTCONV  '/, IPL(12)/-5/
      DATA CNL(13)/'DTLARG  '/, IPL(13)/-5/
      DATA CNL(14)/'DTSHAL  '/, IPL(14)/-5/
      DATA CNL(15)/'DTVRDF  '/, IPL(15)/-5/
      DATA CNL(16)/'DQCONV  '/, IPL(16)/-8/
      DATA CNL(17)/'DQSHAL  '/, IPL(17)/-8/
      DATA CNL(18)/'DQVRDF  '/, IPL(18)/-8/
      DATA CNL(19)/'DUVRDF  '/, IPL(19)/-5/
      DATA CNL(20)/'DVVRDF  '/, IPL(20)/-5/
      DATA CNL(21)/'DTHSW   '/, IPL(21)/-5/
      DATA CNL(22)/'DTHLW   '/, IPL(22)/-5/
CDG3..   INSERTED MULTI-LAYERED CLOUD HERE, RATHER THAN AT THE END,
CDG3..     SINCE IT SEEMS TO BE JUST BEFORE THE SINGLE LYR DATA...KAC
      DATA CNL(23)/'CLOUD   '/, IPL(23)/0/
      DATA CNL(24)/'CVCLD   '/, IPL(24)/0/
      DATA CNL(25)/'RAIN    '/, IPL(25)/-3/
      DATA CNL(26)/'RAINC   '/, IPL(26)/-3/
      DATA CNL(27)/'DTSFC   '/, IPL(27)/0/
      DATA CNL(28)/'DQSFC   '/, IPL(28)/0/
      DATA CNL(29)/'DUSFC   '/, IPL(29)/-3/
      DATA CNL(30)/'DVSFC   '/, IPL(30)/-3/
      DATA CNL(31)/'RCOV    '/, IPL(31)/0/
      DATA CNL(32)/'RCOVC   '/, IPL(32)/0/
      DATA CNL(33)/'TSKIN   '/, IPL(33)/0/
      DATA CNL(34)/'WETNESS '/, IPL(34)/0/
      DATA CNL(35)/'SNOW    '/, IPL(35)/1/
      DATA CNL(36)/'TG1     '/, IPL(36)/0/
      DATA CNL(37)/'TG2     '/, IPL(37)/0/
      DATA CNL(38)/'TG3     '/, IPL(38)/0/
      DATA CNL(39)/'SFCSW   '/, IPL(39)/0/
      DATA CNL(40)/'SFCLW   '/, IPL(40)/0/
      DATA CNL(41)/'ZORL    '/, IPL(41)/0/
      DATA CNL(42)/'SLMSK   '/, IPL(42)/0/
      DATA CNL(43)/'PS      '/, IPL(43)/-1/
      DATA CNL(44)/'TVS     '/, IPL(44)/0/
      DATA CNL(45)/'QS      '/, IPL(45)/-3/
      DATA CNL(46)/'TS      '/, IPL(46)/0/
      DATA CNL(47)/'RHS     '/, IPL(47)/0/
      DATA CNL(48)/'DUGWD   '/, IPL(48)/-3/
      DATA CNL(49)/'DVGWD   '/, IPL(49)/-3/
      DATA CNL(50)/'UA      '/, IPL(50)/10/
      DATA CNL(51)/'UAP     '/, IPL(51)/10/
      DATA CNL(52)/'DUASFC  '/, IPL(52)/3/
      DATA CNL(53)/'DUAGWD  '/, IPL(53)/3/
      DATA CNL(54)/'DUAMTN  '/, IPL(54)/3/
      DATA CNL(55)/'EP      '/, IPL(55)/0/
      DATA CNL(56)/'CLDWORK '/, IPL(56)/0/
C
      N=ISC8(NL,CNL,CN)
      IPWRCN=IPL(N)
      RETURN
      END
