C
      SUBROUTINE FULLYR(IYYS)
C     ===========================================
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FULLYR
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT: CALCULATES THE 4-DIGIT YEAR
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL FULLYR(IYYS)
C   INPUT ARGUMENT LIST:
C     IYYS - 2-digit year
C
C   OUTPUT ARGUMENT LIST:
C     IYYS - 4-digit year
C
C   SUBPROGRAMS CALLED
C     UNIQUE:
C
C ATTRIBUTES:
C   LANGUAGE: IBM 370 VS FORTRAN
C   MACHINE:  NAS, CRAY C-90, IBM SP
C
C$$$
C
      IF (IYYS.LT.100) THEN
        IF (IYYS.GE.80) THEN
          IYYS=1900+IYYS
        ELSE
          IYYS=2000+IYYS
        ENDIF
      ENDIF
      RETURN
      END
