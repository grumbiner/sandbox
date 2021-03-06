C
      SUBROUTINE PRNCRN (TITLE,A,IM,JM,IMIN,IMAX,JMIN,JMAX)
C     ===========================================
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PRNCRN
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT:
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL PRNCRN (TITLE,A,IM,JM,IMIN,IMAX,JMIN,JMAX)
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
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
      DIMENSION A(IM,JM)
      CHARACTER*10 TITLE
      WRITE (*,'(A10)') TITLE
      DO J=JMIN,JMAX
        WRITE (*,'(10(F7.3,1X))') (A(I,J), I=IMIN,IMAX)
      ENDDO
      RETURN
      END
