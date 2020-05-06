C
	SUBROUTINE FNDBIT  ( rmin, rmax, mxsgdg, nmbts, iscale, iret )
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    FNDBIT
C   PRGMMR: RIVIN            ORG: W/NMC21    DATE: 00-11-01
C
C ABSTRACT: COMPUTE NUMBER OF PACKING BITS GIVEN THE MAX NUMBER OF 
C   SIGNIFICANT DIGITS TO PRESERVE
C
C PROGRAM HISTORY LOG:
C
C USAGE:    CALL FNDBIT ( AMIN, AMAX, MXSGDG, NBITS, ISCALE, IRET )
C   INPUT ARGUMENT LIST:
C     AMIN - Minimum value
C     AMAX - Maximum value
C     MXSGDG - Maximum # of significant digits
C
C   OUTPUT ARGUMENT LIST:
C     NBITS - Number of bits for packing
C     ISCALE - Power of 10 scaling to use
C     IRET - Return code
C            0 = normal return
C            +1 = all missing
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
C* K. Brill/NMC		06/92						*
C* K. Brill/NMC		08/92	Add all missing check			*
C************************************************************************
C*
	DATA		rln2/0.69314718/
C-----------------------------------------------------------------------
	iret = 0
	icnt = 0
	iscale = 0
C*
	range = rmax - rmin
	IF ( range .le. 0.00 ) THEN
	    nmbts = 8
	    RETURN
	END IF
	ipo = INT ( ALOG10 ( range ) )
	IF ( range .lt. 1.00 ) ipo = ipo - 1
	ipo = ipo - mxsgdg + 1
	iscale = -ipo
	rr = range * 10. ** ( -ipo )
	nmbts = INT ( ALOG ( rr ) / rln2 ) + 1
C*
	RETURN
	END
