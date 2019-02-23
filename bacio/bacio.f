C-----------------------------------------------------------------------
      MODULE BACIO_MODULE
C$$$  F90-MODULE DOCUMENTATION BLOCK
C
C F90-MODULE: BACIO_MODULE   BYTE-ADDRESSABLE I/O MODULE
C   PRGMMR: IREDELL          ORG: NP23        DATE: 98-06-04
C
C ABSTRACT: MODULE TO SHARE FILE DESCRIPTORS
C   IN THE BYTE-ADDESSABLE I/O PACKAGE.
C
C PROGRAM HISTORY LOG:
C   98-06-04  IREDELL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      INTEGER,EXTERNAL:: BACIO
      INTEGER,DIMENSION(999),SAVE:: FD=999*0
      INTEGER,PARAMETER:: BACIO_OPENR=1
      INTEGER,PARAMETER:: BACIO_OPENW=2
      INTEGER,PARAMETER:: BACIO_OPENRW=4
      INTEGER,PARAMETER:: BACIO_CLOSE=8
      INTEGER,PARAMETER:: BACIO_READ=16
      INTEGER,PARAMETER:: BACIO_WRITE=32
      INTEGER,PARAMETER:: BACIO_NOSEEK=64
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPEN(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPEN         BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPEN(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENRW,IB,JB,1,NB,KA,FD(LU),CFN,A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENR(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENW        BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR READ ONLY.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENR(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENR,IB,JB,1,NB,KA,FD(LU),CFN,A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAOPENW(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPENW        BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: OPEN A BYTE-ADDRESSABLE FILE FOR WRITE ONLY.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAOPENW(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER FILENAME TO OPEN
C                  (CONSISTING OF NONBLANK PRINTABLE CHARACTERS)
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER CFN*(*)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_OPENW,IB,JB,1,NB,KA,FD(LU),CFN,A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BACLOSE(LU,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BACLOSE        BYTE-ADDRESSABLE CLOSE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: CLOSE A BYTE-ADDRESSABLE FILE.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BACLOSE(LU,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO CLOSE
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_CLOSE,IB,JB,1,NB,KA,FD(LU),CFN,A)
      IF(IRET.EQ.0) FD(LU)=0
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAREAD(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAREAD         BYTE-ADDRESSABLE READ
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: READ A GIVEN NUMBER OF BYTES FROM AN UNBLOCKED FILE,
C   SKIPPING A GIVEN NUMBER OF BYTES.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAREAD(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO READ
C     IB           INTEGER NUMBER OF BYTES TO SKIP
C     NB           INTEGER NUMBER OF BYTES TO READ
C   OUTPUT ARGUMENTS:
C     KA           INTEGER NUMBER OF BYTES ACTUALLY READ
C     A            CHARACTER*1 (NB) DATA READ
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER A(NB)
      CHARACTER CFN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_READ,IB,JB,1,NB,KA,FD(LU),CFN,A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE BAWRITE(LU,IB,NB,KA,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAWRITE        BYTE-ADDRESSABLE WRITE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 1998-06-04
C
C ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE,
C   SKIPPING A GIVEN NUMBER OF BYTES.
C
C PROGRAM HISTORY LOG:
C   1998-06-04  IREDELL
C
C USAGE:    CALL BAWRITE(LU,IB,NB,KA,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WRITE
C     IB           INTEGER NUMBER OF BYTES TO SKIP
C     NB           INTEGER NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C   OUTPUT ARGUMENTS:
C     KA           INTEGER NUMBER OF BYTES ACTUALLY WRITTEN
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER A(NB)
      CHARACTER CFN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_WRITE,IB,JB,1,NB,KA,FD(LU),CFN,A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
C-----------------------------------------------------------------------
      SUBROUTINE WRYTE(LU,NB,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: WRITE A GIVEN NUMBER OF BYTES TO AN UNBLOCKED FILE.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   95-10-31  IREDELL     WORKSTATION VERSION
C
C USAGE:    CALL WRYTE(LU,NB,A)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WHICH TO WRITE
C     NB           INTEGER NUMBER OF BYTES TO WRITE
C     A            CHARACTER*1 (NB) DATA TO WRITE
C
C MODULES USED:
C   BACIO_MODULE   BYTE-ADDRESSABLE I/O FORTRAN INTERFACE
C
C SUBPROGRAMS CALLED:
C   BACIO          BYTE-ADDRESSABLE I/O C PACKAGE
C
C REMARKS:  A BAOPEN MUST HAVE ALREADY BEEN CALLED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      USE BACIO_MODULE
      CHARACTER A(NB)
      CHARACTER CFN
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      IRET=BACIO(BACIO_WRITE+BACIO_NOSEEK,IB,JB,1,NB,KA,FD(LU),CFN,A)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
