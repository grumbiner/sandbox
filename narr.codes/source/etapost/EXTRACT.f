      SUBROUTINE EXTRACT(DUM,A)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    EXTRACT    EXTRACT THE DATA WE WILL "OWN"
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     EXTRACT THE DATA WE WILL "OWN". SETUP ONE HALO ROW.
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C
C USAGE:    CALL EXTRACT(DUM,A)
C   INPUT ARGUMENT LIST:
C      DUM - ARRAY TO EXTRACT FROM
C
C   OUTPUT ARGUMENT LIST:
C      A - ARRAY WITH EXTRACTED DATA
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON - CTLBLK.comm
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$
      include 'parmeta'
      include "CTLBLK.comm"
      real dum ( im, jm ), a ( im, jm )
C
      do j = jsta_2l, jend_2u
         do i = 1, im
            a ( i, j ) = dum ( i, j )
         end do
      end do
c
      end
