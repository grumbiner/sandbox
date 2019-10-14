      PROGRAM restnh
C     Edit the restart file for the sea ice model
C     Set the thickness of new ice (defined to be ice in an area
C       that the satellite sees ice, but the model has _no_ ice)
C       to an externally passed value.
C     Much external assignment is required.  See makestart for
C       a sample script.
C     Bob Grumbine 16 June 1994.

      IMPLICIT none

      INCLUDE "icegrid.inc"

C     Variables read in:
      REAL thickr
      REAL CONC(0:L, 0:M)

      READ(11) CONC
      READ (*,*) thickr

      CALL mpi(conc, 14, thickr)

      STOP
      END
