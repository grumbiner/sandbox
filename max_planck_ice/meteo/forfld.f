      PROGRAM forfld
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C     Robert Grumbine       NMC, Camp Springs                     Nov 92
C     Robert Grumbine       NMC, Camp Springs                     Jul 93
C     Robert Grumbine       NCEP, Camp Springs                    Sep 96
C  LAST MODIFIED: 4 November 1994.
C  PURPOSE:
C     -READS TEMPORALLY VARYING BOUNDARY CONDITIONS (FORCING FIELDS)
C     -Rewritten to obtain forcing fields from NMC-MRF output
C     -Rewritten to be program to get MRF output and convert to a
C       stand alone input file for MPI and PIPS models.
C     -Revised to call only a single routine for processing grids
C       (terp handles the regridding)
C  EXTERNALS:
C     -thregt: Convert MRF input to desired grid output
C=======================================================================
      IMPLICIT none
      INCLUDE "icegrid.inc"
C=======================================================================
      INTEGER IIC
      REAL UWIN(L,M), VWIN(L,M)
      REAL TAIR(0:L,0:M), tsfc(0:l,0:m), rh(0:L,0:M)
      REAL ACL(0:L,0:M), PA(0:L,0:M), RPREC(0:L,0:M), mask(0:L,0:M)
     1  
C     LWUP is not used by MPI model.  It is included here for the
C       Navy model, and written out.
      REAL LWUP(0:L,0:M), LWDN(0:L,0:M), SWDN(0:L,0:M)
C=======================================================================
      INTEGER POLE, NSTEP
C-----------------------------------------------------------------------
C    Call routine to get information from the MRF
C-----------------------------------------------------------------------
      READ (*,9001) NSTEP
 9001 FORMAT (I3)
  
      DO 1000 IIC = 1, NSTEP

        PRINT *,'ptype = ',PTYPE
        IF (LATMIN .GE. 0) THEN
          POLE = 1
         ELSE
          POLE = 2
        ENDIF

        CALL thregt(POLE, 1.0, UWIN, VWIN, TAIR, tsfc, rh, 
     1     PA, RPREC, ACL, LWUP, LWDN, SWDN, mask, IIC)

C       Write all met data out to file 20
        WRITE (20) TAIR
        WRITE (20) PA
        WRITE (20) rh
        WRITE (20) tsfc
        WRITE (20) SWDN
        WRITE (20) LWDN
        WRITE (20) LWUP
        WRITE (20) RPREC
        WRITE (20) mask
        WRITE (20) UWIN
        WRITE (20) VWIN

 1000 CONTINUE

      STOP
      END
