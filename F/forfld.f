      SUBROUTINE FORFLD(LWDN, SWDN, INTYP)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C     Robert Grumbine       NMC, Camp Springs                     Nov 92
C     Robert Grumbine       NMC, Camp Springs                     Jul 93
C  PURPOSE:
C     -READS TEMPORALLY VARYING BOUNDARY CONDITIONS (FORCING FIELDS)
C     -Rewritten to obtain forcing fields from NMC-MRF output
C     -Jul 93 variant to split off interpolations to a separate 
C       program.  This sbr. now just reads in the fields.
C  EXTERNALS:
C     -None
C=======================================================================
      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
C=======================================================================
      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
      REAL T
      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN
      COMMON/THFOR/TAIR(0:L,0:M), TD(0:L,0:M), ACL(0:L,0:M), PA(0:L,0:M)
     1  ,UG(0:L,0:M), TA(0:L,0:M), RPREC(0:L,0:M)
      REAL TAIR, TD, ACL, PA, UG, TA, RPREC
C     LWUP is not used by MPI model.  It is included here for the
C       Navy model, and written out.
      REAL LWUP(0:L,0:M), LWDN(0:L,0:M), SWDN(0:L,0:M)
      REAL ts(0:L, 0:M), mask(0:L, 0:M)
C=======================================================================
      INTEGER I, J, INTYP
C     INTYP is now a dummy
C-----------------------------------------------------------------------
C    Call routine to get information from the MRF
C-----------------------------------------------------------------------
        IF (MOD(IIC,14) .EQ. 1) REWIND(20)
        READ (20) TAIR
        READ (20) PA
        READ (20) TD
        READ (20) ts
        READ (20) SWDN
        READ (20) LWDN
        READ (20) LWUP
        READ (20) RPREC
        READ (20) mask
        READ (20) UWIN
        READ (20) VWIN
C-----------------------------------------------------------------------
C  CALCULATION OF WIND SPEED
C-----------------------------------------------------------------------
C     Indices changed by BG to avoid referencing non-extant
C       elements of uwin, vwin.
      DO 6 J = 0, MM
        DO 6 I = 0, LM
          RPREC(I,J)=0.0 !temporary measure until units are checked
       UG(I,J)=.25*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     1             +SQRT(UWIN(I+1,J  )**2+VWIN(I+1,J  )**2)
     2             +SQRT(UWIN(I  ,J+1)**2+VWIN(I  ,J+1)**2)
     3             +SQRT(UWIN(I+1,J+1)**2+VWIN(I+1,J+1)**2))
    6 CONTINUE

      I = L
      DO 10 J = 0, MM
       UG(I,J)=.5*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     2            +SQRT(UWIN(I  ,J+1)**2+VWIN(I  ,J+1)**2) )
  10  CONTINUE

      J = M
      DO 20 I = 0, LM
       UG(I,J)=.5*(SQRT(UWIN(I  ,J  )**2+VWIN(I  ,J  )**2)
     1            +SQRT(UWIN(I+1,J  )**2+VWIN(I+1,J  )**2) )
   20 CONTINUE

      I = L
      J = M
      UG(I,J) = SQRT(UWIN(I,J)**2+VWIN(I,J)**2 )

      RETURN
      END
