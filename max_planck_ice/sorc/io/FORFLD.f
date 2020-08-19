      SUBROUTINE FORFLD(LWDN, SWDN, INTYP, IIC, UWIN, VWIN)
      IMPLICIT none
C=======================================================================
C  PROGRAMMED BY:
C     W.BRECHNER OWENS      MPI, HAMBURG                          AUG.87
C  MODIFIED BY:
C     ACHIM STOESSEL        MPI, HAMBURG                          MAY 91
C     Robert Grumbine       NMC, Camp Springs                     Nov 92
C     Robert Grumbine       NMC, Camp Springs                     Jul 93
C     Robert Grumbine       NCEP, Camp Springs                    Feb 97
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
CD      COMMON/STP/T,NTMES,NRST,NRREC,NPLT,NSTAT,IIC,NFLD,NSTA
CD      REAL T
CD      INTEGER NTMES, NRST, NRREC, NPLT, NSTAT, IIC, NFLD, NSTA
      INTEGER IIC
CD      COMMON/FRWND/CDWIN,SINWIN,COSWIN,UWIN(L,M),VWIN(L,M)
CD      REAL CDWIN, SINWIN, COSWIN, UWIN, VWIN
      REAL UWIN(L,M), VWIN(L,M)
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
      LOGICAL overload
C-----------------------------------------------------------------------
C    Call routine to get information from the MRF
C-----------------------------------------------------------------------
        IF (MOD(IIC,14) .EQ. 1) REWIND(20)
        READ (20) TAIR
        READ (20) PA
        READ (20) TD
        READ (20) ts
        READ (20) SWDN
CD        CALL rescale(SWDN, LP, MP, 0.75, 0.0) 
        READ (20) LWDN
CD        CALL rescale(LWDN, LP, MP, 1.0, -20.0)
        READ (20) LWUP
        READ (20) RPREC
        READ (20) mask
        READ (20) UWIN
        READ (20) VWIN
        IF (overload(LWDN, LP, MP, 1000., .TRUE.)) THEN
          PRINT *,'Overloaded LWDN in FORFLD, 1,000 cutoff'
        ENDIF
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
      SUBROUTINE rescale(x, nx, ny, m, b)
C     Do a linear rescaling of the array x, making a new x = mx +b
C     Robert Grumbine 30 April 1998

      INTEGER nx, ny
      REAL x(nx, ny), m, b
      INTEGER i, j
      DO 1000 j = 1, ny
      DO 1100 i = 1, nx
        x(i,j) = m*x(i,j) + b
 1100 CONTINUE
 1000 CONTINUE

      RETURN
      END
