      SUBROUTINE mpi(conc, unit, thickr)
C     Adjust the model's concentration (A) and ice thickness (H, HSN)
C       according to the observed (conc) concentration:
C       If observed concentration is nonzero, and model has zero ice,
C         put observed concentration of ice 'thickr' thick with no snow cover.
C       If observed concentration is nonzero, and model concentration is
C         nonzero, change the model concentration to match the observed.
C         no change to model ice thickness.
C       If observed concentration is zero, set model concentration and 
C         thickness to zero.
C     Bob Grumbine 16 June 1994.

      IMPLICIT none

      INCLUDE "icegrid.inc"
      INCLUDE "physical.inc"
      REAL conc(0:L, 0:M), thickr
      INTEGER unit, i, j

C     Fields for the MPI model
      REAL HM(0:L, 0:M), FLM(0:L, 0:M,2)
      REAL OM(0:L, 0:M)
      REAL TICM(0:L, 0:M, NLEVEL), QH(0:L, 0:M)
      REAL A(0:L, 0:M, 2), H(0:L, 0:M, 2), HSN(0:L, 0:M, 2)
      REAL U(L, M, 3), V(L, M, 3)
      REAL QS(0:L, 0:M), QT(0:L, 0:M), VM(L, M)
      REAL QTB(0:L, 0:M), QSB(0:L, 0:M), QHB(0:L, 0:M)
      REAL QDT(0:L, 0:M), QDS(0:L, 0:M), TICE(0:L, 0:M)

      READ (unit) U
      READ (unit) V
      READ (unit) H
      READ (unit) A
      READ (unit) HSN
      READ (unit) TICE
      READ (unit) QT
      READ (unit) QS
      READ (unit) QH
      READ (unit) QTB
      READ (unit) QSB
      READ (unit) QHB
      READ (unit) QDT
      READ (unit) QDS
      READ (unit) TICM

      CALL BCSINIT(VM, HM, OM, FLM)

      DO 1000 j = 0, M
        DO 1100 i = 0, L
          IF (conc(i,j)*OM(i,j) .NE. 0.0 .AND. A(i,j,1) .EQ. 0.0 
     1          .AND. A(i,j,2) .EQ. 0.0) THEN
            H(i,j,1) = thickr
            H(i,j,2) = thickr
            HSN(i,j,1) = 0.0
            HSN(i,j,2) = 0.0
            QT(i,j)    = tfrez
           ELSE IF (conc(i,j)*OM(i,j) .EQ. 0.0) THEN
            H(i,j,1) = 0.0
            H(i,j,2) = 0.0
            HSN(i,j,1) = 0.0
            HSN(i,j,2) = 0.0
           ELSE
C           Do nothing
          ENDIF
          A(i,j,1) = conc(i,j)/100. * OM(i,j)
          A(i,j,2) = conc(i,j)/100. * OM(i,j)
 1100   CONTINUE
 1000 CONTINUE

      REWIND (unit+1)
      WRITE (unit+1) U
      WRITE (unit+1) V
      WRITE (unit+1) H
      WRITE (unit+1) A
      WRITE (unit+1) HSN
      WRITE (unit+1) TICE
      WRITE (unit+1) QT
      WRITE (unit+1) QS
      WRITE (unit+1) QH
      WRITE (unit+1) QTB
      WRITE (unit+1) QSB
      WRITE (unit+1) QHB
      WRITE (unit+1) QDT
      WRITE (unit+1) QDS
      WRITE (unit+1) TICM

      CLOSE (unit+1, STATUS='KEEP')

      RETURN
      END
