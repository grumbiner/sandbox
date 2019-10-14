      SUBROUTINE mulreg(x, y, a, nexp, nvar)
C     Compute the multiple-linear regression of y on x
C     Robert Grumbine 15 Dec 1994

      IMPLICIT none

      INTEGER nexp, nvar
      REAL x(nexp, nvar), y(nexp), a(nvar)
     
      REAL coef(20, 20)
      INTEGER i, j, k

C     For LINPACK
      INTEGER lda, info
      PARAMETER (lda = 200)
      INTEGER ipvt(lda)

      PRINT *,'Summing for the rhs'
      DO 1000 j = 1, nvar
        a(j) = 0.0
        DO 1010 i = 1, nexp
          a(j) = x(i,j)*y(i) + a(j)
 1010   CONTINUE
 1000 CONTINUE

      DO 2000 j = 1, nvar
        DO 2010 i = j, nvar
        coef(i,j) = 0.0
          DO 2020 k = 1, nexp
            coef(i,j) = x(k,j)*x(k,i) + coef(i,j)
 2020     CONTINUE
          coef(j,i) = coef(i,j)
          PRINT *,i, j, coef(i,j)
 2010   CONTINUE
 2000 CONTINUE

C     Now call the IMSL routines to solve for a:
      PRINT *,'Calling IMSL'
      CALL SGEFA(coef, lda, nvar, ipvt, info)
      IF (info .NE. 0) PRINT *,'Nearly singular matrix, info=',info
      CALL SGESL(coef, lda, nvar, ipvt, a, 0)

      RETURN
      END      
