      PROGRAM eigenregress
! Construct the regression equations and then find the eigenvalues
      IMPLICIT none

      INTEGER np
!Max to survive compile time is 11568 give or take very little.
      PARAMETER (np = 11500)
      DOUBLE PRECISION eigen(np, np)

! Variables for LINPACK - regression
      INTEGER lda, info
      PARAMETER (lda = np)
      INTEGER ipvt(lda)

!Variables for linpack - eigenvalues
      CHARACTER jobvs*1, sort*1
      INTEGER sdim, ldvs
      DOUBLE PRECISION wr(np), wi(np), vs(np, np)
      INTEGER lwork 
      DOUBLE PRECISION work(34*np)
      LOGICAL bwork(np)
      LOGICAL, EXTERNAL :: select

!Local variables
      INTEGER i, j
      REAL xcrit, ycrit

      DO j = 1, np
      DO i = 1, np
        eigen(i,j) = i*j
      ENDDO
      ENDDO

!Find the eigenvalues
      jobvs = 'N'
      sort = 'N'
      sdim = 0
      ldvs = np
      lwork = 34*np

      CALL DGEES(jobvs, sort, select, np, eigen, lda, sdim, wr, wi, vs, 
     1                ldvs, work, lwork, bwork, info)
      IF (info .EQ. 0) THEN
        PRINT *,'optimal lwork = ',work(1),' vs the used ',lwork
      ELSE
        PRINT *,'problem in eigen-computation, info = ',info
      ENDIF
!      DO i = 1, np
!        PRINT *,wr(i), wi(i), sqrt(wr(i)**2 + wi(i)**2)
!      ENDDO


      END

!-------------------------------
      LOGICAL FUNCTION select(x, y)
      DOUBLE PRECISION x, y
      select = .TRUE.
      RETURN
      END
