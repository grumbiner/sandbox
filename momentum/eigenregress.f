      PROGRAM eigenregress
! Construct the regression equations and then find the eigenvalues
      IMPLICIT none

      INTEGER np, nstep
      PARAMETER (np = 4)
      PARAMETER (nstep = 10804)
      REAL x(np,nstep), y(np, nstep), sun(nstep), z(np+2, nstep)
      DOUBLE PRECISION m(np, np), rhs(np, np), mref(np, np)
      DOUBLE PRECISION eigen(np, np)
      DOUBLE PRECISION alpha(np), beta(np)
      DOUBLE PRECISION xdot(np), ydot(np)
      REAL dot

! Variables for LINPACK - regression
      INTEGER lda, info
      PARAMETER (lda = np)
      INTEGER ipvt(lda)

!Variables for linpack - eigenvalues
      CHARACTER jobvs*1, sort*1
      INTEGER sdim, ldvs
      DOUBLE PRECISION wr(np), wi(np), vs(np, np)
      INTEGER lwork 
      DOUBLE PRECISION work(200)
      LOGICAL bwork(np)
      LOGICAL, EXTERNAL :: select

!Local variables
      INTEGER i, j
      REAL xcrit, ycrit

      OPEN(UNIT=10, FILE="round1", FORM="FORMATTED", STATUS="OLD")
      DO i = 1, nstep
        READ (10,*) z(:,i),y(:,i),sun(i),x(:,i)
        !PRINT *,y(:,i),x(:,i)
      ENDDO
     
!Orthogonalize and normalize all with respect to the sun
      DO i = 1, np
        alpha(i) = dot(x(i,:), sun, nstep)
        beta(i)  = dot(y(i,:), sun, nstep)
        xdot (i) = dot(sun, sun, nstep)
        x(i,:) = x(i,:) - alpha(i)*sun/xdot(i)
        !y(i,:) = y(i,:) - beta (i)*sun/xdot(i)
      ENDDO

!Construct the regression matrix
      !DO j = 1, np
      !  x(j,:) = x(j,:)/sqrt(FLOAT(nstep))
      !  y(j,:) = y(j,:)/sqrt(FLOAT(nstep))
      !ENDDO

      xcrit = 0.0
      ycrit = 0.0
      DO j = 1, np
      DO i = 1, np
        m(i,j)   = dot(x(i,:),x(j,:),nstep)
        rhs(i,j) = dot(x(i,:),y(j,:),nstep)
        IF (abs(m(i,j)) .LT. xcrit) THEN
          m(i,j) = 0
        ENDIF
        IF (abs(rhs(i,j)) .LT. ycrit) THEN
          rhs(i,j) = 0
        ENDIF
      ENDDO
      WRITE (*, 9001) j, m(:,j), rhs(:,j)
      ENDDO
 9001 FORMAT(I3,4F13.6,2x,4F13.6)

!Find the regression coefficients:
      DO j = 1, np
        mref = m
        CALL DGESV(np, 1, mref, lda, ipvt, rhs(:,j), np, info)
        IF (info .NE. 0) PRINT *,'Nearly singular matrix, info = ',info
        eigen(:,j) = rhs(:,j)
      ENDDO
      PRINT *,'matrix to find eigenvalues of '
      DO i = 1, np
        PRINT *, eigen(i,:)
      ENDDO

!Find the eigenvalues
      jobvs = 'N'
      sort = 'N'
      sdim = 0
      ldvs = np
      lwork = 200
! Test case, eigenvalues should be 1-np
!      eigen = 0.D0
!      DO i = 1, np
!        eigen(i,i) = i
!      ENDDO
      PRINT *,'Calling dgees with ',eigen

      CALL DGEES(jobvs, sort, select, np, eigen, lda, sdim, wr, wi, vs, 
     1                ldvs, work, lwork, bwork, info)
      IF (info .EQ. 0) THEN
        PRINT *,'optimal lwork = ',work(1)
      ELSE
        PRINT *,'problem in eigen-computation, info = ',info
      ENDIF
      DO i = 1, np
        PRINT *,wr(i), wi(i), sqrt(wr(i)**2 + wi(i)**2)
      ENDDO


      END

!------------------------------------------------------
      REAL FUNCTION dot(x, y, n)
      IMPLICIT none
      INTEGER n
      REAL x(n), y(n)
      DOUBLE PRECISION sum
      INTEGER i
      sum = 0.D0
      DO i = 1, n
        sum = sum + x(i)*y(i)
      ENDDO
      dot = sum
      END
!-------------------------------
      LOGICAL FUNCTION select(x, y)
      DOUBLE PRECISION x, y
      select = .TRUE.
      RETURN
      END
