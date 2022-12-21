      PROGRAM response
      IMPLICIT none
C     compute the response coefficients for n terms
      INTEGER ndata, nterms, maxterms
      PARAMETER (ndata = 6136)
      PARAMETER (maxterms = 3200)
      REAL radius(ndata), press(ndata)

      DOUBLE PRECISION mik(0:maxterms, 0:maxterms), curly(0:maxterms)
      DOUBLE PRECISION sumx, xy, ysq, t1, t2, t3
      INTEGER ipvt(0:maxterms), info
      
      INTEGER i, j

      OPEN (10, FILE="radius", FORM="FORMATTED", STATUS="OLD")
      OPEN (11, FILE="press", FORM="FORMATTED", STATUS="OLD")
       
      DO i = 1, ndata
        READ (10, *) radius(i)
        READ (11, *) press(i)
      ENDDO

      ysq = xy(press, press, maxterms, ndata, 0)
      sumx = xy(radius, radius, maxterms, ndata, 0)

      DO j = 0, maxterms-1
        curly(j) = xy(press, radius, maxterms, ndata, j)
        t3 = curly(j)
        t2 = xy(press, press, maxterms, ndata, j)
        t1 = xy(radius, radius, maxterms, ndata, j)
!        PRINT *,curly(j)/sqrt(sumx)/sqrt(sumx)
!        PRINT *,curly(j)/sqrt(ysq)/sqrt(ysq)
!        PRINT *,curly(j)/sqrt(ysq)/ sqrt(sumx)
        PRINT *,j, t1/sqrt(sumx)/sqrt(sumx), t2/sqrt(ysq)/sqrt(ysq), 
     1                  t3/sqrt(sumx)/sqrt(ysq)
      ENDDO

!      PRINT *, curly(0), sumx, curly(0)/sumx

!      CALL crossum(radius, press, maxterms, ndata, maxterms, mik)
!      PRINT *,mik

!      CALL dgesv(maxterms, 1, mik, maxterms+1, ipvt, curly, 
!     1                               maxterms+1, info)
!      PRINT *,'info = ',info
!      DO i = 0, maxterms-1
!        PRINT *,i,curly(i)
!      ENDDO


      STOP
      END
      DOUBLE PRECISION FUNCTION xy(y, x, nterms, ndata, lag)
      INTEGER ndata, nterms
      REAL x(ndata), y(ndata)
      INTEGER i, lag
      DOUBLE PRECISION sumx

      sumx = 0
      DO i = nterms, ndata
        sumx = sumx + x(i-lag)*y(i)
      ENDDO

      xy = sumx
      RETURN
      END

      SUBROUTINE crossum(x, y, nterms, ndata, maxterms, mik)
      INTEGER nterms, ndata, maxterms
      REAL x(ndata), y(ndata)
      DOUBLE PRECISION mik(0:maxterms, 0:maxterms)
      DOUBLE PRECISION sumx

      INTEGER i, j, k

      DO k = 0, nterms-1
        DO i = 0, k
          sumx = 0.0
          DO j = nterms, ndata
            sumx = sumx + x(j-k)*x(j-i)
          ENDDO
!          PRINT *,'sumx = ',sumx
          mik(i,k) = sumx 
        ENDDO
      ENDDO

      RETURN 
      END
