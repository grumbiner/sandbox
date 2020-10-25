      PROGRAM unweaver
! Test/demo of successive over-relaxation method to fill in land
! temperatures -- replacement for 'weaver'
      IMPLICIT none
      INTEGER nx, ny, landflag
      REAL dlat, dlon
      PARAMETER (nx = 12*360)
      PARAMETER (ny = 12*180)
      PARAMETER (landflag = 157)
      PARAMETER (dlat = 1./12.)
      PARAMETER (dlon = 1./12.)

      REAL sst(nx, ny), orig(nx, ny), mask(nx, ny)
      REAL weight

      OPEN(11, FILE="outputs", FORM="UNFORMATTED", STATUS="NEW")

      OPEN(10, FILE="sstin", FORM="UNFORMATTED", STATUS="OLD")
      READ(10) orig
      PRINT *,'orig max min ',MAXVAL(orig), MINVAL(orig)
      CLOSE(10)
      WRITE(11) orig

      OPEN(12, FILE="mask", FORM="UNFORMATTED", STATUS="OLD")
      READ(12) mask
      PRINT *,'mask max min ',MAXVAL(mask), MINVAL(mask)
      CLOSE(12)
      mask = mask * 100
      WHERE (mask .GT. 100.1) mask = 157
      WHERE (mask .LT. 100.1) mask = 0
      WRITE (11) mask
      

!      READ (*,*) weight
      weight = 1.0
      sst = orig - 273.15
      CALL sor(sst, mask, nx, ny, dlat, dlon, landflag, weight)
      PRINT *,'sst-post max min ',MAXVAL(sst), MINVAL(sst)
      sst = sst + 273.15
      WRITE(11) sst

      sst = sst - orig
      WRITE(11) sst
      PRINT *,'delta max min ',MAXVAL(sst), MINVAL(sst)
      
      END

! Note that this will over-write the input analysis grid
      SUBROUTINE sor(analysis, mask, nx, ny, dlat, dlon, landflag, weight) 
      IMPLICIT none
      INTEGER nx, ny, landflag
      REAL dlat, dlon, weight
      REAL analysis(nx, ny)
      REAL mask(nx, ny)

      INTEGER i, itmax, delta
      REAL rms, limit, dmax, dmin
      REAL(SELECTED_REAL_KIND(6,10)) :: tmp(nx, ny), old(nx, ny), analy2(nx, ny)

      i      = 0
      rms    = 9e9
      itmax  = 10000
      limit  = 0.00001

      analy2 = analysis
      old = analysis
      delta = 256 

      DO WHILE (delta .GE. 1) 
        rms    = 9e9
        DO WHILE (rms .GT. limit .AND. i .LT. itmax)
          CALL iterate(analy2, mask, nx, ny, dlat, dlon, landflag, delta)
          tmp = analy2 - old;
      
          dmax = MAXVAL(tmp)
          dmin = MINVAL(tmp)
          rms = max(dmax, -dmin);
    
          old = old + weight*tmp;
          i = i + 1
          PRINT *,'iteration ',delta, i,'max = ',rms
          IF (rms .LT. 0.01) delta = delta / 2
          IF (delta .EQ. 0)  delta = 256
        END DO
        PRINT *,'final iteration ',delta, i, rms
      END DO

      RETURN
      END

      SUBROUTINE iterate(y, mask, nx, ny, dlat, dlon, landflag, delta)
      IMPLICIT none
      INTEGER nx, ny, landflag
      REAL dlat, dlon
      REAL(SELECTED_REAL_KIND(6,10))  y(nx, ny)
      REAL mask(nx, ny)

      INTEGER i, j, delta 
      REAL rdlat, rdlon, firstlat
      REAL(SELECTED_REAL_KIND(6,10))  theta, divisor, del, rpdg, c1, c2, c3
      INTEGER ipi, ipj, imi, imj, jpi, jpj, jmi, jmj

      rpdg = ABS(dacos(-1.d0)/180.)
      rdlat = dlat * rpdg
      rdlon = dlon * rpdg
      c1 = 1./rdlat/rdlat

      firstlat = 90 - dlat/2.

! Need to take care of j = 1, ny seam/edge
!  well, not really -- in antarctic, it's a fool who thinks the pole is
!  not land, and in the arctic, it's analyzed.
      DO j = 1+delta, ny-delta
        ipj = j
        jpj = j + delta
        imj = j
        jmj = j - delta
        theta = rpdg * (firstlat - dlat*(j-1))
        divisor = 2./cos(theta)/cos(theta)/rdlon/rdlon + 2./rdlat/rdlat
        c2 = 1./rdlon/rdlon/cos(theta)/cos(theta)
        c3 = -tan(theta)/2./rdlat

      DO i = 1, nx

        IF (mask(i,j) .EQ. landflag) THEN
          jmi = i
          jpi = i
          imi = i-delta
          IF (imi .LE. 0) imi = nx-imi
          ipi = i+delta
          IF (ipi .GE. nx+1) ipi = 1+ipi-nx
          del = (y(jpi, jpj)+y(jmi, jmj))*c1 + & 
                (y(ipi, ipj)+y(imi, imj))*c2
          del = del - c3*(y(jpi, jpj)-y(jmi, jmj))
          y(i,j) = del/divisor
        ENDIF

      ENDDO
      ENDDO
      

      RETURN
      END
