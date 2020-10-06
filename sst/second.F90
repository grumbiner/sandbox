      PROGRAM unweaver
! Test/demo of successive over-relaxation method to fill in land
! temperatures -- replacement for 'weaver'
      IMPLICIT none
      INTEGER nx, ny
      REAL landflag
      REAL dlat, dlon, firstlat
      PARAMETER (nx = 12*360)
      PARAMETER (ny = 12*180)
      PARAMETER (landflag = 157)
      PARAMETER (dlat = 1./12.)
      PARAMETER (dlon = 1./12.)
      PARAMETER (firstlat = -90 + dlat/2)
      INTEGER ratio

      REAL sst(nx, ny), orig(nx, ny), mask(nx, ny), guess(nx, ny)
      REAL weight, limit
      INTEGER i, j
      REAL, ALLOCATABLE :: y(:,:), mask2(:,:)

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
      OPEN(12, FILE="guess", FORM="UNFORMATTED", STATUS="OLD")
      READ(12) guess
      CLOSE(12)

      weight = 1.0
      sst = orig - 273.15
      WHERE (mask .NE. 0) sst = guess
   
      limit = 2.e-4
        ratio = 6
        ALLOCATE (y(nx/ratio, ny/ratio), mask2(nx/ratio, ny/ratio))
        CALL reduced(sst, y, mask, mask2, nx, ny, ratio, landflag, dlat, dlon, limit)
        DEALLOCATE(y, mask2)
        WRITE (11) sst -orig + 273.15

!Now on the fine grid for increasingly fine tolerances:
      limit = 2.5e-3
      CALL sor(sst, mask, nx, ny, firstlat, dlat, dlon, landflag, weight, limit)
      WRITE (11) sst -orig + 273.15
      limit = 5.0e-4
      CALL sor(sst, mask, nx, ny, firstlat, dlat, dlon, landflag, weight, limit)
      WRITE (11) sst -orig + 273.15
      limit = 2.5e-4
      CALL sor(sst, mask, nx, ny, firstlat, dlat, dlon, landflag, weight, limit)
      WRITE (11) sst -orig + 273.15

      PRINT *,'sst-post max min ',MAXVAL(sst), MINVAL(sst)
      OPEN(13, FILE="guess", FORM="UNFORMATTED")
      WRITE(13) sst
      CLOSE(13)

      sst = sst + 273.15
      WRITE(11) sst
      sst = sst - orig
      WRITE(11) sst
      PRINT *,'delta max min ',MAXVAL(sst), MINVAL(sst)
      
      END

! Note that this will over-write the input analysis grid
      SUBROUTINE sor(analysis, mask, nx, ny, firstlat, dlat, dlon, landflag, weight, limit) 
      IMPLICIT none
      INTEGER nx, ny
      REAL firstlat, dlat, dlon, weight, limit
      REAL analysis(nx, ny)
      REAL mask(nx, ny), landflag

      INTEGER i, itmax, delta
      REAL rms, dmax, dmin
      REAL(SELECTED_REAL_KIND(14,10)) :: tmp(nx, ny), old(nx, ny), analy2(nx, ny)

      PRINT *,'entered sor, nx = ',nx, ny, firstlat, dlat, dlon

      i      = 0
      rms    = 9e9
      itmax  = 10000

      analy2 = analysis
      old    = analysis
      delta = 1 

      DO WHILE (delta .GE. 1) 
        rms    = 9e9
        DO WHILE (rms .GT. limit .AND. i .LT. itmax .AND. delta .GE. 1)
          CALL iterate(analy2, mask, nx, ny, firstlat, dlat, dlon, landflag, delta)
          tmp = analy2 - old
      
          dmax = MAXVAL(tmp)
          dmin = MINVAL(tmp)
          rms = max(dmax, -dmin)
    
          old = old + weight*tmp
          i = i + 1
          PRINT *,'iteration ',delta, i,'max = ',rms
          IF (rms .LT. limit) delta = delta / 2
        END DO
        PRINT *,'final iteration ',delta, i, rms
        delta = delta / 2
      END DO
      analysis = old

      RETURN
      END
      SUBROUTINE average(x, y, mask1, mask2, nx, ny, ratio, landflag)
      IMPLICIT none
      INTEGER nx, ny, ratio
      REAL x(nx, ny), y(nx/ratio, ny/ratio)
      REAL mask1(nx, ny), mask2(nx/ratio, ny/ratio)
      REAL npts(nx/ratio, ny/ratio),  tally(nx/ratio, ny/ratio)
      REAL landflag
      INTEGER i,j, i2, j2

      PRINT *,'orig max, min = ',MAXVAL(x), MINVAL(x), ratio, landflag
      tally = 0
      mask2 = 0
      y     = 0 
      npts = 0
      DO j = 1, ny
      DO i = 1, nx
        j2 = ((j-1) / ratio) + 1
        i2 = ((i-1) / ratio) + 1
        IF (mask1(i,j) .EQ. landflag) THEN
          tally(i2,j2) = tally(i2,j2) + 1
        ELSE
          npts(i2, j2) = npts(i2,j2) + 1
          y(i2,j2) = y(i2,j2) + x(i,j) 
        ENDIF
      ENDDO
      ENDDO

      DO j = 1, ny/ratio
      DO i = 1, nx/ratio
        IF (npts(i,j) .NE. 0) THEN
          y(i,j) = y(i,j) / npts(i,j)
        ELSE
          mask2(i,j) = landflag
!          y(i,j) = 273.15 + 15.0
          y(i,j) = 15.0
        ENDIF
      ENDDO
      ENDDO

!      WHERE (tally .EQ. ratio*ratio) mask2 = landflag
!      WHERE (mask2 .EQ. 0) y = y / (ratio*ratio - tally)
!      WHERE (mask2 .EQ. 0) y = y / npts

      PRINT *,'average max, min = ',MAXVAL(y), MINVAL(y)
      PRINT *,'mask2 max, min = ',MAXVAL(mask2), MINVAL(mask2)
      PRINT *,'tally max, min = ',MAXVAL(tally), MINVAL(tally)

      RETURN
      END 
!----------------------------------------------------------------------------------    
      SUBROUTINE iterate(y, mask, nx, ny, firstlat, dlat, dlon, landflag, delta)
      IMPLICIT none
      INTEGER nx, ny
      REAL dlat, dlon
      REAL(SELECTED_REAL_KIND(14,10))  y(nx, ny)
      REAL mask(nx, ny), landflag

      INTEGER i, j, delta 
      REAL rdlat, rdlon, firstlat
      REAL(SELECTED_REAL_KIND(14,10))  theta, divisor, del, rpdg, c1, c2, c3
      INTEGER ipi, ipj, imi, imj, jpi, jpj, jmi, jmj

      rpdg = ABS(dacos(-1.D0)/180.)
      rdlat = dlat * rpdg
      rdlon = dlon * rpdg

! Need to take care of j = 1, ny seam/edge
!  well, not really -- in antarctic, it's a fool who thinks the pole is
!  not land, and in the arctic, it's analyzed.
      DO j = 1+delta, ny-delta
        ipj = j
        imj = j
        theta = rpdg * (firstlat + dlat*(j-1))
        divisor = 2./cos(theta)/cos(theta)/rdlon/rdlon + 2./rdlat/rdlat
        c1 = 1./rdlat/rdlat
        c2 = 1./rdlon/rdlon/cos(theta)/cos(theta)
        c3 = -tan(theta)/2./rdlat

        jpj = j + delta
        jmj = j - delta
        divisor = divisor / delta / delta
        c1 = c1 / delta / delta
        c2 = c2 / delta / delta
        c3 = c3 / delta / delta

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
      SUBROUTINE reduced(sst, y, mask, mask2, nx, ny, ratio, landflag, dlat, dlon, limit)
      IMPLICIT none
      REAL landflag
      REAL dlat, dlon
      INTEGER nx, ny, ratio
      REAL sst(nx, ny), mask(nx, ny)
!      REAL, ALLOCATABLE :: y(:,:), mask2(:,:)
      REAL :: y(nx/ratio,ny/ratio), mask2(nx/ratio,ny/ratio)

      REAL weight, limit
      INTEGER i,j,i2, j2
  

      weight = 1.0
! Do a low resolution grid fine convergence
      PRINT *,'calling average'
      CALL average(sst, y, mask, mask2, nx, ny, ratio, landflag)
      PRINT *,'calling sor'
      CALL sor(y, mask2, nx/ratio, ny/ratio, -90+dlat*ratio/2., &
                  dlat*ratio, dlon*ratio, landflag, weight, limit)
!feed back to sst
      DO j = 1, ny
      DO i = 1, nx
        IF (mask(i,j) .EQ. landflag) THEN
          j2 = (j-1)/ratio + 1
          i2 = (i-1)/ratio + 1
          sst(i,j) = y(i2, j2)
        ENDIF
      ENDDO
      ENDDO

      RETURN
      END
