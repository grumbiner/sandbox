      PROGRAM temps
C     Construct some analyses of the temperature records from Jones
      INTEGER nm, ny, unit
      PARAMETER (nm = 12)
      PARAMETER (ny = 1984-1851)
      PARAMETER (unit = 10)
      REAL series(ny, nm), avg(ny), tseries(nm, ny)

      REAL tempor1(ny*nm), tempor2(ny*nm)
      REAL tempor3(ny*nm), tempor4(ny*nm)

      INTEGER i, j

      OPEN (10, FILE="NDP003R1.NH", FORM="FORMATTED", STATUS="OLD")
      OPEN (11, FILE="tempout", FORM="FORMATTED", STATUS="NEW")
      CALL readin(series, ny, nm, avg, unit)

C     Now we have some data to process.
C     First pass: dT/dt vs Tbar
CD      DO 1000 i = 1, 12
CD        CALL deriv(i, series(1,i), nm, ny, tempor1, tempor2,
CD     1                 tempor3, tempor4)
CD 1000 CONTINUE
      CALL deriv(i, avg, nm, ny, tempor1, tempor2,
     1                 tempor3, tempor4)
      
      DO 2000 j = 1, ny
        DO 2100 i = 1, nm
          tseries(i,j) = series(j,i)
 2100   CONTINUE
 2000 CONTINUE
      CALL mderiv(tseries, nm, ny, tempor1, tempor2, tempor3, tempor4)

      STOP
      END
C============================================================
      SUBROUTINE mderiv(series, nm, ny, tempor1, tempor2,
     1                 tempor3, tempor4)
      INTEGER nm, ny
      REAL series(ny*nm), tempor1(ny*nm), tempor2(ny*nm), 
     1 tempor3(ny*nm), tempor4(ny*nm)
      INTEGER i, j
      INTEGER k, kp1, kp2, km1, km2

      DO 1000 k = 3, ny*nm-2
        kp1 = k+1
        km1 = k-1
        kp2 = k+2
        km2 = k-2

        tempor1(k) = (series(kp1) - series(km1) ) / 2.
        tempor2(k) = (series(kp1) - 2.*series(k) + series(km1) )
        tempor3(k) = (series(kp2) - 3.*series(kp1) + 3.*series(km1) - 
     1                series(km2) )
        tempor4(k) = (series(kp2) - 4.*series(kp1) + 6.*series(k) 
     1               -4.*series(km1) + series(km2) )
        WRITE (11,9001) series(k), char(9), tempor1(k), char(9), 
     1           tempor2(k), char(9), tempor3(k), char(9), tempor4(k)
 1100 CONTINUE
      IF (MOD(j,10) .EQ. 0) WRITE (11,9002)
 1000 CONTINUE

      WRITE (11,9002)
 9001 FORMAT ( 4(F7.3, A1), F7.3)
 9002 FORMAT (" ")


      RETURN
      END
C============================================================

      SUBROUTINE deriv(i, series, nm, ny, tempor1, tempor2,
     1                 tempor3, tempor4)
      INTEGER nm, ny
      REAL series(ny), tempor1(ny), tempor2(ny), tempor3(ny),
     1                 tempor4(ny)
      INTEGER i, j

      DO 1000 j = 3, ny-2
        tempor1(j) = (series(j+1) - series(j-1) ) / 2.
        tempor2(j) = (series(j+1) - 2.*series(j) + series(j-1) )
        tempor3(j) = (series(j+2) - 3.*series(j+1) + 3.*series(j-1) - 
     1                series(j-2) )
        tempor4(j) = (series(j+2) - 4.*series(j+1) + 6.*series(j) 
     1               -4.*series(j-1) + series(j-2) )
        WRITE (11,9001) series(j), char(9), tempor1(j), char(9), 
     1           tempor2(j), char(9), tempor3(j), char(9), tempor4(j)
 1000 CONTINUE

      WRITE (11,9002)
 9001 FORMAT ( 4(F7.3, A1), F7.3)
 9002 FORMAT (" ")

      RETURN
      END

      SUBROUTINE readin(series, ny, nm, avg, unit)
C     Read in Jones et al temperature series
      INTEGER nm, ny, unit
      REAL series(ny, nm), avg(ny)
      
      INTEGER i, j

      DO 1000 j = 1, ny
        READ (unit, 9001) (series(j,i),i=1,12),avg(j)
        WRITE(*,9001) avg(j)
 1000 CONTINUE
 9001 FORMAT (5x,13F5.2)

      RETURN
      END
