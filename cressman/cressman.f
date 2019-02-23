      PROGRAM cressman
      IMPLICIT none
!Implement a simple version of cressman (1960) filtering/objective analysis

      INTEGER nx, ny, nk
      PARAMETER (nx = 360*2)
      PARAMETER (ny = 180*2)
      PARAMETER (nk =   5  )
      REAL first(nx,ny), grid_inc(nx, ny), grid_weight(nx, ny)
      REAL tempor(nx, ny)

      REAL dlat, dlon, firstlat, firstlon
      PARAMETER (dlat = 0.5)
      PARAMETER (dlon = 0.5)
      PARAMETER (firstlat = -89.75)
      PARAMETER (firstlon =   0.25)
      INTEGER i,j, k, nobs
      INTEGER l, m
      REAL w, d
      REAL lat1, lat2, lon1, lon2
      REAL distlimit(nk)
      REAL weight, arcdis


      INTEGER maxobs, arblimit
      PARAMETER (maxobs = 10*1000*1000)
      PARAMETER (arblimit = 20*1000)
      REAL lat(maxobs), lon(maxobs), obs(maxobs)

      REAL incr, maxincr
      PARAMETER (maxincr = 5.0)
 
!define distance weight function
!define distance limit progression
      
      DO i = 1, nk
        distlimit(i) = 40 / i
      ENDDO

! Get first guess
      OPEN (10, FILE="first", FORM="UNFORMATTED", STATUS="OLD")
      READ (10) first

! Read in observations to be used -- lat, lon, obs
      OPEN (11, FILE="obs", FORM="UNFORMATTED", STATUS="OLD")
      nobs = 1
  10  CONTINUE
        READ(11,END=20) lat(nobs), lon(nobs), obs(nobs)
        !PRINT *,lat(nobs), lon(nobs), obs(nobs)
        nobs = nobs + 1
        IF (nobs .GT. arblimit) GO TO 20
      GO TO 10
  20  CONTINUE
      nobs = nobs - 1
      PRINT *,'Found ',nobs,' observations'
      PRINT *,'lat max etc', MAXVAL(lat), MINVAL(lat)
      PRINT *,'lon max etc', MAXVAL(lon), MINVAL(lon)
      PRINT *,'obs max etc', MAXVAL(obs), MINVAL(obs)
      IF (nobs .LT. 10) STOP "failed to read reasonable data count"

! Iterate:
      tempor = first
      DO k = 1, nk
        !PRINT *,'radial search ',k
        grid_inc = 0.0
        grid_weight = 0.0
        DO l = 1, MIN(nobs, arblimit)
          !IF (MOD(l,500) .EQ. 0) PRINT *,'pass ',k,' working on ob ',l
!   for all obs, 
!     if excessive difference from first guess, reject 
!     compute weights for all grid points out to distance limits 
          CALL spiral(firstlat, firstlon, dlat, dlon, 
     1        lat(l), lon(l), distlimit(k), obs(l), tempor, maxincr,
     2        grid_inc, grid_weight, nx, ny)
        ENDDO ! done with all obs
!       add increment*weight to increment summation, add weight to weight summation
!     compute new guess = prev - sum(incr*weight)/sum(weight) for nonzero weights
        PRINT *,'inc max min ',
     1       MAXVAL(grid_inc), MINVAL(grid_inc)
        PRINT *,'weight max min ',
     2       MAXVAL(grid_weight), MINVAL(grid_weight)
        DO j = 1, ny
        DO i = 1, nx
          IF (grid_weight(i,j) .GT. 1.e-4) THEN
            tempor(i,j) = tempor(i,j) +
     1                        grid_inc(i,j) / grid_weight(i,j)
          ENDIF
        ENDDO
        ENDDO
! Iterate with successively smaller scan ranges
      ENDDO

! Output analysis field
      OPEN (12, FILE="cressout", FORM="UNFORMATTED", STATUS="UNKNOWN")
      WRITE (12) tempor
      WRITE (12) (tempor - first)

      END

      REAL FUNCTION weight(d, distlimit)
      IMPLICIT none
      REAL d, distlimit

      IF (d .LT. distlimit) THEN
        weight = (distlimit**2 - d**2) / (distlimit**2 + d**2)
      ELSE
        weight = 0
      ENDIF
      !IF (weight .LT. 0.0) THEN
      !  PRINT *,'weight, d, distlimit ',weight, d, distlimit
      !ENDIF

      RETURN
      END
      REAL FUNCTION weight2(d, distlimit)
      IMPLICIT none
      REAL d, distlimit

      IF (d .LT. distlimit) THEN
        weight2 = (distlimit**2 - d) / (distlimit**2 + d)
      ELSE
        weight2 = 0
      ENDIF
      !IF (weight2 .LT. 0.0) THEN
      !  PRINT *,'weight2, d, distlimit ',weight2, d, distlimit
      !ENDIF

      RETURN
      END
      SUBROUTINE locate(i,j,dlat,dlon, firstlat, firstlon, lon2, lat2)
      IMPLICIT none
      INTEGER i, j
      REAL dlat, dlon, firstlat, firstlon
      REAL lon2, lat2
      lat2 = firstlat + (j-1)*dlat
      lon2 = firstlon + (i-1)*dlon
      RETURN
      END
      SUBROUTINE ilocate(i,j,dlat,dlon, firstlat, firstlon, lon2, lat2)
      IMPLICIT none
      INTEGER i, j
      REAL dlat, dlon, firstlat, firstlon
      REAL lon2, lat2
      i = INT( 0.5 + (lon2 - firstlon)/dlon + 1.0)
      j = INT( 0.5 + (lat2 - firstlat)/dlat + 1.0)
      RETURN
      END


! Main, slow, loop:
! spiral outward from point nearest the given (lat1, lon1) until
!  all points are farther away than distlimit
      SUBROUTINE spiral(firstlat, firstlon, dlat, dlon, 
     1        lat1, lon1, distlimit, obs, tempor, maxincr, 
     2        grid_inc, grid_weight, nx, ny)
      IMPLICIT none
      REAL arcdis, firstlat, firstlon, dlat, dlon
      REAL lat1, lon1, lat2, lon2, distlimit
      INTEGER nx, ny
      REAL maxincr, obs
      REAL tempor(nx, ny), grid_inc(nx, ny), grid_weight(nx, ny)
      REAL weight

      INTEGER i,j, centeri, centerj, effi, effj, delta
      REAL d, w, incr
      LOGICAL nearer, inside
      INTEGER niter

      CALL ilocate(centeri, centerj, dlat, dlon, firstlat, firstlon, 
     1                   lon1, lat1)
!DB      IF (centeri .LE. 0 .OR. centeri .GT. nx .OR.
!DB     1    centerj .LE. 0 .OR. centerj .GT. ny) THEN
!DB        PRINT *,'iloc failure ',centeri, centerj
!DB        CALL fixji(centeri, nx, centerj, ny)
!DB      ENDIF

      niter = 0
      DO delta = 0, MAX(nx, ny)
        nearer = .FALSE.
        i = -delta
        DO j = -delta, delta
          effi = centeri + i !must keep here as fixji may change effi
          effj = centerj + j
          CALL fixji(effi, nx, effj, ny)
          niter = niter + 1
          CALL realwork(effi, effj, dlat, dlon, firstlat, firstlon,
     1                    distlimit, lon1, lat1, obs,
     2                    nx, ny, maxincr,
     3                    tempor, grid_inc, grid_weight, inside)
          IF (inside) nearer = .TRUE.
        ENDDO

        i = delta
        DO j = -delta, delta
          effi = centeri + i
          effj = centerj + j
          CALL fixji(effi, nx, effj, ny)
          niter = niter + 1
          CALL realwork(effi, effj, dlat, dlon, firstlat, firstlon,
     1                    distlimit, lon1, lat1, obs,
     2                    nx, ny, maxincr,
     3                    tempor, grid_inc, grid_weight, inside)
          IF (inside) nearer = .TRUE.

        ENDDO

        j = -delta
        DO i = -delta, delta
          effi = centeri + i
          effj = centerj + j
          CALL fixji(effi, nx, effj, ny)
          niter = niter + 1
          CALL realwork(effi, effj, dlat, dlon, firstlat, firstlon,
     1                    distlimit, lon1, lat1, obs,
     2                    nx, ny, maxincr,
     3                    tempor, grid_inc, grid_weight, inside)
          IF (inside) nearer = .TRUE.

        ENDDO

        j = delta
        DO i = -delta, delta
          effi = centeri + i
          effj = centerj + j
          CALL fixji(effi, nx, effj, ny)
          niter = niter + 1
          CALL realwork(effi, effj, dlat, dlon, firstlat, firstlon,
     1                    distlimit, lon1, lat1, obs,
     2                    nx, ny, maxincr,
     3                    tempor, grid_inc, grid_weight, inside)
          IF (inside) nearer = .TRUE.
        ENDDO


        IF (.NOT. nearer) exit
      ENDDO

! Look at points that require large number of iterations
! should also print distlimit
!      IF (niter .GT. nx*ny/6 .OR. ABS(lat1) .GT. 75.0) THEN
!        WRITE(*,9001) niter, delta, centeri, centerj, lat1, lon1
! 9001   FORMAT ("overload ",I9, I4, I4, I4, F9.4, F9.4)
!      ENDIF

!! Inefficient version of original:
!      DO j = 1, ny
!      DO i = 1, nx  ! note that this is enormously inefficient -- 
!                    !    checking whole globe for every grid point
!        CALL realwork(i, j, dlat, dlon, firstlat, firstlon,
!     1                    distlimit, lon1, lat1, obs,
!     2                    nx, ny, maxincr,
!     3                    tempor, grid_inc, grid_weight, inside)
!      ENDDO
!      ENDDO

      RETURN
      END

      SUBROUTINE fixi(effi, nx)
      IMPLICIT none
      INTEGER nx, effi, start
!ensure that effi lies in 1..nx, take advantage of cyclicity
      start = effi
      IF (effi .EQ. 2*nx) THEN
        effi = 1
      ELSEIF (effi .GT. nx) THEN
        !PRINT *,'fixi ',effi, effi - nx + 1
        effi = effi - nx + 1 !add 1 for ftn
      ENDIF

      IF (effi .LE.  0) THEN
        !PRINT *,'fixi ',effi, effi + nx
        effi = effi + nx
      ENDIF

      IF (effi .GT. nx .OR. effi .LE. 0) THEN
        PRINT *,"fixi failed, start final = ",start, effi
      ENDIF
      RETURN
      END
      SUBROUTINE fixji(effi, nx, effj, ny)
!ensure that effj, effi lie in 1..ny, 1..nx -- handle pole-crossing
      IMPLICIT none
      INTEGER nx, effi, ny, effj, startj, starti
      startj = effj
      starti = effi

      !fixi
      IF (effi .EQ. 2*nx) THEN
        effi = 1
      ELSEIF (effi .GT. nx) THEN
        effi = effi - nx + 1 !add 1 for ftn
      ENDIF
      IF (effi .LE.  0) THEN
        effi = effi + nx
      ENDIF
      !CALL fixi(effi, nx)

      IF (effj .EQ. 0 .OR. effj .EQ. 2*ny) THEN
        !PRINT *,'fixji0 ',effj, 1 
        effj = 1
        effi = effi + nx/2
        CALL fixi(effi, nx)
      ENDIF

      IF (effj .GT. ny) THEN
        !PRINT *,'fixji ',effj, ny - (effj - ny)
        effj = ny - (effj - ny)
        effi = effi + nx/2
        CALL fixi(effi, nx)
      ENDIF
      IF (effj .LT. 0) THEN
        !PRINT *,'fixji ',effj, -effj 
        effj = -effj 
        effi = effi + nx/2
        CALL fixi(effi, nx)
        IF (effj .GT. ny) THEN
          !PRINT *,'fixji ',effj, ny - (effj - ny)
          effj = ny - (effj - ny)
          effi = effi + nx/2
          CALL fixi(effi, nx)
        ENDIF
      ENDIF
      IF (effj .GT. ny .OR. effj .LE. 0) THEN
        PRINT *,"fixji failed, start finalj = ",startj, effj
      ENDIF
      IF (effi .GT. nx .OR. effi .LE. 0) THEN
        PRINT *,"fixji failed, start finali = ",starti, effi
      ENDIF
      RETURN
      END

      SUBROUTINE realwork(i, j, dlat, dlon, firstlat, firstlon, 
     1                    distlimit, lon1, lat1, obs, 
     2                    nx, ny, maxincr, 
     3                    tempor, grid_inc, grid_weight, inside)
      IMPLICIT none
      INTEGER i, j, nx, ny
      REAL dlat, dlon, firstlat, firstlon, lon2, lat2

      REAL lon1, lat1, distlimit, obs, arcdis
      REAL d, w, weight, weight2, incr, maxincr
      REAL tempor(nx, ny), grid_inc(nx, ny), grid_weight(nx, ny)
      LOGICAL inside

      !replace call with inline for speed
      !CALL locate(i,j,dlat,dlon, firstlat, firstlon, lon2, lat2)
      lat2 = firstlat + (j-1)*dlat
      lon2 = firstlon + (i-1)*dlon

      !d = arcdis(lon1, lat1, lon2, lat2)
      d = (lat1-lat2)**2 + (lon1-lon2)**2
      IF (d .LT. distlimit ) THEN
        incr = obs - tempor(i,j)
        IF (ABS(incr) .LT. maxincr) THEN
          !w =  weight(d, distlimit )
          w =  weight2(d, distlimit )
          grid_inc(i,j) = grid_inc(i,j) + incr*w
          grid_weight(i,j) = grid_weight(i,j) + w
        ENDIF
        inside = .TRUE.
      ELSE
        inside = .FALSE.
      ENDIF


      RETURN
      END
