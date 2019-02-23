      PROGRAM cressman
      IMPLICIT none
!Implement a simple version of cressman (1960) filtering/objective analysis

      INTEGER nx, ny, nk
      PARAMETER (nx = 360*2)
      PARAMETER (ny = 180*2)
      PARAMETER (nk =   7  )
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


      INTEGER maxobs
      PARAMETER (maxobs = 10*1000*1000)
      REAL lat(maxobs), lon(maxobs), obs(maxobs)

      REAL incr, maxincr
      PARAMETER (maxincr = 5.0)
 
!define distance weight function
!define distance limit progression
      
      DO i = 1, nk
        distlimit(i) = 400 / i
      ENDDO

! Get first guess
      OPEN (10, FORM="UNFORMATTED", STATUS="OLD")
      READ (10) first

! Read in observations to be used -- lat, lon, obs
      OPEN (11, FORM="UNFORMATTED", STATUS="OLD")
      nobs = 1
  10  CONTINUE
        READ(11,END=20) lat(nobs), lon(nobs), obs(nobs)
        nobs = nobs + 1
        IF (nobs .GT. 500) GO TO 20
      GO TO 10
  20  CONTINUE
      nobs = nobs - 1
      PRINT *,'Found ',nobs,' observations'
      PRINT *,'lat max etc', MAXVAL(lat), MINVAL(lat)
      PRINT *,'lon max etc', MAXVAL(lon), MINVAL(lon)
      PRINT *,'obs max etc', MAXVAL(obs), MINVAL(obs)

! Iterate:
      tempor = first
      DO k = 1, nk
        PRINT *,'radial search ',k
        grid_inc = 0.0
        grid_weight = 0.0
        DO l = 1, MIN(nobs, 500)
          IF (MOD(l,100) .EQ. 0) PRINT *,'working on ob ',l
!   for all obs, 
!     if excessive difference from first guess, reject 
!     compute weights for all grid points out to distance limits 
          DO j = 1, ny
          DO i = 1, nx  ! note that this is enormously inefficient -- checking whole globe
            CALL locate(i,j,dlat,dlon, firstlat, firstlon, lon2, lat2)
            !d = arcdis(lat(l), lon(l), lon2, lat2)
            d = arcdis(lon(l), lat(l), lon2, lat2)
            IF (d .LT. distlimit(k) ) THEN
              w =  weight(d, distlimit(k) )
              incr = obs(l) - tempor(i,j)
              IF (ABS(incr) .LT. maxincr) THEN
                grid_inc(i,j) = grid_inc(i,j) + incr*w
                grid_weight(i,j) = grid_weight(i,j) + w
              ENDIF
            ENDIF
          ENDDO
          ENDDO
        ENDDO ! done with all obs
!       add increment*weight to increment summation, add weight to weight summation
!     compute new guess = prev - sum(incr*weight)/sum(weight) for nonzero weights
        DO j = 1, ny
        DO i = 1, nx
          IF (grid_weight(i,j) .GT. 0) THEN
            tempor(i,j) = tempor(i,j) - 
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
      lat2 = firstlat + (j-1)*dlat
      lon2 = firstlon + (i-1)*dlon
      RETURN
      END
