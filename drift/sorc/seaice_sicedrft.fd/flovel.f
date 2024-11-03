      SUBROUTINE flovel(ui, vi, nlong, nlat, 
     1                  uf, vf, eta, lambda, ig, jg, npts)
!     Given the atmospheric velocity (uf), compute the air velocity
!       at the virtual floe points.
!     Robert Grumbine 4 April 1994.
!     Move to un-numbered do, 1 May 2014

      IMPLICIT none

!     Declare the arguments
      INTEGER npts, nlat, nlong
      REAL ui(nlong, nlat), vi(nlong, nlat)
      REAL uf(npts), vf(npts), eta(npts), lambda(npts)
      INTEGER ig(npts), jg(npts)

!     Declare local utility variables
      INTEGER k
      REAL epsilon
      PARAMETER (epsilon = 1.e-7)

!     Bullet-proofing variables
      REAL uimx, uimn, vimx, vimn

!     Bullet-proofing code -- verify that the coordinates
!       of the floe lie in the model domain, and that the 
!       interpolation weights are in [0,1)
!C    Should fix the algorithms to deal with floes on boundary
!C      cells.  Also fndflo and movice.
      DO k = 1, npts
        IF (ig(k) .LT. 1) THEN
          ig(k) = nlong - ig(k)
!          PRINT *,'i coordinate reset to ',ig(k),' point # ',k
          IF (ig(k) .LT. 1) THEN
            PRINT *,'ig too far west in flovel, stopping ',ig(k),k
            STOP
          ENDIF
        ENDIF
        IF (ig(k) .GT. nlong-1) THEN
!          IF (ig(k) .GT. nlong-0.5) 
!     1          PRINT *,'i coordinate reset to nlong-1, point # ',k
          ig(k) = nlong-1
        ENDIF
        IF (jg(k) .LT. 1) THEN
!          PRINT *,'j coordinate reset to 1, point # ',k
          jg(k) = 1
        ENDIF
        IF (jg(k) .GT. nlat-1) THEN
!          PRINT *,'j coordinate reset to nlat-1, point # ',k
          jg(k) = nlat-1
        ENDIF
        IF (eta(k) .LT. 0.) THEN
!          PRINT *,'eta reset to 0.0, point # ',k
          eta(k) = 0.0
        ENDIF
        IF (eta(k) .GE. 1.) THEN
!          PRINT *,'eta reset to 0.9999, point # ',k
          eta(k) = 0.9999
        ENDIF
        IF (lambda(k) .LT. 0.) THEN
!          PRINT *,'lambda reset to 0.0, point # ',k
          lambda(k) = 0.0
        ENDIF
        IF (lambda(k) .GE. 1.) THEN
!          PRINT *,'lambda reset to 0.9999, point # ',k
          lambda(k) = 0.9999
        ENDIF
      ENDDO
    
!     Operational code - compute the interpolated velocities. 
!     Compute the velocity of the floes by bilinear interpolation.
      DO k = 1, npts
        uf(k) = (1.-lambda(k))*( (1.-eta(k))*ui(ig(k)  ,jg(k)) +
     1                               eta(k) *ui(ig(k)+1,jg(k)) )
     2             +lambda(k) *( (1.-eta(k))*ui(ig(k)  ,jg(k)+1) +
     3                               eta(k) *ui(ig(k)+1,jg(k)+1) )
        vf(k) = (1.-lambda(k))*( (1.-eta(k))*vi(ig(k)  ,jg(k)) +
     1                               eta(k) *vi(ig(k)+1,jg(k)) )
     2             +lambda(k) *( (1.-eta(k))*vi(ig(k)  ,jg(k)+1) +
     3                               eta(k) *vi(ig(k)+1,jg(k)+1) )
      ENDDO

!     Bullet-proofing -- verify that the interpolated velocity
!       lies in the bounds of the four corner points.
      DO k = 1, npts
        uimx = AMAX1(ui(ig(k)  , jg(k)  ), 
     1               ui(ig(k)+1, jg(k)  ),
     2               ui(ig(k)  , jg(k)+1),
     3               ui(ig(k)+1, jg(k)+1)  ) 
        uimn = AMIN1(ui(ig(k)  , jg(k)  ), 
     1               ui(ig(k)+1, jg(k)  ),
     2               ui(ig(k)  , jg(k)+1),
     3               ui(ig(k)+1, jg(k)+1)  ) 
        vimx = AMAX1(vi(ig(k)  , jg(k)  ), 
     1               vi(ig(k)+1, jg(k)  ),
     2               vi(ig(k)  , jg(k)+1),
     3               vi(ig(k)+1, jg(k)+1)  ) 
        vimn = AMIN1(vi(ig(k)  , jg(k)  ), 
     1               vi(ig(k)+1, jg(k)  ),
     2               vi(ig(k)  , jg(k)+1),
     3               vi(ig(k)+1, jg(k)+1)  ) 
        IF (abs(uf(k)-uimx) .GT. epsilon) THEN
!          PRINT *,'Interp failure-hi, ui ',uf(k),' reset to ',uimx, 
!     1               'delta = ',uf(k)-uimx,' at ',k
          uf(k) = uimx
        ENDIF
        IF (abs(uf(k)-uimn) .GT. epsilon) THEN
!          PRINT *,'Interpolation failure-lo, ui reset to ',uimn,' at ',k
          uf(k) = uimn
        ENDIF
        IF (abs(vf(k)-vimx) .GT. epsilon) THEN
!          PRINT *,'Interpolation failure-hi, vi reset to ',vimx,' at ',k
          vf(k) = vimx
        ENDIF
        IF (abs(vf(k)-vimn) .GT. epsilon) THEN
!          PRINT *,'Interpolation failure-lo, vi reset to ',vimn,' at ',k
          vf(k) = vimn
        ENDIF
      ENDDO


      RETURN
      END
