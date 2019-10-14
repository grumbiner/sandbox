      SUBROUTINE flovel(ui, vi, nlong, nlat, 
     1                  uf, vf, eta, lambda, ig, jg, npts)

      IMPLICIT none

C     Compute the velocity of the ice floes
C     3 September 1993

C     Declare the arguments
      INTEGER npts, nlat, nlong
      REAL ui(nlong, nlat), vi(nlong, nlat)
      REAL uf(npts), vf(npts), eta(npts), lambda(npts)
      INTEGER ig(npts), jg(npts)

C     Declare local utility variables
      INTEGER k

C     Bullet-proofing variables
      REAL uimx, uimn, vimx, vimn

C     Bullet-proofing code -- verify that the coordinates
C       of the floe lie in the model domain, and that the 
C       interpolation weights are in [0,1)
CC    Should fix the algorithms to deal with floes on boundary
CC      cells.  Also fndflo and movice.
      DO 1000 k = 1, npts
        IF (ig(k) .LT. 1) THEN
          ig(k) = nlong - ig(k)
          PRINT *,'i coordinate reset to ',ig(k),' point # ',k
          IF (ig(k) .LT. 1) THEN
            PRINT *,'ig too far west in flovel, stopping ',ig(k),k
            STOP
          ENDIF
        ENDIF
        IF (ig(k) .GT. nlong-1) THEN
          PRINT *,'i coordinate reset to nlong-1, point # ',k
          ig(k) = nlong-1
        ENDIF
        IF (jg(k) .LT. 1) THEN
          PRINT *,'j coordinate reset to 1, point # ',k
          jg(k) = 1
        ENDIF
        IF (jg(k) .GT. nlat-1) THEN
          PRINT *,'j coordinate reset to nlat-1, point # ',k
          jg(k) = nlat-1
        ENDIF
        IF (eta(k) .LT. 0.) THEN
          PRINT *,'eta reset to 0.0, point # ',k
          eta(k) = 0.0
        ENDIF
        IF (eta(k) .GE. 1.) THEN
          PRINT *,'eta reset to 0.9999, point # ',k
          eta(k) = 0.9999
        ENDIF
        IF (lambda(k) .LT. 0.) THEN
          PRINT *,'lambda reset to 0.0, point # ',k
          lambda(k) = 0.0
        ENDIF
        IF (lambda(k) .GE. 1.) THEN
          PRINT *,'lambda reset to 0.9999, point # ',k
          eta(k) = 0.9999
        ENDIF
 1000 CONTINUE
    
C     Operational code - compute the interpolated velocities. 
C     Compute the velocity of the floes by bilinear interpolation.
      DO 4000 k = 1, npts
        uf(k) = (1.-lambda(k))*( (1.-eta(k))*ui(ig(k)  ,jg(k)) +
     1                               eta(k) *ui(ig(k)+1,jg(k)) )
     2             +lambda(k) *( (1.-eta(k))*ui(ig(k)  ,jg(k)+1) +
     3                               eta(k) *ui(ig(k)+1,jg(k)+1) )
        vf(k) = (1.-lambda(k))*( (1.-eta(k))*vi(ig(k)  ,jg(k)) +
     1                               eta(k) *vi(ig(k)+1,jg(k)) )
     2             +lambda(k) *( (1.-eta(k))*vi(ig(k)  ,jg(k)+1) +
     3                               eta(k) *vi(ig(k)+1,jg(k)+1) )
 4000 CONTINUE

C     Bullet-proofing -- verify that the interpolated velocity
C       lies in the bounds of the four corner points.
      DO 5000 k = 1, npts
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
        IF (uf(k) .GT. uimx) THEN
          PRINT *,'Interpolation failure-hi, ui reset to ',uimx
          PRINT *,' point = ',k
          uf(k) = uimx
        ENDIF
        IF (uf(k) .LT. uimn) THEN
          PRINT *,'Interpolation failure-lo, ui reset to ',uimn
          PRINT *,' point = ',k
          uf(k) = uimn
        ENDIF
        IF (vf(k) .GT. vimx) THEN
          PRINT *,'Interpolation failure-hi, vi reset to ',vimx
          PRINT *,' point = ',k
          vf(k) = vimx
        ENDIF
        IF (vf(k) .LT. vimn) THEN
          PRINT *,'Interpolation failure-lo, vi reset to ',vimn
          PRINT *,' point = ',k
          vf(k) = vimn
        ENDIF
 5000 CONTINUE


      RETURN
      END
