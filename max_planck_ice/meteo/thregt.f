      SUBROUTINE thregt(pole, siglev, uwin, vwin, tair, tsfc, rh,
     1  ps, precip, acl, lwup, lwic, swic, mask, step)
C=====================================================================--
C  Programmed by:
C     Robert W. Grumbine         NMC  Camp Springs, MD          Dec. '92
C     Robert W. Grumbine         NMC  Camp Springs, MD          Oct. '94
C  Purpose:
C     Obtain meteorological forcing data from the MRF forecasts in the
C       case that the ice model grid is polar stereographic (PTYPE=3).
C       Argument siglev selects the sigma level to attempt to 
C       get the data from.  
C     Added the surface type mask and surface temperature 26 October 1994.
C  EXTERNALS:
C     getsig   Read in the MRF sigma files ! removed
C     getflx   Read in the MRF flux files
C     SPHERT   Convert sigma file data to regular spherical grid
C     terp     Convert regular spherical grid to polar stereographic
C     W3FA09   Compute the saturation vapor pressure
C=====================================================================--
      IMPLICIT none
      INCLUDE "icegrid.inc"
      INCLUDE "mgrid.inc"
      INCLUDE "physical.inc"
C=====================================================================--

      INTEGER step
      REAL siglev

C     Interpolation parameters
      INTEGER tlat, tlong
      PARAMETER (tlong = 360./dlonm   )
      PARAMETER (tlat  = 180./dlatm +1)

      REAL field(tlong+1, tlat/2+1)
      INTEGER pole

C     Flux file data 
      REAL slpf(tlong, tlat)
      REAL lwup2(tlong, tlat), lwdn(tlong, tlat), swdn(tlong, tlat)
      REAL uten(tlong, tlat), vten(tlong, tlat)
      REAL t2(tlong, tlat)  , q2(tlong, tlat), ftsfc(tlong, tlat)
      REAL fmask(tlong, tlat), cloud(tlong, tlat), fprec(tlong, tlat)

C     Arguments to return
      REAL uwin(L,M), vwin(L,M), rh(0:L, 0:M)
      REAL ps(0:L, 0:M), acl(0:L, 0:M), precip(0:L, 0:M)
      REAL tair(0:L, 0:M), tsfc(0:L, 0:M), mask(0:L, 0:M)
      REAL lwup(0:L, 0:M), lwic(0:L, 0:M), swic(0:L, 0:M)

C=====================================================================--
C     Local variables. 
      INTEGER i, j, iunit
      DOUBLE PRECISION W3FA09
      REAL tempa(0:L, 0:M), tempb(0:L, 0:M)

C=====================================================================--
C     Begin the routine 
C     Read in data here

      iunit = 50+step
CD      PRINT *,'Calling getflx '
      CALL getflx(slpf, swdn, lwup2, lwdn, t2, ftsfc, q2, uten, vten,
     1            fprec, cloud, fmask, iunit)
      CLOSE (iunit)
CD      PRINT *,'Returned from getflx '
C     Zero out the temporary data file used for hemispheric values
      DO 100 j = 1, tlat
        DO 110 i = 1, tlong
          field(i,j/2+1) = 0.0
  110   CONTINUE
  100 CONTINUE

C     Interpolate the data fields
      CALL terp(slpf, ps, pole, .FALSE.)
      CALL terp(t2, tair, pole, .FALSE.)
      CALL terp(ftsfc, tsfc, pole, .FALSE.)

      CALL terp(swdn, swic, pole, .FALSE.)
      CALL terp(lwdn, lwic, pole, .FALSE.)
      CALL terp(lwup2, lwup, pole, .FALSE.)

      CALL terp(cloud, acl, pole, .FALSE.)
      CALL terp(fmask, mask, pole, .FALSE.)
      CALL terp(fprec, precip, pole, .FALSE.)
      
      CALL terp(q2, rh, pole, .FALSE.)
      DO 1200 j = 0, M
        DO 1300 i = 0, L
CD          rh(i,j) = rh(i,j)/1.E4           ! q2 in 0.1 g/kg
          rh(i,j) = rh(i,j)/(rh(i,j)+EPSI)          ! q2 in  kg/kg by 1/9/95
     1                *ps(i,j)                   ! ps in pa
     2               /(W3FA09(tair(i,j))*1000.)  ! qsat in kpa
     3     *100.
          IF (rh(i,j) .GT. 100.) THEN
            IF (rh(i,j) .GT. 200.) PRINT *,' rh > 200  ',i, j,
     1                rh(i,j), ps(i,j), tair(i,j)
            rh(i,j) = 100.
          ENDIF
          IF (rh(i,j) .LT. 0.) rh(i,j) = 0.
C         Change units for MPI model.
C         Stossel model expects t in degrees C, rather than K as stored.
          tair(i,j) = tair(i,j) - TMELT
          tsfc(i,j) = tsfc(i,j) - TMELT
C         Stossel model expects ps in Pa, rather than mb as stored.
CD          ps(i,j) = ps(i,j)*100.
C         Pa used by 950109.
 1300   CONTINUE
 1200 CONTINUE

      CALL terp(uten, tempa, pole, .TRUE.)
      CALL terp(vten, tempb, pole, .TRUE.)
      DO 2000 j = 1, M
        DO 2100 i = 1, L
          uwin(i,j) = tempa(i,j)
          vwin(i,j) = tempb(i,j)
 2100   CONTINUE
 2000 CONTINUE

      RETURN
      END
