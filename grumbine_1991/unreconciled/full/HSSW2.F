      PROGRAM cshelf
C     Model bouyancy (and perhaps wind) forced continental shelf
C       motions and conditions for the polar regions.

      IMPLICIT none
      INCLUDE "grid.inc"

C     Data arrays
      REAL uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny), qss(nx, ny), qsd(nx, ny)
      REAL we(nx, ny), h(nx, ny)

C     Physical parameters
      REAL ahm, avm, ahs, avs
      REAL sdref, ssref, rhoref
      REAL g, f, beta
      INTEGER xmin, xmax, ymin, ymax, strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm

C     Numerical parameters
      REAL delx, dely, delt
      INTEGER nout, nflout, ntot, loy, tav
      REAL scrit

C     Local variables
      INTEGER i
      REAL href, tots

C***********************************************************----------!!
C     BEGIN EXECUTION

C     Initialize the variables
      CALL init (uc, vc, ut, vt, ss, sd, we, h, nx, ny,
     2           ahm, avm, ahs, avs,
     3           sdref, ssref, rhoref, g, f,
     4           delx, dely, delt,
     5           nout, nflout, ntot, scrit, tav,
     6           xmin, xmax, ymin, ymax, qsfmax, qsfref, qsm,
     7           strspr, strsum, strfll, strwin, loy,
     8           beta                                         )

      i = 0
      href  = h(1,1)

C     Open the output files
      CALL outstr(uc, vc, ut, vt, ss, sd, h, delx)

C     Compute the Barotropic flow solution.
      CALL uvtrop(ut, vt, we, href, delx, dely, f, beta, ahm)
C***********************************************************----------!!
CM      OPEN (1, FILE='timeinfo', FORM='FORMATTED', STATUS='NEW')
CM      PRINT *,LONG(362)
CM      WRITE(1,9001) 0, LONG(362)
 9001 FORMAT (2I12)

C     Extrapolation loop
      DO 1000 i = 0, ntot
CM        WRITE(*,9001) i, LONG(362)
CM        WRITE(1,9001) i, LONG(362)
        CALL uvext (uc, vc, ss, href,
     2              rhoref, g, f, beta, ahm, delx, dely    )

        CALL qsext (ss, qss, qsd, we, h, nx, ny, i, loy,
     1              xmin, xmax, ymin, ymax,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin, delt        )

        CALL stext (uc, vc, ut, vt, we, ss, sd, qss, qsd, h,
     1              delx, dely, delt, sdref, ssref,
     2              ahs, avs, i                                 )

        CALL convec(sd, nx, ny, i)

      IF (MOD(I,240) .EQ. 0) THEN
        CALL summer(ss, nx, ny, tots)
        PRINT *,'total salt ',i, tots/nx/ny
      ENDIF

        CALL timav(uc, vc, ut, vt, ss, sd, h, scrit, tav, i,
     1                     delx, dely                            )

        IF (MOD(i,nout) .EQ. 0) THEN
           CALL outdat (uc, vc, ut, vt, ss, sd, h, delx          )
        ENDIF
        IF (MOD(i,nflout) .EQ. 0)
     1      CALL outfl (uc, vc, ut, vt, ss, sd, h, delx          )
 1000 CONTINUE

      CALL outend (uc, vc, ut, vt, ss, sd, h, delx               )

 9002 FORMAT (I5,A1,D20.14,A1,D20.14)

      STOP
      END
      FUNCTION yes(defalt)
C     Function to return .TRUE. if the user responds y, .FALSE. if he
C       says n, and the default value otherwise.

      LOGICAL yes, defalt
      CHARACTER resp

      READ (11,9001) resp
 9001 FORMAT(A1)

      yes = (resp.EQ.'y') .OR. (defalt .AND. resp.NE.'y'
     1                                 .AND. resp.NE.'n')

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE read2(x, nnx, nny, unit, fname, ope, clos)
C     Read in a 2d, unformatted array, with external control on
C       opening and closing.
      INCLUDE "grid.inc"

      INTEGER nnx, nny, unit
      REAL x(nx, ny)
      CHARACTER*60 fname
      LOGICAL ope, clos

      IF (ope) OPEN(unit, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      READ (unit) x
      IF (clos) CLOSE(unit, STATUS='KEEP')

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE arset (x, nnx, nny, value)
C     Set all elements of array x equal to value.
      INCLUDE "grid.inc"

      INTEGER nnx, nny
      REAL x(nx, ny), value

      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          x(i,j) = value
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE convec(sd, nnx, nny, tstep)
C     Subroutine to look for convective overturning and cabbeling.
C     Overturn added 3-9-88.
C     Uses short form entry point to rho 5-26-88.
C     Drop the passing of SS  7-16-90.
C     Note that this method is at least as fast as using sd = min(sd, 0)
C       - on the macintosh.  7-16-90.  don't know about other machines.
      IMPLICIT none
      INCLUDE "grid.inc"

      INTEGER nnx, nny, tstep
      REAL sd(nx, ny)

      INTEGER i, j
      LOGICAL ovrtrn, ovrlst
      SAVE    ovrlst

      ovrtrn = .FALSE.
      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          IF (sd(i,j) .GT. 0.0) THEN
            sd(i,j) = 0.0
            ovrtrn = .TRUE.
          ENDIF
 1010   CONTINUE
 1000 CONTINUE

      IF (tstep .EQ. 1) GO TO 9999

CD      IF (ovrtrn .AND. (.NOT. ovrlst) ) THEN
CD        PRINT *,'Started convection at tstep',tstep
CD       ELSE IF ((.NOT. ovrtrn)  .AND. ovrlst ) THEN
CD        PRINT *,'Stopped convection at tstep',tstep
CD      ENDIF

 9999 CONTINUE
      ovrlst = ovrtrn

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE reflux(vt, vc, ss, sd, h, flm, fls,
     1                          scrit, nnx, nny, dx)
C     Recompute fluxes using a bottom water definition
      IMPLICIT none
      INCLUDE "grid.inc"

      INTEGER nnx, nny
      REAL vt(nx, ny), vc(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny), flm(ny), fls(ny)
      REAL dx, scrit
      INTEGER i, j
      REAL iconst

      iconst = dx*0.5*h(1,1)
      DO 1000 j = 1, ny
        flm(j) = 0.0
        fls(j) = 0.0
        DO 1010 i = 1, nx
C         Lower layer
          IF (vt(i,j)-vc(i,j) .GT. 0.0) THEN
            IF (ss(i,j)-sd(i,j) .GT. scrit) THEN
              flm(j) = flm(j) + (vt(i,j) - vc(i,j))
              fls(j) = fls(j) +
     1                    (vt(i,j) - vc(i,j))*(ss(i,j)-sd(i,j))
            ENDIF
          ENDIF
C         upper layer
          IF (vt(i,j)+vc(i,j) .GT. 0.0) THEN
            IF (ss(i,j)+sd(i,j) .GT. scrit) THEN
              flm(j) = flm(j) + (vt(i,j) + vc(i,j))
              fls(j) = fls(j) +
     1                    (vt(i,j) + vc(i,j))*(ss(i,j)+sd(i,j))
            ENDIF
          ENDIF
 1010   CONTINUE
        flm(j) = flm(j)*iconst
        fls(j) = fls(j)*iconst
 1000 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      FUNCTION rho(s, t, p)
C     Compute the density of water as a function of salinity,
C       temperature, and pressure.
C     Use the entire equation of state Given in Gill, 1982.
C     s in psu, t in degrees C, p in bars

      REAL rho
      REAL s, t, p

      DOUBLE PRECISION sigmaw, sigma
      DOUBLE PRECISION deltaw, delta, deltap

      sigmaw = 999.842594D0 + t*(6.793952D-2 + t*(-9.09529D-3
     1      + t*(1.001685D-4 +t*(-1.120083D-6 + 6.536332D-9*t))))

      sigma  = sigmaw +
     1    s* ( .824493 + t*(-4.0899D-3 + t*(7.6438D-5
     2                      + t*(-8.2467D-7 + t*5.3875D-9))) ) +
     3    s**1.5*( -5.72466D-3 + t*(1.0227D-4 - t*1.6546D-6)) +
     4    s*s   *( 4.8314D-4 )

C     Now compute the compressibility terms:
      deltaw = 19652.21D0 + t*(148.4206 + t*(-2.327105 +
     1                     t*(-1.360477D-2 - t*5.155288D-5 )))

      delta  = deltaw +
     1    s  * (54.676 + t*(-.603459 + t*(1.09987D-2 - t*6.167D-5)) )
     2  + s**1.5*( 7.944D-2 + t*(1.6483D-2 - t*5.3009D-4) )

      deltap = delta +
     1   p *  (3.239908+t*(1.43713D-3+t*(1.16092D-4-t*5.77905D-7)) )
     2  +p*s* (2.2838D-3 + t*(-1.0981D-5*t - t*1.6078D-6) )
     3  +p*s**1.5*( 1.91075D-4 )
     4  +p*p* (8.50935D-5 + t*(-6.12293D-6 + t*5.2787D-8) )
     5  +p*p*s*(-9.9348D-7 +t*(2.0816D-8 + t*9.1697D-10) )

C     Now compute the density:
      rho = sigma/ (1 - p/deltap )

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE uvtrop(ut, vt, we, h, dx, dy, f, beta, am)
C     Barotropic solution.
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL ut(nx, ny), vt(nx, ny), we(nx, ny)
      REAL dx, dy, f, beta, am, h

      REAL psi(nx, ny)
      REAL num, lb, wave
      INTEGER i, j

C     Compute the interior solution
      DO 1001 i = 1, nx
        psi(i,1) = 0.0
 1001 CONTINUE
      DO 1000 j = 1, ny
        psi(nx, j)  = 0.0
        psi(nx-1,j) = 0.0
        DO 1010 i = nx-2, 1, -1
          psi(i, j) = psi(i+1,j) + we(i+1,j) + we(i,j)
 1010   CONTINUE
 1000 CONTINUE
      num = f*dx/2./beta/h
      DO 1020 j = 1, ny
        DO 1030 i= 1, nx
          psi(i,j) = -psi(i,j) * num
 1030   CONTINUE
 1020 CONTINUE

C     Apply the boundary layer corrections.
      lb = (am/beta)**(1./3.)
      wave = SQRT(3.)*dx/2./lb
      DO 2000 j = 1, ny
        num = lb*(psi(nx,j)-psi(nx-1,j))/dx
        DO 2010 i = 1, nx
          psi(i,j) = psi(i,j)*(1.-EXP(-(i-1)*dx/2./lb)*
     1                 (COS(wave*(i-1)) + SIN(wave*(i-1))/SQRT(3.) ))
     2              - num*EXP( (i-nx)*dx/lb)
 2010   CONTINUE
 2000 CONTINUE

C     Now that we have psi, compute ut, vt.
      CALL stmfc4(psi, ut, vt, dx, dy, nx, ny)

      RETURN
      END
      SUBROUTINE summer(g, nnx, nny, value)
C     Perform a 2-d integration
      INCLUDE "grid.inc"
      REAL nnx, nny
      REAL g(nx, ny)
      DOUBLE PRECISION value
      REAL tempor(ny)

      INTEGER i, j

      value = 0.

      DO 1000 j = 2, ny-1
        tempor(j) = 0.0
        DO 1100 i = 2, nx-1
          tempor(j) = tempor(j)+g(i,j)
 1100   CONTINUE
        value = value+tempor(j)
 1000 CONTINUE

      RETURN
      END
C*************************************************----------++++++++++!!
      SUBROUTINE stmfc4(psi, ut, vt, dx, dy, nnx, nny)
C     Now, given a psi function, compute the velocities.
C     Now should have a complete soln for psi.
      IMPLICIT none
      INCLUDE "grid.inc"

      INTEGER nnx, nny
      REAL psi(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL dx, dy

      INTEGER i, j
      DO 1000 j = 1, ny
        i = 1
C       Use a forward fourth order difference.
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx
        i = 2
        vt(i,j) = (-11.*psi(i  ,j)+18.*psi(i+1,j)
     1              -9.*psi(i+2,j)+2. *psi(i+3,j))/6./dx
        DO 1010 i = 3, nx-2
C     Implement a 4th order accurate first derivative in x
C       in order to get a decent treatment of v near the boundaries.
          vt(i,j) = (-psi(i+2,j)+8.*psi(i+1,j)
     1                          -8.*psi(i-1,j)+psi(i-2,j))/12./dx
 1010   CONTINUE
C       Use traditional forms for the eastern boundary, where
C         there is less structure.
        i = nx-1
        vt(i,j) = (psi(i+1,j)-psi(i-1,j))/2./dx
        i = nx
        vt(i,j) = (psi(i,j)-psi(i-1,j))/dx
 1000 CONTINUE

C     Use ordinary centered differences for u velocity.
      DO 4000 j = 2, ny-1
        DO 4010 i = 1, nx
          ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
 4010   CONTINUE
 4000 CONTINUE
      DO 4030 i = 1, nx
        j = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j))/dy
        j = ny
        ut(i,j) = -(psi(i,j)-psi(i,j-1))/dy
 4030 CONTINUE

C     Give values to the corner points
      ut(1,1)    = 0.0
      ut(1,ny)   = 0.0
      ut(nx,1)   = 0.0
      ut(nx, ny) = 0.0
      vt(1,1)    = 0.0
      vt(1,ny)   = 0.0
      vt(nx,1)   = 0.0
      vt(nx, ny) = 0.0

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE outdat (uc, vc, ut, vt, ss, sd, h, dx       )

C     Write out the velocity, salinity, and temperature.
      IMPLICIT none

      INTEGER nfield
      PARAMETER (nfield = 6)

      INCLUDE "grid.inc"
      REAL uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny), h(nx, ny)
      REAL dx

      INTEGER i, j
      REAL uctemp, vctemp, iconst
      REAL flm(ny), fls(ny)

      CHARACTER*60 fname(nfield)
      SAVE flm, fls, fname

      uctemp  = uc(1,1)
      vctemp  = vc(1,1)
      uc(1,1) = 0.0125
      vc(1,1) = 0.0125

      WRITE (30) uc
      WRITE (31) vc
      WRITE (32) ut
      WRITE (33) vt
      WRITE (34) ss
      WRITE (35) sd

      uc(1,1) = uctemp
      vc(1,1) = vctemp

      RETURN

      ENTRY outfl(uc, vc, ut, vt, ss, sd, h, dx)

      iconst = 0.5*h(1,1)*dx
      DO 1000 j = 1, ny
        flm(j) = 0.0
        fls(j) = 0.0
        DO 1010 i = 1, nx
          flm(j) = flm(j) + (vt(i,j) - vc(i,j))
          fls(j) = fls(j) +
     1                (vt(i,j) - vc(i,j))*(ss(i,j) - sd(i,j))
 1010   CONTINUE
 1000 CONTINUE
      DO 1100 j = 1, ny
        flm(j) = flm(j)*iconst
        fls(j) = fls(j)*iconst
 1100 CONTINUE

      WRITE (40) flm
      WRITE (41) fls

      RETURN

      ENTRY outstr(uc, vc, ut, vt, ss, sd, h, dx)

C     Open the necessary output files
      DO 2000 i = 1, nfield
        PRINT *,'What is the name of instantaneous output file # ',i
        READ  (11,9002) fname(i)
        WRITE (*,9002) fname(i)
        OPEN (29+i, FILE=fname(i), FORM='UNFORMATTED', STATUS='NEW')
 2000 CONTINUE
      PRINT *,'Name for the mass flux file '
      READ  (11, 9002) fname(1)
      WRITE (*, 9002) fname(1)
      OPEN (40, FILE=fname(1), FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'Name for the salt flux file '
      READ  (11, 9002) fname(1)
      WRITE (*, 9002) fname(1)
      OPEN (41, FILE=fname(1), FORM='UNFORMATTED', STATUS='NEW')
C     Averaged output files
      DO 2010 i = 1, nfield
        PRINT *,'What is the name of averaged output file # ',i
        READ  (11,9002) fname(i)
        WRITE (*,9002) fname(i)
        OPEN (49+i, FILE=fname(i), FORM='UNFORMATTED', STATUS='NEW')
 2010 CONTINUE
      PRINT *,'Name for the mass flux file '
      READ  (11, 9002) fname(1)
      WRITE (*, 9002) fname(1)
      OPEN (56, FILE=fname(1), FORM='FORMATTED', STATUS='NEW')
      PRINT *,'Name for the salt flux file '
      READ  (11, 9002) fname(1)
      WRITE (*, 9002) fname(1)
      OPEN (57, FILE=fname(1), FORM='FORMATTED', STATUS='NEW')
 9002 FORMAT (A60)

      RETURN

      ENTRY outend(uc, vc, ut, vt, ss, sd, h, dx)

C     Close the data files:
      CLOSE (30, STATUS='KEEP')
      CLOSE (31, STATUS='KEEP')
      CLOSE (32, STATUS='KEEP')
      CLOSE (33, STATUS='KEEP')
      CLOSE (34, STATUS='KEEP')
      CLOSE (35, STATUS='KEEP')
      CLOSE (40, STATUS='KEEP')
      CLOSE (41, STATUS='KEEP')
      CLOSE (50, STATUS='KEEP')
      CLOSE (51, STATUS='KEEP')
      CLOSE (52, STATUS='KEEP')
      CLOSE (53, STATUS='KEEP')
      CLOSE (54, STATUS='KEEP')
      CLOSE (55, STATUS='KEEP')
      CLOSE (56, STATUS='KEEP')
      CLOSE (57, STATUS='KEEP')

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE uvext (uc, vc, ss, href,
     1                  rhoref, g, f, beta, ahm, delx, dely)
C     Extrapolate u, v to the next time level
C     Computation of common constants added prior to 5-26-88.
C     Version rewritten for geostrophy, a la derivation. 4-5-89.
C       Much commented program deleted 4-5-89.
C     Unused variables deleted 7-16-90.
C     Per reviewer notes, overspecified boundary conditions removed.--
C       Note though that the bc. on SS effectively produce the same
C       result.  7-16-90.
C     Rho computation done via statement function, cuts about 98%
C       of computational time (overhead is tremendous!) 11/30/92
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL uc(nx, ny), vc(nx, ny)
      REAL ss(nx, ny)
      REAL rhoref, f, g, beta, ahm, href
      REAL delx, dely

      REAL rhos1p, rhos(nx, ny), bcorr
      INTEGER i, j, k, l

C     Params for speedier numerics:
      REAL dx2, dy2, g8rref

C     Density statement function
      REAL gamma
      PARAMETER (gamma =  0.80908  )
C     reference values are T= -0.5, S=34.6, P = 0.0
      rhos1p(i,j) = gamma*ss(i,j)

C     Compute params for speedier numerics:
      dx2    = 2.*delx
      dy2    = 2.*dely
      g8rref = g*href/4./rhoref/f

C     Compute the density field before entering the extrapolation.
C     This reduces the number of calls to the density function by
C       almost a factor of 4.  8-4-88.
      DO 900 l = 1, ny
        DO 910 k = 1, nx
            rhos(k,l) = rhos1p( k, l)
  910   CONTINUE
  900 CONTINUE

C     Compute the geostrophic velocity
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
          vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2

 1010   CONTINUE
 1000 CONTINUE

C     Now compute the free slip velocities along boundaries.
      j = 1
      DO 1100 i = 2, nx-1
        uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j) )/dely
        vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2
 1100 CONTiNUE
      j = ny
      DO 1200 i = 2, nx-1
        uc(i,j) = +g8rref*( rhos(i,j) - rhos(i,j-1) )/dely
        vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2
 1200 CONTiNUE
      i = 1
      DO 1300 j = 2, ny-1
        uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
        vc(i,j) = -g8rref*( rhos(i+1,j) - rhos(i,j) )/delx
 1300 CONTiNUE
      i = nx
      DO 1400 j = 2, ny-1
        uc(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
        vc(i,j) = -g8rref*( rhos(i,j) - rhos(i-1,j) )/delx
 1400 CONTiNUE
      uc(1,1)   = +g8rref*( rhos(1 ,2)  - rhos(1 ,1) )/dely
      uc(1,ny)  = +g8rref*( rhos(1 ,ny) - rhos(1 ,ny-1) )/dely
      uc(nx,1)  = +g8rref*( rhos(nx,2)  - rhos(nx,1) )/dely
      uc(nx,ny) = +g8rref*( rhos(nx,ny) - rhos(nx,ny-1) )/dely
      vc(1,1)   = -g8rref*( rhos(2 ,1)  - rhos(1 ,1) )/delx
      vc(1,ny)  = -g8rref*( rhos(2 ,ny) - rhos(1 ,ny) )/delx
      vc(nx,1)  = -g8rref*( rhos(nx,1)  - rhos(nx-1,1) )/delx
      vc(nx,ny) = -g8rref*( rhos(nx,ny) - rhos(nx-1,ny) )/delx

C     Apply the beta correction to the velocities.
      DO 2000 j = 1, ny
        bcorr = 1. + beta*FLOAT(ny/2-j)*dely/f
        DO 2100 i = 1, nx
          uc(i,j) = uc(i,j)/bcorr
          vc(i,j) = vc(i,j)/bcorr
 2100   CONTINUE
 2000 CONTINUE

C     Now consider the boundary conditions:
C       7-16-90:
C         At i = 1         u = 0.0
C         At i = nx        u = 0.0
C         At j = 1         v = 0.0
C      8-2-90
C         At j = ny        v = 0.0

      DO 3000 i = 1, nx
C       BC on v at the y boundaries
        vc(i,1)  = 0.0
        vc(i,ny) = 0.0
 3000 CONTINUE

      DO 3010 j = 1, ny
C       u = 0.0 implemented 1-26-89
        uc(1,  j) = 0.0
        uc(nx, j) = 0.0
 3010 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE stext(uc, vc, ut, vt, we, ss, sd, qs, qd, h,
     1                 delx, dely, delt, dref, sref,
     2                 ash, asv, tstep)
C     Subroutine to extrapolate the salinity field to the next
C       time step.
      IMPLICIT none
      INCLUDE "grid.inc"

      INTEGER tstep
      REAL we(nx, ny), uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL qs(nx, ny), qd(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny)
      REAL delx, dely, delt, dref, sref, ash, asv

      INTEGER i, j
      REAL fss(nx, ny), fsd(nx, ny), lapl(nx, ny)
      REAL dx2, dy2, dxtdx, dytdy, difu, pvdif, href

      dx2   = 2.*delx
      dy2   = 2.*dely
      dxtdx = delx*delx
      dytdy = dely*dely
      href  = h(1,1)
      pvdif = 8.*asv/href
      difu  = ash/dxtdx

C     Evaluate the diffusion:
CD      DO 100 j = 3, ny-2
CD        DO 101 i = 3, nx-2
CD          lapl(i,j) =
CD     1    (-ss(i+2,j)+16.*ss(i+1,j)-30.*ss(i,j)
CD     2               +16.*ss(i-1,j)-ss(i-2,j)   )/48.
CD     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
CD     4               +16.*ss(i,j-1)-ss(i,j-2)   )/48.
CD  101   CONTINUE
CD        i = 2
CD        lapl(i,j) = (11.*ss(i-1,j)-20.*ss(i,j)+6.*ss(i+1,j)
CD     1               +4.*ss(i+2,j)-ss(i+3,j) )/48.
CD     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
CD     4               +16.*ss(i,j-1)-ss(i,j-2)   )/48.
CD        i = nx-1
CD        lapl(i,j) = ss(i+1,j)-2.*ss(i,j)+ss(i-1,j)
CD     3   +(-ss(i,j+2)+16.*ss(i,j+1)-30.*ss(i,j)
CD     4               +16.*ss(i,j-1)-ss(i,j-2)   )/48.
CD
CD  100 CONTINUE
CD      DO 102 i = 2, nx-1
CD        j = 2
CD        lapl(i,j) = ss(i+1,j)-2.*ss(i,j)+ss(i-1,j)
CD     1   +(11.*ss(i,j-1)-20.*ss(i,j)+6.*ss(i,j+1)
CD     2               +4.*ss(i,j+2)-ss(i,j+3) )/48.
CD        j = ny-1
CD        lapl(i,j) = ss(i+1,j)-4.*ss(i,j)+ss(i-1,j)+ss(i,j+1)+ss(i,j-1)
CD  102 CONTINUE
      CALL laplac(lapl, ss, nx, ny, delx, dely, ash)

C     Compute forcing for the interior points:
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          fss(i,j) =
     1     + lapl(i,j)
     2     - (  ut(i,j)*(ss(i+1,j)-ss(i-1,j))
     3        + vt(i,j)*(ss(i,j+1)-ss(i,j-1))
     4        + uc(i+1,j)*sd(i+1,j)-uc(i-1,j)*sd(i-1,j)
     5        + vc(i,j+1)*sd(i,j+1)-vc(i,j-1)*sd(i,j-1) )/dx2
     6     + (qs(i,j) - we(i,j)*sd(i,j))/href
C           Adopt Lax-Wendroff Differencing 8-8-89.
     1     + (ut(i,j)*ut(i,j)*delt*0.5)*
     2             (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/dxtdx
     3     + (vt(i,j)*vt(i,j)*delt*0.5)*
     4             (ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/dxtdx


          fsd(i,j) =
     1     - ( ut(i,j)*(sd(i+1,j)-sd(i-1,j))
     2        +vt(i,j)*(sd(i,j+1)-sd(i,j-1))
     3        +uc(i,j)*(ss(i+1,j)-ss(i-1,j))
     4        +vc(i,j)*(ss(i,j+1)-ss(i,j-1))   ) /dx2
     5     + (qd(i,j) - sd(i,j)*(pvdif+we(i,j)) )/href
C           Adopt Lax-Wendroff Differencing 8-8-89.
     1     + (ash+ut(i,j)*ut(i,j)*delt*0.5)*
     2             (sd(i+1,j)-2.*sd(i,j)+sd(i-1,j))/dxtdx
     3     + (ash+vt(i,j)*vt(i,j)*delt*0.5)*
     4             (sd(i,j+1)-2.*sd(i,j)+sd(i,j-1))/dxtdx
C      Upwind vertical differencing for test 8-10-90.BG
CUP     1     - sd(i,j)*ABS(we(i,j)/href+(uc(i+1,j)-uc(i-1,j)+
CUP     2                                 vc(i,j+1)-vc(i,j-1))/dx2 )

 1010   CONTINUE
 1000 CONTINUE

C     Extrapolate the interior values:
      DO 3000 j = 2, ny-1
        DO 3010 i = 2, nx-1
          ss(i,j) = ss(i,j) + delt*fss(i,j)
          sd(i,j) = sd(i,j) + delt*fsd(i,j)
 3010   CONTINUE
 3000 CONTINUE

C     Now must apply the boundary conditions.
C     No normal gradient (no flux) condition at all boundaries
C     BC:
      DO 4000 i = 2, nx-1
        ss(i,1)  = ss(i,2)
        sd(i,1)  = sd(i,2)
        ss(i,ny) = ss(i,ny-1)
        sd(i,ny) = sd(i,ny-1)
 4000 CONTINUE
      DO 4010 j = 2, ny-1
        ss(nx,j) = ss(nx-1,j)
        sd(nx,j) = sd(nx-1,j)
        ss(1,j)  = ss(2,j)
        sd(1,j)  = sd(2,j)
 4010 CONTINUE
      ss(1,1)   = ss(2,2)
      ss(1,ny)  = ss(2,ny-1)
      ss(nx,1)  = ss(nx-1,2)
      ss(nx,ny) = ss(nx-1, ny-1)
      sd(1,1)   = sd(2,2)
      sd(1,ny)  = sd(2,ny-1)
      sd(nx,1)  = sd(nx-1,2)
      sd(nx,ny) = sd(nx-1, ny-1)
      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE timav(uc, vc, ut, vt, ss, sd, h, scrit, tav, tstep,
     1                     delx, dely)
C     Program to compute averaged quantities from model output
C     Version modified to work 'on the fly'.  Original
C       assumed that the files were already written and merely
C       needed averaging.  Now work with data as it is being generated.
C       BG 9-29-89
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL uc(nx, ny), vc(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL ut(nx, ny), vt(nx, ny), h(nx, ny)
      REAL scrit, delx, dely

      INTEGER nscrit
      PARAMETER (nscrit = 1)
      REAL delscrit
      PARAMETER (delscrit = 0.005)
      REAL ftav
      INTEGER i, j, k, l
      INTEGER tstep, tav
      REAL r1(nx, ny), r2(nx, ny), r3(nx, ny), r4(nx, ny)
      REAL r5(nx, ny), r6(nx, ny)
      REAL f1(ny,nscrit), f2(ny,nscrit)
      REAL f1t(ny,nscrit), f2t(ny,nscrit)
      SAVE r1, r2, r3, r4, r5, r6, f1, f2, f1t, f2t

C***********************************************************----------!!

C     Number of steps to average over is tav.
      ftav = FLOAT(tav)
      IF (tstep .EQ. 1) THEN
        DO 1040 k = 1, ny
          DO 1050 l = 1, nx
            r1(l,k) = 0.0
            r2(l,k) = 0.0
            r3(l,k) = 0.0
            r4(l,k) = 0.0
            r5(l,k) = 0.0
            r6(l,k) = 0.0
 1050     CONTINUE
 1040   CONTINUE
         DO 1060 k = 1, ny
          DO 1060 l = 1, nscrit
            f1(k,l) = 0.0
            f2(k,l) = 0.0
 1070     CONTINUE
 1060   CONTINUE

       ELSE

        DO 1900 i = 1, nscrit
          CALL reflux(vt, vc, ss, sd, h, f1t(1,i), f2t(1,i),
     1                scrit+(i-1)*delscrit, nx, ny, delx, dely)
          DO 1910 k = 1, ny
            f1(k,i) = f1(k,i)+f1t(k,i)
            f2(k,i) = f2(k,i)+f2t(k,i)
 1910     CONTINUE
 1900   CONTINUE

        DO 2000 k = 1, ny
          DO 2010 l = 1, nx
            r1(l,k) = r1(l,k) + uc(l,k)
            r2(l,k) = r2(l,k) + vc(l,k)
            r3(l,k) = r3(l,k) + ut(l,k)
            r4(l,k) = r4(l,k) + vt(l,k)
            r5(l,k) = r5(l,k) + ss(l,k)
            r6(l,k) = r6(l,k) + sd(l,k)
 2010     CONTINUE
 2000   CONTINUE

        IF ( MOD(tstep,tav) .EQ. 0) THEN
          DO 2020 k = 1, ny
            DO 2030 l = 1, nx
              r1(l,k) = r1(l,k) / ftav
              r2(l,k) = r2(l,k) / ftav
              r3(l,k) = r3(l,k) / ftav
              r4(l,k) = r4(l,k) / ftav
              r5(l,k) = r5(l,k) / ftav
              r6(l,k) = r6(l,k) / ftav
 2030       CONTINUE
            DO 2040 i = 1, nscrit
              f1(k,i) = f1(k,i) / ftav / 1.E5
              f2(k,i) = f2(k,i) / ftav / 1.E5
 2040       CONTINUE
 2020     CONTINUE
          WRITE (50) r1
          WRITE (51) r2
          WRITE (52) r3
          WRITE (53) r4
          WRITE (54) r5
          WRITE (55) r6
          DO 2100 j = 1, nscrit
           WRITE (56,9009) (f1(i,j),i=1,ny)
           WRITE (57,9009) (f2(i,j),i=1,ny)
 2100          CONTINUE
           WRITE (56,9010)
          WRITE (57,9010)
          DO 2060 k = 1, ny
            DO 2050 l = 1, nx
              r1(l,k) = 0.0
              r2(l,k) = 0.0
              r3(l,k) = 0.0
              r4(l,k) = 0.0
              r5(l,k) = 0.0
              r6(l,k) = 0.0
 2050       CONTINUE
             DO 2070 i = 1, nscrit
              f1(k,i) = 0.0
              f2(k,i) = 0.0
 2070       CONTINUE
 2060     CONTINUE
        ENDIF

      ENDIF

 9001 FORMAT (BN, I5)

 9002 FORMAT (A60)

 9009 FORMAT (16F5.2)

 9010 FORMAT (' end of step')

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE init(ub, vb, ut, vt, ss, sd, we, h, nnx, nny,
     1                ahm, avm, ahs, avs,
     2                sdref, ssref, rhoref, g, f,
     3                delx, dely, delt,
     4                nout, nflout, ntot, scrit, tav,
     4                xmin, xmax, ymin, ymax, qfmax, qfref, qm,
     5                strspr, strsum, strfll, strwin, loy, beta    )
C     Initialize the data arrays and constants

      IMPLICIT none
      REAL secpyr
      PARAMETER (secpyr = 3.1556908E7)
      REAL pi
      PARAMETER (pi = 3.141592654)
      INCLUDE "grid.inc"

      INTEGER nnx, nny
      REAL ub(nx, ny), vb(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny)
      REAL we(nx, ny), h(nx, ny)

      REAL ahm, avm, ahs, avs
      REAL sref, sdref, ssref, rhoref
      REAL g, f
      REAL delx, dely, delt
      INTEGER nout, nflout, ntot, tav
      REAL scrit

C     Parameters for buoyancy forcing
      INTEGER xmin, xmax, ymin, ymax
      REAL qfmax, qfref, qm
      INTEGER strspr, strsum, strfll, strwin, loy

C     Parameters for barotropic flow
      REAL beta

      REAL rho, value, a, b
      CHARACTER*60 fname
      INTEGER i, j, dummy
      LOGICAL yes

C***********************************************************----------**
      PRINT *,'What is the name of the parameter file?'
      READ (*,9001) fname
      WRITE (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
C***********************************************************----------**
C     Get array data:

      PRINT *,'Would you like to use output from an old run?'
      IF (yes(.FALSE.)) THEN
        PRINT *,'What is the file name for ub?'
        READ (11,9001) fname
        OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for vb?'
        READ (11,9001) fname
        OPEN (13, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'ut   '
        READ (11,9001) fname
        OPEN (14, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'vt   '
        READ (11,9001) fname
        OPEN (15, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for ss?'
        READ (11,9001) fname
        OPEN (16, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for sd?'
        READ (11,9001) fname
        OPEN (17, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')

        PRINT *,'At what time step do you want the data?'
        READ (11,9003) dummy
        DO 1000 i = 1, dummy
          READ (12) ub
          READ (13) vb
          READ (14) ut
          READ (15) vt
          READ (16) ss
          READ (17) sd
 1000   CONTINUE

       ELSE
        WRITE (*,9011) 'ub   '
        READ (11,9002) value
        CALL arset(ub, nx, ny, value)
        WRITE (*,9011) 'vb   '
        READ (11,9002) value
        CALL arset(vb, nx, ny, value)
        WRITE (*,9011) 'ut   '
        READ (11,9002) value
        CALL arset(ut, nx, ny, value)
        WRITE (*,9011) 'vt   '
        READ (11,9002) value
        CALL arset(vt, nx, ny, value)
        WRITE (*,9011) 'ss   '
        READ (11,9002) value
        CALL arset(ss, nx, ny, value)
        WRITE (*,9011) 'sd   '
        READ (11,9002) value
        CALL arset(sd, nx, ny, value)
      ENDIF

CU    PRINT *,'What is the name of the ekman pumping file?'
CU      READ (*,9001) fname
CU      CALL read2(we, nx, ny, 2, fname, .TRUE., .TRUE.)
      WRITE (*,9011) 'we   '
      READ (11,9002) value
      PRINT *,'What is the wave length in x?'
      READ (11,9002) a
      PRINT *,'What is the wave length in y?'
      READ (11,9002) b
      DO 7000 j = 0, ny-1
        DO 7010 i = 0, nx-1
          we(i+1,j+1) = value*SIN(2.*pi*j/b)
 7010   CONTINUE
 7000 CONTINUE

      WRITE (*,9011) 'h    '
      READ (11,9002) value
      CALL arset(h, nx, ny, value)

 9011 FORMAT (' What is the value of ',A5,'?')

C***********************************************************----------**
CU    PRINT *,'What is the name of the scalar parameter file?'
CU    READ (*,9001) fname
CU    WRITE (*,9001) fname
CU    OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')

C        Forcing
        READ (11,9003) xmin
        READ (11,9003) xmax
        READ (11,9003) ymin
        READ (11,9003) ymax
        READ (11,9002) qfmax
        READ (11,9002) qfref
        READ (11,9002) qm
        READ (11,9003) strspr
        READ (11,9003) strsum
        READ (11,9003) strfll
        READ (11,9003) strwin
C        output
        READ (11,9003) nout
        READ (11,9003) nflout
        READ (11,9003) ntot
        READ (11,9002) scrit
        READ (11,9003) tav
C        grid
        READ (11,9002) delx
        READ (11,9002) dely
        READ (11,9002) delt
C        boundary condition info.
        READ (11,9002) sref
        READ (11,9002) sdref
        READ (11,9002) ssref
C        diffusion
        READ (11,9002) ahm
        READ (11,9002) avm
        READ (11,9002) ahs
        READ (11,9002) avs
C        physical constants
        READ (11,9002) f
        READ (11,9002) g
C        terms for barotropic solution.
        READ (11,9002) beta
CU    CLOSE (11, STATUS='KEEP')

C     Computed parameters:
      rhoref = rho(sref, 0.0, 0.)
      loy    = INT(secpyr/delt)

C     Rescale input values of qfmax (m/day), qfref,qm (m/season) to
C       kg/m**2/s.  Must divide by h in uvext.  8-10-88.
        qfmax = qfmax / 2.88E3
        qfref = qfref * (30./FLOAT(strspr-strwin)/delt)
        qm    = qm    * (30./FLOAT(strfll-strsum)/delt)
C        Forcing
        WRITE (*,9003) xmin
        WRITE (*,9003) xmax
        WRITE (*,9003) ymin
        WRITE (*,9003) ymax
        WRITE (*,9002) qfmax
        WRITE (*,9002) qfref
        WRITE (*,9002) qm
        WRITE (*,9003) strspr
        WRITE (*,9003) strsum
        WRITE (*,9003) strfll
        WRITE (*,9003) strwin
C        output
        WRITE (*,9003) nout
        WRITE (*,9003) nflout
        WRITE (*,9003) ntot
        WRITE (*,9002) scrit
        WRITE (*,9003) tav
C        grid
        WRITE (*,9002) delx
        WRITE (*,9002) dely
        WRITE (*,9002) delt
C        boundary condition info.
        WRITE (*,9002) sref
        WRITE (*,9002) sdref
        WRITE (*,9002) ssref
C        diffusion
        WRITE (*,9002) ahm
        WRITE (*,9002) avm
        WRITE (*,9002) ahs
        WRITE (*,9002) avs
C        physical constants
        WRITE (*,9002) f
        WRITE (*,9002) g
C        terms for barotropic solution.
        WRITE (*,9002) beta
C        computed constants
        WRITE (*,9003) loy

 9001 FORMAT (A60)

 9002 FORMAT (BN, E13.6)

 9003 FORMAT (BN, I8)

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE qsext(ss, qss, qsd, we, h, nnx, nny, tstep, loy,
     1              xcen, xlen, ycen, ylen,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin, delt        )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL ss(nx, ny), qss(nx, ny), qsd(nx, ny), we(nx, ny), h(nx, ny)
      INTEGER tstep, loy
      INTEGER xcen, xlen, ycen, ylen
      INTEGER strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm, delt, delx, dely

      INTEGER nnx, nny

      INTEGER i, j, t

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq, sumw, qfix
      REAL yterm, xpart, qssum

      SAVE a, qfix

      t = MOD(tstep,loy)
      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      xpart = 0.5/sigx/sigx

      IF (tstep .EQ. 0) THEN
        qfix = 0.0
        a = 0.0
        CALL qstart(ss, qss, qsd, we, h, nnx, nny, a, loy,
     1              xcen, xlen, ycen, ylen,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin, delt        )
      ENDIF

      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
        DO 1000 j = 1, ny
          dy = dely*FLOAT(j)
          yterm = (dy - yref)*(dy - yref)*0.5/sigy/sigy
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            qss(i,j) = qsfref + qfix + qsfmax*
     1        EXP( -(dx-xref)*(dx-xref)*xpart - yterm )
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (t .GE. strspr .AND. t .LT. strsum) THEN
C       spring, qss = qsd = 0.0
        DO 1100 j = 1, ny
          DO 1110 i = 1, nx
            qss(i,j) = 0.0
 1110     CONTINUE
 1100   CONTINUE
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
C       Summer
        DO 2000 j = 1, ny
          qssum = qsm - a*FLOAT(j)*dely
          DO 2010 i = 1, nx
            qss(i,j) = qssum
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       fall, do nothing
        DO 2100 j = 1, ny
          DO 2110 i = 1, nx
            qss(i,j) = 0.0
 2110     CONTINUE
 2100   CONTINUE
      ENDIF

      qsd = qss

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE qstart(ss, qss, qsd, we, h, nnx, nny, a, loy,
     1              xcen, xlen, ycen, ylen,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin, delt        )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.
      IMPLICIT none
      INCLUDE "grid.inc"

      REAL ss(nx, ny), qss(nx, ny), qsd(nx, ny), we(nx, ny), h(nx, ny)
      INTEGER loy
      INTEGER xcen, xlen, ycen, ylen
      INTEGER strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm, delt, delx, dely

      INTEGER nnx, nny
      INTEGER i, j, t

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq, sumw, qfix
      REAL yterm, xpart, qssum

      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      xpart = 0.5/sigx/sigx

      DO 400 j = 1, ny
        DO 410 i = 1, nx
          qss(i,j) = 0.0
  410   CONTINUE
 400  CONTINUE

      DO 9000 t = 0, loy-1
      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
        DO 1000 j = 1, ny
          dy = dely*FLOAT(j)
          yterm = (dy - yref)*(dy - yref)*0.5/sigy/sigy
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            qss(i,j) = qss(i,j) + qsfref + qsfmax*
     1        EXP( -(dx-xref)*(dx-xref)*xpart - yterm )
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (t .GE. strspr .AND. t .LT. strsum) THEN
C       spring, qss = qsd = 0.0
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
        DO 2000 j = 1, ny
          qssum = qsm - a*FLOAT(j)*dely
          DO 2010 i = 1, nx
            qss(i,j) = qss(i,j)+qsm
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       fall, do nothing
      ENDIF
 9000 CONTINUE

      CALL summer(qss, nx, ny, sumq)
      CALL summer(we, nx, ny, sumw)
      sumw = sumw*(-0.1*FLOAT(loy))/h(nx/2, ny/2)
      sumq = (sumq + sumw)

      DO 3000 j = 1, ny
        DO 3100 i = 1, nx
          qss(i,j) = j*dely
 3100   CONTINUE
 3000 CONTINUE
      CALL summer(qss, nx, ny, sumw)
      a = sumq/sumw/FLOAT(strfll-strsum-0)
      PRINT *,'net source ', sumq/delx/dely, a

      RETURN
      END
      SUBROUTINE laplac(diffus, ss, nnx, nny, dx, dy, diffu)
      IMPLICIT none

      INTEGER nnx, nny
      INCLUDE "grid.inc"
      REAL dx, dy, diffu
      REAL ss(nx, ny), diffus(nx, ny)

      INTEGER i, j
      REAL difu

      difu  = diffu/dx/dx

C     Evaluate the diffusion:
C     Laplacean in the central part of the domain.  Centered in
C       x and y.
      DO 100 j = 2, ny-1
        DO 101 i = 2, nx-1
          diffus(i,j) =
     1    (ss(i+1,j)-4.*ss(i,j)
     2               +ss(i-1,j)   )
CD     3   +(ss(i,j+1)-2.*ss(i,j)
CD     4               +ss(i,j-1)   )
  101   CONTINUE
  100 CONTINUE
      DO 110 j = 2, ny-1
        DO 111 i = 2, nx-1
          diffus(i,j) = diffus(i,j)
     3   +(ss(i,j+1)
     4               +ss(i,j-1)   )
  111   CONTINUE
  110 CONTINUE

C     Diffusion along walls
      DO 200 j = 1, ny
        diffus(1,j) = 0.0
        diffus(nx,j) = 0.0
 200  CONTINUE
      DO 210 i = 1, nx
        diffus(i,1) = 0.0
        diffus(i,ny) = 0.0
  210 CONTINUE

      DO 2000 j = 1, ny
        DO 2100 i = 1, nx
          diffus(i,j) = diffus(i,j)*difu
 2100   CONTINUE
 2000 CONTINUE

      RETURN
      END
