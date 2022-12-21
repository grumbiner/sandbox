      Program cshelf
C     Model bouyancy (and perhaps wind) forced continental shelf
C       motions and conditions for the polar regions.

      INTEGER nx, ny
      PARAMETER (nx = 36, ny = 36)

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
      INTEGER nout, nflout, ntot, loy

C     Local variables
      INTEGER i
      CHARACTER*60 fname
C     Conservation test variables
      DOUBLE PRECISION s1, s2, s3, s4, s5, s6, s7, s8, s1old

C***********************************************************----------!!
C     BEGIN EXECUTION

C     Initialize the variables
      CALL init (uc, vc, ut, vt, ss, sd, we, h, nx, ny,
     2           ahm, avm, ahs, avs,
     3           sdref, ssref, rhoref,
     4           g, f,
     5           delx, dely, delt, nout, nflout, ntot,
     6           xmin, xmax, ymin, ymax, qsfmax, qsfref, qsm,
     7           strspr, strsum, strfll, strwin, loy,
     8           beta                                         )
      i = 0
      s1old = 0.D0

C     Open the output files
CD    OPEN (1, FILE='qout', FORM='UNFORMATTED', STATUS='NEW')
      CALL outstr(uc, vc, ut, vt, ss, sd, h, delx, nx, ny)
      CALL uvtrop(ut, vt, we, h, nx, ny, delx, dely, f, beta, ahm)
C***********************************************************----------!!
CM      OPEN (1, FILE='timeinfo', FORM='FORMATTED', STATUS='NEW')
CM      PRINT *,LONG(362)
CM      WRITE(1,9001) 0, LONG(362)
CM 9001 FORMAT (2I12)
C     Extrapolation loop
      DO 1000 i = 1, ntot
CM        WRITE(*,9001) i, LONG(362)
CM        WRITE(1,9001) i, LONG(362)
        CALL uvext (uc, vc, ss, sd, h, nx, ny,
     2              rhoref, g, f, ahm, avm, delx, dely, delt    )

        CALL qsext (qss, qsd, h, nx, ny, i, loy,
     1              xmin, xmax, ymin, ymax,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin              )
CD      WRITE (1) qss

CD      CALL sconv (uc, vc, ut, vt, we, ss, sd, qss, qsd, h,
CD   1              nx, ny, delx, dely, delt, sdref, ssref,
CD   2              ahs, avs, i, s1, s2,
CD   3              s3, s4, s5, s6, s7, s8                      )
CD      WRITE (*,9015) i, (s1-s1old)/delt, s2, s1,
CD   1                    s3, s4, s5, s6, s7, s8
CD      s1old = s1

        CALL stext (uc, vc, ut, vt, we, ss, sd, qss, qsd, h,
     1              nx, ny, delx, dely, delt, sdref, ssref,
     2              ahs, avs, i                                 )

        CALL convec(ss, sd, nx, ny, i)
        IF (MOD(i,nout) .EQ. 0) THEN
           CALL outdat (uc, vc, ut, vt, ss, sd, h, delx, nx, ny  )
           PRINT *,'tstep=',i
        ENDIF
        IF (MOD(i,nflout) .EQ. 0)
     1      CALL outfl (uc, vc, ut, vt, ss, sd, h, delx, nx, ny  )
 1000 CONTINUE

      CALL outend (uc, vc, ut, vt, ss, sd, h, delx, nx, ny       )

 9015 FORMAT (I4,3D16.10,/,6D12.6)

      END
C***********************************************************----------!!
      SUBROUTINE qsext(qss, qsd, h, nx, ny, tstep, loy,
     1                 xcen, xlen, ycen, ylen,
     2                 qsfmax, qsfref, qsm, delx, dely,
     3                 strspr, strsum, strfll, strwin            )
C     Subroutine to extrapolate the salinization forcing to the next
C       time step.  BG 3-25-88.

      INTEGER nx, ny, tstep
      REAL qss(nx, ny), qsd(nx, ny), h(nx, ny)
      REAL delx, dely

      INTEGER i, j, t

      INTEGER xcen, ycen, xlen, ylen
      REAL qsfmax, qsfref
      REAL qsm
      INTEGER strspr, strsum, strfll, strwin, loy

      REAL pi
      PARAMETER (pi = 3.141592654)
      REAL xref, yref, dx, dy, sigx, sigy
      REAL a, sumq
      SAVE a

      t = MOD(tstep,loy)
      xref = FLOAT(xcen)
      yref = FLOAT(ycen)
      sigx = FLOAT(xlen)
      sigy = FLOAT(ylen)

      IF (t .EQ. 1) THEN
C       compute the required slope for salt conservation.
        sumq = 0.0
        DO 100 j = 2, ny-1
          dy = dely*FLOAT(j)
          DO 101 i = 2, nx-1
            dx = delx*FLOAT(i)
            sumq = sumq + qsfref + qsfmax*
     1      EXP((-1.)*(( dx-xref )**2/2./sigx**2
     2                +( dy-yref )**2/2./sigy**2 ))
 101      CONTINUE
 100    CONTINUE
        sumq = sumq*FLOAT(strspr-strwin)/FLOAT(strfll-strsum)/
     1         FLOAT(nx*ny-2*nx-2*ny+4)
        sumq = sumq + qsm
        a    = sumq*2./FLOAT(ny+1)/dely
        PRINT *,'linear melting parameter = ',a
      ENDIF

      IF ((t .GE. strwin) .AND. (t .LT. strspr)) THEN
        DO 1000 j = 1, ny
          DO 1010 i = 1, nx
            dx = delx*FLOAT(i)
            dy = dely*FLOAT(j)
            qss(i,j) = qsfref + qsfmax*
     1      EXP((-1.)*(( dx-xref )**2/2./sigx**2
     2                +( dy-yref )**2/2./sigy**2 ))
            qsd(i,j) = qss(i,j)
 1010     CONTINUE
 1000   CONTINUE
       ELSEIF (t .GE. strspr .AND. t .LT. strsum) THEN
C       spring, qss = qsd = 0.0
        DO 1100 j = 1, ny
          DO 1110 i = 1, nx
            qss(i,j) = 0.0
            qsd(i,j) = 0.0
 1110     CONTINUE
 1100   CONTINUE
       ELSEIF (t .GE. strsum .AND. t .LT. strfll) THEN
        DO 2000 j = 1, ny
          DO 2010 i = 1, nx
            qss(i,j) = qsm - a*FLOAT(j)*dely
            qsd(i,j) = qss(i,j)
 2010     CONTINUE
 2000   CONTINUE
       ELSE
C       fall, do nothing
        DO 2100 j = 1, ny
          DO 2110 i = 1, nx
            qss(i,j) = 0.0
            qsd(i,j) = 0.0
 2110     CONTINUE
 2100   CONTINUE
      ENDIF

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE stext(uc, vc, ut, vt, we, ss, sd, qs, qd, h,
     1                 nx, ny, delx, dely, delt, dref, sref,
     2                 ash, asv, tstep)
C     Subroutine to extrapolate the salinity field to the next
C       time step.
C     Del(us) computed as d(us)/dx + d(vs)/dy 3-30-88.

      INTEGER nx, ny, tstep
      REAL we(nx, ny), uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL qs(nx, ny), qd(nx, ny), ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny)
      REAL delx, dely, delt, dref, sref, ash, asv

      INTEGER i, j, nnx, nny
      PARAMETER (nnx = 36)
      PARAMETER (nny = 36)
      REAL fss(nnx, nny), fsd(nnx, nny)
      REAL dx2, dy2

      REAL psi
      INTEGER k, l
      psi(k,l) = 0.0
CD    psi(k,l) = SIGN(1., we(k,l)
CD   1  +  h(k,l)*((uc(i+1,j)-uc(i-1,j))/dx2+(vc(i,j+1)-vc(i,j-1))/dy2)
CD   2  +  uc(i,j)*(h(i+1,j)-h(i-1,j))/dx2
CD   3  +  vc(i,j)*(h(i,j+1)-h(i,j-1))/dy2
CD   4  -  ut(i,j)*(h(i+1,j)-h(i-1,j))/dx2
CD   5  -  vt(i,j)*(h(i,j+1)-h(i,j-1))/dy2          )

      dx2 = 2.*delx
      dy2 = 2.*dely

C     Compute forcing for the interior points:
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          fss(i,j) =
     1     - ut(i,j)*(ss(i+1,j)-ss(i-1,j))/dx2
     2     - vt(i,j)*(ss(i,j+1)-ss(i,j-1))/dy2
     3     - uc(i,j)*(sd(i+1,j)-sd(i-1,j))/dx2
     4     - vc(i,j)*(sd(i,j+1)-sd(i,j-1))/dy2
     5     - sd(i,j)*(  (uc(i+1,j)-uc(i-1,j))/dx2
     6                + (vc(i,j+1)-vc(i,j-1))/dy2   ) * (4./3.)
     7     + qs(i,j)/h(i,j)
     8     - 2.*we(i,j)*sd(i,j)/h(i,j)
C          Add diffusive terms 3-30-88
     9     + ash*( (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/(delx*delx)
     1            +(ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/(dely*dely) )
C          Adopt Lax-Wendroff differencing. 8-8-89.
     2     + ut(i,j)**2*delt/2.
     3          *( (ss(i+1,j)-2.*ss(i,j)+ss(i-1,j))/(delx*delx) )
     4     + vt(i,j)**2*delt/2.
     5          *( (ss(i,j+1)-2.*ss(i,j)+ss(i,j-1))/(dely*dely) )


          fsd(i,j) =
     1     - ut(i,j)*(sd(i+1,j)-sd(i-1,j))/dx2
     2     - vt(i,j)*(sd(i,j+1)-sd(i,j-1))/dy2
     3     - uc(i,j)*(ss(i+1,j)-ss(i-1,j))/dx2
     4     - vc(i,j)*(ss(i,j+1)-ss(i,j-1))/dy2
     5     + qd(i,j) / h(i,j)
     6     - 8.*asv*sd(i,j)/h(i,j)**2
     7     + ash*( (sd(i+1,j)-2.*sd(i,j)+sd(i-1,j))/(delx*delx)
     8            +(sd(i,j+1)-2.*sd(i,j)+sd(i,j-1))/(dely*dely) )
     9     - we(i,j)*sd(i,j)/h(i,j)
C          Adopt Lax-Wendroff differencing. 8-8-89.
     2     + ut(i,j)**2*delt/2.
     3          *( (sd(i+1,j)-2.*sd(i,j)+sd(i-1,j))/(delx*delx) )
     4     + vt(i,j)**2*delt/2.
     5          *( (sd(i,j+1)-2.*sd(i,j)+sd(i,j-1))/(dely*dely) )


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
C     Condition for the y=0 bndy, no flux: s(x,1) = s(x,2)
C                     y = ymax  ,          s(x,ny) = s(x,ny-1)
C         x=0 bc changed to no flux 5-26-88
C         x = xmax bc changed to Robin 5-26-88.
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
      SUBROUTINE uvtrop(ut, vt, we, h, nx, ny, dx, dy, f, beta, am)
C     Barotropic solution.
      INTEGER nx, ny
      REAL ut(nx, ny), vt(nx, ny), we(nx, ny), h(nx, ny)
      REAL dx, dy, f, beta, am

      INTEGER nnx, nny
      PARAMETER (nnx = 36)
      PARAMETER (nny = 36)
      REAL psi(nnx, nny)
      REAL num, lb, wave
      INTEGER i, j

C     Compute the interior solution
      DO 1000 j = 1, ny
        psi(nx, j) = we(nx, j)
        DO 1010 i = nx-1, 1, -1
          psi(i, j) = psi(i+1,j) + we(i+1,j) + we(i,j)
 1010   CONTINUE
 1000 CONTINUE
      num = f*dx/2./beta/h(1,1)
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
          psi(i,j) = psi(i,j)*(1.-exp(-(i-1)*dx/2./lb)*
     1                 (cos(wave*(i-1)) + sin(wave*(i-1))/SQRT(3.) ))
     2              - num*exp( (i-nx)*dx/lb)
 2010   CONTINUE
 2000 CONTINUE

C     Now that we have psi, compute ut, vt.
      DO 3000 j = 2, ny-1
        DO 3010 i = 2, nx-1
          ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
          vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx
 3010   CONTINUE
 3000 CONTINUE
      DO 3020 j = 2, ny-1
        i = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
CD      vt(i,j) =  (psi(i+1,j)-psi(i  ,j))   /dx
        vt(i,j) = 0.0
        i = nx
        ut(i,j) = -(psi(i,j+1)-psi(i,j-1))/2./dy
        vt(i,j) =  (psi(i  ,j)-psi(i-1,j))   /dx
 3020 CONTINUE
      DO 3030 i = 1, nx
        j = 1
        ut(i,j) = -(psi(i,j+1)-psi(i,j))/dy
        vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx
        j = ny
        ut(i,j) = -(psi(i,j)-psi(i,j-1))/dy
        vt(i,j) =  (psi(i+1,j)-psi(i-1,j))/2./dx
 3030 CONTINUE

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE init(ub, vb, ut, vt, ss, sd, we, h, nx, ny,
     1                ahm, avm, ahs, avs,
     2                sdref, ssref, rhoref, g, f,
     3                delx, dely, delt, nout, nflout, ntot,
     4                xmin, xmax, ymin, ymax, qfmax, qfref, qm,
     5                strspr, strsum, strfll, strwin, loy, beta    )
C     Initialize the data arrays and constants

      REAL secpyr
      PARAMETER (secpyr = 3.1556908E7)

      INTEGER nx, ny
      REAL ub(nx, ny), vb(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny)
      REAL we(nx, ny), h(nx, ny)

      REAL ahm, avm, ahs, avs
      REAL sref, sdref, ssref, rhoref
      REAL g, f
      REAL delx, dely, delt
      INTEGER nout, nflout, ntot

C     Parameters for buoyancy forcing
      INTEGER xmin, xmax, ymin, ymax
      REAL qfmax, qfref, qm
      INTEGER strspr, strsum, strfll, strwin, loy
C     Parms for barotropic flow
      REAL beta

      REAL rho, value
      CHARACTER*60 fname
      INTEGER i, j, dummy
      LOGICAL yes

C***********************************************************----------**
C     Get array data:

      PRINT *,'Would you like to use output from an old run?'
      IF (yes(.FALSE.)) THEN
        PRINT *,'What is the file name for ub?'
        READ (*,9001) fname
        OPEN (12, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for vb?'
        READ (*,9001) fname
        OPEN (13, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'ut   '
        READ (*,9001) fname
        OPEN (14, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        WRITE (*,9011) 'vt   '
        READ (*,9001) fname
        OPEN (15, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for ss?'
        READ (*,9001) fname
        OPEN (16, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
        PRINT *,'What is the file name for sd?'
        READ (*,9001) fname
        OPEN (17, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')

        PRINT *,'At what time step do you want the data?'
        READ (*,9003) dummy
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
        READ (*,9002) value
        CALL arset(ub, nx, ny, value)
        WRITE (*,9011) 'vb   '
        READ (*,9002) value
        CALL arset(vb, nx, ny, value)
        WRITE (*,9011) 'ut   '
        READ (*,9002) value
        CALL arset(ut, nx, ny, value)
        WRITE (*,9011) 'vt   '
        READ (*,9002) value
        CALL arset(vt, nx, ny, value)
        WRITE (*,9011) 'ss   '
        READ (*,9002) value
        CALL arset(ss, nx, ny, value)
        WRITE (*,9011) 'sd   '
        READ (*,9002) value
        CALL arset(sd, nx, ny, value)
      ENDIF

      PRINT *,'What is the name of the ekman pumping file?'
        READ (*,9001) fname
        CALL read2(we, nx, ny, 2, fname, .TRUE., .TRUE.)
CU    WRITE (*,9011) 'we   '
CU    READ (*,9002) value
CU    CALL arset(we, nx, ny, value)

      WRITE (*,9011) 'h    '
      READ (*,9002) value
      CALL arset(h, nx, ny, value)

 9011 FORMAT (' What is the value of ',A5,'?')

C***********************************************************----------**
      PRINT *,'What is the name of the scalar parameter file?'
      READ (*,9001) fname
      WRITE (*,9001) fname
      OPEN (11, FILE=fname, FORM='FORMATTED', STATUS='OLD')
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
      CLOSE (11, STATUS='KEEP')

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
      SUBROUTINE outdat (uc, vc, ut, vt, ss, sd, h, dx, nx, ny       )

C     Write out the velocity, salinity, and temperature.
      INTEGER nfield
      PARAMETER (nfield = 6)

      INTEGER nx, ny
      REAL uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny), h(nx, ny)
      REAL dx

      INTEGER i, j
      REAL uctemp, vctemp
      INTEGER nfl
      PARAMETER (nfl = 36)
      REAL flm(nfl), fls(nfl)

      CHARACTER*60 fname(nfield)
      SAVE flm, fls

      uctemp  = uc(1,1)
      vctemp  = vc(1,1)
      uc(1,1) = 0.025
      vc(1,1) = 0.025
      WRITE (30) uc
      WRITE (31) vc
      WRITE (32) ut
      WRITE (33) vt
      WRITE (34) ss
      WRITE (35) sd

      uc(1,1) = uctemp
      vc(1,1) = vctemp

      RETURN

      ENTRY outfl(uc, vc, ut, vt, ss, sd, h, dx, nx, ny)

      DO 1000 j = 1, ny
        flm(j) = 0.0
        fls(j) = 0.0
        DO 1010 i = 1, nx
          flm(j) = flm(j) + 0.5*h(i,j)*dx*(vt(i,j) - vc(i,j))
          fls(j) = fls(j) + 0.5*h(i,j)*dx*
     1                (vt(i,j) - vc(i,j))*(ss(i,j) - sd(i,j))
 1010   CONTINUE
 1000 CONTINUE

      WRITE (40) flm
      WRITE (41) fls

      RETURN

      ENTRY outstr(uc, vc, ut, vt, ss, sd, h, dx, nx, ny)

C     Open the necessary output files
      DO 2000 i = 1, nfield
        PRINT *,'What is the name of output file # ',i
        READ  (*,9002) fname(i)
        WRITE (*,9002) fname(i)
        OPEN (29+i, FILE=fname(i), FORM='UNFORMATTED', STATUS='NEW')
 2000 CONTINUE
      PRINT *,'Name for the mass flux file '
      READ  (*, 9002) fname(1)
      WRITE (*, 9002) fname(1)
      OPEN (40, FILE=fname(1), FORM='UNFORMATTED', STATUS='NEW')
      PRINT *,'Name for the salt flux file '
      READ  (*, 9002) fname(1)
      WRITE (*, 9002) fname(1)
      OPEN (41, FILE=fname(1), FORM='UNFORMATTED', STATUS='NEW')

 9002 FORMAT (A60)

      RETURN

      ENTRY outend(uc, vc, ut, vt, ss, sd, h, dx, nx, ny)

C     Close the data files:
      CLOSE (30, STATUS='KEEP')
      CLOSE (31, STATUS='KEEP')
      CLOSE (32, STATUS='KEEP')
      CLOSE (33, STATUS='KEEP')
      CLOSE (34, STATUS='KEEP')
      CLOSE (35, STATUS='KEEP')
      CLOSE (40, STATUS='KEEP')
      CLOSE (41, STATUS='KEEP')

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE uvext (ub, vb, ss, sd, h, nx, ny,
     1                  rhoref, g, f, ahm, avm, delx, dely, delt)
C     Extrapolate u, v to the next time level
C     Computation of common constants added prior to 5-26-88.
C     Version rewritten for geostrophy, a la derivation. 4-5-89.
C       Much commented program deleted 4-5-89.

      INTEGER nx, ny
      REAL ub(nx, ny), vb(nx, ny)
      REAL ss(nx, ny), sd(nx, ny)
      REAL h(nx, ny)
      REAL rhoref, f, g, ahm, avm
      REAL delx, dely, delt

      INTEGER nnx, nny
      PARAMETER (nnx = 36)
      PARAMETER (nny = 36)
      REAL rhos1p, rhos(nnx, nny)
      INTEGER i, j, k, l
C     Params for speedier numerics:
      REAL dx2, dy2, g8rref

C     Compute params for speedier numerics:
      dx2    = 2.*delx
      dy2    = 2.*dely
      g8rref = g*h(1,1)/4./rhoref/f

C     Compute the density field before entering the extrapolation.
C     This reduces the number of calls to the density function by
C       almost a factor of 4.  8-4-88.
      DO 900 l = 1, ny
        DO 910 k = 1, nx
            rhos(k,l) = rhos1p( ss(k,l), 0.0, 0.0)
  910   CONTINUE
  900 CONTINUE

C     Compute the geostrophic velocity
      DO 1000 j = 2, ny-1
        DO 1010 i = 2, nx-1

          ub(i,j) = +g8rref*( rhos(i,j+1) - rhos(i,j-1) )/dy2
          vb(i,j) = -g8rref*( rhos(i+1,j) - rhos(i-1,j) )/dx2

 1010   CONTINUE
 1000 CONTINUE

C     Now consider the boundary conditions:
C       1-26-89:
C         At i = 1         u = v = 0.0
C         At i = nx        u = v = 0.0
C         At j = 1         u = v = 0.0
C         At j = ny normal deriv = 0.0

      DO 2000 i = 1, nx
C       BC on v at the y boundaries
        vb(i,1)  = 0.0
        vb(i,ny) = vb(i, ny-1)
C       BC on u at the y boundaries
        ub(i,1)  = 0.0
        ub(i,ny) = ub(i, ny-1)
 2000 CONTINUE

      DO 2010 j = 1, ny
C       v = 0.0 implemented 1-26-89
        vb(1,  j) = 0.0
        vb(nx, j) = 0.0
C       u = 0.0 implemented 1-26-89
        ub(1,  j) = 0.0
        ub(nx, j) = 0.0
 2010 CONTINUE

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

      DOUBLE PRECISION sigmaw, sigma, sigmap
      DOUBLE PRECISION deltaw, delta, deltap

      sigmaw = -.157406 + t*(6.793952D-2 + t*(-9.09529D-3
     1      + t*(1.001685D-4 +t*(-1.120083D-6 + 6.536332D-9*t))))

      sigma  = sigmaw +
     1    s* ( .824493 + t*(-4.0899D-3 + t*(7.6438D-5
     2                      + t*(-8.2467D-7 + t*5.3875D-9))) ) +
     3    s**1.5*( -5.72466D-3 + t*(1.0227D-4 - t*1.6546D-6)) +
     4    s*s   *( 4.8314D-4 )

C     Now compute the compressibility terms:
      deltaw = -347.79 + t*(148.4206 + t*(-2.327105 +
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
      rho = (1000. + sigma)/ (1 - p/(20000.+ deltap) )

      RETURN
      END
      FUNCTION rhos1p(s, t, p)
C     This is an approximate representation of rho, fitted over a
C       characteristic range of t, s to the complete eqn. of state.
C     Compute the deviation from the reference density.
      REAL s, t, p, rhos1p
      REAL alpha, beta, gamma
      PARAMETER (alpha = -4.5795E-2)
      PARAMETER (beta  = -6.8927E-3)
      PARAMETER (gamma =  0.80908  )
C     reference values are T= -0.5, S=34.6, P = 0.0

      rhos1p = t*(alpha + beta*t) + gamma*s

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE convec(ss, sd, nx, ny, tstep)
C     Subroutine to look for convective overturning and cabbeling.
C     Overturn added 3-9-88.
C     Uses short form entry point to rho 5-26-88.

      INTEGER nx, ny, tstep
      REAL ss(nx, ny), sd(nx, ny)

      INTEGER i, j
      LOGICAL ovrtrn, ovrlst
      SAVE    ovrlst

      ovrtrn = .FALSE.
      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
CF        rho1 = rhos1p(ss(i,j)+sd(i,j), 0.0, 0.)
CF        rho2 = rhos1p(ss(i,j)-sd(i,j), 0.0, 0.)
          IF (sd(i,j) .GT. 0.0) THEN
            sd(i,j) = 0.0
            ovrtrn = .TRUE.
          ENDIF
 1010   CONTINUE
 1000 CONTINUE

      IF (tstep .EQ. 1) GO TO 9999

      IF (ovrtrn .AND. (.NOT. ovrlst) ) THEN
        PRINT *,'Started convection at tstep',tstep
       ELSE IF ((.NOT. ovrtrn)  .AND. ovrlst ) THEN
        PRINT *,'Stopped convection at tstep',tstep
      ENDIF

 9999 CONTINUE
      ovrlst = ovrtrn

      RETURN
      END
C***********************************************************----------!!
      SUBROUTINE read2(x, nx, ny, unit, fname, ope, clos)
C     Read in a 2d, unformatted array, with external control on
C       opening and closing.

      INTEGER nx, ny, unit
      REAL x(nx, ny)
      CHARACTER*60 fname
      LOGICAL ope, clos

      IF (ope) OPEN(unit, FILE=fname, FORM='UNFORMATTED', STATUS='OLD')
      READ (unit) x
      IF (clos) CLOSE(unit, STATUS='KEEP')

      RETURN
      END
      FUNCTION yes(defalt)
C     Function to return .TRUE. if the user responds y, .FALSE. if he
C       says n, and the default value otherwise.

      LOGICAL yes, defalt
      CHARACTER resp

      READ (*,9001) resp
 9001 FORMAT(A1)

      yes = (resp.EQ.'y') .OR. (defalt .AND. resp.NE.'y'
     1                                 .AND. resp.NE.'n')

      RETURN
      END
      SUBROUTINE arset (x, nx, ny, value)
C     Set all elements of array x equal to value.

      INTEGER nx, ny
      REAL x(nx, ny), value

      INTEGER i, j

      DO 1000 j = 1, ny
        DO 1010 i = 1, nx
          x(i,j) = value
 1010   CONTINUE
 1000 CONTINUE

      RETURN
      END
