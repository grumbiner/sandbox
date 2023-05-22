      Program cshelf2
C     Model bouyancy (and perhaps wind) forced continental shelf
C       motions and conditions for the polar regions.

      INTEGER nx, ny
      PARAMETER (nx = 30, ny = 30)

C     Data arrays
      REAL uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny), ts(nx, ny), td(nx, ny)
      REAL cs(nx, ny), cd(nx, ny)
      REAL qss(nx, ny), qsd(nx, ny)
      REAL qts(nx, ny), qtd(nx, ny)
      REAL qcs(nx, ny), qcd(nx, ny)
      REAL we(nx, ny)
      REAL h(nx, ny)

C     Physical parameters
      REAL ahm, avm, aht, ahs, avt, avs
      REAL sdref, ssref, tdref, tsref, rhoref
      REAL g, f, beta
      INTEGER xmin, xmax, ymin, ymax, strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm
      REAL c0, dcadt, csref, cdref, lambda

C     Numerical parameters
      REAL delx, dely, delt, gamma
      REAL contol
      INTEGER itmax, lnorm
      INTEGER nout, nflout, ntot, loy

C     Local variables
      INTEGER i
      CHARACTER*60 fname
CT    For timing:
CT    INTEGER before, after, clock
CT    INTEGER uvtime, sttime, qtime

C     BEGIN EXECUTION

CT    Initialize the timing variables:
CT    uvtime = 0
CT    sttime = 0
CT    qtime  = 0

      i = 0
C     Initialize the variables
      CALL init (uc, vc, ut, vt, ss, sd, ts, td, cs, cd,
     1           we, h, nx, ny,
     2           ahm, avm, aht, avt, ahs, avs,
     3           sdref, ssref, tdref, tsref, rhoref,
     4           g, f,
     5           delx, dely, delt, gamma, nout, nflout, ntot,
     6           xmin, xmax, ymin, ymax, qsfmax, qsfref, qsm,
     7           strspr, strsum, strfll, strwin, loy,
     8           beta, contol, itmax, lnorm,
     9           csref, cdref, lambda, dcadt, c0                  )

C     Open the output files
      CALL outstr(uc, vc, ut, vt, ss, sd, ts, td, cs, cd, h,
     1            delx, nx, ny)

C     Extrapolation loop
      DO 1000 i = 1, ntot
        CALL qsext (qss, qsd, h, nx, ny, i, loy,
     1              xmin, xmax, ymin, ymax,
     2              qsfmax, qsfref, qsm, delx, dely,
     2              strspr, strsum, strfll, strwin              )

CD      PRINT *,'checking for output'
        IF (MOD(i,nout) .EQ. 0) THEN
           CALL outdat (uc, vc, qss, qsd, ss, sd, ts, td, cs, cd, h,
     1                  delx, nx, ny                                 )
           PRINT *,'tstep=',i
CT         PRINT *,'tstep=',i,' timing =',uvtime/1E6, sttime/1E6
        ENDIF
        IF (MOD(i,nflout) .EQ. 0) CALL outfl (uc, vc, qss, qsd,
     1            ss, sd, ts, td, cs, cd, h,
     2            delx, nx, ny             )
 1000 CONTINUE

CT    PRINT *,' uv time=  ',uvtime/1E6,' st time= ', sttime/1E6
      CALL outend (uc, vc, qss, qsd, ss, sd, ts, td, cs, cd, h,
     1             delx, nx, ny                                 )

      END
