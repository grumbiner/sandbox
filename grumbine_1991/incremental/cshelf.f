      Program cshelf
C     Model bouyancy (and perhaps wind) forced continental shelf
C       motions and conditions for the polar regions.

      INTEGER nx, ny
      PARAMETER (nx = 30, ny = 27)

C     Data arrays
      REAL uc(nx, ny), vc(nx, ny), ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny), ts(nx, ny), td(nx, ny)
      REAL cs(nx, ny), cd(nx, ny)
      REAL qss(nx, ny), qsd(nx, ny)
CF    REAL qts(nx, ny), qtd(nx, ny)
CC    REAL qcs(nx, ny), qcd(nx, ny)
      REAL we(nx, ny)
      REAL h(nx, ny)

C     Physical parameters
      REAL ahm, avm, aht, ahs, avt, avs
      REAL sdref, ssref, tdref, tsref, rhoref
      REAL g, f, beta
      INTEGER xmin, xmax, ymin, ymax, strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm
      REAL dcadt, csref, cdref, lambda

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
CD    PRINT *,'Calling init'
      CALL init (uc, vc, ut, vt, ss, sd, ts, td, cs, cd,
     1           we, h, nx, ny,
     2           ahm, avm, aht, avt, ahs, avs,
     3           sdref, ssref, tdref, tsref, rhoref,
     4           g, f,
     5           delx, dely, delt, gamma, nout, nflout, ntot,
     6           xmin, xmax, ymin, ymax, qsfmax, qsfref, qsm,
     7           strspr, strsum, strfll, strwin, loy,
     8           beta, contol, itmax, lnorm,
     9           csref, cdref, lambda, dcadt                      )

C     Open the output files
CD    PRINT *,'Calling out'
      CALL outstr(uc, vc, ut, vt, ss, sd, ts, td, cs, cd, h,
     1            delx, nx, ny)
CD    CALL outdat(uc, vc, ut, vt, ss, sd, ts, td, cs, cd, h,
CD   1            delx, nx, ny)

C     Extrapolation loop
      DO 1000 i = 1, ntot
CD      PRINT *,'step ',i
CT      before = clock(0)
CD      PRINT *,'Calling ucext'
        CALL uvext (uc, vc, ss, sd, ts, td,
     1              h, nx, ny,
     2              rhoref, g, f,
     3              ahm, avm, delx, dely, delt          )
CT      after = clock(0)
CT      uvtime = uvtime + after - before

CT      before = clock(0)
CD      PRINT *,'Calling qsext'
        CALL qsext (qss, qsd, h, nx, ny, i, loy,
     1              xmin, xmax, ymin, ymax,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin              )
CD      PRINT *,'forcing, 10, 1; 10, 10; 10, 20; 10, 25'
CD      PRINT *,qss(10,1)
CD      PRINT *,qss(10,10)
CD      PRINT *,qss(10,20)
CD      PRINT *,qss(10,25)
CD      CALL qtext
CT      after = clock(0)
CT      qtime = qtime + after - before

CD      PRINT *,'calling stext'
CT      before = clock(0)
        CALL stext(uc, vc, ut, vt, we, ss, sd, qss, qsd, h,
     1             nx, ny, delx, dely, delt, sdref, ssref,
     2             gamma, ahs, avs, i)
CD      PRINT *,'finished sext'
C       Note that the extrapolation for t is not actually carried out,
C         it is currently unforced (3-29-88)
C       CALL stext(uc, vc, ts, td, qts, qtd, h, nx, ny,
C    1             delx, dely, delt, tdref, tsref, gamma,
C    2             aht, avt, i)
CD      PRINT *,'finished text'
CT      after = clock(0)
CT      sttime = sttime + after - before

CC      CALL qcext (qcs, qcd, cs, cd, h, nx, ny, i, loy,
CC   1              xmin, xmax, ymin, ymax, lambda,
CC   2              qsfmax, qsfref, qsm,
CC   2              strspr, strsum, strfll, strwin, dcadt, delt  )
CC      CALL chext(uc, vc, ut, vt, we, cs, cd, qcs, qcd, h,
CC   1             nx, ny, delx, dely, delt, cdref, csref, lambda,
CC   2             gamma, ahs, avs, i)

CD      PRINT *,'Calling utrop'
        CALL utrop (delx, dely, ahm, f, beta, g, rhoref,
     1              h, we, ut, vt, uc, vc, ss, sd, ts, td,
     2              nx, ny, contol, itmax, lnorm     )

CD      PRINT *,'calling convec'
        CALL convec(ss, sd, ts, td, cd, nx, ny, i)
CD      PRINT *,'checking for output'
        IF (MOD(i,nout) .EQ. 0) THEN
           CALL outdat (uc, vc, ut, vt, ss, sd, ts, td, cs, cd, h,
     1                  delx, nx, ny                                 )
           PRINT *,'tstep=',i
CT         PRINT *,'tstep=',i,' timing =',uvtime/1E6, sttime/1E6
        ENDIF
        IF (MOD(i,nflout) .EQ. 0) CALL outfl (uc, vc, ut, vt,
     1            ss, sd, ts, td, cs, cd, h,
     2            delx, nx, ny             )
 1000 CONTINUE

CT    PRINT *,' uv time=  ',uvtime/1E6,' st time= ', sttime/1E6
      CALL outend (uc, vc, ut, vt, ss, sd, ts, td, cs, cd, h,
     1             delx, nx, ny                                 )

      END
