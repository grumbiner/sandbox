      Program main
C     Model bouyancy (and perhaps wind) forced continental shelf
C       motions and conditions for the polar regions.

      INTEGER nx, ny
      PARAMETER (nx = 20, ny = 20)

C     Data arrays
      REAL uc(nx, ny), vc(nx, ny)
      REAL ut(nx, ny), vt(nx, ny)
      REAL ss(nx, ny), sd(nx, ny), ts(nx, ny), td(nx, ny)
      REAL qss(nx, ny), qsd(nx, ny)
      REAL qts(nx, ny), qtd(nx, ny)
      REAL uwind(nx, ny), vwind(nx, ny), we(nx, ny)
      REAL h(nx, ny), lnh(nx, ny)

C     Physical parameters
      REAL ahm, avm, aht, ahs, avt, avs
      REAL sdref, ssref, sref, tdref, tsref, tref, rhoref, g, f
      INTEGER xcen, xlen, ycen, ylen
      INTEGER strspr, strsum, strfll, strwin
      REAL qsfmax, qsfref, qsm
      REAL beta

C     Numerical parameters
      REAL delx, dely, delt, gamma
      INTEGER nout, nflout, ntot, loy
      INTEGER lnorm, itmax
      REAL contol, crit

C     Local variables
      INTEGER i
      CHARACTER*60 fname
      REAL delu(nx, ny)

C     testin parameters
      REAL s1, s2, s3, s4, s5, s6, sr, s1old

C     BEGIN EXECUTION

      i = 0
      s1old = 0.0
C     Initialize the variables
      PRINT *,'Name of the mean value output file?'
      READ (*,9100) fname
      OPEN (99, FILE=fname, FORM='FORMATTED', STATUS='NEW')

CD    PRINT *,'Calling init'
      CALL init (uc, vc, ut, vt, ss, sd, ts, td,
     1           uwind, vwind, we, h, lnh, nx, ny,
     2           ahm, avm, aht, avt, ahs, avs,
     3           sref, sdref, ssref, tref, tdref, tsref, rhoref,
     4           g, f,
     5           delx, dely, delt, gamma, nout, nflout, ntot,
     6           xcen, xlen, ycen, ylen, qsfmax, qsfref, qsm,
     7           strspr, strsum, strfll, strwin, loy,
     8           beta, contol, itmax, lnorm )

      CALL outstr(uc, vc, ut, vt, ss, sd, ts, td, nx, ny,
     1            rhoref, sref, tref, i)

CD    CALL outdat(uc, vc, ut, vt, ss, sd, ts, td, nx, ny,
CD   1            rhoref, sref, tref, i)

C     Extrapolation loop
      DO 1000 i = 1, ntot
CD      PRINT *,'uvext',i
        CALL uvext (uc, vc, ss, sd, ts, td,
     1              uwind, vwind, h, lnh, nx, ny,
     2              sref, tref, rhoref, g, f,
     3              ahm, avm, delx, dely, delt          )

CD      PRINT *,'gext',i
        CALL qsext (qss, qsd, nx, ny, i, loy,
     1              xcen, xlen, ycen, ylen,
     2              qsfmax, qsfref, qsm, delx, dely,
     3              strspr, strsum, strfll, strwin)

        CALL sconv (ss, sd, qss, h, we, uc, vc, ut, vt, nx, ny,
     1              delx, dely, ahs, gamma, ssref,
     2              s1, s2, s3, s4, s5, s6, sr)
        WRITE (99,9015) i, (s1-s1old)/delt, s2, s3, s4, s5, sr
        WRITE (* ,9015) i, (s1-s1old)/delt, s2, s3, s4, s5, sr
 9015   FORMAT (I4,E13.6,4E10.3,E13.6)
        s1old = s1

CD      PRINT *,'stext',i
        CALL stext (uc, vc, ut, vt, we, ss, sd, qss, qsd, h, lnh,
     1              nx, ny, delx, dely, delt, sdref, ssref, sref,
     2              gamma, ahs, avs, i                  )

CD      PRINT *,'convec',i
        CALL convec(ss, sd, ts, td, nx, ny, sref, tref, i)

        IF (MOD(i, nout) .EQ. 0) THEN
          CALL outdat(uc, vc, ut, vt, ss, sd, qss, qsd, nx, ny,
     1                rhoref, sref, tref, i)
        ENDIF
 1000 CONTINUE

 9001 FORMAT (E13.5)

 9100 FORMAT (A60)

      END
