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
      DOUBLE PRECISION s1, s2, s3, s4, s5, s6, sr, s1old

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

CT      CALL sconv (uc, vc, ut, vt, we, ss, sd, qss, qsd, h,
CT   1              nx, ny, delx, dely, delt, sdref, ssref,
CT   2              ahs, avs, i, s1, s2                        )
CT      WRITE (*,9015) i, (s1-s1old)/delt, s2, s1
CT      s1old = s1

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

 9015 FORMAT (I4,3D16.10)

      END
