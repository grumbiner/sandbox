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
