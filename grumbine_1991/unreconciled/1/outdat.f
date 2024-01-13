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
