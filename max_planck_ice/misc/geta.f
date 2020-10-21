      PROGRAM geta

C      extract the concentration from a restart file
      INTEGER l, lp, m, mp
      PARAMETER (l = 76)
      PARAMETER (lp = l+1)
      PARAMETER (m = 92)
      PARAMETER (mp = m+1)
      REAL u(l,m,3)
      REAL a(lp, mp, 2)

      OPEN (10, FILE='restart.830', FORM='UNFORMATTED', STATUS='OLD')
      READ (10) u
      READ (10) u
      READ (10) a
      READ (10) a

      OPEN (11, FILE='conc.830', FORM='UNFORMATTED', STATUS='NEW')
      WRITE (11) a

      END
