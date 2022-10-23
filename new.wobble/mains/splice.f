      PROGRAM splic
      REAL j1, u1, v1, j2
      DOUBLE PRECISION r, rdot

      OPEN (10, FILE="utmp", status="old")
      OPEN (11, FILE="radtmp", status="old")

      DO i = 1, 55516
        READ (10, *) j1, u1, v1
        READ (11, *) j2, r, rdot
        WRITE (*,*) j2, u1, v1, r, rdot
      ENDDO

      STOP
      END 
