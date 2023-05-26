      SUBROUTINE ICERESTART(h, a, hsn, tice, qt, qs, qh, qtb,
     1                      qsb, qhb, qdt, qds, ticm, u, v,
     2                      nx, ny, fname)
      INTEGER nx, ny
      CHARACTER*60 fname
      REAL h(nx, ny), a(nx, ny), hsn(nx, ny), tice(nx, ny)
      REAL qt(nx, ny), qs(nx, ny), qh(nx, ny)
      REAL qtb(nx, ny), qsb(nx, ny), qhb(nx, ny)
      REAL qds(nx, ny), qdt(nx, ny)
      REAL ticm(nx, ny, 7)
      REAL u(nx-1, ny-1), v(nx-1, ny-1)


      OPEN (10, FILE=fname, FORM="UNFORMATTED", STATUS="OLD")

      READ (10) h
      READ (10) a
      READ (10) hsn
      READ (10) tice
      READ (10) qt
      READ (10) qs
      READ (10) qh
      READ (10) qtb
      READ (10) qsb
      READ (10) qhb
      READ (10) qdt
      READ (10) qds
      READ (10) ticm
      READ (10) u
      READ (10) v

      CLOSE(10)

      RETURN
      END
