C 9001 FORMAT (A7, I4, I5, I6, I9, 5(I5, I4) )

      CHARACTER*7 id
      INTEGER code, lat, long, date, dp, p, dt, t, ddir, dir, dsp, sp
      INTEGER dsst, sst

      INTEGER DB, SHIP, FB, CMAN
      PARAMETER (DB   = 562)
      PARAMETER (SHIP = 522)
      PARAMETER (FB   = 561)
      PARAMETER (CMAN = 521)

      INTEGER ENDDAT, RERROR
      PARAMETER (ENDDAT = 3)
      PARAMETER (RERROR = 2)

      INTEGER DATFLG, DELFLG
      PARAMETER (DATFLG =  -99)
      PARAMETER (DELFLG = 9999)

      REAL kmtonm, toler
      PARAMETER (kmtonm = 1.852)
      PARAMETER (toler  = 55.  )

