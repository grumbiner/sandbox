      PROGRAM bsreform
      REAL f1, f2, f3, f4, f5, f6, f7
      INTEGER tdum
      OPEN (10, FILE="bsice.dat", FORM="FORMATTED", STATUS="OLD")

 1000 CONTINUE
        READ (10, *, END=2000) tdum, f1, f2, f3, f4, f5, f6, f7
        WRITE (*, 9001) tdum, NINT(.5+4*f1), NINT(.5+4*f2), 
     1       NINT(.5+4*f3), 
     1       NINT(.5+4*f4), NINT(.5+4*f5), NINT(.5+4*f6), NINT(.5+4*f7)
        GO TO 1000
 2000 CONTINUE
 9001 FORMAT (I3, 7I5)
CD7F7.3)

      END
