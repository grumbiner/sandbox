      SUBROUTINE buoyck( id, code, rlat, rlong, date, dp, p, dt, t,
     1  ddir, dir, dsp, sp, dsst, sst,
     2  latok, lonok, pok, tok, dirok, spok, sstok )

C     Check a buoy entry for correctness (unflagged values)

      IMPLICIT none

      INCLUDE "buoy.inc"
      REAL rlat, rlong

      LOGICAL latok, lonok, pok, tok, dirok, spok, sstok

      latok = .FALSE.
      lonok = .FALSE.
      pok   = .FALSE.
      tok   = .FALSE.
      dirok = .FALSE.
      spok  = .FALSE.
      sstok = .FALSE.

      IF ( ABS(rlat)  .LT. 90.1 )  latok = .TRUE.
      IF ( ABS(rlong) .LT. 360.1)  lonok = .TRUE. 
      IF ( p   .NE. DATFLG ) pok   = .TRUE.
      IF ( t   .NE. DATFLG ) tok   = .TRUE.
      IF ( dir .NE. DATFLG ) dirok = .TRUE.
      IF ( sp  .NE. DATFLG ) spok  = .TRUE.
      IF ( sst .NE. DATFLG ) sstok = .TRUE.

      RETURN
      END
