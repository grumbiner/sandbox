C***********************************************************__________!!
      FUNCTION random(seed)
C     Robert Grumbine 2 May 1995
      IMPLICIT none

      INTEGER seed
      REAL random
      INTEGER modulus, mult, incr
      PARAMETER ( modulus = 65536 )
      PARAMETER ( mult    = 25173 )
      PARAMETER ( incr    = 13849 )
 
      random = FLOAT(seed)/FLOAT(modulus)
      seed   = MOD(mult*seed+incr, modulus)
 
C      Algorithm from Grogono, p. 118
C      Random is in range [0., 1.)
 
      RETURN
      END
