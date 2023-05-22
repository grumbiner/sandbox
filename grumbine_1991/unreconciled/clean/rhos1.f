
      FUNCTION RHOS1(S, T, P)
C     THIS IS AN APPROXIMATE REPRESENTATION OF RHO, FITTED OVER A
C       CHARACTERISTIC RANGE OF T, S TO THE COMPLETE EQN. OF STATE.
      REAL S, T, P, RHOS1
      REAL RHONOT, ALPHA, BETA, GAMMA
      PARAMETER (ALPHA = -8.0520E-2)
      PARAMETER (BETA  = -6.3285E-3)
      PARAMETER (GAMMA =  0.80112  )
      PARAMETER (RHONOT= 1029.2676 )
C     REFERENCE VALUES ARE T=+1.5, S=35.1

      RHOS1 = RHONOT + T*(ALPHA + BETA*T) + GAMMA*S

      RETURN
      END
