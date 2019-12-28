      SUBROUTINE gasses(gas, xu, q)
C     Compute the surfaxe flux for arbitrary gasses into the ocean
C       from the atmosphere.
C     This could actually be a function.
      IMPLICIT none
 
      INTEGER gas
      DOUBLE PRECISION zf, xu, q
      PARAMETER (zf = 40.D-6)
 
      INTEGER ngas
      PARAMETER (ngas = 9)
      REAL diff(ngas)
      REAL henry(ngas)
      REAL pref(ngas)
 
      INTEGER He, Ne, N2, O2, Ar, Kr, Xe, CO2, N2O
      PARAMETER (He  = 1)
      PARAMETER (Ne  = 2)
      PARAMETER (N2  = 3)
      PARAMETER (O2  = 4)
      PARAMETER (Ar  = 5)
      PARAMETER (Kr  = 6)
      PARAMETER (Xe  = 7)
      PARAMETER (CO2 = 8)
      PARAMETER (N2O = 9)
	  
	  SAVE diff, henry, pref
 
C     Molecular diffusivity across surface film.  m2/s.
      diff(He)  = 2.0E-9 
      diff(Ne)  = 1.4E-9 
      diff(N2)  = 1.1E-9 
      diff(O2)  = 1.2E-9 
      diff(Ar)  = 0.8E-9 
      diff(Kr)  = 0.7E-9 
      diff(Xe)  = 0.7E-9 
      diff(CO2) = 1.0E-9
      diff(N2O) = 1.0E-9
 
      henry(He)  = 0.34E-3 
      henry(Ne)  = 0.44E-3 
      henry(N2)  = 0.80E-3 
      henry(O2)  = 1.69E-3 
      henry(Ar)  = 1.83E-3 
      henry(Kr)  = 3.8E-3 
      henry(Xe)  = 8.4E-3 
      henry(CO2) = 63.E-3
      henry(N2O) = 47.E-3
 
C     Equilibrium atmospheric mixing ratios.  (Not pressures)
C       Note that pref(CO2) is fixed at the pre-industrial value. 
      pref(He)  = 5.2E-6 
      pref(Ne)  = 1.8E-5 
      pref(N2)  = 0.781 
      pref(O2)  = 0.209 
      pref(Ar)  = 9.3E-3 
      pref(Kr)  = 1.1E-6 
      pref(Xe)  = 8.6E-8 
      pref(CO2) = 280.E-6
      pref(N2O) = 3.0E-7
 
      q = diff(gas)*(pref(gas)*henry(gas)-xu)/zf
	  
CD	  PRINT *,'q, diff, pref, henry, xu, zf'
CD	  PRINT *,q, diff(gas), pref(gas), henry(gas), xu, zf
       
      RETURN
      END
