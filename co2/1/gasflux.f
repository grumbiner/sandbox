C*************************************************----------++++++++++!!
      FUNCTION gasses(gas, xu, pref, temper, rsalt, wind)
C     Compute the surface flux for arbitrary gasses into the ocean
C       from the atmosphere.
C     This could actually be a function.
      IMPLICIT none

      REAL revell
      COMMON /carbon/ revell

      INTEGER gas
      DOUBLE PRECISION zf, xu, q, gasses, pref
      DOUBLE PRECISION temper, wind
      PARAMETER (zf = 40.D-6)
      REAL rsalt

      INCLUDE "chmptr.inc"

      REAL diff(nchem,2)
      REAL henry(nchem,2)
      REAL d, h, zfa
      REAL tk

C     Parameters for henry's law constant for co2.
      REAL a1, a2, a3, b1, b2, b3
      PARAMETER (a1 = -60.2409)
      PARAMETER (a2 =  93.4517)
      PARAMETER (a3 =  23.3585)
      PARAMETER (b1 =   0.023517)
      PARAMETER (b2 = - 0.023656)
      PARAMETER (b3 =   0.0047036)

      SAVE diff, henry

C     Molecular diffusivity across surface film.  m2/s.
      DATA diff(He,1)  / 2.0E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(Ne,1)  / 1.4E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(N2,1)  / 1.1E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(O2,1)  / 1.2E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(Ar,1)  / 0.8E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(Kr,1)  / 0.7E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(Xe,1)  / 0.7E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(CO2,1) / 1.0E-9/ !B+P, 1982, pp. 119 0 !C
      DATA diff(N2O,1) / 1.0E-9/ !B+P, 1982, pp. 119 0 !C
C     Include terms for the others, formally.
      DATA diff(temp,1) / 0.0/
      DATA diff(salt,1) / 0.0/
      DATA diff(NO3,1)  / 0.0/
      DATA diff(PO4,1)  / 0.0/
      DATA diff(SiO2,1) / 0.0/
      DATA diff(dc14,1) / 0.0/
      DATA diff(tco2,1) / 1.0E-9/
      DATA diff(alk,1)  / 0.0/
C     Now include an exponential temperature effect, BG derived.
      DATA diff(He,2)  / 28.9E-3/
      DATA diff(Ne,2)  / 28.9E-3/
      DATA diff(N2,2)  / 26.9E-3/
      DATA diff(O2,2)  / 27.1E-3/
      DATA diff(Ar,2)  / 26.2E-3/
      DATA diff(Kr,2)  / 28.9E-3/
      DATA diff(Xe,2)  / 28.9E-3/
      DATA diff(CO2,2) / 26.7E-3/
      DATA diff(N2O,2) / 28.9E-3/
C     Include terms for the others, formally.
      DATA diff(temp,2) / 0.0/
      DATA diff(salt,2) / 0.0/
      DATA diff(NO3,2)  / 0.0/
      DATA diff(PO4,2)  / 0.0/
      DATA diff(SiO2,2) / 0.0/
      DATA diff(dc14,2) / 0.0/
      DATA diff(tco2,2) / 26.7E-3/
      DATA diff(alk,2)  / 0.0/

      DATA henry(He,1)  / 0.34E-3/ !B+P, 1982, pp. 112 0 !C
      DATA henry(Ne,1)  / 0.44E-3/ !B+P, 1982, pp. 112 0 !C
      DATA henry(N2,1)  / 0.80E-3/ !B+P, 1982, pp. 112 0 !C
      DATA henry(O2,1)  / 1.69E-3/ !B+P, 1982, pp. 112 0 !C
      DATA henry(Ar,1)  / 1.83E-3/ !B+P, 1982, pp. 112 0 !C
      DATA henry(Kr,1)  / 3.8E-3/  !B+P, 1982, pp. 112 0 !C
      DATA henry(Xe,1)  / 8.4E-3/  !B+P, 1982, pp. 112 0 !C
      DATA henry(CO2,1) / 63.E-3/  !B+P, 1982, pp. 112 0 !C
      DATA henry(N2O,1) / 47.E-3/  !B+P, 1982, pp. 112 0 !C
      DATA henry(temp,1) / 1.0/
      DATA henry(salt,1) / 1.0/
      DATA henry(NO3,1)  / 0.0/
      DATA henry(PO4,1)  / 0.0/
      DATA henry(SiO2,1) / 0.0/
      DATA henry(dc14,1) / 1.0/
      DATA henry(tco2,1) / 1.0/
      DATA henry(alk,1)  / 1.0/
C     Now include an exponential temperature effect, BG derived.
      DATA henry(He,2)  / - 2.53E-3/
      DATA henry(Ne,2)  / - 7.22E-3/
      DATA henry(N2,2)  / -18.8E-3 /
      DATA henry(O2,2)  / -20.6E-3/
      DATA henry(Ar,2)  / -20.1E-3/
      DATA henry(Kr,2)  / -24.7E-3/
      DATA henry(Xe,2)  / -27.9E-3/
      DATA henry(CO2,2) / -32.3E-3/
      DATA henry(N2O,2) / -33.6E-3/
      DATA henry(temp,2) / 0.0/
      DATA henry(salt,2) / 0.0/
      DATA henry(NO3,2)  / 0.0/
      DATA henry(PO4,2)  / 0.0/
      DATA henry(SiO2,2) / 0.0/
      DATA henry(dc14,2) / 0.0/
      DATA henry(tco2,2) / 0.0/
      DATA henry(alk,2)  / 0.0/

      zfa = zf*(6./wind)
      d = diff(gas,1)* EXP(diff(gas,2 )*temper)
      h = henry(gas,1)*EXP(henry(gas,2)*temper)

      IF (gas .EQ. tco2 .OR. gas .EQ. CO2) THEN
        tk = 273.15 + temper
        h = a1 + a2*(100./tk) + a3*LOG(tk/100.)
     1     +rsalt*(b1 + b2*(tk/100.)
     2                   + b3*(tk/100.)*(tk/100.) )
        h = EXP(h)
        q = d*(pref*h-xu)/zfa
        gasses = q
        RETURN
CD        PRINT *,'co2 h=',h
      ENDIF

      q = d*(pref*h-xu)/zfa
      gasses = q

      RETURN
      END
