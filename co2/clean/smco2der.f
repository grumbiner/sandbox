C*************************************************----------++++++++++!!
      SUBROUTINE smco2der(t, x, dxdt)
C     To be called by the Numerical Recipes Runge-Kutta
C       routines.  Slightly different calling pattern,
C       since I don't like common blocks.  This should
C       eventually be made into a function (or set
C       thereof).

      IMPLICIT NONE

C     Declare arguments:
      REAL x(12), t, dxdt(12)

C     Declare parameters.  This is long an messy.
      REAL secpyr
      PARAMETER (secpyr = 365.2422*86400.)

C     Phosphorous parameters:
      REAL redo2, redfld, decay, produc
      PARAMETER (redfld =  130.)  !P:C
      PARAMETER (redo2  = -170.)  !P:O2
      PARAMETER (decay  =  577.)
      PARAMETER (produc =  0.5 )  !Carbon production, moles C/m2/yr

C     Carbon 14 Parameters:
      REAL carbas, delair, delpar, lamc14, tco2(4)
      PARAMETER (carbas =  produc)  !Air-sea carbon pump, moles C/m2/yr
      PARAMETER (delpar = -70.E-3)  ! delta 14C in biogenic particles
      PARAMETER (lamc14 = 3.83E-12) ! 14C decay constant
      REAL c14sat, c14la, c14ln, c14ls, diff14
      REAL aafudg, nafudg

C     O2 Parameters
      REAL mixed, d, h, zfa, pref
      REAL d0, d1, h0, h1
      PARAMETER (mixed = 100.)
      PARAMETER (zfa   = 40.E-6)
      PARAMETER (pref  = 0.20 )
      PARAMETER (d0    = 1.2E-9)
      PARAMETER (d1    = 27.1E-3)
      PARAMETER (h0    = 1.69E-3)
      PARAMETER (h1    =-20.6E-3)
      REAL o2sata, o2satn, o2sats
      REAL o2la, o2ln, o2ls
      REAL mxpole, qspo2
  
      REAL zeta, faa, fan, faw, fas
      PARAMETER (zeta = 0.75)
      PARAMETER (faa  = 0.45 /3.5)
      PARAMETER (fan  = 0.075/3.5)
      PARAMETER (faw  = 1.-faa-fan)
      PARAMETER (fas  = faw)

      REAL vt, va, vn, vw, vs
      PARAMETER (vt = 1.37E18) !m^3, Broecker and Peng, 1982
      PARAMETER (va = vt*faa)
      PARAMETER (vn = vt*fan)
      PARAMETER (vw = vt*faw*zeta)
      PARAMETER (vs = vt*fas*(1.-zeta))

      REAL at, aa, an, aw, as
      PARAMETER (at = 3.61E14) !m^2, Gill, 1982
      PARAMETER (aa = at*faa)
      PARAMETER (an = at*fan)
      PARAMETER (aw = at*faw)
      PARAMETER (as = at*fas)

      REAL     aap, anp
C     Area perpendicular to the other areas.
C       For now, neglect.
      PARAMETER (aap = 2.57E7*vt/at)
      PARAMETER (anp = aap/6.)

      REAL diffh, diffv, bwmult
      PARAMETER (diffv = 1.5E-5)
      PARAMETER (diffh = diffv*1.0E6)
      PARAMETER (bwmult = 2.5)

      REAL la, ln, lw, ls
      PARAMETER (la = 1.E6)
      PARAMETER (ln = 500.E3)
      PARAMETER (lw = 6.4E6)
      PARAMETER (ls = 6.4E6)

      REAL href
      PARAMETER (href = vt/at)

C     Declare the flux variables of aabw and nadw
      REAL fluxaa, fluxna

C     Temporary variables used in the solutions:
      REAL betaa, betan
      REAL gammas, gammaw
      REAL time

C     Source terms
      REAL qwp, qsp, qsc14, qwc14, qatc14

C     Now declare functions (rhs of equations)
      INTEGER nchem
      PARAMETER (nchem = 3)
      REAL rhsaa(nchem), rhsna(nchem), rhswo(nchem), rhssu(nchem)

      REAL asdf

      DATA tco2(1) /2200.E-6/
      DATA tco2(2) /2200.E-6/
      DATA tco2(3) /2200.E-6/
      DATA tco2(4) /2200.E-6/

C*************************************************----------++++++++++!!
C     Deep water flux terms
      time = t/secpyr
      CALL fluxes(fluxaa, fluxna, time)
      delair = 2.5E-3*(-time)/1.E3
      delair = 0.0

C     Compute the diffusive constants (probably don't need to recompute.
      betaa = diffh* aap/la
      betan = diffh* anp/ln

      gammas = diffv* as*2./href/(1.-zeta)
      gammaw = diffv* as*2./href/(1.-zeta)

C     Biological Source
C     The exponential term reflects the particulate dissolution curve,
C       integrated to the boundary between upper and lower layers.

C     Phosphorous
      qwp = produc*EXP(-(1.-zeta)*href/decay)/decay/redfld/secpyr*aw
      qsp = -qwp*as/aw

C     Carbon 14
      qwc14 = qwp*redfld*(delpar+delair - x(7))
      qsc14 = ABS(qsp)*redfld*(delpar+delair - x(8))

C     Atmospheric input
C     C-14
      qatc14 = carbas/secpyr
C     O2
      d = d0
      h = h0
      o2sata = pref*h
      o2satn = pref*h
      h = h0*EXP(h1*20.)
      o2sats = pref*h
      qspo2 = redo2*(produc/decay/secpyr/redfld*aw - qwp)
      mxpole = mixed

C     Radio-decay -- put in with the RHS terms.  Only reasonable way?

C*************************************************----------++++++++++!!
C     RHS:
C     Phosphate
      rhsaa(1) = ( fluxaa*(x(3)-x(1))
     1            + betaa*((1.-zeta)*(x(4)-x(1))+zeta*(x(3)-x(1)))
     2                                     ) /va

      rhsna(1) = ( fluxna*(x(4)-x(2))
     1            - betan*((1.-zeta)*(x(4)-x(2))+zeta*(x(3)-x(2))) 
     2                                     )/vn

      rhswo(1) = ( fluxaa*(x(1)-x(3))
     1            +fluxna*(x(2)-x(3))
     2            - betaa*zeta*(x(3)-x(1))
     3            + betan*zeta*(x(3)-x(2))
     4            + gammaw*(x(4)-x(3))
     5        + qwp                       )
     6       /vw

      rhssu(1) = (
     1            - betaa*(x(4)-x(1))*(1.-zeta)
     2            + betan*(x(4)-x(2))*(1.-zeta)
     3            +fluxna*(x(3)-x(4)) 
     4            +gammas*(x(3)-x(4))
     5         + qsp                       )
     6         /vs

C     delta C14
      aafudg = 0.27
      nafudg = 0.93
      c14sat = delair
      diff14 = diffv/asdf
      c14la = (x(5) - (mxpole/href)*c14sat ) * (href / (href-mxpole))
      c14ln = (x(6) - (mxpole/href)*c14sat ) * (href / (href-mxpole))
      c14ls = (x(8) - mixed*c14sat/href/(1.-zeta)) *(1.-zeta)*href
     1 /( (1.-zeta)*href - mixed)

CD      PRINT *,'fluxaa, fluxna, c14sat, c14la, bwmult, betaa, 
CD     1                         zeta, diffv, aa, an, mixed, ba',
CD     2           fluxaa, fluxna, c14sat, c14la, bwmult, betaa,
CD     1                         zeta, diffv, aa, an, mixed, va
CD      PAUSE
CD      STOP
      rhsaa(2) = ( fluxaa*tco2(1)*(x(7)-x(5))
     2            + 0.375*fluxaa*tco2(1)*(c14sat - c14la)*2.*aafudg
     1            + betaa*((1.-zeta)*(x(8)*tco2(4)-x(5)*tco2(1))
     2                     +   zeta *(x(7)*tco2(3)-x(5)*tco2(1)) )
     4            + diffv*aa*2./mixed*tco2(1)*(c14sat-c14la)*aafudg
     5                                              ) /va
     4            - lamc14*tco2(1)*(1.+x(5))

      rhsna(2) = ( fluxna*tco2(2)*(x(8)-x(6))
     2           + fluxna*tco2(2)*(c14sat - c14ln)*2.*nafudg
     1            - betan*((1.-zeta)*(x(8)*tco2(4)-x(6)*tco2(2))
     2                    +    zeta* (x(7)*tco2(3)-x(6)*tco2(2)))
     4            + diffv*an*2./mixed*tco2(2)*(c14sat-c14ln)*nafudg
     5                                              )/vn
     4            - lamc14*tco2(2)*(1.+x(6))

      rhswo(2) = ( fluxaa*tco2(3)*(x(5)-x(7))
     1            +fluxna*tco2(3)*(x(6)-x(7))
     2            - betaa*zeta*(x(7)*tco2(3)-x(5)*tco2(1))
     3            + betan*zeta*(x(7)*tco2(3)-x(6)*tco2(2))
     4            +diffv* as*2./href/(1.-zeta)*
     4                       (c14ls*tco2(4)-x(7)*tco2(3))
     5            + qwc14                       )
     6       /vw
     7            -lamc14*tco2(3)*(1.+x(7))

      rhssu(2) = (
     1            - betaa*(x(8)*tco2(4)-x(5)*tco2(1))*(1.-zeta)
     2            + betan*(x(8)*tco2(4)-x(6)*tco2(1))*(1.-zeta)
     3            +fluxna*tco2(4)*(x(7)-x(8))
     4            +diffv* as*2./href/(1.-zeta)*
     4                    (x(7)*tco2(3)-c14ls*tco2(4))
     5            +diffv*as*2./mixed*tco2(4)*(c14sat-c14ls)/3.0
     4            + qsc14
     5         ) /vs
     6            -lamc14*tco2(4)*(1.+x(8))

C     Oxygen:
      o2la = (x(9) -  (mxpole/href)*o2sata ) * (href / (href-mxpole))
      o2ln = (x(10) - (mxpole/href)*o2satn ) * (href / (href-mxpole))
      o2ls = (x(12) - mixed*o2sats/href/(1.-zeta)) *(1.-zeta)*href
     1 /( (1.-zeta)*href - mixed)

      rhsaa(3) = ( fluxaa*(x(11)-x(9))
     1            + betaa*((1.-zeta)*(x(12)-x(9))+zeta*(x(11)-x(9)))
     2            + 0.375*fluxaa*(o2sata - o2la)*bwmult
     3            + diffv*aa*2./mixed*(o2sats - o2la)
     4                            ) /va

      rhsna(3) = ( fluxna*(x(12)-x(10)) 
     1            - betan*((1.-zeta)*(x(12)-x(10))+zeta*(x(11)-x(10)))
     2            + fluxna*(o2satn - o2ln)*bwmult
     3            + diffv*an*2./mixed*(o2sats - o2ln)
     3                              ) /vn

      rhswo(3) = ( fluxaa*(x(9)-x(11))
     1            +fluxna*(x(10)-x(11))
     2            - betaa*zeta*(x(11)-x(9))
     3            + betan*zeta*(x(11)-x(10))
     4            + gammaw*(o2ls-x(11))
     5            + qwp*redo2                  )
     6       /vw

      gammas = diffv* as*2./mixed
      rhssu(3) = (
     1            - betaa*(x(12)-x(9))*(1.-zeta)
     2            + betan*(x(12)-x(10))*(1.-zeta)
     3            + fluxna*(x(11)-x(12))
     4            + gammaw*(x(11)-o2ls)
     5            + gammas*(o2sats - o2ls)
     5            + qspo2
     7                    )
     6         /vs

      dxdt(1) = rhsaa(1)
      dxdt(2) = rhsna(1)
      dxdt(3) = rhswo(1)
      dxdt(4) = rhssu(1)
      dxdt(5) = rhsaa(2)/tco2(1)
      dxdt(6) = rhsna(2)/tco2(2)
      dxdt(7) = rhswo(2)/tco2(3)
      dxdt(8) = rhssu(2)/tco2(4)
      dxdt(9) = rhsaa(3)
      dxdt(10) = rhsna(3)
      dxdt(11) = rhswo(3)
      dxdt(12) = rhssu(3)

      RETURN
      END
