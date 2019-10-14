C Dmitry Chalikov author 17 December 1997
C Subroutine QC calculates actual values of salinity based on
C static stability considerations
C Subroute calls equation of state RONN 
C and inverse equation of state SALNN
C Both are based on NN approximation of exact eqs of state
C*********************************T*S*PROFILES*QUALITY*CONTROL*********
C INPUT:
C L - total number of levels (=34 for OTIS)
C LA - actual number of levels 
C Z(L) - array of depths (positive, growing)
C T(L) - array of temperature
C S(L) - array of salinity
C GAP - missing value (for example, GAP=-999.)
C OUTPUT:
C T - returned array of T
C S - returned array of S
C______________________________________________________________________
      subroutine QCTS(L, LA, Z, T, S, GAP, modcode)
      !real T(L),S(L),Z(L),ZI(L),TI(L),SI(L),R(L)
      real T(L),S(L),Z(L),R(90) !number used for f77 compliance
      real RON(3),SAL(3)
      integer modcode
Checkup stability and T_S correction
      modcode = 0
      do k=LA,2,-1
        RON(1)=T(k-1)
        RON(2)=S(k-1)
        RON(3)=Z(k-1)
        R(k-1)=RONN(RON)
        RON(1)=T(k)
        RON(2)=S(k)
        RON(3)=Z(k)
        R(k)=RONN(RON)
        if (R(k).lt.R(k-1)) then
          R(k-1)=R(k)
          SAL(1)=T(k-1)
          SAL(2)=R(k-1)
          SAL(3)=Z(k-1) 
          S(k-1)=SALNN(SAL)
CD          NPC=NPC+1
          modcode = modcode + 1
        endif
      enddo !k
C      enddo !it
      return
      end
