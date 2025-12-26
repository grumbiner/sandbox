subroutine one_adiabatic_eta_step(t_eta0,q_eta0,q2_eta0,cwm_eta0,u_eta0,v_eta0,peta_eta0, &
                                  t_eta1,q_eta1,q2_eta1,cwm_eta1,u_eta1,v_eta1,peta_eta1, &
                                  div0_eta,div1_eta, &
                                  dt_eta,imeta,jmeta,lmeta)

!  do one adiabatic time-step of the eta model

! input:   t_eta0, etc.:  prognostic eta grid variables at time t
! output:  t_eta1, etc.:  same at time t + dt_eta
! output:  dt_eta:        time step used by eta model, in seconds.

  include "CTLBLK.comm"
  include "PARMETA.comm"
  include "VRBLS.comm"
  include "PVRBLS.comm"
  include "CONTIN.comm"
  include "CLDWTR.comm"
  include "mpp.h"

  real(4) t_eta0(imeta,jmeta,lmeta),q_eta0(imeta,jmeta,lmeta)
  real(4) q2_eta0(imeta,jmeta,lmeta),cwm_eta0(imeta,jmeta,lmeta)
  real(4) u_eta0(imeta,jmeta,lmeta),v_eta0(imeta,jmeta,lmeta),peta_eta0(imeta,jmeta)
  real(4) t_eta1(imeta,jmeta,lmeta),q_eta1(imeta,jmeta,lmeta)
  real(4) q2_eta1(imeta,jmeta,lmeta),cwm_eta1(imeta,jmeta,lmeta)
  real(4) u_eta1(imeta,jmeta,lmeta),v_eta1(imeta,jmeta,lmeta),peta_eta1(imeta,jmeta)
  real(4) div0_eta(imeta,jmeta,lmeta),div1_eta(imeta,jmeta,lmeta)
  
!   first reset common/vrbls/ with input fields t_eta0, etc.

  call putetaanl(t_eta0,q_eta0,q2_eta0,cwm_eta0,u_eta0,v_eta0,peta_eta0,imeta,jmeta,lmeta)

!   stuff for 1 adiabatic time step

!  start the adjustment step: integrate forward the continuity equation (update mass field)

!        divergence and horizontal part of omega-alpha term

  call exch (t,lm,2,2)       ! exchange t,u,v,q
  call exch (u,lm,2,2)       ! exchange t,u,v,q
  call exch (v,lm,2,2)       ! exchange t,u,v,q
  call exch (q,lm,2,2)       ! exchange t,u,v,q
  call divhoa(div0_eta)

!         press tend, eta dot and vert. omega-alpha

  call pdte

!       vertical advection 

  call exch(etadt,lm-1,1,1)
  call vtadv
  call exch(t,lm,1,1)              ! exchange t,u,v,q   !  coalesce these into one call
  call exch(u,lm,1,1)              ! exchange t,u,v,q   !   by making t,u,v,q contiguous in memory
  call exch(v,lm,1,1)              ! exchange t,u,v,q   !     as is done in operational model
  call exch(q,lm,1,1)              ! exchange t,u,v,q
  call exch(q2,lm,1,1)

!       updating pressure difference

  call pdnew
  
!       integrate backward the momentum equation (update wind field)

!       pressure gradient and coriolis force terms

  call exch(pd,1,2,2)       ! exchange pd and t  --coalesce these into one call as in operational
  call exch(t,lm,2,2)
  call exch(q,lm,2,2)
  call pgcor
  call exch(pdsl,1,5,5)

!      the adjustment step is now done.  

!   horizontal advection

  call exch(t,lm,4,4)
  call exch(u,lm,4,4)
  call exch(v,lm,4,4)
  call exch(q2,lm,5,5)
  call hzadv

!   retrieve variables at time dt

  call getetaanl(t_eta1,q_eta1,q2_eta1,cwm_eta1,u_eta1,v_eta1,peta_eta1,imeta,jmeta,lmeta)
  dt_eta=dt

!vvvvvvvvvvvvvvvvvvvvvv below is diagnostic only

  call exch(u,lm,2,2)
  call exch(v,lm,2,2)
  call exch(q,lm,2,2)
  call exch(cwm,lm,2,2)
  call hzadv2

!   now do second adiabatic time step

!  start the adjustment step: integrate forward the continuity equation (update mass field)

!        divergence and horizontal part of omega-alpha term

  call exch (t,lm,2,2)       ! exchange t,u,v,q
  call exch (u,lm,2,2)       ! exchange t,u,v,q
  call exch (v,lm,2,2)       ! exchange t,u,v,q
  call exch (q,lm,2,2)       ! exchange t,u,v,q
  call divhoa(div1_eta)

!^^^^^^^^^^^^^^^ above is diagnostic only

!   this is the end of the adiabatic time step.

return
end subroutine one_adiabatic_eta_step
