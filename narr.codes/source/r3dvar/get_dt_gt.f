subroutine get_dt_gt(t_eta0,q_eta0,q2_eta0,cwm_eta0,u_eta0,v_eta0,peta_eta0, &
                     imeta,jmeta,lmeta,nxc,nyc,myis2,myie2,myjs2,myje2)

!   just get dD/dt and dps/dt for diagnostic purposes only

  real(4) t_eta0(imeta,jmeta,lmeta),q_eta0(imeta,jmeta,lmeta)
  real(4) q2_eta0(imeta,jmeta,lmeta),cwm_eta0(imeta,jmeta,lmeta)
  real(4) u_eta0(imeta,jmeta,lmeta),v_eta0(imeta,jmeta,lmeta),peta_eta0(imeta,jmeta)

  real(4),allocatable::t_eta1(:,:,:),q_eta1(:,:,:)
  real(4),allocatable::q2_eta1(:,:,:),cwm_eta1(:,:,:)
  real(4),allocatable::u_eta1(:,:,:),v_eta1(:,:,:),peta_eta1(:,:)
  real(4),allocatable::div0_eta(:,:,:)
  real(4),allocatable::div1_eta(:,:,:)

!     do model time step

  allocate(div0_eta(imeta,jmeta,lmeta))
  allocate(div1_eta(imeta,jmeta,lmeta))
  allocate(t_eta1(imeta,jmeta,lmeta))
  allocate(q_eta1(imeta,jmeta,lmeta))
  allocate(q2_eta1(imeta,jmeta,lmeta))
  allocate(cwm_eta1(imeta,jmeta,lmeta))
  allocate(u_eta1(imeta,jmeta,lmeta))
  allocate(v_eta1(imeta,jmeta,lmeta))
  allocate(peta_eta1(imeta,jmeta))
  call one_adiabatic_eta_step(t_eta0,q_eta0,q2_eta0,cwm_eta0,u_eta0,v_eta0,peta_eta0, &
                              t_eta1,q_eta1,q2_eta1,cwm_eta1,u_eta1,v_eta1,peta_eta1, &
                              div0_eta,div1_eta, &
                              dt_eta,imeta,jmeta,lmeta)
      call getdtends(div0_eta,div1_eta)
      call getptends(peta_eta0,peta_eta1)
  deallocate(div0_eta)
  deallocate(div1_eta)
  deallocate(q_eta1)
  deallocate(q2_eta1)
  deallocate(cwm_eta1)
  deallocate(t_eta1)
  deallocate(u_eta1)
  deallocate(v_eta1)
  deallocate(peta_eta1)


return
end subroutine get_dt_gt
