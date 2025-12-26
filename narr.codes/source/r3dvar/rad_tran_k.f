  subroutine rad_tran_k(pin5,prsr5,temp5,wmix5,omix5,ts5,&
          pems5,xpath,xpaths,knchan,kchan,ptau5,ptbcl5, &
          temp,wmix,omix,ts,pems, &
          itype,nsig,jpch,knchpf,nprof)
  
  use type_kinds, only: fp_kind
  use error_handler
  use k_matrix_model

  implicit none

  integer nprof,knchpf,i,nsig,error_status,jpch
  integer,dimension(nprof):: knchan       
  integer,dimension(knchpf):: kchan
  integer,dimension(jpch):: itype
  real(fp_kind),dimension(knchpf):: ts,pems,surface_reflectivity_k,pems5,ptbcl5
  real(fp_kind),dimension(nsig,knchpf):: ptau5,temp,wmix,omix
  real(fp_kind),dimension(nprof):: ts5,xpath,xpaths
  real(fp_kind),dimension(nsig,nprof):: prsr5,temp5,wmix5,omix5,pin5

!
  real(fp_kind),dimension(nsig,knchpf):: prsr,flux_tau,solar_tau,tau_k
  real(fp_kind),dimension(nsig,knchpf):: flux_tau_k,solar_tau_k,pin
  real(fp_kind),dimension(knchpf) :: refl5,prdcl5,upwelling_radiance_k
  real(fp_kind),dimension(knchpf) :: brightness_temperature_k


!
! call radiative transfer model
     prsr = 0.0
     temp = 0.0
     wmix = 0.0
     omix = 0.0
     pin  = 0.0
     ts   = 0.0
     pems = 0.0
     refl5= 0.0
     brightness_temperature_k=1.
     upwelling_radiance_k=0.
     surface_reflectivity_k=0.
     tau_k=0.
     flux_tau_k=0.
     solar_tau_k=0.

     do i=1,knchpf
      if(itype(kchan(i)) .eq. 1)then
        refl5(i)=1.-pems5(i)
      else
        refl5(i)=(1.-pems5(i))/3.1459         ! Isotropic
!       refl5(i)=(1.-pems5(i))                ! Specular
      end if
     end do
     error_status = compute_rtm_k(pin5,prsr5,temp5,wmix5,omix5,ts5,&
          pems5,refl5,tau_k,flux_tau_k,solar_tau_k,upwelling_radiance_k, &
          brightness_temperature_k,xpath,xpaths,knchan,kchan, &
          ptau5,flux_tau,solar_tau,prdcl5,ptbcl5, &
          pin,prsr,temp,wmix,omix,ts,pems,surface_reflectivity_k, &
          n_input_profiles=nprof)

!    Adjoint
     do i=1,knchpf
      if(itype(kchan(i)) .eq. 1)then
        pems(i)=pems(i)-surface_reflectivity_k(i)
      else
        pems(i)=pems(i)-surface_reflectivity_k(i)/3.1459
      end if
     end do


      IF ( error_status /= SUCCESS ) then
         print *,'STOP FORWARD_RTM error'
         call stop2(88)
      end if
      return
      end
