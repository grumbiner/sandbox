subroutine update_satbias(allbias_new,allbias_old,errbias0, &
               rad_dat,nrad_dat,mrad_dat,nangs,npred,jpch,mype)

!   do minimization to get new values for the bias coefficients predx.

  include 'mpif.h'
      include "my_comm.h"
  include 'types.h'
  include 'satbias_type.h'

  type(satbias_stuff) allbias_new(jpch),allbias_old(jpch)

  type(rad_obs) rad_dat(max(1,nrad_dat))

  real(4) x_b(jpch,npred)
  real(4) big_c(npred)
  real(4) big_a(jpch,npred,npred),small_b(jpch,npred)
  real(4) big_h(npred)
  real(4) work(jpch,npred)
  real(4) big_work(jpch,npred,npred)
  real(4) x_tilde(jpch,npred)
  real(4) x(jpch,npred)
  real(4) cbias(jpch,nangs),cbias0(jpch,nangs)
  real(4) cbias_new(jpch,nangs)
  real(4) wgtbias_new(jpch,nangs),wgtbias0(jpch,nangs)
  real(4) smoothcb(nangs,nangs)
  real(4) q_b(jpch)
  real(4) wgtlapmean(jpch),tlapmean_new(jpch)
  real(4) wgtlapmean0(jpch),tlapmean0(jpch)

!    set up smoothing matrix for cbias_new

  z0=allbias_new(1)%cbias_step_clen
  smoothcb=0.
  do m=1,nangs
   do k=m,nangs
    arg=abs(k-m)/z0
    if(arg.lt.40.) smoothcb(k,m)=(1.+arg)*exp(-arg)  ! soar function
    smoothcb(m,k)=smoothcb(k,m)
   end do
  end do

!    set up sqrt of background error matrix.

  do m=1,npred
   big_c(m)=errbias0
  end do

!   save current value of variables

  do j=1,jpch
   q_b(j)=allbias_old(j)%tlapmean
   cbias(j,1:nangs)=allbias_old(j)%cbias(1:nangs)
   x_b(j,1:npred)=allbias_new(j)%predx(1:npred)
  end do


!               T -1                    T -1
!   accumulate H O  H in big_a and get H O  (T-Hx_b) in  small_b
!
  big_a=0.
  small_b=0.
  tlapmean_new=0.
  wgtlapmean=0.
  cbias_new=0.
  wgtbias_new=0.
  if(mrad_dat.gt.0) then
   do i=1,mrad_dat
    nadir=rad_dat(i)%nsigx1    !   scan step 
    do j=1,rad_dat(i)%ncc
     jch=rad_dat(i)%icx(j)
     do k=1,npred-2
      big_h(k)=rad_dat(i)%pred(k)
     end do
     big_h(npred)=rad_dat(i)%pred(npred-2+j)
     big_h(npred-1)=big_h(npred)**2
     big_oinv=rad_dat(i)%var(j)
     tlapmean_new(jch)=tlapmean_new(jch)+big_oinv*(big_h(npred)+q_b(jch))
     wgtlapmean(jch)=wgtlapmean(jch)+big_oinv
     rest=rad_dat(i)%obsbt(j)-rad_dat(i)%gesbt(j)
     cbias_new(jch,nadir)=cbias_new(jch,nadir)+big_oinv*rest
     wgtbias_new(jch,nadir)=wgtbias_new(jch,nadir)+big_oinv
     rest=rest-cbias(jch,nadir)
     do k=1,npred
      rest=rest-big_h(k)*x_b(jch,k)
     end do
     do k=1,npred
      small_b(jch,k)=small_b(jch,k)+big_h(k)*big_oinv*rest
     end do
     do m=1,npred
      do k=1,npred
       big_a(jch,m,k)=big_a(jch,m,k)+big_h(m)*big_oinv*big_h(k)
      end do
     end do
    end do
   end do
  end if

!                        T T -1
!   now get in small_b  C H O  (T-Hx_b)
!
  call mpi_allreduce(small_b,work,jpch*npred,mpi_real4, &
                      mpi_sum,my_comm,ierr)
  do m=1,npred
   do j=1,jpch
    small_b(j,m)=big_c(m)*work(j,m)
   end do
  end do
!                        T T -1
!   now get in big_a    C H O  HC
!
  call mpi_allreduce(big_a,big_work,jpch*npred*npred,mpi_real4, &
                                  mpi_sum,my_comm,ierr)
  do m=1,npred
   do k=1,npred
    do j=1,jpch
     big_a(j,m,k)=big_c(m)*big_work(j,m,k)*big_c(k)
    end do
   end do
  end do
!                                             T T -1
!  finally add I to big_a so we now have I + C H O  HC
!
  do m=1,npred
   do j=1,jpch
    big_a(j,m,m)=big_a(j,m,m)+1.
   end do
  end do
!
!      now invert big_a
!
  call vinvmm(big_a,big_work,npred,npred,npred,jpch,jpch)
        if(mype.eq.0) print *,' at 4 in update_satbias, max(big_a)=',maxval(abs(big_a))
!
!      check that inverse is correct
!
! errmax=0.
! amax=0.
! do k=1,npred
!  do m=1,npred
!   sum=0.
!   if(k.eq.m) sum=-1.
!   do j=1,npred
!    sum=sum+big_work(m,j)*big_a(j,k)
!    amax=max(abs(big_a(j,k)),amax)
!   end do
!   errmax=max(abs(sum),errmax)
!  end do
! end do
!  if(mype.eq.0) &
!    print *,' inverse max error in update_satbias for channel ',jch,' = ',errmax
!  if(mype.eq.0) &
!    print *,' max of big_a in update_satbias for channel ',jch,' = ',amax
!
!  obtain solution vector x_tilde
!
  x_tilde=0.
  do k=1,npred
   do m=1,npred
    do j=1,jpch
     x_tilde(j,k)=x_tilde(j,k)+big_work(j,m,k)*small_b(j,m)
    end do
   end do
  end do
!
!  obtain solution vector x
!
  do k=1,npred
   do j=1,jpch
    x(j,k)=x_b(j,k)+big_c(k)*x_tilde(j,k)
   end do
  end do


!   get new tlapmean, cbias

  eps=10.*epsilon(eps)
  call mpi_allreduce(tlapmean_new,tlapmean0,jpch,mpi_real4,mpi_sum,my_comm,ierr)
  call mpi_allreduce(wgtlapmean,wgtlapmean0,jpch,mpi_real4,mpi_sum,my_comm,ierr)
  do j=1,jpch
   tlapmean_new(j)=tlapmean0(j)/max(eps,wgtlapmean0(j))
  end do
  call mpi_allreduce(cbias_new,cbias0,jpch*nangs,mpi_real4,mpi_sum,my_comm,ierr)
  call mpi_allreduce(wgtbias_new,wgtbias0,jpch*nangs,mpi_real4,mpi_sum,my_comm,ierr)
  cbias_new=0.
  wgtbias_new=0.
  do m=1,nangs
   do k=1,nangs
    do j=1,jpch
     cbias_new(j,m)=cbias_new(j,m)+smoothcb(k,m)*cbias0(j,k)
     wgtbias_new(j,m)=wgtbias_new(j,m)+smoothcb(k,m)*wgtbias0(j,k)
    end do
   end do
  end do
  do m=1,nangs
   do j=1,jpch
    cbias_new(j,m)=cbias_new(j,m)/max(eps,wgtbias_new(j,m))
   end do
  end do

!  update allbias_new

  do j=1,jpch
   allbias_new(j)%predx(1:npred)=x(j,1:npred)
   allbias_new(j)%age_bias=wgtlapmean0(j)
   allbias_new(j)%tlapmean=tlapmean_new(j)
   allbias_new(j)%age_tlapmean=wgtlapmean0(j)
   allbias_new(j)%cbias(1:nangs)=cbias_new(j,1:nangs)
   allbias_new(j)%age_cbias(1:nangs)=wgtbias_new(j,1:nangs)
  end do

return
end subroutine update_satbias
