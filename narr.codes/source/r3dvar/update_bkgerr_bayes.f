subroutine update_bkgerr_bayes(&
                rlenxyp,rlenxyt,rlenxyq,rlenxypsi,rlenxychi, &
                rlenpt,rlenpq,rlenppsi,rlenpchi, &
                ep,et,bscaleq,epsi,echi, &
                lmetaex,lambda_in,nlambda,mype)

!   update background error parameters with new values of lambda

!          parameter definition is as follows:

!        parm(lambda) = parm0*exp(lambda)

  include 'mpif.h'
      include "my_comm.h"

  real(4) rlenxyp0                     !  1
  real(4) rlenxyt0  (lmetaex+4)          !  2
  real(4) rlenxyq0  (lmetaex+4)          !  3
  real(4) rlenxypsi0(lmetaex+4)          !  4
  real(4) rlenxychi0(lmetaex+4)          !  5
  real(4) rlenpt0   (lmetaex+4)          !  6
  real(4) rlenpq0   (lmetaex+4)          !  7
  real(4) rlenppsi0 (lmetaex+4)          !  8
  real(4) rlenpchi0 (lmetaex+4)          !  9
  real(4) ep0                          !  10
  real(4) et0       (lmetaex+4)          !  11
  real(4) bscaleq0                     !  12
  real(4) epsi0     (lmetaex+4)          !  13
  real(4) echi0     (lmetaex+4)          !  14

  real(4) rlenxyp                     !  1
  real(4) rlenxyt  (lmetaex)          !  2
  real(4) rlenxyq  (lmetaex)          !  3
  real(4) rlenxypsi(lmetaex)          !  4
  real(4) rlenxychi(lmetaex)          !  5
  real(4) rlenpt   (lmetaex)          !  6
  real(4) rlenpq   (lmetaex)          !  7
  real(4) rlenppsi (lmetaex)          !  8
  real(4) rlenpchi (lmetaex)          !  9
  real(4) ep                          !  10
  real(4) et       (lmetaex)          !  11
  real(4) bscaleq                     !  12
  real(4) epsi     (lmetaex)          !  13
  real(4) echi     (lmetaex)          !  14
  real(4) lambda_in(nlambda)

  if(mype.eq.0) then
   open(234,file='lambda_parms',form='unformatted',action='readwrite')
   read(234) mlambda,mmetaex,lambda_in, &
         rlenxyp0,rlenxyt0,rlenxyq0,rlenxypsi0,rlenxychi0, &
               rlenpt0,rlenpq0,rlenppsi0,rlenpchi0, &
               ep0,et0,bscaleq0,epsi0,echi0
          if(mlambda.ne.nlambda.or.mmetaex.ne.lmetaex+4) then
                print *,' PROBLEM UPDATING BACKGROUND ERROR PARMS'
                print *,'     mlambda,nlambda=',mlambda,nlambda
                print *,'     lmetaex,mmetaex=',lmetaex,mmetaex
                 stop
          end if
   close(234)
  end if
  call mpi_bcast(rlenxyp0,1,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenxyt0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenxyq0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenxypsi0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenxychi0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenpt0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenpq0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenppsi0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(rlenpchi0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(ep0,1,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(et0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(bscaleq0,1,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(epsi0,lmetaex,mpi_real4,0,my_comm,ierr)
  call mpi_bcast(echi0,lmetaex,mpi_real4,0,my_comm,ierr)

  corlmin=rlenxyp0
  corlmin=min(corlmin,minval(rlenxyt0(1:lmetaex)))
  corlmin=min(corlmin,minval(rlenxyq0(1:lmetaex)))
  corlmin=min(corlmin,minval(rlenxypsi0(1:lmetaex)))
  corlmin=min(corlmin,minval(rlenxychi0(1:lmetaex)))
  corlmin=min(corlmin,minval(rlenpt0(1:lmetaex)))
  corlmin=min(corlmin,minval(rlenpq0(1:lmetaex)))
  corlmin=min(corlmin,minval(rlenppsi0(1:lmetaex)))
  corlmin=min(corlmin,minval(rlenpchi0(1:lmetaex)))
       
   rlenxyp=    corlmin+(rlenxyp0-corlmin)      *exp(lambda_in( 1))
   rlenxyt(:lmetaex)=  corlmin+(rlenxyt0(:lmetaex)-corlmin)  *exp(lambda_in( 2))
   rlenxyq(:lmetaex)=  corlmin+(rlenxyq0(:lmetaex)-corlmin)  *exp(lambda_in( 3))
   rlenxypsi(:lmetaex)=corlmin+(rlenxypsi0(:lmetaex)-corlmin)*exp(lambda_in( 4))
   rlenxychi(:lmetaex)=corlmin+(rlenxychi0(:lmetaex)-corlmin)*exp(lambda_in( 5))
   rlenpt(:lmetaex)=   corlmin+(rlenpt0(:lmetaex)-corlmin)   *exp(lambda_in( 6))
   rlenpq(:lmetaex)=   corlmin+(rlenpq0(:lmetaex)-corlmin)   *exp(lambda_in( 7))
   rlenppsi(:lmetaex)= corlmin+(rlenppsi0(:lmetaex)-corlmin) *exp(lambda_in( 8))
   rlenpchi(:lmetaex)= corlmin+(rlenpchi0(:lmetaex)-corlmin) *exp(lambda_in( 9))
   ep=                                      ep0*exp(lambda_in(10))
   et(:lmetaex)=                                et0(:lmetaex)*exp(lambda_in(11))
   bscaleq=                            bscaleq0*exp(lambda_in(12))
   epsi(:lmetaex)=                            epsi0(:lmetaex)*exp(lambda_in(13))
   echi(:lmetaex)=                            echi0(:lmetaex)*exp(lambda_in(14))
  if(mype.eq.0) then
   write(0,*)' in update_bkgerr_bayes, rlenmin=',corlmin
   write(0,*)' in update_bkgerr_bayes, rlenxyp=',rlenxyp
   write(0,*)' min,max(rlenxyt)=',minval(rlenxyt(:lmetaex)),maxval(rlenxyt(:lmetaex))
   write(0,*)' min,max(rlenxyq)=',minval(rlenxyq(:lmetaex)),maxval(rlenxyq(:lmetaex))
   write(0,*)' min,max(rlenxypsi)=',minval(rlenxypsi(:lmetaex)),maxval(rlenxypsi(:lmetaex))
   write(0,*)' min,max(rlenxychi)=',minval(rlenxychi(:lmetaex)),maxval(rlenxychi(:lmetaex))
   write(0,*)' min,max(rlenpt)=',minval(rlenpt(:lmetaex)),maxval(rlenpt(:lmetaex))
   write(0,*)' min,max(rlenpq)=',minval(rlenpq(:lmetaex)),maxval(rlenpq(:lmetaex))
   write(0,*)' min,max(rlenppsi)=',minval(rlenppsi(:lmetaex)),maxval(rlenppsi(:lmetaex))
   write(0,*)' min,max(rlenpchi)=',minval(rlenpchi(:lmetaex)),maxval(rlenpchi(:lmetaex))
   write(0,*)' ep=',ep
   write(0,*)' min,max(et)=',minval(et(:lmetaex)),maxval(et(:lmetaex))
   write(0,*)' bscaleq=',bscaleq
   write(0,*)' min,max(epsi)=',minval(epsi(:lmetaex)),maxval(epsi(:lmetaex))
   write(0,*)' min,max(echi)=',minval(echi(:lmetaex)),maxval(echi(:lmetaex))
   do k=1,nlambda
    write(0,*)' lambda(',k,')=',lambda_in(k)
   end do
  end if

return
end subroutine update_bkgerr_bayes
