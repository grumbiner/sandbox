subroutine perturbparms_bayes(&
                rlenxyp0,rlenxyt0,rlenxyq0,rlenxypsi0,rlenxychi0, &
                rlenpt0,rlenpq0,rlenppsi0,rlenpchi0, &
                ep0,et0,eq0,epsi0,echi0, &
                rlenxyp,rlenxyt,rlenxyq,rlenxypsi,rlenxychi, &
                rlenpt,rlenpq,rlenppsi,rlenpchi, &
                ep,et,eq,epsi,echi, &
                lat1,lat2,lmetaex,lambda_in,dlambda,nlambda,mype)

!   compute perturbations to all background error parameters so finite diff estimate of gradient wrt lambda
!          of log(maxl) function can be computed

!          parameter definition is as follows:

!        parm(lambda) = parm0*exp(lambda)   lambda = lambda_in + dlambda

  real(4) rlenxyp0                     !  1
  real(4) rlenxyt0  (lmetaex)          !  2
  real(4) rlenxyq0  (lmetaex)          !  3
  real(4) rlenxypsi0(lmetaex)          !  4
  real(4) rlenxychi0(lmetaex)          !  5
  real(4) rlenpt0   (lmetaex)          !  6
  real(4) rlenpq0   (lmetaex)          !  7
  real(4) rlenppsi0 (lmetaex)          !  8
  real(4) rlenpchi0 (lmetaex)          !  9
  real(4) ep0       (        lat1:lat2)!  10
  real(4) et0       (lmetaex,lat1:lat2)!  11
  real(4) eq0       (lmetaex,lat1:lat2)!  12
  real(4) epsi0     (lmetaex,lat1:lat2)!  13
  real(4) echi0     (lmetaex,lat1:lat2)!  14

  real(4) rlenxyp          (2)          !  1
  real(4) rlenxyt  (lmetaex,2)          !  2
  real(4) rlenxyq  (lmetaex,2)          !  3
  real(4) rlenxypsi(lmetaex,2)          !  4
  real(4) rlenxychi(lmetaex,2)          !  5
  real(4) rlenpt   (lmetaex,2)          !  6
  real(4) rlenpq   (lmetaex,2)          !  7
  real(4) rlenppsi (lmetaex,2)          !  8
  real(4) rlenpchi (lmetaex,2)          !  9
  real(4) ep               (lat1:lat2,2)!  10
  real(4) et       (lmetaex,lat1:lat2,2)!  11
  real(4) eq       (lmetaex,lat1:lat2,2)!  12
  real(4) epsi     (lmetaex,lat1:lat2,2)!  13
  real(4) echi     (lmetaex,lat1:lat2,2)!  14
  real(4) lambda_in(nlambda),dlambda(nlambda)

  corlmin=rlenxyp0
  corlmin=min(corlmin,minval(rlenxyt0))
  corlmin=min(corlmin,minval(rlenxyq0))
  corlmin=min(corlmin,minval(rlenxypsi0))
  corlmin=min(corlmin,minval(rlenxychi0))
  corlmin=min(corlmin,minval(rlenpt0))
  corlmin=min(corlmin,minval(rlenpq0))
  corlmin=min(corlmin,minval(rlenppsi0))
  corlmin=min(corlmin,minval(rlenpchi0))
       
  do k=0,1
   rlenxyp(k+1)=    corlmin+(rlenxyp0-corlmin)     *exp(lambda_in( 1)+k*dlambda( 1))
   rlenxyt(:,k+1)=  corlmin+(rlenxyt0(:)-corlmin)  *exp(lambda_in( 2)+k*dlambda( 2))
   rlenxyq(:,k+1)=  corlmin+(rlenxyq0(:)-corlmin)  *exp(lambda_in( 3)+k*dlambda( 3))
   rlenxypsi(:,k+1)=corlmin+(rlenxypsi0(:)-corlmin)*exp(lambda_in( 4)+k*dlambda( 4))
   rlenxychi(:,k+1)=corlmin+(rlenxychi0(:)-corlmin)*exp(lambda_in( 5)+k*dlambda( 5))
   rlenpt(:,k+1)=   corlmin+(rlenpt0(:)-corlmin)   *exp(lambda_in( 6)+k*dlambda( 6))
   rlenpq(:,k+1)=   corlmin+(rlenpq0(:)-corlmin)   *exp(lambda_in( 7)+k*dlambda( 7))
   rlenppsi(:,k+1)= corlmin+(rlenppsi0(:)-corlmin) *exp(lambda_in( 8)+k*dlambda( 8))
   rlenpchi(:,k+1)= corlmin+(rlenpchi0(:)-corlmin) *exp(lambda_in( 9)+k*dlambda( 9))
   ep(:,k+1)=                                ep0(:)*exp(lambda_in(10)+k*dlambda(10))
   et(:,:,k+1)=                            et0(:,:)*exp(lambda_in(11)+k*dlambda(11))
   eq(:,:,k+1)=                            eq0(:,:)*exp(lambda_in(12)+k*dlambda(12))
   epsi(:,:,k+1)=                        epsi0(:,:)*exp(lambda_in(13)+k*dlambda(13))
   echi(:,:,k+1)=                        echi0(:,:)*exp(lambda_in(14)+k*dlambda(14))
  end do
  if(mype.eq.0) then
   write(0,*)' in perturbparms, rlenmin=',corlmin
   write(0,*)' in perturbparms, rlenxyp=',rlenxyp(1)
   write(0,*)' min,max(rlenxyt)=',minval(rlenxyt(:,1)),maxval(rlenxyt(:,1))
   write(0,*)' min,max(rlenxyq)=',minval(rlenxyq(:,1)),maxval(rlenxyq(:,1))
   write(0,*)' min,max(rlenxypsi)=',minval(rlenxypsi(:,1)),maxval(rlenxypsi(:,1))
   write(0,*)' min,max(rlenxychi)=',minval(rlenxychi(:,1)),maxval(rlenxychi(:,1))
   write(0,*)' min,max(rlenpt)=',minval(rlenpt(:,1)),maxval(rlenpt(:,1))
   write(0,*)' min,max(rlenpq)=',minval(rlenpq(:,1)),maxval(rlenpq(:,1))
   write(0,*)' min,max(rlenppsi)=',minval(rlenppsi(:,1)),maxval(rlenppsi(:,1))
   write(0,*)' min,max(rlenpchi)=',minval(rlenpchi(:,1)),maxval(rlenpchi(:,1))
   write(0,*)' min,max(ep)=',minval(ep(:,1)),maxval(ep(:,1))
   write(0,*)' min,max(et)=',minval(et(:,:,1)),maxval(et(:,:,1))
   write(0,*)' min,max(eq)=',minval(eq(:,:,1)),maxval(eq(:,:,1))
   write(0,*)' min,max(epsi)=',minval(epsi(:,:,1)),maxval(epsi(:,:,1))
   write(0,*)' min,max(echi)=',minval(echi(:,:,1)),maxval(echi(:,:,1))
   do k=1,nlambda
    write(0,*)' lambda(',k,')=',lambda_in(k)
   end do
  end if

return
end subroutine perturbparms_bayes
