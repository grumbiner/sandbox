subroutine blend_satbias(allbias_new,allbias_old,idate5, &
              tbias,tcbias,nangs,dt_assimilation)

!    update allbias_new by blending contents with allbias_old

!    bias is updated with age parameter, so if no data for a long time,
!        or a new channel, then bias will be the current spun up value.

!    cbias, tlapmean are updated with a constant running mean factor
!          so they always change by a small amount each time, and should
!         asymtote eventually to a stable value, with bias evolving slowly
!           to compensate.

!      dt_assimilation is assimilation time interval in hours

  include 'satbias_type.h'

  type(satbias_stuff) allbias_new,allbias_old
  integer(4) idate5(5)

!   obtain time-difference between old date and current date
!      and get age factor alpha for bias and cbias,tlapmean

  eps=10.*epsilon(eps)
  days_per_minute=1./(24.*60.)
  call w3fs21(idate5,nming)
  daynew=float(nming)*days_per_minute
  call w3fs21(allbias_old%idate,nming)
  dayold=float(nming)*days_per_minute
  arg=min(0.,(dayold-daynew)/tbias)
  alpha_bias=0.
  if(arg.gt.-40.) alpha_bias=exp(arg)
  alpha_cbias=dt_assimilation/(24.*tcbias)

!   update bias coefs (predx)

  age=alpha_bias*allbias_old%age_bias

  bign=allbias_new%age_bias
  age_new=age+bign
  allbias_new%predx= &
      (age*allbias_old%predx+bign*allbias_new%predx)/max(eps,age_new)
  allbias_new%age_bias=age_new

!   update tlapmean

  allbias_new%tlapmean=(1.-alpha_cbias)*allbias_old%tlapmean &
                             +alpha_cbias*allbias_new%tlapmean
  allbias_new%age_tlapmean=(1.-alpha_cbias)*allbias_old%age_tlapmean &
                             +alpha_cbias*allbias_new%age_tlapmean

!   update cbias

  do i=1,nangs
   allbias_new%cbias(i)=(1.-alpha_cbias)*allbias_old%cbias(i) &
                             +alpha_cbias*allbias_new%cbias(i)
   allbias_new%age_cbias(i)=(1.-alpha_cbias)*allbias_old%age_cbias(i) &
                             +alpha_cbias*allbias_new%age_cbias(i)
  end do

return
end subroutine blend_satbias
