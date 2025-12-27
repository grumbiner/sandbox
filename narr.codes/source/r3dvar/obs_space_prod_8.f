function obs_space_prod_8( &
     xbart,xbarw,xbarq,xbarpw,xbarp,xbarrad, &
     pbart,pbarw,pbarq,pbarpw,pbarp,pbarrad, &
     mtdata,mwdata,mqdata,mpwdata,mpdata,mraddata,userad)

!     compute preconditioned norm of gradient

!  <-- rnorm8t: output norm (real(8))

  real(8) obs_space_prod_8
  logical userad
  real(8) rnorm8t

  real(4) xbart(max(1,mtdata)),xbarw(max(1,mwdata)),xbarq(max(1,mqdata))
  real(4) xbarpw(max(1,mpwdata)),xbarp(max(1,mpdata)),xbarrad(max(1,mraddata))

  real(4) pbart(max(1,mtdata)),pbarw(max(1,mwdata)),pbarq(max(1,mqdata))
  real(4) pbarpw(max(1,mpwdata)),pbarp(max(1,mpdata)),pbarrad(max(1,mraddata))

  real(8) prod_8

  rnorm8t=0._8
  call dot_prod8(prod_8,pbart,xbart,mtdata)
  rnorm8t=rnorm8t+prod_8
  call dot_prod8(prod_8,pbarw,xbarw,mwdata)
  rnorm8t=rnorm8t+prod_8
  call dot_prod8(prod_8,pbarq,xbarq,mqdata)
  rnorm8t=rnorm8t+prod_8
  call dot_prod8(prod_8,pbarpw,xbarpw,mpwdata)
  rnorm8t=rnorm8t+prod_8
  call dot_prod8(prod_8,pbarp,xbarp,mpdata)
  rnorm8t=rnorm8t+prod_8
  if(userad) then
   call dot_prod8(prod_8,pbarrad,xbarrad,mraddata)
   rnorm8t=rnorm8t+prod_8
  end if

  obs_space_prod_8=rnorm8t
return
end function obs_space_prod_8
