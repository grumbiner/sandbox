subroutine count2_rads(rad_dat,mrad_dat,mraddata)

!   get total number of radiance measurements (each channel counts as one)

  include 'types.h'

  type(rad_obs) rad_dat(max(1,mrad_dat))

  mraddata=0
  if(mrad_dat.gt.0) then
   do i=1,mrad_dat
    mraddata=mraddata+rad_dat(i)%ncc
   end do
  end if

return
end subroutine count2_rads
