subroutine obscorr2t(eyo0,mdata,bigh,yo,xbarb,lbig3ges)

!-------- normalize stuff by obs error

  real(4) eyo0(max(1,mdata))
  real(4) bigh(lbig3ges,max(1,mdata))
  real(4) yo(max(1,mdata)),xbarb(max(1,mdata))

  if(mdata.gt.0) then
   do i=1,mdata
    yo(i)=yo(i)/eyo0(i)
    xbarb(i)=xbarb(i)/eyo0(i)
   end do
   do i=1,mdata
    do lll=1,lbig3ges
     bigh(lll,i)=bigh(lll,i)/eyo0(i)
    end do
   end do
  end if

return
end subroutine obscorr2t
