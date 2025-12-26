subroutine obscorr2wr(eyo0,mdata,bigh,yo,xbarb,lbig2ges,lmetaex)

!-------- normalize stuff by obs error

  real(4) eyo0(max(1,mdata))
  real(4) bigh(lbig2ges,2,max(1,mdata))
  real(4) yo(max(1,mdata)),xbarb(lmetaex,max(1,mdata))

  if(mdata.gt.0) then
   do i=1,mdata
    yo(i)=yo(i)/eyo0(i)
    xbarb(1:lmetaex,i)=xbarb(1:lmetaex,i)/eyo0(i)
   end do
   do i=1,mdata
    do k=1,2
     do lll=1,lbig2ges
      bigh(lll,k,i)=bigh(lll,k,i)/eyo0(i)
     end do
    end do
   end do
  end if

return
end subroutine obscorr2wr
