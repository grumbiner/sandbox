subroutine lastevents_pw(wtpw,mpwdata,lbig2ges,mype,lmetaex)

!!-------- last write to events file.  write all data, with flag of
!--------  zero if successfully used, and 1 if rejected by non-lin qc
!--------

  real(4) wtpw(4,max(1,mpwdata))
  real(4) yopw(max(1,mpwdata))
  real(4) eyopw0(max(1,mpwdata))
  real(4) xbarbpw(max(1,mpwdata))
  real(4) bighpw(lmetaex,lbig2ges,max(1,mpwdata))
  integer(4) ibighpw(lbig2ges,max(1,mpwdata))
  real(4) pwdata(max(1,mpwdata),6)
  character(8) pwstaid(max(1,mpwdata))


  real(4),pointer::yopw_com(:),eyopw0_com(:),xbarbpw_com(:)
  real(4),pointer::bighpw_com(:,:,:),pwdata_com(:,:),pwprest_com(:)
  character(8),pointer::pwstaid_com(:)
  integer(4),pointer::ibighpw_com(:,:)
  common/r3dv_pwdata/ibighpw_com,yopw_com,eyopw0_com,xbarbpw_com, &
                     bighpw_com,pwdata_com,pwstaid_com,pwprest_com
  character(10)eventfile

  

  if(mpwdata.gt.0) then
  write(eventfile,'("events",i4)')mype+9000
  ievout=4
  open(ievout,file=eventfile,form='formatted',position='append')
   do i=1,mpwdata
    yopw(i)=yopw_com(i)
    eyopw0(i)=eyopw0_com(i)
    xbarbpw(i)=xbarbpw_com(i)
    do kk=1,lbig2ges
     ibighpw(kk,i)=ibighpw_com(kk,i)
     do k=1,lmetaex
      bighpw(k,kk,i)=bighpw_com(k,kk,i)
     end do
    end do
    do k=1,6
     pwdata(i,k)=pwdata_com(i,k)
    end do
    pwstaid(i)=pwstaid_com(i)
   end do

   wtlim=.1
   do i=1,mpwdata
    pwanl=wtpw(4,i)*eyopw0(i)+xbarbpw(i)
    icode=0
    if(abs(wtpw(1,i)).lt.wtlim) icode=1
    write(ievout,'("PW.",i3.3,11e13.5,2x,a8)')icode, &
      yopw(i),xbarbpw(i),pwanl,pwdata(i,2),pwdata(i,3),pwdata(i,1), &
      pwprest_com(i),pwdata(i,4),pwdata(i,6),eyopw0(i),pwdata(i,5),pwstaid(i)
   end do
   close(ievout)
     print *,' in lastevents_pw, mype,mpwdata=',mype,mpwdata

   deallocate(yopw_com)
   deallocate(eyopw0_com)
   deallocate(xbarbpw_com)
   deallocate(bighpw_com)
   deallocate(ibighpw_com)
   deallocate(pwdata_com)
   deallocate(pwstaid_com)
   deallocate(pwprest_com)

  end if

return
end subroutine lastevents_pw
