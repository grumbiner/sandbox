subroutine read_satbias(allbias_old,allbias_new,nusat,nuchan,jpch, &
       nangs_max,npred,model_id,idate5,tbias,tcbias,cbias_step_clen,mype)

!    read file "satbias_in", filling up internal structure allbias with
!      everything necessary to update radiance bias information

  include 'mpif.h'
         include "my_comm.h"
  include 'satbias_type.h'

  type(satbias_stuff) allbias_old(jpch),allbias_new(jpch)
  integer(4),dimension(jpch):: nusat,nuchan
  integer iffir
  data iffir/0/
  integer(8) model_id
  integer(4) idate5(5)

  integer(8) model_id_in
  integer(4) idate_in(5)
  real(4) age_cbias(nangs_max),cbias(nangs_max),predx(npred)

!   initialize allbias_old,allbias_new
if (iffir .eq. 0) go to 1
do j=1,jpch
!  print *,'GWVX PNTR LOCS ',loc(allbias_old(j)%age_cbias),j
!  print *,'GWVX PNTR LOCS ',loc(allbias_old(j)%cbias),j
!  print *,'GWVX PNTR LOCS ',loc(allbias_old(j)%predx    ),j
end do
  do j=1,jpch
!  print *,'GWVX2 PNTR LOCS ',loc(allbias_old(j)%age_cbias),j
!  print *,'GWVX2 PNTR LOCS ',loc(allbias_old(j)%cbias),j
!  print *,'GWVX2 PNTR LOCS ',loc(allbias_old(j)%predx    ),j
   deallocate(allbias_old(j)%age_cbias,stat=ierr)
   deallocate(allbias_old(j)%cbias,stat=ierr)
   deallocate(allbias_old(j)%predx,stat=ierr)
  end do
1   continue
  do j=1,jpch
   allbias_old(j)%model_id=model_id
   allbias_old(j)%idate=idate5
   allbias_old(j)%kidsat=nusat(j)
   allbias_old(j)%ichan=nuchan(j)
   allbias_old(j)%nangs=nangs_max
   allbias_old(j)%npred=npred
   allbias_old(j)%tbias=tbias
   allbias_old(j)%tcbias=tcbias
   allbias_old(j)%cbias_step_clen=cbias_step_clen
   allbias_old(j)%age_bias=0.
   allbias_old(j)%age_tlapmean=0.
   allocate(allbias_old(j)%age_cbias(nangs_max))
   allbias_old(j)%age_cbias=0.
   allbias_old(j)%tlapmean=0.
   allocate(allbias_old(j)%cbias(nangs_max))
   allbias_old(j)%cbias=0.
   allocate(allbias_old(j)%predx(npred))
   allbias_old(j)%predx=0.

  end do
do j=1,jpch
!  print *,'GWVXX PNTR LOCS ',loc(allbias_old(j)%age_cbias),j
!  print *,'GWVXX PNTR LOCS ',loc(allbias_old(j)%cbias),j
!  print *,'GWVXX PNTR LOCS ',loc(allbias_old(j)%predx    ),j
end do

  iubcof=46
  open(iubcof,file='satbias_in',form='formatted')
  do
   read(iubcof,910,err=200,end=200) &
        model_id_in,idate_in,kidsat_in,ichan_in, &
        nangs_in,npred_in,tbias_in,tcbias_in,cbias_step_clen_in
   read(iubcof,920,err=200,end=200) &
        age_bias,age_tlapmean,(age_cbias(i),i=1,nangs_in), &
        tlapmean,(cbias(i),i=1,nangs_in),predx
   jthis=newchn(kidsat_in,ichan_in,nusat,nuchan,jpch)
   if(model_id_in.eq.model_id.and.nangs_in.le.nangs_max &
      .and.npred_in.eq.npred) then
    if(jthis.gt.0) then
     allbias_old(jthis)%model_id=model_id_in
     allbias_old(jthis)%idate=idate_in
     allbias_old(jthis)%kidsat=kidsat_in
     allbias_old(jthis)%ichan=ichan_in
     allbias_old(jthis)%nangs=nangs_in
     allbias_old(jthis)%age_bias=age_bias
     allbias_old(jthis)%age_tlapmean=age_tlapmean
     allbias_old(jthis)%age_cbias(1:nangs_in)=age_cbias(1:nangs_in)
     allbias_old(jthis)%tlapmean=tlapmean
     allbias_old(jthis)%cbias(1:nangs_in)=cbias(1:nangs_in)
     allbias_old(jthis)%predx=predx
     if(mype.eq.0) &
      print *,' LOAD BIAS FOR SAT ',kidsat_in,', CHAN ',ichan_in
    else
     if(mype.eq.0) &
      print *,' BIAS FOR SAT ',kidsat_in,', CHAN ', &
          ichan_in,' IS BEING DROPPED'
    end if
   else
    if(mype.eq.0) &
     print *,' BIAS FOR SAT ',kidsat_in,', CHAN ', &
          ichan_in,' IS BEING RESET--NEW MODEL'
   end if
  end do
200  continue
  close(iubcof)

  agemax=0.
  do j=1,jpch
   agemax=max(allbias_old(j)%age_bias,allbias_old(j)%age_tlapmean,agemax)
  end do
  if(agemax.eq.0..and.mype.eq.0) print *,' ALL BIAS INFO FOR RADIANCES RESET'

  if(agemax.gt.0..and.mype.eq.0) then
    print *,'guess bias correction:  npred = ',npred
   do j=1,jpch
     predx=allbias_old(j)%predx
     print 1000,j,(predx(n),n=1,npred)
1000    format(1x,'jch=',i3,10f12.6)
   end do
  end if

!  copy some of old into new
do j=1,jpch
!  print *,'GWVXY PNTR LOCS ',loc(allbias_old(j)%age_cbias),j
!  print *,'GWVXY PNTR LOCS ',loc(allbias_old(j)%cbias),j
!  print *,'GWVXY PNTR LOCS ',loc(allbias_old(j)%predx    ),j
end do
if (iffir .eq. 0) go to 2
  do j=1,jpch
   deallocate(allbias_new(j)%age_cbias,stat=ierr)
   deallocate(allbias_new(j)%cbias,stat=ierr)
   deallocate(allbias_new(j)%predx,stat=ierr)
  end do
2 continue
   iffir=1

  do j=1,jpch
   allbias_new(j)%model_id=model_id
   allbias_new(j)%idate=idate5
   allbias_new(j)%kidsat=nusat(j)
   allbias_new(j)%ichan=nuchan(j)
   allbias_new(j)%nangs=nangs_max
   allbias_new(j)%npred=npred
   allbias_new(j)%tbias=tbias
   allbias_new(j)%tcbias=tcbias
   allbias_new(j)%cbias_step_clen=cbias_step_clen
   allbias_new(j)%age_bias=0.
   allbias_new(j)%age_tlapmean=0.
   allocate(allbias_new(j)%age_cbias(nangs_max))
   allbias_new(j)%age_cbias=allbias_old(j)%age_cbias
   allbias_new(j)%tlapmean=allbias_old(j)%tlapmean
   allocate(allbias_new(j)%cbias(nangs_max))
   allbias_new(j)%cbias=allbias_old(j)%cbias
   allocate(allbias_new(j)%predx(npred))
   allbias_new(j)%predx=allbias_old(j)%predx
  end do

910  format(10x,i20,7x,i4.4,4i2.2,13x,2i5,/, &
            13x,2i4,30x,3e15.6)
920  format(10x,e15.6,14x,e15.6,19x, &
                /18(1x,5e15.6/),10x,e15.6,15x, &
                /18(1x,5e15.6/),7x,6e15.6)

return
end subroutine read_satbias
