subroutine write_satbias(allbias_new,predinc,jpch,npred)

!    update bias corrector coefficients with analysis increment, then write
!     to output file "satbias_out"

!      also write out file satangbias.txt for diagnostic purposes

  include 'mpif.h'
         include "my_comm.h"
  include 'satbias_type.h'

  type(satbias_stuff) allbias_new(jpch)

  real(4) predinc(jpch,npred)

  integer(8) model_id
  integer(4) idate(5)
  real(4) predx(npred)

  iobcof=54
  open(iobcof,file='satbias_out',form='formatted')

  print*,'analysis bias correction: npred = ',npred
  do j=1,jpch
   predx(1:npred)=allbias_new(j)%predx(1:npred)+predinc(j,1:npred)
   allbias_new(j)%predx(1:npred)=predx(1:npred)

   model_id=allbias_new(j)%model_id
   idate=allbias_new(j)%idate
   kidsat=allbias_new(j)%kidsat
   ichan=allbias_new(j)%ichan
   nangs=allbias_new(j)%nangs
   tbias=allbias_new(j)%tbias
   tcbias=allbias_new(j)%tcbias
   cbias_step_clen=allbias_new(j)%cbias_step_clen
   age_bias=allbias_new(j)%age_bias
   age_tlapmean=allbias_new(j)%age_tlapmean
   tlapmean=allbias_new(j)%tlapmean
   write(iobcof,910) model_id,idate,kidsat,ichan, &
        nangs,npred,tbias,tcbias,cbias_step_clen
   write(iobcof,920) age_bias,age_tlapmean,(allbias_new(j)%age_cbias(i),i=1,nangs), &
        tlapmean,(allbias_new(j)%cbias(i),i=1,nangs),predx
   print 1000,j,(predx(n),n=1,npred)
  end do
  close(iobcof)
910  format(' model_id=',i20,' idate=',i4.4,4i2.2,' idsat,ichan=',2i5,/, &
            ' nangs,npred=',2i4,' tbias,tcbias,cbias_step_clen=',3e15.6)
920  format(' age_bias=',e15.6,' age_tlapmean=',e15.6,' age_cbias follows:', &
                /18(1x,5e15.6/),' tlapmean=',e15.6,' cbias follows:', &
                /18(1x,5e15.6/),' predx=',6e15.6)
1000    format(1x,'jch=',i3,10e13.6)

return
end subroutine write_satbias
