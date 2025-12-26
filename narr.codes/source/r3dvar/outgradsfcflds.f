subroutine outgradsfcflds(sfcglb,imeta,jmeta,name)

  !  make grads file for fields on filled eta grid 

  character(*) name
  character(50) dsname,title
         data dsname/'eta.dat'/
         data title/'r3dvanl'/
  real(4) sfcglb(imeta,jmeta)
  real(4),allocatable::work(:,:)
  integer(4),allocatable::ishifth(:)
         character(80) datdes(1000)
         character(1) blank
         data blank/' '/

         data undef/-9.99e33/

  allocate(ishifth(jmeta))
  do j=1,jmeta
   ishifth(j)=mod(j-1,2)
  end do

         imeta2=2*imeta-1
         lmeta=1
         startx=1. ; xinc=1.
         starty=1. ; yinc=1.
         startp=1. ; pinc=1.
         ntime=1
         do i=1,1000
          write(datdes(i),10)(blank,k=1,80)
10        format(80a1)
         end do
         write(datdes(1),100)dsname
100      format('DSET ',a50)
         write(datdes(2),200)
200      format('options big_endian sequential')
         write(datdes(3),300)title
300      format('TITLE ',a50)
         write(datdes(4),400)undef
400      format('UNDEF ',e11.2)
         write(datdes(5),500)imeta2,startx,xinc
500      format('XDEF ',i5,' LINEAR ',f7.2,f7.2)
         write(datdes(6),600)jmeta,starty,yinc
600      format('YDEF ',i5,' LINEAR ',f7.2,f7.2)
         next=7
         write(datdes(next),700)lmeta,startp,pinc
700      format('ZDEF ',i5,' LINEAR ',f7.2,f7.2)
         next=next+1
         write(datdes(next),800)ntime
800      format('TDEF ',i5,' LINEAR 0Z23may1992 24hr')
         next=next+1
         write(datdes(next),810)
810      format('VARS 1')
         next=next+1
         write(datdes(next),910)name,lmeta,name
910      format(a5,i5,' 99 ',a5)
         next=next+1
         write(datdes(next),980)
980      format('ENDVARS')
         last=next
         write(1563,2000)(datdes(i),i=1,last)
2000     format(a80)
         rewind 1564
  allocate(work(imeta2,jmeta))
  do j=1,jmeta
   ii=0
   do i=1,imeta
    if(ishifth(j).eq.0) then
     ip=min(imeta,i+1)
     diffthis=sfcglb(i,j)
     diffnext=sfcglb(ip,j)
     ii=min(imeta2,ii+1)
     work(ii,j)=diffthis
     ii=min(imeta2,ii+1)
     work(ii,j)=.5*(diffthis+diffnext)
    else
     im=max(1,i-1)
     diffthis=sfcglb(im,j)
     diffnext=sfcglb(i,j)
     ii=min(imeta2,ii+1)
     work(ii,j)=.5*(diffthis+diffnext)
     ii=min(imeta2,ii+1)
     work(ii,j)=diffnext
    end if
   end do
  end do
  write(1564)work
  close(1564)

return
end
