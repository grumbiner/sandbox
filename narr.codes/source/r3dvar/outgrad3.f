subroutine outgrad3(pdges,tges,qges,uges,vges,zges,hges,imeta,jmeta,lmeta, &
                    imetaglb,jmetaglb,etam,ptop,mype)

  !  make grads file for fields on filled eta grid 

  character(50) dsname,title
         data dsname/'eta.dat'/
         data title/'r3dvanl'/
  real(4) pdges(imeta,jmeta),tges(imeta,jmeta,lmeta),qges(imeta,jmeta,lmeta)
  real(4) uges(imeta,jmeta,lmeta),vges(imeta,jmeta,lmeta),hges(imeta,jmeta,lmeta+1)
  real(4) zges(imeta,jmeta)
  real(4) etam(lmeta)
  real(4),allocatable::wglb(:,:),wglbp(:,:)
  real(4),allocatable::work(:,:)
  real(4),allocatable::pout(:),wtemp(:)
  integer(4),allocatable::ishifth(:),ishiftv(:)
         character(80) datdes(1000)
         character(1) blank
         data blank/' '/

         data undef/-9.99e33/

  allocate(pout(lmeta))
  kk=lmeta+1
  do k=1,lmeta
   kk=kk-1
   pout(k)=(1000.-ptop)*etam(kk)+ptop
  end do

  allocate(ishifth(jmetaglb)) ; allocate(ishiftv(jmetaglb))
  do j=1,jmetaglb
   ishifth(j)=mod(j-1,2)
   ishiftv(j)=mod(j,2)
  end do

         imetaglb2=2*imetaglb-1
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
         write(datdes(5),500)imetaglb2,startx,xinc
500      format('XDEF ',i5,' LINEAR ',f7.2,f7.2)
         write(datdes(6),600)jmetaglb,starty,yinc
600      format('YDEF ',i5,' LINEAR ',f7.2,f7.2)
         next=7
         write(datdes(next),700)lmeta
700      format('ZDEF ',i5,' LEVELS ')
         kend=0
         do
          kstart=kend+1
          kend=min(kstart+9,lmeta)
          if(kstart.gt.lmeta) exit
          next=next+1
          write(datdes(next),'(10f8.2)')(pout(k),k=kstart,kend)
         end do
         next=next+1
         write(datdes(next),800)ntime
800      format('TDEF ',i5,' LINEAR 0Z23may1992 24hr')
         next=next+1
         write(datdes(next),810)
810      format('VARS 7')
         next=next+1
         write(datdes(next),910)lmeta
910      format('t   ',i5,' 99 t   ')
         next=next+1
         write(datdes(next),915)lmeta
915      format('q   ',i5,' 99 q   ')
         next=next+1
         write(datdes(next),920)lmeta
920      format('u   ',i5,' 99 u   ')
         next=next+1
         write(datdes(next),930)lmeta
930      format('v   ',i5,' 99 v   ')
         next=next+1
         write(datdes(next),932)lmeta
932      format('h   ',i5,' 99 h   ')
         next=next+1
         write(datdes(next),934)lmeta
934      format('pd  ',i5,' 99 pd  ')
         next=next+1
         write(datdes(next),940)lmeta
940      format('z   ',i5,' 99 z   ')
         next=next+1
         write(datdes(next),980)
980      format('ENDVARS')
         last=next
         if(mype.eq.0) write(1563,2000)(datdes(i),i=1,last)
2000     format(a80)
         if(mype.eq.0) rewind 1564
  allocate(wglb(imetaglb,jmetaglb))
  allocate(wglbp(imetaglb,jmetaglb))
  allocate(work(imetaglb2,jmetaglb))
  allocate(wtemp(imeta*jmeta))
  do k=lmeta,1,-1
   iii=0
   do j=1,jmeta
    do i=1,imeta
     iii=iii+1
     wtemp(iii)=tges(i,j,k)
    end do
   end do
   call loc2glb(wtemp,wglb)
   if(mype.eq.0) then
    do j=1,jmetaglb
     ii=0
     do i=1,imetaglb
      if(ishifth(j).eq.0) then
       ip=min(imetaglb,i+1)
       diffthis=wglb(i,j)
       diffnext=wglb(ip,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffthis
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
      else
       im=max(1,i-1)
       diffthis=wglb(im,j)
       diffnext=wglb(i,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffnext
      end if
     end do
    end do
    write(1564)work
   end if
  end do

  do k=lmeta,1,-1
   iii=0
   do j=1,jmeta
    do i=1,imeta
     iii=iii+1
     wtemp(iii)=qges(i,j,k)
    end do
   end do
   call loc2glb(wtemp,wglb)
   if(mype.eq.0) then
    do j=1,jmetaglb
     ii=0
     do i=1,imetaglb
      if(ishifth(j).eq.0) then
       ip=min(imetaglb,i+1)
       diffthis=wglb(i,j)
       diffnext=wglb(ip,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffthis
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
      else
       im=max(1,i-1)
       diffthis=wglb(im,j)
       diffnext=wglb(i,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffnext
      end if
     end do
    end do
    write(1564)work
   end if
  end do

  do k=lmeta,1,-1
   iii=0
   do j=1,jmeta
    do i=1,imeta
     iii=iii+1
     wtemp(iii)=uges(i,j,k)
    end do
   end do
   call loc2glb(wtemp,wglb)
   if(mype.eq.0) then
    do j=1,jmetaglb
     ii=0
     do i=1,imetaglb
      if(ishiftv(j).eq.0) then
       ip=min(imetaglb,i+1)
       diffthis=wglb(i,j)
       diffnext=wglb(ip,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffthis
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
      else
       im=max(1,i-1)
       diffthis=wglb(im,j)
       diffnext=wglb(i,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffnext
      end if
     end do
    end do
    write(1564)work
   end if
  end do

  do k=lmeta,1,-1
   iii=0
   do j=1,jmeta
    do i=1,imeta
     iii=iii+1
     wtemp(iii)=vges(i,j,k)
    end do
   end do
   call loc2glb(wtemp,wglb)
   if(mype.eq.0) then
    do j=1,jmetaglb
     ii=0
     do i=1,imetaglb
      if(ishiftv(j).eq.0) then
       ip=min(imetaglb,i+1)
       diffthis=wglb(i,j)
       diffnext=wglb(ip,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffthis
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
      else
       im=max(1,i-1)
       diffthis=wglb(im,j)
       diffnext=wglb(i,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffnext)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=diffnext
      end if
     end do
    end do
    write(1564)work
   end if
  end do

  do k=lmeta,1,-1
   iii=0
   do j=1,jmeta
    do i=1,imeta
     iii=iii+1
     wtemp(iii)=hges(i,j,k)
    end do
   end do
   call loc2glb(wtemp,wglb)
   kp=k+1
   iii=0
   do j=1,jmeta
    do i=1,imeta
     iii=iii+1
     wtemp(iii)=hges(i,j,kp)
    end do
   end do
   call loc2glb(wtemp,wglbp)
   if(mype.eq.0) then
    do j=1,jmetaglb
     ii=0
     do i=1,imetaglb
      if(ishifth(j).eq.0) then
       ip=min(imetaglb,i+1)
       diffthis=wglb(i,j)
       diffnext=wglb(ip,j)
       diffthisp=wglbp(i,j)
       diffnextp=wglbp(ip,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffthis+diffthisp)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.25*(diffthis+diffthisp+diffnext+diffnextp)
      else
       im=max(1,i-1)
       diffthis=wglb(im,j)
       diffnext=wglb(i,j)
       diffthisp=wglbp(im,j)
       diffnextp=wglbp(i,j)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.25*(diffthis+diffthisp+diffnext+diffnextp)
       ii=min(imetaglb2,ii+1)
       work(ii,j)=.5*(diffnext+diffnextp)
      end if
     end do
    end do
    write(1564)work
   end if
  end do

  iii=0
  do j=1,jmeta
   do i=1,imeta
    iii=iii+1
    wtemp(iii)=pdges(i,j)
   end do
  end do
  call loc2glb(wtemp,wglb)
  if(mype.eq.0) then
   do j=1,jmetaglb
    ii=0
    do i=1,imetaglb
     if(ishifth(j).eq.0) then
      ip=min(imetaglb,i+1)
      diffthis=wglb(i,j)
      diffnext=wglb(ip,j)
      ii=min(imetaglb2,ii+1)
      work(ii,j)=diffthis
      ii=min(imetaglb2,ii+1)
      work(ii,j)=.5*(diffthis+diffnext)
     else
      im=max(1,i-1)
      diffthis=wglb(im,j)
      diffnext=wglb(i,j)
      ii=min(imetaglb2,ii+1)
      work(ii,j)=.5*(diffthis+diffnext)
      ii=min(imetaglb2,ii+1)
      work(ii,j)=diffnext
     end if
    end do
   end do
   do k=lmeta,1,-1
    write(1564)work
   end do
  end if

  iii=0
  do j=1,jmeta
   do i=1,imeta
    iii=iii+1
    wtemp(iii)=zges(i,j)
   end do
  end do
  call loc2glb(wtemp,wglb)
  if(mype.eq.0) then
   do j=1,jmetaglb
    ii=0
    do i=1,imetaglb
     if(ishifth(j).eq.0) then
      ip=min(imetaglb,i+1)
      diffthis=wglb(i,j)
      diffnext=wglb(ip,j)
      ii=min(imetaglb2,ii+1)
      work(ii,j)=diffthis
      ii=min(imetaglb2,ii+1)
      work(ii,j)=.5*(diffthis+diffnext)
     else
      im=max(1,i-1)
      diffthis=wglb(im,j)
      diffnext=wglb(i,j)
      ii=min(imetaglb2,ii+1)
      work(ii,j)=.5*(diffthis+diffnext)
      ii=min(imetaglb2,ii+1)
      work(ii,j)=diffnext
     end if
    end do
   end do
   do k=lmeta,1,-1
    write(1564)work
   end do
   close(1564)
  end if

  deallocate(pout)
  deallocate(wglb)
  deallocate(wglbp)
  deallocate(work)
  deallocate(wtemp)
  deallocate(ishifth)
  deallocate(ishiftv)

return
end
