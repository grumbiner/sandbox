       subroutine outgrad2(t,q,psi,chi,u,v,h,nx,ny,np,rx,ry,startp,pinc, &
            erlon0,erlat0,nlonmap,nlatmap,dlonmap,dlatmap, &
             rlonmap0,rlatmap0,rp,nphgeo,iordmap,lbigmap)

  integer(4) nx,ny,np,nlonmap,nlatmap,nphgeo
  real(4) startp,pinc,erlon0,erlat0,dlonmap,dlatmap,rlonmap0,rlatmap0

         character(50) dsname,title
         data dsname/'anl.dat'/
         data title/'r3dvanl'/
  real(4) t(nx,ny,np),u(nx,ny,np),v(nx,ny,np),h(nx,ny,np)
  real(4) psi(nx,ny,np),chi(nx,ny,np)
  real(4) q(nx,ny,np),rx(nx),ry(ny),rp(np)
         character(80) datdes(1000)
         character(1) blank
         data blank/' '/

  integer(4) k,l,i,next,last,ntime,iuv,j,kstart,kend,loop
  integer(4),allocatable::ipmb(:)
  real(4) undef,rog,conmc,sjstar,sjmstar,spbar,sjpstar
         data undef/-9.99e33/
  real(4),allocatable,dimension(:,:)::work,wgth
  real(4),allocatable,dimension(:)::sp
  real(4),allocatable,dimension(:,:,:)::hall
  real(4) zero4,two4
  real(4),allocatable::deltags(:),epsilongs(:)
  real(4),allocatable::deltagu(:),epsilongu(:)
  real(4),allocatable::deltagv(:),epsilongv(:),wgts(:)
  integer(4),allocatable::iwgts(:),iflag(:)

!-------- compute heights from ref height and temps

  zero4=0._8 ; two4=2._8

         rog=conmc('rd$')/conmc('g$')
  allocate(sp(0:np+1))
  allocate(ipmb(np))
         do k=1,np
          sp(k)=-rog*rp(k)
          ipmb(k)=nint(exp(rp(k)))
          if(ipmb(k).eq.0) ipmb(k)=ipmb(k-1)-1
         end do
         print *,' in outgrad2, ipmb=',ipmb
         sp(0)=two4*sp(1)-sp(2)
         sp(np+1)=two4*sp(np)-sp(np-1)
  allocate(wgth(np,np))
         wgth=zero4
         do l=1,np
          if(sp(l).ge.sp(1).and.sp(l).lt.sp(nphgeo)) then
           do k=1,nphgeo
            sjstar=max(sp(l),sp(k))
            sjmstar=max(sp(l),sp(k-1))
            spbar=min(sp(k+1),sp(nphgeo))
            wgth(k,l)=-max(zero4,spbar-sjstar)**2/(two4*(sp(k+1)-sp(k))) &
                -max(zero4,sp(k)-sjmstar)*(sp(k)-two4*sp(k-1)+sjmstar)/ &
                                  (two4*(sp(k)-sp(k-1)))
           end do
          end if
          if(sp(l).ge.sp(nphgeo).and.sp(l).le.sp(np)) then
           do k=nphgeo,np
            sjstar=min(sp(l),sp(k))
            sjpstar=min(sp(l),sp(k+1))
            spbar=max(sp(nphgeo),sp(k-1))
            wgth(k,l)= max(zero4,sjstar-spbar)**2/(two4*(sp(k)-sp(k-1))) &
                -max(zero4,sjpstar-sp(k))*(sjpstar-two4*sp(k+1)+sp(k))/ &
                         (two4*(sp(k+1)-sp(k)))
           end do
          end if
!         do k=1,np
!          print *,' wgth(',k,',',l,')=',wgth(k,l)
!         end do
         end do
  allocate(hall(nx,ny,np))
         do k=1,np
          do j=1,ny
           do i=1,nx
            hall(i,j,k)=h(i,j,nphgeo)
           end do
          end do
         end do
         do l=1,np
          do k=1,np
           do j=1,ny
            do i=1,nx
             hall(i,j,l)=hall(i,j,l)+wgth(k,l)*t(i,j,k)
            end do
           end do
          end do
         end do
  deallocate(wgth)
  deallocate(sp)

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
         write(datdes(5),500)nlonmap,rlonmap0,dlonmap
500      format('XDEF ',i5,' LINEAR ',f7.2,f7.2)
         write(datdes(6),600)nlatmap,rlatmap0,dlatmap
600      format('YDEF ',i5,' LINEAR ',f7.2,f7.2)
         next=7
         kstart=1
         kend=min(10,np)
         write(datdes(next),700)np,(ipmb(k),k=kstart,kend)
700      format('ZDEF ',i5,' LEVELS ',10i5)
         next=next+1
         do loop=1,100
          kstart=kend+1
          kend=min(kstart+9,np)
          if(kstart.gt.np) go to 703
          write(datdes(next),702)(ipmb(k),k=kstart,kend)
702       format('   ',10i5)
          next=next+1
         end do
703      continue
         write(datdes(next),800)ntime
800      format('TDEF ',i5,' LINEAR 0Z23may1992 24hr')
         next=next+1
         write(datdes(next),810)
810      format('VARS 8')
         next=next+1
         write(datdes(next),910)np
910      format('t   ',i5,' 99 t   ')
         next=next+1
         write(datdes(next),915)np
915      format('q   ',i5,' 99 q   ')
         next=next+1
         write(datdes(next),920)np
920      format('u   ',i5,' 99 u   ')
         next=next+1
         write(datdes(next),930)np
930      format('v   ',i5,' 99 v   ')
         next=next+1
         write(datdes(next),932)np
932      format('psi ',i5,' 99 psi ')
         next=next+1
         write(datdes(next),934)np
934      format('chi ',i5,' 99 chi ')
         next=next+1
         write(datdes(next),940)np
940      format('h   ',i5,' 99 h   ')
         next=next+1
         write(datdes(next),942)np
942      format('hd  ',i5,' 99 hd  ')
         next=next+1
         write(datdes(next),980)
980      format('ENDVARS')
         last=next
         write(563,2000)(datdes(i),i=1,last)
2000     format(a80)
         rewind 564
  allocate(work(nlonmap,nlatmap))
  allocate(deltags(nlonmap*nlatmap))
  allocate(epsilongs(nlonmap*nlatmap))
  allocate(deltagu(nlonmap*nlatmap))
  allocate(epsilongu(nlonmap*nlatmap))
  allocate(deltagv(nlonmap*nlatmap))
  allocate(epsilongv(nlonmap*nlatmap))
  allocate(wgts(nlonmap*nlatmap*lbigmap))
  allocate(iwgts(nlonmap*nlatmap*lbigmap))
  allocate(iflag(nlonmap*nlatmap))
  call grid2mapg_init(nx,ny,rx,ry,nlonmap,nlatmap,rlonmap0,rlatmap0,erlon0,erlat0, &
                       dlonmap,dlatmap,iordmap,lbigmap,deltags,epsilongs, &
                       deltagu,epsilongu,deltagv,epsilongv,wgts,iwgts,iflag)
         do k=1,np
          call grid2mapg(t(1,1,k),t(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltags,epsilongs,wgts,iwgts,iflag)
          write(564)work
         end do
         do k=1,np
          call grid2mapg(q(1,1,k),q(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltags,epsilongs,wgts,iwgts,iflag)
          write(564)work
         end do
         do k=1,np
          call grid2mapg(u(1,1,k),v(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltagu,epsilongu,wgts,iwgts,iflag)
          write(564)work
         end do
         iuv=2
         do k=1,np
          call grid2mapg(u(1,1,k),v(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltagv,epsilongv,wgts,iwgts,iflag)
          write(564)work
         end do
         do k=1,np
          call grid2mapg(psi(1,1,k),psi(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltags,epsilongs,wgts,iwgts,iflag)
          write(564)work
         end do
         do k=1,np
          call grid2mapg(chi(1,1,k),chi(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltags,epsilongs,wgts,iwgts,iflag)
          write(564)work
         end do
         do k=1,np
          call grid2mapg(h(1,1,k),h(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltags,epsilongs,wgts,iwgts,iflag)
          write(564)work
         end do
         do k=1,np
          call grid2mapg(hall(1,1,k),hall(1,1,k),work,nlonmap,nlatmap, &
                         lbigmap,deltags,epsilongs,wgts,iwgts,iflag)
          write(564)work
         end do
         close(563)
         close(564)
       return
       end subroutine outgrad2
