       subroutine rfftmlt(t,work,trigs,ifax,inc,jump,n,lot,isign)

!!!   lot=nlath*2  
!!!   nlax=lot+1

!  this is an interface to substitute a different fft for the
!  cray supplied fft, without changing the source code.

  real(4) t(inc,n+2),work(inc,n+2,2),trigs(n*2)
  integer(4) ifax(10)

!  move two grid wave 

         if(isign.eq.1) t(1:inc,2)=t(1:inc,n+1)
         call m2fftm(t,work,trigs,ifax,inc,jump,n,lot,isign)
         t(1:inc,n+1)=0.
         t(1:inc,n+2)=0.
         if(isign.eq.-1) then
          t(1:inc,n+1)=t(1:inc,2)
          t(1:inc,2)=0.
         end if

       return
       end subroutine rfftmlt

       subroutine fftfax(n,ifax,trigs)

!  this is an interface to substitute a different fft for
!  the cray supplied fft, without changing the source code.

  real(4) trigs(n*2)
  integer(4) ifax(10)

         call m1fax(ifax,n,3)
         call m1ftrg(trigs,n,3)

       return
       end subroutine fftfax

      subroutine m1fax(ifax,n,mode)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1fax       get factors of fft length.
!   prgmmr: parrish          org: w/nmc22    date: 90-10-31
!
! abstract: get factors of fft transform length (2,3,5 only).
!
! program history log:
!   90-10-31  parrish (this is some version of temperton code)
!
!   input argument list:
!     n        - length of transform (must be even with factors 2,3,5)
!     mode     - =3 used
!   output argument list:
!     ifax     - ifax(1)=number of factors, ifax(2-)= factors
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$

  integer(4) ifax(10)


  itwo=2 ; ithree=3 ; ifour=4
      nn=n
      if (iabs(mode).eq.1) go to 10
      if (iabs(mode).eq.8) go to 10
      nn=n/2
      if ((nn+nn).eq.n) go to 10
      ifax(1)=-99
      return
   10 k=1
!     test for factors of 4
   20 if (mod(nn,ifour).ne.0) go to 30
      k=k+1
      ifax(k)=4
      nn=nn/4
      if (nn.eq.1) go to 80
      go to 20
!     test for extra factor of 2
   30 if (mod(nn,itwo).ne.0) go to 40
      k=k+1
      ifax(k)=2
      nn=nn/2
      if (nn.eq.1) go to 80
!     test for factors of 3
   40 if (mod(nn,ithree).ne.0) go to 50
      k=k+1
      ifax(k)=3
      nn=nn/3
      if (nn.eq.1) go to 80
      go to 40
!     now find remaining factors
   50 l=5
      inc=2
!     inc alternately takes on values 2 and 4
   60 if (mod(nn,l).ne.0) go to 70
      k=k+1
      ifax(k)=l
      nn=nn/l
      if (nn.eq.1) go to 80
      go to 60
   70 l=l+inc
      inc=6-inc
      go to 60
   80 ifax(1)=k-1
!     ifax(1) contains number of factors
!     ifax(1) contains number of factors
      nfax=ifax(1)
!     sort factors into ascending order
      if (nfax.eq.1) go to 110
      do 100 ii=2,nfax
      istop=nfax+2-ii
      do 90 i=2,istop
      if (ifax(i+1).ge.ifax(i)) go to 90
      item=ifax(i)
      ifax(i)=ifax(i+1)
      ifax(i+1)=item
   90 continue
  100 continue
  110 continue
      return
      end subroutine m1fax

      subroutine m1ffta(a,work,trigs,inc,jump,n,lot)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1ffta      preprocessing for fft (spectral to grid)
!   prgmmr: parrish          org: w/nmc22    date: 90-10-31
!
! abstract: preprocessing step for fft, isign=1 (spectral to grid).
!
! program history log:
!   90-10-31  parrish (this is some version of temperton code)
!
!   input argument list:
!     n        - length of transform (must be even with factors 2,3,5)
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$
!

  real(4) a(n),work(n),trigs(n)

  real(4) s,c

      nh=n/2
      nx=n
      ink=inc+inc
!
!     a(0)   a(n/2)
      ia=1
      ib=n*inc+1
      ja=1
      jb=2
!dir$ ivdep
      do 10 l=1,lot
!     work(ja)=a(ia)            !  original, with two-grid
!     work(jb)=a(ia)            !  contribution left out
      work(ja)=a(ia)+a(ia+inc)  !  new, with two-grid
      work(jb)=a(ia)-a(ia+inc)  !  contribution included, added by dp 9-3-98
      ia=ia+jump
      ib=ib+jump
      ja=ja+nx
      jb=jb+nx
   10 continue
!
!     remaining wavenumbers
      iabase=2*inc+1
      ibbase=(n-2)*inc+1
      jabase=3
      jbbase=n-1
!
      do 30 k=3,nh,2
      ia=iabase
      ib=ibbase
      ja=jabase
      jb=jbbase
      c=trigs(n+k)
      s=trigs(n+k+1)
!dir$ ivdep
      do 20 l=1,lot
      work(ja)=(a(ia)+a(ib))-(s*(a(ia)-a(ib))+c*(a(ia+inc)+a(ib+inc)))
      work(jb)=(a(ia)+a(ib))+(s*(a(ia)-a(ib))+c*(a(ia+inc)+a(ib+inc)))
      work(ja+1)=(c*(a(ia)-a(ib))-s*(a(ia+inc)+a(ib+inc)))+(a(ia+inc)-a(ib+inc))
      work(jb+1)=(c*(a(ia)-a(ib))-s*(a(ia+inc)+a(ib+inc)))-(a(ia+inc)-a(ib+inc))
      ia=ia+jump
      ib=ib+jump
      ja=ja+nx
      jb=jb+nx
   20 continue
      iabase=iabase+ink
      ibbase=ibbase-ink
      jabase=jabase+2
      jbbase=jbbase-2
   30 continue
!
      if (iabase.ne.ibbase) go to 50
!     wavenumber n/4 (if it exists)
      ia=iabase
      ja=jabase
!dir$ ivdep
      do 40 l=1,lot
      work(ja)=2.0e0*a(ia)
      work(ja+1)=-2.0e0*a(ia+inc)
      ia=ia+jump
      ja=ja+nx
   40 continue
!
   50 continue
      return
      end subroutine m1ffta

      subroutine m1fftb(work,a,trigs,inc,jump,n,lot)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1fftb      postprocessing for fft (grid to spectral)
!   prgmmr: parrish          org: w/nmc22    date: 90-10-31
!
! abstract: postprocessing step for fft, isign=-1 (grid to spectral).
!
! program history log:
!   90-10-31  parrish (this is some version of temperton code)
!
!   input argument list:
!     n        - length of transform (must be even with factors 2,3,5)
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$
!

  real(4) work(n),a(n),trigs(n)

  real(4) s,c
  real(8) scale,half,one,two

      half=.5
      one=1.
      two=2.
!
      nh=n/2
      nx=n
      ink=inc+inc
!
!     a(0)   a(n/2)
      scale=one/float(n)
      ia=1
      ib=2
      ja=1
      jb=n*inc+1
!dir$ ivdep
      do 10 l=1,lot
      a(ja)=scale*(work(ia)+work(ib))
!     a(ja+inc)=0.0e0                     ! original--zeroes out two-grid wave
      a(ja+inc)=scale*(work(ia)-work(ib))   ! added by dp, 9-3-98
      ia=ia+nx
      ib=ib+nx
      ja=ja+jump
      jb=jb+jump
   10 continue
!
!     remaining wavenumbers
      scale=half*scale
      iabase=3
      ibbase=n-1
      jabase=2*inc+1
      jbbase=(n-2)*inc+1
!
      do 30 k=3,nh,2
      ia=iabase
      ib=ibbase
      ja=jabase
      jb=jbbase
      c=trigs(n+k)
      s=trigs(n+k+1)
!dir$ ivdep
      do 20 l=1,lot
      a(ja)=scale*((work(ia)+work(ib)) &
         +(c*(work(ia+1)+work(ib+1))+s*(work(ia)-work(ib))))
      a(jb)=scale*((work(ia)+work(ib)) &
         -(c*(work(ia+1)+work(ib+1))+s*(work(ia)-work(ib))))
      a(ja+inc)=scale*((c*(work(ia)-work(ib))-s*(work(ia+1)+work(ib+1))) &
          +(work(ib+1)-work(ia+1)))
      a(jb+inc)=scale*((c*(work(ia)-work(ib))-s*(work(ia+1)+work(ib+1))) &
          -(work(ib+1)-work(ia+1)))
      ia=ia+nx
      ib=ib+nx
      ja=ja+jump
      jb=jb+jump
   20 continue
      iabase=iabase+2
      ibbase=ibbase-2
      jabase=jabase+ink
      jbbase=jbbase-ink
   30 continue
!
      if (iabase.ne.ibbase) go to 50
!     wavenumber n/4 (if it exists)
      ia=iabase
      ja=jabase
      scale=two*scale
!dir$ ivdep
      do 40 l=1,lot
      a(ja)=scale*work(ia)
      a(ja+inc)=-scale*work(ia+1)
      ia=ia+nx
      ja=ja+jump
   40 continue
!
   50 continue
      return
      end subroutine m1fftb

      subroutine m1ftrg(trigs,n,mode)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1ftrg      get trig fns for fft.
!   prgmmr: parrish          org: w/nmc22    date: 90-10-31
!
! abstract: get trig fns for fft of length n.
!
! program history log:
!   90-10-31  parrish (this is some version of temperton code)
!
!   input argument list:
!     n        - length of transform (must be even with factors 2,3,5)
!     mode     - =3 used
!   output argument list:
!     trigs    - trigs(2*n)--desired trig functions
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$

  real(4) trigs(*)

  real(8) trigsc,trigss,pi,del,angle,two,one,half

      two=2._8
      one=1._8
      half=.5_8
      pi=two*asin(one)
      imode=iabs(mode)
      nn=n
      if (imode.gt.1.and.imode.lt.6) nn=n/2
      del=(pi+pi)/float(nn)
      l=nn+nn
      do 10 i=1,l,2
        angle=half*float(i-1)*del
        trigsc=cos(angle)
        trigss=sin(angle)
        trigs(i)=trigsc
        trigs(i+1)=trigss
   10 continue
      if (imode.eq.1) return
      if (imode.eq.8) return
      del=half*del
      nh=(nn+1)/2
      l=nh+nh
      la=nn+nn
      do 20 i=1,l,2
        angle=half*float(i-1)*del
        trigsc=cos(angle)
        trigss=sin(angle)
        trigs(la+i)=trigsc
        trigs(la+i+1)=trigss
   20 continue
      if (imode.le.3) return
      del=half*del
      la=la+nn
      if (mode.eq.5) go to 40
      do 30 i=2,nn
        angle=float(i-1)*del
        trigss=two*sin(angle)
        trigs(la+i)=trigss
   30 continue
      return
   40 continue
      del=half*del
      do 50 i=2,n
        angle=float(i-1)*del
        trigss=sin(angle)
        trigs(la+i)=trigss
   50 continue
      return
      end subroutine m1ftrg

      subroutine m1vpas(a,b,c,d,trigs,inc1,inc2,inc3,inc4,lot,n,ifac,la)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1vpas      part of fft code
!   prgmmr: parrish          org: w/nmc22    date: 90-10-31
!
! abstract: part of temperton fft code.
!
! program history log:
!   90-10-31  parrish (this is some version of temperton code)
!
!   input argument list:
!     n        - length of transform (must be even with factors 2,3,5)
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$

  real(4) a(n),b(n),c(n),d(n),trigs(n)

  real(4) c1,c2,c3,c4,s1,s2,s3,s4
  real(4) sin36,cos36,sin72,cos72,sin60
  data sin36/0.587785252292473/,cos36/0.809016994374947/, &
       sin72/0.951056516295154/,cos72/0.309016994374947/,sin60/0.866025403784437/
!
      m=n/ifac
      iink=m*inc1
      jink=la*inc2
      jump=(ifac-1)*jink
      ibase=0
      jbase=0
      igo=ifac-1
      if (igo.gt.4) return
      go to (10,50,90,130),igo
!
!     coding for factor 2
!
   10 ia=1
      ja=1
      ib=ia+iink
      jb=ja+jink
      do 20 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 15 ijk=1,lot
      c(ja+j)=a(ia+i)+a(ib+i)
      d(ja+j)=b(ia+i)+b(ib+i)
      c(jb+j)=a(ia+i)-a(ib+i)
      d(jb+j)=b(ia+i)-b(ib+i)
      i=i+inc3
      j=j+inc4
   15 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
   20 continue
      if (la.eq.m) return
      la1=la+1
      jbase=jbase+jump
      do 40 k=la1,m,la
      kb=k+k-2
      c1=trigs(kb+1)
      s1=trigs(kb+2)
      do 30 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 25 ijk=1,lot
      c(ja+j)=a(ia+i)+a(ib+i)
      d(ja+j)=b(ia+i)+b(ib+i)
      c(jb+j)=c1*(a(ia+i)-a(ib+i))-s1*(b(ia+i)-b(ib+i))
      d(jb+j)=s1*(a(ia+i)-a(ib+i))+c1*(b(ia+i)-b(ib+i))
      i=i+inc3
      j=j+inc4
   25 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
   30 continue
      jbase=jbase+jump
   40 continue
      return
!
!     coding for factor 3
!
   50 ia=1
      ja=1
      ib=ia+iink
      jb=ja+jink
      ic=ib+iink
      jc=jb+jink
      do 60 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 55 ijk=1,lot
      c(ja+j)=a(ia+i)+(a(ib+i)+a(ic+i))
      d(ja+j)=b(ia+i)+(b(ib+i)+b(ic+i))
      c(jb+j)=(a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))-(sin60*(b(ib+i)-b(ic+i)))
      c(jc+j)=(a(ia+i)-0.5e0*(a(ib+i)+a(ic+i)))+(sin60*(b(ib+i)-b(ic+i)))
      d(jb+j)=(b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))+(sin60*(a(ib+i)-a(ic+i)))
      d(jc+j)=(b(ia+i)-0.5e0*(b(ib+i)+b(ic+i)))-(sin60*(a(ib+i)-a(ic+i)))
      i=i+inc3
      j=j+inc4
   55 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
   60 continue
      if (la.eq.m) return
      la1=la+1
      jbase=jbase+jump
      do 80 k=la1,m,la
      kb=k+k-2
      kc=kb+kb
      c1=trigs(kb+1)
      s1=trigs(kb+2)
      c2=trigs(kc+1)
      s2=trigs(kc+2)
      do 70 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 65 ijk=1,lot
      c(ja+j)=a(ia+i)+(a(ib+i)+a(ic+i))
      d(ja+j)=b(ia+i)+(b(ib+i)+b(ic+i))
      c(jb+j)= &
          c1*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i))) &
                                            -(sin60*(b(ib+i)-b(ic+i)))) &
         -s1*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i))) &
                                            +(sin60*(a(ib+i)-a(ic+i))))
      d(jb+j)= &
          s1*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i))) &
                                            -(sin60*(b(ib+i)-b(ic+i)))) &
         +c1*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i))) &
                                            +(sin60*(a(ib+i)-a(ic+i))))
      c(jc+j)= &
          c2*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i))) &
                                            +(sin60*(b(ib+i)-b(ic+i)))) &
         -s2*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i))) &
                                            -(sin60*(a(ib+i)-a(ic+i))))
      d(jc+j)= &
          s2*((a(ia+i)-0.5e0*(a(ib+i)+a(ic+i))) &
                                            +(sin60*(b(ib+i)-b(ic+i)))) &
         +c2*((b(ia+i)-0.5e0*(b(ib+i)+b(ic+i))) &
                                            -(sin60*(a(ib+i)-a(ic+i))))
      i=i+inc3
      j=j+inc4
   65 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
   70 continue
      jbase=jbase+jump
   80 continue
      return
!
!     coding for factor 4
!
   90 ia=1
      ja=1
      ib=ia+iink
      jb=ja+jink
      ic=ib+iink
      jc=jb+jink
      id=ic+iink
      jd=jc+jink
      do 100 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 95 ijk=1,lot
      c(ja+j)=(a(ia+i)+a(ic+i))+(a(ib+i)+a(id+i))
      c(jc+j)=(a(ia+i)+a(ic+i))-(a(ib+i)+a(id+i))
      d(ja+j)=(b(ia+i)+b(ic+i))+(b(ib+i)+b(id+i))
      d(jc+j)=(b(ia+i)+b(ic+i))-(b(ib+i)+b(id+i))
      c(jb+j)=(a(ia+i)-a(ic+i))-(b(ib+i)-b(id+i))
      c(jd+j)=(a(ia+i)-a(ic+i))+(b(ib+i)-b(id+i))
      d(jb+j)=(b(ia+i)-b(ic+i))+(a(ib+i)-a(id+i))
      d(jd+j)=(b(ia+i)-b(ic+i))-(a(ib+i)-a(id+i))
      i=i+inc3
      j=j+inc4
   95 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
  100 continue
      if (la.eq.m) return
      la1=la+1
      jbase=jbase+jump
      do 120 k=la1,m,la
      kb=k+k-2
      kc=kb+kb
      kd=kc+kb
      c1=trigs(kb+1)
      s1=trigs(kb+2)
      c2=trigs(kc+1)
      s2=trigs(kc+2)
      c3=trigs(kd+1)
      s3=trigs(kd+2)
      do 110 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 105 ijk=1,lot
      c(ja+j)=(a(ia+i)+a(ic+i))+(a(ib+i)+a(id+i))
      d(ja+j)=(b(ia+i)+b(ic+i))+(b(ib+i)+b(id+i))
      c(jc+j)= c2*((a(ia+i)+a(ic+i))-(a(ib+i)+a(id+i))) &
              -s2*((b(ia+i)+b(ic+i))-(b(ib+i)+b(id+i)))
      d(jc+j)= s2*((a(ia+i)+a(ic+i))-(a(ib+i)+a(id+i))) &
              +c2*((b(ia+i)+b(ic+i))-(b(ib+i)+b(id+i)))
      c(jb+j)= c1*((a(ia+i)-a(ic+i))-(b(ib+i)-b(id+i))) &
              -s1*((b(ia+i)-b(ic+i))+(a(ib+i)-a(id+i)))
      d(jb+j)= s1*((a(ia+i)-a(ic+i))-(b(ib+i)-b(id+i))) &
              +c1*((b(ia+i)-b(ic+i))+(a(ib+i)-a(id+i)))
      c(jd+j)= c3*((a(ia+i)-a(ic+i))+(b(ib+i)-b(id+i))) &
              -s3*((b(ia+i)-b(ic+i))-(a(ib+i)-a(id+i)))
      d(jd+j)= s3*((a(ia+i)-a(ic+i))+(b(ib+i)-b(id+i))) &
              +c3*((b(ia+i)-b(ic+i))-(a(ib+i)-a(id+i)))
      i=i+inc3
      j=j+inc4
  105 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
  110 continue
      jbase=jbase+jump
  120 continue
      return
!
!     coding for factor 5
!
  130 ia=1
      ja=1
      ib=ia+iink
      jb=ja+jink
      ic=ib+iink
      jc=jb+jink
      id=ic+iink
      jd=jc+jink
      ie=id+iink
      je=jd+jink
      do 140 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 135 ijk=1,lot
      c(ja+j)=a(ia+i)+(a(ib+i)+a(ie+i))+(a(ic+i)+a(id+i))
      d(ja+j)=b(ia+i)+(b(ib+i)+b(ie+i))+(b(ic+i)+b(id+i))
      c(jb+j)=(a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
        -(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))
      c(je+j)=(a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
        +(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))
      d(jb+j)=(b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i))) &
        +(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i)))
      d(je+j)=(b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i))) &
        -(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i)))
      c(jc+j)=(a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
        -(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))
      c(jd+j)=(a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
        +(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))
      d(jc+j)=(b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i))) &
        +(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i)))
      d(jd+j)=(b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i))) &
        -(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i)))
      i=i+inc3
      j=j+inc4
  135 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
  140 continue
      if (la.eq.m) return
      la1=la+1
      jbase=jbase+jump
      do 160 k=la1,m,la
      kb=k+k-2
      kc=kb+kb
      kd=kc+kb
      ke=kd+kb
      c1=trigs(kb+1)
      s1=trigs(kb+2)
      c2=trigs(kc+1)
      s2=trigs(kc+2)
      c3=trigs(kd+1)
      s3=trigs(kd+2)
      c4=trigs(ke+1)
      s4=trigs(ke+2)
      do 150 l=1,la
      i=ibase
      j=jbase
!dir$ ivdep
      do 145 ijk=1,lot
      c(ja+j)=a(ia+i)+(a(ib+i)+a(ie+i))+(a(ic+i)+a(id+i))
      d(ja+j)=b(ia+i)+(b(ib+i)+b(ie+i))+(b(ic+i)+b(id+i))
      c(jb+j)= &
          c1*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
            -(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))) &
         -s1*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i))) &
            +(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
      d(jb+j)= &
          s1*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
            -(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))) &
         +c1*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i))) &
            +(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
      c(je+j)= &
          c4*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
            +(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))) &
         -s4*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i))) &
            -(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
      d(je+j)= &
          s4*((a(ia+i)+cos72*(a(ib+i)+a(ie+i))-cos36*(a(ic+i)+a(id+i))) &
            +(sin72*(b(ib+i)-b(ie+i))+sin36*(b(ic+i)-b(id+i)))) &
         +c4*((b(ia+i)+cos72*(b(ib+i)+b(ie+i))-cos36*(b(ic+i)+b(id+i))) &
            -(sin72*(a(ib+i)-a(ie+i))+sin36*(a(ic+i)-a(id+i))))
      c(jc+j)= &
          c2*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
            -(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))) &
         -s2*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i))) &
            +(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
      d(jc+j)= &
          s2*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
            -(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))) &
         +c2*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i))) &
            +(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
      c(jd+j)= &
          c3*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
            +(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))) &
         -s3*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i))) &
            -(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
      d(jd+j)= &
          s3*((a(ia+i)-cos36*(a(ib+i)+a(ie+i))+cos72*(a(ic+i)+a(id+i))) &
            +(sin36*(b(ib+i)-b(ie+i))-sin72*(b(ic+i)-b(id+i)))) &
         +c3*((b(ia+i)-cos36*(b(ib+i)+b(ie+i))+cos72*(b(ic+i)+b(id+i))) &
            -(sin36*(a(ib+i)-a(ie+i))-sin72*(a(ic+i)-a(id+i))))
      i=i+inc3
      j=j+inc4
  145 continue
      ibase=ibase+inc1
      jbase=jbase+inc2
  150 continue
      jbase=jbase+jump
  160 continue
      return
      end subroutine m1vpas

      subroutine m2fftm(a,work,trigs,ifax,inc,jump,n,lot,isign)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m1fftm      main fft code
!   prgmmr: parrish          org: w/nmc22    date: 90-10-31
!
! abstract: main fft code.
!
! program history log:
!   90-10-31  parrish (this is some version of temperton code)
!
!   input argument list:
!     n        - length of transform (must be even with factors 2,3,5)
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$

  real(4) a(n),work((inc+1)*n),trigs(n)
  integer(4) ifax(*)


  itwo=2
!
      nfax=ifax(1)
      nx=n
      nh=n/2
      ink=inc+inc
      if (isign.eq.+1) go to 30
!
!     if necessary, transfer data to work area
      igo=50
      if (mod(nfax,itwo).eq.1) goto 40
      ibase=1
      jbase=1
      do 20 l=1,lot
      i=ibase
      j=jbase
!dir$ ivdep
      do 10 m=1,n
      work(j)=a(i)
      i=i+inc
      j=j+1
   10 continue
      ibase=ibase+jump
      jbase=jbase+nx
   20 continue
!
      igo=60
      go to 40
!
!     preprocessing (isign=+1)
!     ------------------------
!
   30 continue
      call m1ffta(a,work,trigs,inc,jump,n,lot)
      igo=60
!
!     complex transform
!     -----------------
!
   40 continue
      ia=1
      la=1
      do 80 k=1,nfax
      if (igo.eq.60) go to 60
   50 continue
      call m1vpas(a(ia),a(ia+inc),work(1),work(2),trigs, &
            ink,2,jump,nx,lot,nh,ifax(k+1),la)
      igo=60
      go to 70
   60 continue
      call m1vpas(work(1),work(2),a(ia),a(ia+inc),trigs, &
          2,ink,nx,jump,lot,nh,ifax(k+1),la)
      igo=50
   70 continue
      la=la*ifax(k+1)
   80 continue
!
      if (isign.eq.-1) go to 130
!
!     if necessary, transfer data from work area
      if (mod(nfax,itwo).eq.1) go to 110
      ibase=1
      jbase=1
      do 100 l=1,lot
      i=ibase
      j=jbase
!dir$ ivdep
      do 90 m=1,n
      a(j)=work(i)
      i=i+1
      j=j+inc
   90 continue
      ibase=ibase+nx
      jbase=jbase+jump
  100 continue
!
!     fill in zeros at end
  110 continue
      go to 140
!
!     postprocessing (isign=-1):
!     --------------------------
!
  130 continue
      call m1fftb(work,a,trigs,inc,jump,n,lot)
!
  140 continue
      return
      end subroutine m2fftm
