C     Mark Iredell Subroutine From 7 April 1994

      subroutine fftfax(n,ifax,trigs)
      parameter(pi=3.14159265358979)
      dimension trigs(2*n),ifax(20),jfax(20),lfax(6)
      data lfax/8,5,4,3,2,1/
      del=2.*pi/n
      nhl=(n/2)-1
      trigs(1)=1.
      trigs(2)=0.
cdir$ ivdep
      do k=1,nhl
        angle=k*del
        trigs(2*k+1)=cos(angle)
        trigs(2*k+2)=sin(angle)
        trigs(2*k+1+n)=trigs(2*k+1)/trigs(2*k+2)
        trigs(2*k+2+n)=trigs(2*k+2)
      enddo
      nu=n
      k=0
      l=1
      ifac=lfax(l)
      ifax(1)=0
   20 continue
        nw=nu/ifac
        if(nw*ifac.eq.nu.and.(ifac.ne.8.or.k.eq.0)) then
          k=k+1
          jfax(k)=ifac
          nu=nw
        else
          l=l+1
          ifac=lfax(l)
          if(ifac.eq.1) then
            write(6,*) n,' contains illegal factors'
            return
          endif
        endif
      if(nu.gt.1) go to 20
      ifax(1)=k
      do i=2,k+1
        ifax(i)=jfax(k+2-i)
      enddo
      return
      end
