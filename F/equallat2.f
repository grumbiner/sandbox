      subroutine equallat(k,a,w)
      dimension a(k),w(k)
      parameter(pi=3.14159265358979)
      kh=k/2
      dlt=pi/(k-1)
cdir$ ivdep
      do j=1,kh
        a(j)=cos((j-1)*dlt)
        a(k+1-j)=-a(j)
      enddo
      w(1)=1.-cos(dlt*0.5)
      w(k)=w(1)
      sindlt=2.*sin(dlt*0.5)
cdir$ ivdep
      do j=2,kh
        w(j)=sin((j-1)*dlt)*sindlt
        w(k+1-j)=w(j)
      enddo
      if(k.ne.kh*2) then
        a(kh+1)=0.
        w(kh+1)=sindlt*0.5
      endif
      return
      end
