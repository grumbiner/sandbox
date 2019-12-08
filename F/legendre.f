      subroutine legendre(ipd,cosclt,maxwv,iromb,ex,px,pnm)
      dimension pnm((maxwv+1)*((iromb+1)*maxwv+2)/2)
      dimension ex(0:maxwv+1),px(-1:maxwv+1)
      sinclt=sqrt(1.-cosclt**2)
      k=0
      ex(0)=0.
      px(-1)=0.
      px(0)=sqrt(0.5)
      do m=0,maxwv
        if(m.gt.0) px(0)=px(0)*sinclt/(ex(1)*sqrt(float(2*m)))
        do n=m+1,maxwv+iromb*m+1
          ex(n-m)=sqrt(float(n**2-m**2)/float(4*n**2-1))
        enddo
cdir$ nextscalar
        do n=m+1,maxwv+iromb*m+1
          px(n-m)=(cosclt*px(n-m-1)-ex(n-m-1)*px(n-m-2))/ex(n-m)
        enddo
        if(ipd.eq.0) then
          do n=m,maxwv+iromb*m
            k=k+1
            pnm(k)=px(n-m)
          enddo
        else
          do n=m,maxwv+iromb*m
            k=k+1
            pnm(k)=n*ex(n-m+1)*px(n-m+1)-(n+1)*ex(n-m)*px(n-m-1)
          enddo
        endif
      enddo
      return
      end
