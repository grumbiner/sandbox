      subroutine rfftmlt(a,w,trigs,ifax,inc,jump,n,lot,isign)
C     Mark Iredell 2 May 1995
      parameter(ldx=64)
      dimension a(inc,jump,lot),trigs(2*n),ifax(20)
      dimension w((lot/ldx*ldx+ldx/lot*lot)/(lot/ldx+ldx/lot),n,2)
      ib=1
      ic=2
      ldm=min(lot,ldx)
      nfax=ifax(1)
      if(isign.eq.-1) then
        do m=0,lot-1,ldx
          ldo=min(lot-m,ldx)
          la=n
          if(nfax.eq.1) then
            do j=1,n
              do l=1,ldo
                w(l,j,ib)=a(1,j,m+l)
              enddo
            enddo
          else
            k=1+nfax
            la=la/ifax(1+nfax)
            call qpass(a(1,1,m+1),w(1,1,ib),trigs,
     &                 inc,ldm,jump,1,ldo,n,ifax(1+nfax),la,ierr)
            do k=nfax,3,-1
              la=la/ifax(k)
              call qpass(w(1,1,ib),w(1,1,ic),trigs,
     &                   ldm,ldm,1,1,ldo,n,ifax(k),la,ierr)
              id=ib
              ib=ic
              ic=id
            enddo
          endif
          la=la/ifax(2)
          call qpass(w(1,1,ib),a(1,1,m+1),trigs,
     &               ldm,inc,1,jump,ldo,n,ifax(2),la,ierr)
        enddo
      else
        do m=0,lot-1,ldx
          ldo=min(lot-m,ldx)
          la=1
          call rpass(a(1,1,m+1),w(1,1,ib),trigs,
     &               inc,ldm,jump,1,ldo,n,ifax(2),la,ierr)
          if(nfax.eq.1) then
            do j=1,n
              do l=1,ldo
                a(1,j,m+l)=w(l,j,ib)
              enddo
            enddo
          else
            la=la*ifax(2)
            do k=3,nfax
              call rpass(w(1,1,ib),w(1,1,ic),trigs,
     &                   ldm,ldm,1,1,ldo,n,ifax(k),la,ierr)
              id=ib
              ib=ic
              ic=id
              la=la*ifax(k)
            enddo
            call rpass(w(1,1,ib),a(1,1,m+1),trigs,
     &                 ldm,inc,1,jump,ldo,n,ifax(nfax+1),la,ierr)
          endif
          do l=1,ldo
            a(1,n+1,m+l)=0.
            a(1,n+2,m+l)=0.
          enddo
        enddo
      endif
      return
      end
