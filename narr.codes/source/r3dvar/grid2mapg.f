       subroutine grid2mapg(u,v,work,nlonmap,nlatmap, &
                             lbigmap,deltag,epsilong,wgts,iwgts,iflag)
            
!--------
!-------- interpolate from analysis grid to lat-lon grid
!-------- so we can use grads map background
!-------- do zero fill for areas outside analysis grid domain
!--------
!--------  iuv=0, scalar--just interpolate u
!--------  iuv=1, vector-- interpolate u,v, then rotate to ulon
!--------  iuv=2, vector-- interoplate u,v, then rotate to vlat
!--------
         dimension u(*),v(*)
         dimension work(nlonmap*nlatmap)
         dimension deltag(nlonmap*nlatmap)
         dimension epsilong(nlonmap*nlatmap)
         dimension wgts(nlonmap*nlatmap,lbigmap)
         dimension iwgts(nlonmap*nlatmap,lbigmap)
         dimension iflag(nlonmap*nlatmap)
!--------
         work=0.
         do i=1,nlonmap*nlatmap
          ug=0.
          vg=0.
          if(iflag(i).ne.0) then
           do k=1,lbigmap
            ug=ug+wgts(i,k)*u(iwgts(i,k))
            vg=vg+wgts(i,k)*v(iwgts(i,k))
            work(i)=deltag(i)*ug+epsilong(i)*vg
           end do
          end if
         end do
       return
       end
