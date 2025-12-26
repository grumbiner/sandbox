subroutine unstagadr(ixeta_of_ix,iyeta_of_iy,nx,ny,imeta,jmeta, &
                     imetaglb,jmetaglb,wb,sb,wbglb,sbglb,dlon0,dlat0,igrid)

! obtain address lists of which points to pick from an eta grid so that 
!   we have every other row for the unstaggered preliminary analysis grid

         include 'types.h'
         include "mpif.h"
      include "my_comm.h"
         include "r3dv_data.comm"

         integer(4) ixeta_of_ix(nx),iyeta_of_iy(ny)
         real(4) rx(nx),ry(ny)


!          print *,' in unstagadr, nx,nxcom=',nx,gridcom(igrid)%nxcom       !xx
!          print *,' in unstagadr, ny,nycom=',ny,gridcom(igrid)%nycom       !xx

         rx=gridcom(igrid)%rxcom
         ry=gridcom(igrid)%rycom

         call mpi_comm_rank(my_comm,mype,ierr)

         xetamin=wbglb
         xetamax=wbglb+(imetaglb-1)*2.*dlon0


         x3=rx(kx3_loc(mype,igrid))
         rieta3=1.+(x3-wb)/(2.*dlon0)
         ieta3=nint(rieta3)
         iplus=nint((xetamax-x3)/(2.*dlon0))
         ietamax=ieta3+iplus
         iminus=nint((x3-xetamin)/(2.*dlon0))
         ietamin=ieta3-iminus

         errx=abs(rieta3-ieta3)
!        print *,' in unstagadr should have errx<<1, mype,errx=', &
!                    mype,errx
         ibad=0
         do i=kx3_loc(mype,igrid),kx8_loc(mype,igrid)
          ieta=max(ietamin,min(ieta3+i-kx3_loc(mype,igrid),ietamax))
          if(ieta.lt.1.or.ieta.gt.imeta) ibad=ibad+1
          ixeta_of_ix(i)=ieta
         end do
         do i=1,kx3_loc(mype,igrid)
          ixeta_of_ix(i)=ixeta_of_ix(kx3_loc(mype,igrid))
         end do
         do i=kx8_loc(mype,igrid),nx
          ixeta_of_ix(i)=ixeta_of_ix(kx8_loc(mype,igrid))
         end do
         if(ibad.gt.0) print *,' in unstagadr, mype,ibad=',mype,ibad
         

         yetamin=sbglb
         yetamax=sbglb+(jmetaglb-1)*dlat0

         y3=ry(ky3_loc(mype,igrid))
         rjeta3=1.+(y3-sb)/dlat0
         jeta3=nint(rjeta3)
         jplus=nint((yetamax-y3)/dlat0)
!        jetamax=jeta3+jplus-mod(jplus,2) ????????
         jetamax=jeta3+jplus
         jminus=nint((y3-yetamin)/dlat0)
         jetamin=jeta3-jminus
       
         
         erry=abs(rjeta3-jeta3)
!        print *,' in unstagadr should have erry<<1, mype,erry=', &
!                    mype,erry
         jbad=0
         do j=ky3_loc(mype,igrid),ky8_loc(mype,igrid)
          jeta=max(jetamin,min(jeta3+2*(j-ky3_loc(mype,igrid)),jetamax))
          if(jeta.lt.1.or.jeta.gt.jmeta) jbad=jbad+1
          iyeta_of_iy(j)=jeta
         end do
         do j=1,ky3_loc(mype,igrid)
          iyeta_of_iy(j)=iyeta_of_iy(ky3_loc(mype,igrid))
         end do
         do j=ky8_loc(mype,igrid),ny
          iyeta_of_iy(j)=iyeta_of_iy(ky8_loc(mype,igrid))
         end do
         if(jbad.gt.0) print *,' in unstagadr, mype,jbad=',mype,jbad

       return
       end subroutine unstagadr
