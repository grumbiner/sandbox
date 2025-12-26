      subroutine gautoice(glats,nglats,nglons,hlat,hlon,sm,sice,
     &                            ggsli,ggrid,egrid)
C
      INCLUDE "parmeta.res"
C

      dimension glats(nglats),sm(im,jm),ggrid(nglons,nglats)
      dimension egrid(im,jm),ggsli(nglons,nglats)
      dimension hlat(im,jm),hlon(im,jm),sice(im,jm)

      do j = 1,jm
      do i = 1,im

        if(sice(i,j).eq.1.0) then
          y = (nglats-1) / (glats(nglats) - glats(1))
     &        * (hlat(i,j)-glats(1)) + 1
          x = float(nglons) / 360.0 * hlon(i,j) + 1
          iy = y
          ix = x
          print *, ix,iy, hlon(i,j), hlat(i,j)
          iyp1 = iy + 1
          ixp1 = ix + 1
c  First calculate the bilinear weights assuming a constant dx and dy
          wxy =     (ixp1-x) * (iyp1 - y)
          wxp1y =   (x-ix)   * (iyp1 - y)
          wxp1yp1 = (x-ix)   * (y-iy)
          wxyp1 = (ixp1 - x) * (y-iy)
c  Take care of the wrap around the 0 meridan when necessary
          if(ix.eq.nglons) ixp1 = 1

c  Only use ice points (ggsli=2) for the interpolation
          if(ggsli(ix,iy).ne.2.0) wxy = 0.0
          if(ggsli(ixp1,iy).ne.2.0) wxp1y = 0.0
          if(ggsli(ixp1,iyp1).ne.2.0) wxp1yp1 = 0.0
          if(ggsli(ix,iyp1).ne.2.0) wxyp1 = 0.0

          sum = wxy + wxp1y + wxp1yp1 + wxyp1
          if(sum.eq.0.0) then

c  The eta ice point is surrounded by gaussian grid non-ice points
c  Look for nearest gaussian grid ice point and assign it to the
c  Eta grid point. Find nearest at this latitude
            do jfnd = iy,nglats
            do ifnd = ix,nglons
            if(ggsli(ifnd,jfnd).eq.2.0) then
                wxy = 1.0
                sum = 1.0
                ix = ifnd
                iy = jfnd
                go to 3356
            end if
            end do
            do ifnd = ix,1,-1
            if(ggsli(ifnd,jfnd).eq.2.0) then
                wxy = 1.0
                sum = 1.0
                ix = ifnd
                iy = jfnd
                go to 3356
            end if
            end do
            end do
       print *,"Oh Oh can't find a land point at:",hlat(i,j),hlon(i,j)
       print *,((ggsli(ii,jj),ii=ix,ix+3),jj=iy,iy+2)
                wxy=1.0
                sum=1.0
                ggrid(ix,iy)=260.0
3356        continue

          end if
          wxy = wxy / sum
          wxp1y = wxp1y / sum
          wxp1yp1 = wxp1yp1 / sum
          wxyp1 = wxyp1 / sum
          egrid(i,j) = wxy     * ggrid(ix,iy)     +
     &                 wxp1y   * ggrid(ixp1,iy)   +
     &                 wxp1yp1 * ggrid(ixp1,iyp1) +
     &                 wxyp1   * ggrid(ix,iyp1)

        end if
      end do
      end do
      return
      end
