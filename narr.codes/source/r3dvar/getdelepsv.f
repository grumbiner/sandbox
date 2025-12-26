       subroutine getdelepsv(etheta,rlon,rlat,erlon0,dg2rad, &
            cerlat0,serlat0,rlong,rlatg,deltag,epsilong,n)
!--------
!-------- given the angle a vector makes with the x-axis (positive east)
!-------- on the earth (etheta), compute the cosine and sine of the
!-------- angle the same vector makes with the x-axis of the rotated grid.
!--------
!-- etheta:   input angle vector makes with earth positive east direction
!-- rlon,rlat:    input earth coordinates of point of interest
!-- erlon0:   earth longitude of origin of rotated grid
!-- dg2rad:   degrees to radians conversion
!-- cerlat0,serlat0: cos,sin of earth latitude of origin of rotated grid
!-- rlong,rlatg: previously computed coordinates of point in rotated system
!-- deltag,epsilong: desired output cosine and sine of angle vector
!--                   makes with x-axis of rotated grid
!-- n:     number of points to process
!--------
         dimension etheta(n),rlon(n),rlat(n),rlong(n),rlatg(n)
         dimension deltag(n),epsilong(n)
!--------
         deltag=1.
         epsilong=0.
         do i=1,n
          if(abs(abs(rlat(i))-90.).gt.1.e-3) then
           delta=cos(etheta(i)*dg2rad)
           epsilon=sin(etheta(i)*dg2rad)
           crlat=cos(rlat(i)*dg2rad)
           salpha=serlat0*sin(rlong(i)*dg2rad)/crlat
           calpha=(cos(rlatg(i)*dg2rad)*cerlat0 &
             -serlat0*sin(rlatg(i)*dg2rad)*cos(rlong(i)*dg2rad))/crlat
           deltag(i)=delta*calpha-epsilon*salpha
           epsilong(i)=delta*salpha+epsilon*calpha
          end if
         end do
       return
       end
