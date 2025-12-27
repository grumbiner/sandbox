      subroutine stat ( a, im, jm )
      real(8) sum
      real(8) count
      real a ( im, jm )
       imax = 1
       imin = 1
       jmax = 1
       jmin = 1
       aamax = a(1,1)
       aamin = a(1,1)
       sum = 0.
       count = im * jm
       do j = 1, jm
          do i = 1, im
             sum = sum + a(i,j)
             if ( a(i,j) .gt. aamax ) then
                aamax = a(i,j)
                imax = i
                jmax = j
             end if
             if ( a(i,j) .lt. aamin ) then
                aamin = a(i,j)
                imin = i
                jmin = j
             end if
          end do
       end do
       print *, ' amax(',imax,',',jmax,')= ',aamax,
     &          ' amin(',imin,',',jmin,')= ',aamin,' avg = ',sum/count
      end
