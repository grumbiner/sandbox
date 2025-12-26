subroutine eta_ymdh(ietay,ietam,ietad,ietah)

!   return valid time of eta guess for date check against data

         include "PARMETA.comm"
         include "mpif.h"
         include "my_comm.h"
         include "mpp.h"

         LOGICAL RUN,FIRST,RESTRT,SIGMA

         include "CTLBLK.comm"

         real(4) rinc(5)
         integer(4) iolddat(8),inewdat(8)

         ietay=idat(3)
         ietam=idat(1)
         ietad=idat(2)
         ietah=ihrst
                 if(mype.eq.0) print *,' old ymdh=',ietay,ietam,ietad,ietah
         rinc(1)=0.        ! days
         iaddhrst=ihrst+nint(max(ntsd-1.,0.)*dt/3600.)
         rinc(2)=iaddhrst
              if(mype.eq.0) print *,' in eta_ymdh, ntsd,dt=',ntsd,dt
              if(mype.eq.0) print *,' in eta_ymdh, ihrst,rinc(2)=',ihrst,rinc(2)
         rinc(3)=0.        ! minutes
         rinc(4)=0.        ! seconds
         rinc(5)=0.        ! milliseconds
         iolddat(1)=idat(3)                      ! year
         iolddat(2)=idat(1)                      ! month
         iolddat(3)=idat(2)                      ! day
         iolddat(4)=0                            ! time zone
         iolddat(5)=0                            ! hour
         iolddat(6)=0                            ! minute
         iolddat(7)=0                            ! second
         iolddat(8)=0                            ! millisecond
         call w3movdat(rinc,iolddat,inewdat)
         ietay=inewdat(1)
         ietam=inewdat(2)
         ietad=inewdat(3)
         ietah=inewdat(5)
                 if(mype.eq.0) print *,' new ymdh=',ietay,ietam,ietad,ietah

       return
       end subroutine eta_ymdh
