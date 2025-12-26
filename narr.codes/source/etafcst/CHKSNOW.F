      subroutine chksnow
c
c At the first physics time step after the top of each hour, check the snow
c array against the SR (snow/total precip ratio).  If SR .ge. 0.9, set this
c point to missing (so we won't do snow adjustment here).
c
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
                             P A R A M E T E R
     & (ITB=76,JTB=134,ITBQ=152,JTBQ=440,LP1=LM+1)
C-----------------------------------------------------------------------
      INCLUDE "PPTASM.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS2.comm"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (RTD=57.2957795)
C
      TIMES=(NTSD-1)*DT
      if (mod(times,3600.) .lt. nphs*dt) then
        ihr=(times-1.0)/3600.+1
        call loc2glb(sr,temp1)
        if (mype.eq.0) then 
           write(98,*) 'NTSD=', NTSD,' times=', times,' output SR:'
           write(90) temp1
        endif
        DO 20 J=MYJS,MYJE
          DO 10 I=MYIS,MYIE
C
C In the operational Eta, we do not use the data in the area where ETA
C thinks it's snowing (we use sr(i,j).ge.0.9 as criterion), because the
C real-time hourly observations (radar or gauge) are highly unreliable.
C For reanalysis, more reliable hourly snow observations may be available
C within CONUS, so we are changing it to 'mask out the precip data when
C it's north of 49N and likely to be snowing'.
C
C OPNL      if (sr(i,j).ge.0.9) pptdat(i,j,ihr) = 999.
            if (glat(i,j)*RTD.ge.49. .and. sr(i,j).ge.0.9) 
     &         pptdat(i,j,ihr) = 999.
  10	  continue
  20    continue
      endif
      return
      end
