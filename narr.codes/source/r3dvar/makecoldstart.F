C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE makecoldstart
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    makecoldstart  create coldstart file
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 94-03-08
C     
C ABSTRACT:
C     TEMPERATURE, SPECIFIC HUMIDITY, AND SURFACE PRESSURE
C     ARE UPDATED ON THE DOMAIN BOUNDARY BY APPLYING THE
C     PRE-COMPUTED TENDENCIES AT EACH TIME STEP.
C     
C PROGRAM HISTORY LOG:
C   87-??-??  MESINGER   - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D in HORIZONTAL
C   96-12-13  BLACK      - FINAL MODIFICATION FOR NESTED RUNS
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C     
C USAGE: CALL BOCOH FROM MAIN PROGRAM EBU
C   INPUT ARGUMENT LIST:
C     NONE
C  
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C  
C     UNIQUE: NONE
C  
C     LIBRARY: NONE
C  
C   COMMON BLOCKS: CTLBLK
C                  MASKS
C                  VRBLS
C                  PVRBLS
C                  CLDWTR
C                  BOCO
C                  MAPOT
C   
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$  
C     ******************************************************************
C-----------------------------------------------------------------------
      INCLUDE "mpif.h"
         include "my_comm.h"
      INCLUDE "PARMETA.comm"
      INCLUDE "PARMTBL.comm"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
!#include "sp.h"
C-----------------------------------------------------------------------
!                            P A R A M E T E R
!    & (IMJM=IM*JM-JM/2,LB=2*IM+JM-3,LP1=LM+1)
!                            P A R A M E T E R
!    & (ISIZ1=2*LB,ISIZ2=2*LB*LM)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS2.comm"
C-----------------------------------------------------------------------
      INCLUDE "BOCO.comm"
C-----------------------------------------------------------------------
      INCLUDE "MAPOT.comm"
C-----------------------------------------------------------------------
      INCLUDE "SOIL.comm"
C-----------------------------------------------------------------------
C***********************************************************************

       real(4) tempsoil(im,jm,nsoil)


      if(mype.eq.0) then
       rewind 52 
       write(52)run,idat,ihrst,ntsd
      end if
      call loc2glb(pd,temp1)
      if(mype.eq.0) write(52) temp1
      call loc2glb(res,temp1)
      if(mype.eq.0) write(52) temp1
      call loc2glb(fis,temp1)
      if(mype.eq.0) write(52) temp1
      do k=1,lm
       call loc2glb(u(idim1,jdim1,k),temp1)
       if(mype.eq.0) write(52) temp1
      end do
      do k=1,lm
       call loc2glb(v(idim1,jdim1,k),temp1)
       if(mype.eq.0) write(52) temp1
      end do
      do k=1,lm
       call loc2glb(t(idim1,jdim1,k),temp1)
       if(mype.eq.0) write(52) temp1
      end do
      do k=1,lm
       call loc2glb(q(idim1,jdim1,k),temp1)
       if(mype.eq.0) write(52) temp1
      end do
      call loc2glb(si,temp1)
      if(mype.eq.0) write(52) temp1
      call loc2glb(sno,temp1)
      if(mype.eq.0) write(52) temp1
      do k=1,nsoil
       call loc2glb(smc(idim1,jdim1,k),tempsoil(1,1,k))
      end do
      if(mype.eq.0) write(52) tempsoil
      call loc2glb(cmc,temp1)
      if(mype.eq.0) write(52) temp1
      do k=1,nsoil
       call loc2glb(stc(idim1,jdim1,k),tempsoil(1,1,k))
      end do
      if(mype.eq.0) write(52) tempsoil
      do k=1,nsoil
       call loc2glb(sh2o(idim1,jdim1,k),tempsoil(1,1,k))
      end do
      if(mype.eq.0) write(52) tempsoil
      call loc2glb(albedo,temp1)
      if(mype.eq.0) write(52) temp1

      if(mype.eq.0) close(52)

                             RETURN
                             END
