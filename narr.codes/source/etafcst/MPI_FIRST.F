      SUBROUTINE MPI_FIRST
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  MPI_FIRST   INTIALIZES MPI STUFF
C   PRGRMMR: TUCCILLO        ORG:  IBM       DATE: 00-01-20
C
C ABSTRACT:   INTIALIZES MPI STUFF
C
C PROGRAM HISTORY LOG:
C   00-01-20  TUCCILLO - ORIGINATOR
C
C USAGE:  CALL MPI_FIRST
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   INPUT FILES:  NONE
C
C   OUTPUT FILES:  NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C            MPI_INIT
C            MPI_COMM_SIZE
C            MPI_COMM_RANK
C            PARA_RANGE
C
C   EXIT STATES:
C     COND =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C
C$$$
c
      include "parmeta"
      include "PARA.comm"
      include 'mpif.h'
      include "mpp.h"
c
C
      integer ierr
       LNIP = IM/INPES
       LNJP=JM/JNPES
c
C***
C***  NUM_PROCS IS THE NUMBER OF TASKS DOING THE QUILTING
C***  IN THIS SERVER GROUP
C***
      call mpi_comm_size(mpi_comm_comp,num_procs, ierr )
      call mpi_comm_rank(mpi_comm_comp,me, ierr )
c
c     if ( me .eq. 0 ) then
         print *, ' num_procs = ',num_procs
c     end if

      if ( num_procs .gt. JNPES ) then
         print *, ' too many MPI tasks, max is ',JNPES, ', stopping'
         call MPI_ABORT(MPI_COMM_WORLD,1,ierr)
      end if
c
      if ( num_procs .gt. 1024 ) then
         print *, ' too many MPI tasks, max is 1024, stopping'
         call MPI_ABORT(MPI_COMM_WORLD,1,ierr)
      end if
c
C***
C***  JS_X AND JS_Y ARE THE STARTING AND ENDING ROWS OF TASKS
C***  IN THE MODEL FORECAST DECOMPOSITION THAT WILL BE SENDING
C***  TO EACH QUILT TASK
C***
C***  JSTA IS THE FIRST FORECAST TASK AND JEND IS THE LAST
C***  FORECAST TASK IN THE ENTIRE RANGE OF FORECAST TASKS
C***  THAT WILL BE SENDING TO EACH QUILT TASK.  REMEMBER
C***  THAT AN INTEGER NUMBER OF FORECAST TASK ROWS IS
C***  SENT TO EACH QUILT TASK.
C***
      do i = 0, num_procs - 1
         call para_range(0,JNPES-1,num_procs,i,js_x,je_x)
         jsta(i) = js_x * INPES
         jend(i) = jsta(i) + (je_x-js_x+1)*INPES -1
         if ( me .eq. 0 ) then
            print *, ' task id, jsta, end = ',i,jsta(i),jend(i)
         end if
      end do
c
c     locations
C
C***  PARAMETER LNIP (LNJP) IS SMALLEST THAT THE I (J) EXTENT
C***  OF EACH SUBDOMAIN CAN BE.
C***  IRMND (JRMND) IS THE NUMBER OF "REMAINDER" I (J) POINTS
C***  THAT WILL BE GIVEN TO THE LEFTMOST (LOWERMOST) PEs.

      IRMND=MOD(IM,INPES)
      JRMND=MOD(JM,JNPES)

      DO IPE = 0, NPES - 1
C
      IPOSN=MOD(IPE,INPES)+1
      JPOSN=IPE/INPES+1
C
C***  GLOBAL LIMITS OF THIS PE's SUBDOMAIN
C
      MY_IS_GLB_A(IPE)=(IPOSN-1)*LNIP+MIN(IRMND,IPOSN-1)+1
      MY_IE_GLB_A(IPE)=MY_IS_GLB_A(IPE)+LNIP-1
      IF(IPOSN.LE.IRMND)MY_IE_GLB_A(IPE)=MY_IE_GLB_A(IPE)+1
C
      MY_JS_GLB_A(IPE)=(JPOSN-1)*LNJP+MIN(JRMND,JPOSN-1)+1
      MY_JE_GLB_A(IPE)=MY_JS_GLB_A(IPE)+LNJP-1
      IF(JPOSN.LE.JRMND)MY_JE_GLB_A(IPE)=MY_JE_GLB_A(IPE)+1
c
      if ( me .eq. 0 ) then
c        print *, ' ipe,  MY_IS_GLB,MY_IE_GLB,MY_JS_GLB, MY_JE_GLB =',
c    * ipe, MY_IS_GLB_A(IPE),MY_IE_GLB_A(IPE),
c    * MY_JS_GLB_A(IPE),MY_JE_GLB_A(IPE)
      end if
      END DO
c
c     dimensioning information
c
      MY_ISD = 1
      MY_IED = IM
      MY_JSD = MY_JS_GLB_A(jsta(me)) -2
      MY_JED = MY_JE_GLB_A(jend(me)) +2
      IF ( MY_JSD .lt. 1 ) MY_JSD = 1
      IF ( MY_JED .gt. JM ) MY_JED = JM
C
      print *, ' ME, MY_ISD,MY_IED,MY_JSD,MY_JED = ',
     & me, MY_ISD,MY_IED,MY_JSD,MY_JED
C
      jsta_i = MY_JS_GLB_A(jsta(me))
      jend_i = MY_JE_GLB_A(jend(me))
      jsta_im  = jsta_i
      jsta_im2 = jsta_i
      jend_im  = jend_i
      jend_im2 = jend_i
      if ( me .eq. 0 ) then
         jsta_im  = 2
         jsta_im2 = 3
      end if
      if ( me .eq. num_procs - 1 ) then
         jend_im  = jm - 1
         jend_im2 = jm - 2
      end if
c
      print *, ' jsta_i,jend_i,jsta_im,jend_im,jsta_im2,jend_im2= ',
     * jsta_i,jend_i,jsta_im,jend_im,jsta_im2,jend_im2
c     neighbors
c
      iup = me + 1
      idn = me - 1
      if ( me .eq. 0 ) then
         idn = MPI_PROC_NULL
      end if
      if ( me .eq. num_procs - 1 ) then
         iup = MPI_PROC_NULL
      end if
C
c     print *, ' ME, NUM_PROCS = ',me,num_procs


      end
