      SUBROUTINE DIST ( A, B ) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  DIST        DISTRIBUTES DATA FROM TASK 0
C   PRGRMMR: TUCCILLO        ORG:  IBM       DATE: 00-01-20
C
C ABSTRACT:   DISTRIBUTES DATA FROM TASK 0
C
C PROGRAM HISTORY LOG:
C   00-01-20  TUCCILLO - ORIGINATOR
C
C USAGE:  CALL DIST(A,B)
C
C   INPUT ARGUMENT LIST:
C     A - ARRAY TO BE DISTRIBUTED   
C
C   OUTPUT ARGUMENT LIST:
C     B - DISTRIBUTED DATA         
C
C   INPUT FILES:  NONE
C
C   OUTPUT FILES:  NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C            MPI_SEND
C            MPI_RECV
C
C   EXIT STATES:
C     COND =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C
C$$$
      include "parmeta"
      include "PARA.comm"
      include 'mpif.h'
      include "mpp.h"
      real a ( im, jm ) 
      real b ( im, my_jsd:my_jed )
      real buf ( im * jm )
      integer status(MPI_STATUS_SIZE)
      integer i, j
      integer ierr
c
      if ( me .eq. 0 ) then
c
         do k = jsta(me), jend(me)
            do j =  my_js_glb_a(k), my_je_glb_a(k)
               do i =  my_is_glb_a(k), my_ie_glb_a(k)
                  b ( i, j ) = a ( i, j )
               end do
            end do
         end do

c        send to everyone else
c
         do ii = 1, num_procs - 1
            iii = 0
            do k = jsta(ii), jend(ii)
               do j =  my_js_glb_a(k), my_je_glb_a(k)
                  do i =  my_is_glb_a(k), my_ie_glb_a(k)
                     iii = iii + 1
                     buf ( iii ) = a ( i, j )
                  end do
               end do
             end do
            call mpi_send(buf,iii,MPI_REAL,ii,
     *           ii,MPI_COMM_COMP,ierr)
         end do
      else
         call mpi_recv(buf,im*jm,MPI_REAL,0,
     *             me,MPI_COMM_COMP,status,ierr)
         iii = 0
         do k = jsta(me), jend(me)
            do j =  my_js_glb_a(k), my_je_glb_a(k)
              do i =  my_is_glb_a(k), my_ie_glb_a(k)
                 iii = iii + 1
                 b ( i, j ) = buf ( iii )
              end do
            end do
         end do
c
      end if
      end               
