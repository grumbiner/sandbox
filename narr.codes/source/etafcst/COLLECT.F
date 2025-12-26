      SUBROUTINE COLLECT ( A, B ) 
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  COLLECT     COLLECT UP DATA ON TASK 0
C   PRGRMMR: TUCCILLO        ORG:  IBM       DATE: 00-01-20
C
C ABSTRACT:  COLLECTS UP DATA ON TASK 0
C
C PROGRAM HISTORY LOG:
C   00-01-20  TUCCILLO - ORIGINATOR
C
C USAGE:  CALL COLLECT(A,B)
C
C   INPUT ARGUMENT LIST:
C     A - ARRAY TO BE COLLECTED FROM
C
C   OUTPUT ARGUMENT LIST:
C     B - RESULTS OF THE COLLECTION
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
      real a ( im, my_jsd:my_jed )
      real b ( im, jm ) 
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

c        receive from everyone else
c
         do ii = 1, num_procs - 1
            call mpi_send(ii,1,MPI_INTEGER,ii,0,MPI_COMM_COMP,ierr)
            call mpi_recv(buf,im*jm,MPI_REAL,ii,
     *           ii,MPI_COMM_COMP,status,ierr)
            iii = 0
            do k = jsta(ii), jend(ii)
               do j =  my_js_glb_a(k), my_je_glb_a(k)
                  do i =  my_is_glb_a(k), my_ie_glb_a(k)
                     iii = iii + 1
                     b ( i, j ) = buf ( iii )
                  end do
               end do
             end do
         end do
      else
         iii = 0
         do k = jsta(me), jend(me)
            do j =  my_js_glb_a(k), my_je_glb_a(k)
              do i =  my_is_glb_a(k), my_ie_glb_a(k)
                 iii = iii + 1
                 buf ( iii ) = a ( i, j )
              end do
            end do
         end do
         call mpi_recv(ii,1,MPI_INTEGER,0,0,MPI_COMM_COMP,status,ierr)
         call mpi_send(buf,iii,MPI_REAL,0,me,MPI_COMM_COMP,ierr)
c
      end if
      end               
