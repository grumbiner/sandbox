       subroutine initialize_parmeta(im_in,jm_in,lm_in,
     &                             inpes_in,jnpes_in,
     &                             itb_in,jtb_in,itbq_in,jtbq_in)

! initialize common/parmeta/ and common/parmtbl/

         include "mpif.h"

         include "PARMETA.comm"
         include "PARMTBL.comm"
         include "my_comm.h"

         call mpi_comm_rank(my_comm,mype,ierr)
         iroot=0

         im=im_in
         jm=jm_in
         lm=lm_in
         inpes=inpes_in
         jnpes=jnpes_in

         igstl=-5 ; igstr=5
         jgstl=-5 ; jgstr=5
         itail=im-(inpes*(im/inpes))
         jtail=jm-(jnpes*(jm/jnpes))
         idim1=igstl ; idim2=im/inpes+igstr+1
         jdim1=jgstl ; jdim2=jm/jnpes+jgstr+1
         imjm=im*jm-jm/2
         jmp1=jm+1
         jam=6+2*(jm-10)
         lb=2*im+jm-3
         lm1=lm-1
         lm2=lm-2
         lp1=lm+1
         imt=2*im-1
         isiz1=2*lb
         isiz2=2*lb*lm
         im1=im-1
         jamd=(jam*2-10)*3

         itb=itb_in ; jtb=jtb_in
         itbq=itbq_in ; jtbq=jtbq_in

         if(mype.eq.iroot) then
          print *,' initialize common/parmeta/'
          print *,'      im,jm,lm=',im,jm,lm
          print *,'      inpes,jnpes=',inpes,jnpes
          print *,'      igstl,igstr,jgstl,jgstr=',
     &                       igstl,igstr,jgstl,jgstr
          print *,'      itail,jtail=',itail,jtail
          print *,'      idim1,idim2=',idim1,idim2
          print *,'      jdim1,jdim2=',jdim1,jdim2
          print *,'      imjm,jmp1=',imjm,jmp1
          print *,'      jam,lb=',jam,lb
          print *,'      lm1,lm2,lp1,imt=',lm1,lm2,lp1,imt

          print *,' initialize common/parmtbl/'
          print *,' itb,jtb=',itb,jtb
          print *,' itbq,jtbq=',itbq,jtbq
         end if

       return
       end
