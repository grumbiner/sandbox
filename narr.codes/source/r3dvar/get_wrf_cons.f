       SUBROUTINE get_wrf_cons_0(myxsc,myxec,myysc,myyec,   &
           myxsc_glb,myxec_glb,myysc_glb,myyec_glb, &
           nxc,nyc,npc,nxcglb,nycglb, &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe,  &                         ! patch indices
             ims, ime, jms, jme, kms, kme,  &                         ! memory indices
             inpesout, jnpesout, mype, npes )                         ! processor info

!    from current analysis domain variables, obtain wrf-style domain variables

         include 'mpif.h'
      include "my_comm.h"
         include 'PARMETA.comm'

         INTEGER(4), INTENT(IN) :: myxsc,myxec,myysc,myyec, &
                           myxsc_glb,myxec_glb,myysc_glb,myyec_glb, &
                            nxc,nyc,npc,nxcglb,nycglb,mype,npes

         INTEGER(4), INTENT(OUT) :: ids,ide,jds,jde,kds,kde, &  ! domain indices
                                    ips,ipe,jps,jpe,kps,kpe, &  ! patch indices
                                    ims,ime,jms,jme,kms,kme     ! memory indices

         INTEGER(4), INTENT(OUT) :: inpesout,jnpesout

         integer(4) ierr

         inpesout=inpes
         jnpesout=jnpes
         ids=1
         ide=nxcglb
         jds=1
         jde=nycglb
         kds=1
         kde=npc
         ips=myxsc_glb
         ipe=myxec_glb
         jps=myysc_glb
         jpe=myyec_glb
         kps=1
         kpe=npc
         ims=max(ids,ips-myxsc+1)
         ime=min(ide,ims+nxc-1)
         jms=max(jds,jps-myysc+1)
         jme=min(jde,jms+nyc-1)
         kms=1
         kme=npc

       return
       end subroutine get_wrf_cons_0

       subroutine get_wrf_cons_1( &
             ids, ide, jds, jde, kds, kde, &                          ! domain indices
             ips, ipe, jps, jpe, kps, kpe, &                          ! patch indices
             ims, ime, jms, jme, kms, kme, &                          ! memory indices
             inpes, jnpes, mype, npes, pe_of_injn, in_of_i, jn_of_j ) ! processor info

         IMPLICIT NONE
         include 'mpif.h'
      include "my_comm.h"

         INTEGER(4), INTENT(IN) :: ids,ide,jds,jde,kds,kde, &   ! domain indices
                                   ips,ipe,jps,jpe,kps,kpe, &   ! patch indices
                                   ims,ime,jms,jme,kms,kme      ! memory indices

         INTEGER(4), INTENT(IN) :: inpes,jnpes,mype,npes
         INTEGER(4), INTENT(OUT) :: pe_of_injn(inpes,jnpes),in_of_i(ids:ide),jn_of_j(jds:jde)

         integer(4) in,jn,mpe,ierr
         integer(4) ips_tab(0:npes-1),ipe_tab(0:npes-1)
         integer(4) jps_tab(0:npes-1),jpe_tab(0:npes-1)


         ips_tab(mype)=ips
         ipe_tab(mype)=ipe
         jps_tab(mype)=jps
         jpe_tab(mype)=jpe
         do mpe=0,npes-1
          call mpi_bcast(ips_tab(mpe),1,mpi_integer4,mpe,my_comm,ierr)
          call mpi_bcast(ipe_tab(mpe),1,mpi_integer4,mpe,my_comm,ierr)
          call mpi_bcast(jps_tab(mpe),1,mpi_integer4,mpe,my_comm,ierr)
          call mpi_bcast(jpe_tab(mpe),1,mpi_integer4,mpe,my_comm,ierr)
         end do

         mpe=0
         do jn=1,jnpes
          do in=1,inpes
           in_of_i(ips_tab(mpe):ipe_tab(mpe))=in
           jn_of_j(jps_tab(mpe):jpe_tab(mpe))=jn
           pe_of_injn(in,jn)=mpe
           mpe=mpe+1
          end do
         end do

       return
       end subroutine get_wrf_cons_1 
