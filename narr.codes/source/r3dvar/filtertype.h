!  declare type structures for recursive anisotropic filter constants

         type filter_cons

           sequence
           logical oldf
           integer(4) icolorx,icolory,icolorz
           integer(4) ids,ide,jds,jde,kds,kde
           integer(4) ips,ipe,jps,jpe,kps,kpe
           integer(4) ims,ime,jms,jme,kms,kme
           integer(4) ids2,ide2,jds2,jde2,kds2,kde2
           integer(4) ips2,ipe2,jps2,jpe2,kps2,kpe2
           integer(4) ims2,ime2,jms2,jme2,kms2,kme2
           integer(4) inpes,jnpes,nhalo,mype,npes
           integer(4) npass
           integer(4) mpass
           integer(4) no_interp
           integer(4) nsmooth
           integer(4) nsmooth_shapiro
           integer(4) int_ord
           integer(4) ifilt_ord
           integer(4) npointsmaxall
           integer(4) npointsmax
           integer(4) npoints_send
           integer(4) npoints_recv
           integer(4) nstrings
           integer(4),pointer::istart(:)
           integer(2),pointer::ia(:)
           integer(2),pointer::ja(:)
           integer(2),pointer::ka(:)
           integer(4),pointer::ib(:)
           integer(4),pointer::nrecv(:)
           integer(4),pointer::ndrecv(:)
           integer(4),pointer::nsend(:)
           integer(4),pointer::ndsend(:)
           integer(4),pointer::pe_of_injn(:,:),in_of_i(:),jn_of_j(:)
           integer(4),pointer::pe_of_injn2(:,:),in_of_i2(:),jn_of_j2(:)
           integer(4),pointer::iasup(:),jasup(:)
           integer(4),pointer::ibsup(:),jbsup(:)
           integer(4),pointer::nrecvsup(:)
           integer(4),pointer::ndrecvsup(:)
           integer(4),pointer::nsendsup(:)
           integer(4),pointer::ndsendsup(:)
           integer(4),pointer::lensstr(:,:)
           integer(4),pointer::lensstrsave(:,:)
           integer(4),pointer::ins1(:,:)
           integer(4),pointer::ins1save(:,:)
           real(4),pointer::wts(:,:,:)
           real(4),pointer::wtssave(:,:,:)
           real(4),pointer::nu(:)
           real(4),pointer::rsnui(:,:)
           real(4),pointer::rsnuisave(:,:)
           real(4),pointer::lnf(:,:,:)
           real(4),pointer::lnfsave(:,:,:)
           real(4),pointer::bnf(:,:)
           real(4),pointer::bnfsave(:,:)
           real(4),pointer::amp(:,:,:)

         end type filter_cons

         type super_grid

           sequence
           real(4),pointer::f(:,:,:)

         end type super_grid
