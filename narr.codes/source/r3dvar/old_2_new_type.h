!  declare type structure for conversion from old partition to new partition

         type old_2_new_cons

           sequence
           integer(4) ids,ide,jds,jde
           integer(4) inpes_old,jnpes_old,mype,npes_old
           integer(2),pointer::isend(:)
           integer(2),pointer::jsend(:)
           integer(2),pointer::irecv(:)
           integer(2),pointer::jrecv(:)
           integer(4),pointer::nrecv(:)
           integer(4),pointer::ndrecv(:)
           integer(4),pointer::nsend(:)
           integer(4),pointer::ndsend(:)
           integer(4),pointer::pe_of_injn_new(:,:),in_of_i_new(:),jn_of_j_new(:)

         end type old_2_new_cons
