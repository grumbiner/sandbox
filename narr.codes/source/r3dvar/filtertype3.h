!  declare type structures for recursive anisotropic filter constants

         type basic_string3

           sequence
           integer(4) beginx
           integer(4) beginy
           integer(4) beginz
           integer(4) endx
           integer(4) endy
           integer(4) endz
           integer(4) lenstring
           integer(4) jumpx
           integer(4) jumpy
           integer(4) jumpz

         end type basic_string3

         type reconcile_ends3

           sequence
           integer(4) lenall
           integer(4) lensend
           integer(4) lenrecv
           integer(4),pointer::x_send(:)
           integer(4),pointer::y_send(:)
           integer(4),pointer::z_send(:)
           integer(4),pointer::nsend(:)
           integer(4),pointer::ndsend(:)
           integer(4),pointer::x_recv(:)
           integer(4),pointer::y_recv(:)
           integer(4),pointer::z_recv(:)
           integer(4),pointer::nrecv(:)
           integer(4),pointer::ndrecv(:)
           real(4),pointer::   wpk(:)

         end type reconcile_ends3

         type filter_cons3

           sequence
           integer(4) nstrings_all
           integer(4) nstrings_forward_interior
           integer(4) nstrings_forward_exterior
           integer(4) nstrings_backward_interior
           integer(4) nstrings_backward_exterior
           integer(4) npass
           integer(4) nsmooth
           integer(4) kitermax
           type(basic_string3),pointer::forward_interior(:)
           type(basic_string3),pointer::forward_exterior(:)
           type(basic_string3),pointer::backward_interior(:)
           type(basic_string3),pointer::backward_exterior(:)
           type(reconcile_ends3),pointer::forward_ends(:,:)
           type(reconcile_ends3),pointer::backward_ends(:,:)
           real(4),pointer::amp(:,:,:)
           real(4),pointer::alpha(:,:,:,:)
           real(4),pointer::beta(:,:,:,:)

         end type filter_cons3
