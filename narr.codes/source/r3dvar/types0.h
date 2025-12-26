!   declare types:

         type rad_obs0

           sequence
           integer(4)          type
           integer(4)          group
           integer(8)          label
           real(4)             lon
           real(4)             lat
           real(4)             long
           real(4)             latg
           real(4)             time
           integer(4)          nsig
           integer(4)          nsigx1
           integer(4)          nadir
           integer(4)          ncc
           integer(4)          npred
           integer(4)          kpbot
           character(10)       obstype
           integer(2)          isat
           integer(4)          isz
           integer(4)          nele
           real(4)             zasat
           real(4)             panglr
           real(4)             cld
           real(4)             pangs
           real(4)             xpaths
           real(4)             sfchgt
           real(4)             zz
           integer(4)          lndsea
           integer(4)          isflg
           integer(4)          icst
           real(4)             ts5
           real(4),pointer::   pressure(:)
           integer(4),pointer::iwgts(:)
           real(4),pointer::   wgts(:)
           integer(4),pointer::icx(:)
           real(4),pointer::   var(:)
           real(4),pointer::   pred(:)
           real(4),pointer::   obsbt(:)
           real(4),pointer::   gesbt(:)
           real(4),pointer::   emissav(:)
           real(4),pointer::   tlapchn(:)
           real(4),pointer::   pems(:)
           real(4),pointer::   htlto(:,:)

         end type rad_obs0
