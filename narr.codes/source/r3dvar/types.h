!   declare types:     Modified by WC

         type grid_specs

           sequence
           integer(4) nxglbcom
           integer(4) nyglbcom
           integer(4) nxcom
           integer(4) nycom
           integer(4) npcom
           real(4),pointer::rxglbcom(:)
           real(4),pointer::ryglbcom(:)
           real(4),pointer::rxcom(:)
           real(4),pointer::rycom(:)

         end type grid_specs

         type general_obs

           sequence
           real(4) qtflag
           real(4) type                  !   2
           real(4) group
           real(4)    error              !   4
           real(4)    maxerror
           real(4)    lon                !   6
           real(4)    lat
           real(4)    long               !   8
           real(4)    latg
           real(4)    pressure           !  10
           real(4)    ptop
           real(4)    tobs               !  12
           real(4)    tges
           real(4)    elevobs            !  14
           real(4)    elevges  
           real(4)    time               !  16
           real(4)    qobs
           real(4)    qges               !  18
           real(4)    qsatges            
           real(4)    wobs               !  20
           real(4)    wges
           real(4)    theta              !  22
           real(4)    delta
           real(4)    epsilnw            !  24
           real(4)    pwobs
           real(4)    pwges              !  26
           real(4)    zqm
           real(4)    tqm                !  28
           real(4)    qqm
           real(4)    pwq                !  30
           real(4)    wqm
           real(4)    rletaobs           !  32
           real(4)    bigh(8)            !  40
           integer(4) ibigh(8)           !  48
           integer(8) label              !  50
           character(8) staid            !  52      length of general_obs in 4-byte chunks

         end type general_obs
         parameter(len4_general_obs=52)

         type rad_addr

           sequence
           integer(4) count
           integer(4),pointer::adlist(:)
         
         end type rad_addr

         type rad_obs

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
           integer(4)          ncc
           integer(4)          npred
           integer(4)          kpbot
           real(4),pointer::   pressure(:)
           integer(4),pointer::iwgts(:)
           real(4),pointer::   wgts(:)
           integer(4),pointer::icx(:)
           real(4),pointer::   var(:)
           real(4),pointer::   pred(:)
           real(4),pointer::   obsbt(:)
           real(4),pointer::   gesbt(:)
           real(4),pointer::   htlto(:,:)

         end type rad_obs

         type data_halo

           sequence
           integer(4) nin
           integer(4) sin
           integer(4) nout
           integer(4) sout
           integer(4) nsout
           integer(4),pointer::adnin(:)
           integer(4),pointer::adsin(:)
           integer(4) ein
           integer(4) win
           integer(4) eout
           integer(4) wout
           integer(4) ensin
           integer(4) wnsin
           integer(4) outall
           integer(4) lbig
           integer(4),pointer::adein(:)
           integer(4),pointer::adwin(:)
           integer(4),pointer::adensin(:)
           integer(4),pointer::adwnsin(:)
           integer(4),pointer::ibighout(:,:)

         end type data_halo
