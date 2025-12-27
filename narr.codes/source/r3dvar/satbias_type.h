!   declare type for satbias history variables:

type satbias_stuff

!  structure for each channel of each satellite currently being used.
!  corresponds directly with each record of binary 
!  files "satbias_in", "satbias_out"

  sequence

!  record 1:

  integer(8) model_id           ! unique model id
  integer(4) idate(5)           ! 4 digit yr, mon,day,hr,min of last update
  integer(4) kidsat             ! satellite id
  integer(4) ichan              ! satellite channel number
  integer(4) nangs              ! number of scan steps for this satellite
  integer(4) npred              ! number of bias predictors
  real(4) tbias                 ! 1/e time of running ave of bias coefs (days)
  real(4) tcbias                ! 1/e time for running ave of cbias, tlapmean
  real(4) cbias_step_clen       ! step correlation length for smoothing cbias
                                !   between scan steps

!  record 2:

  real(4) age_bias              ! age factor for bias coefs
  real(4) age_tlapmean          ! age factor for tlapmean
  real(4),pointer::age_cbias(:) ! age factor for cbias  (dim="nangs")
  real(4) tlapmean              ! running ave of tlapmean
  real(4),pointer::cbias(:)     ! running ave of cbias (dim="nangs")
  real(4),pointer::predx(:)     ! running ave of predx, the bias coefs

end type satbias_stuff
