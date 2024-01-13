      PROGRAM reformat
! Reformat the ensemble-averaged output to the usual WMO TRAN format 
!   -- sk2out, and to kml -- kmlout, called by sk2out
! File usage:
!    fort.31 -- forecast file input
!    fort.47 -- seaice_forecast.points

!    fort.60 -- output
!    fort.61 -- output
!    fort.62 -- output
!    fort.63 -- output
!    fort.64 -- output
!    fort.70 -- kml output
!    fort.90 -- echo $PDY > fort.90
!    fort.91 -- seaice_quote (for tran file construction)
!
! Robert Grumbine original version 2 June 2014

      IMPLICIT none

      INTEGER npts, tmp, ndays, time
      PARAMETER (tmp = 360*180) 
      PARAMETER (ndays = 16)
      INTEGER skpt(tmp)
      REAL lat(tmp), lon(tmp), x0(tmp), y0(tmp) 
      REAL dir(tmp,ndays), dist(tmp,ndays)

      INTEGER i, funit2, code
      
      funit2 = 31
      OPEN(31, FORM="FORMATTED", STATUS="OLD")
 
! Read in skiles point locations:
      OPEN(47,FILE="seaice_forecast.points",FORM="FORMATTED",
     1              STATUS="OLD")
      DO i = 1, 207
        READ(47, *) skpt(i), y0(i), x0(i)
      ENDDO

 
! Read in forecast and skiles point locations
      npts = tmp
      CALL getfcst(funit2, x0, y0, dir, dist, code, npts, ndays)
!      PRINT *,'getfcst return code = ',code
      DO i = 1, npts
        skpt(i) = i
      ENDDO
!      PRINT *,'x0 y0 1 = ',x0(1), y0(1), dir(1,5), dist(1,5)

! Call sk2out day by day, as in original
      DO i = 1, ndays !forecast days
!CD        PRINT *,'printing out day ',i
        time = i*24
        CALL sk2out(x0, y0, dir(:,i), dist(:,i), skpt, npts, time)
      ENDDO

      END
