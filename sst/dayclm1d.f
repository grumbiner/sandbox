      subroutine dayclm1d (iclim,imo,ida,dclim,ii2,ierr)
c$$$  subprogram documentation block                                    
c                .      .    .                                       .  
c subprogram:    dayclm1d      finds climatology of given day and month   
c   prgmmr: d. c. stokes     org: w/np24     date: 1996-8-9             
c                                                                       
c abstract: temporally interpolates the climatology field of the day    
c           (defined by input month and day of the month) from the      
c           12 monthly climatology fields.  all fields are defined on   
c           a one degree field.  the fields are all centered on the    
c           half-degree.                                                    
c                                                                       
c program history log:                                                  
c   1996-08-09  diane c. stokes
c   1998-03-17  diane c. stokes - corrected initial value of do loop
c                                 for cases when ii2 gt 360.
c   2000-02-01  diane c. stokes - move to IBM SP
c                                                                       
c usage:    call dayclm1d (clim,imo,ida,dclim,ii2,ierr)                       
c                                                                       
c   input argument list:                                                
c    iclim     - field of the 12 months of climatology                  
c              - dimension (12,360,180)                                  
c              - the first index is the month,                          
c              - from january (= 1) to december (=12)                   
c              - the second index is longitude: (0.5e to 359.5e)
c              - the third index is latitude: (89.5s to 89.5n)              
c     imo      - month of desired interpolated climatology              
c     ida      - day of desired interpolated climatology                
c     ii2      - desired first index of output climatology field.
c              - must be 360 or larger.                                 
c                                                                       
c   output argument list:                                               
c     dclim    - field of the temporally interpolated                   
c              - climatology for imo and ida                            
c              - dimension (ii2,180)                                     
c              - the first index is longitude: (0.5e to 359.5e, or beyond)
c              - the second index is latitude: (89.5s to 89.5n)         
c              - note: if extra longitude values are requested,
c              -       dclim(1,j) = dclim(360,j)   
c              -       dclim(1,j) = dclim(362,j) etc 
c     ierr     -  equal to 0 if interpolated field found                
c              -  equal to 1 if no interpolated field because of        
c              -  bad imo or ida or bad ii2 (ii2 should be 360 or more)
c              -  an error message is printed if ierr=1                 
c                                                                       
c   output files:                                                       
c     fort.06  - printout                                               
c                                                                       
c remarks:  
c                                                                       
c attributes:                                                           
c   language: fortran 90                                                
c   machine:  IBM SP                                                       
c                                                                       
c$$$                                                                    
      integer ndays(12)
      integer(2) iclim(12,360,180)
      real dclim(ii2,180)
      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/
c  change february 29 to february 28: effect of leap year ignored       
      ierr = 0
      if(ii2.lt.360)then
         print*,' ii2 lt 360.  need to re-write dayclm1d.f'
         ierr = 1
         return
       endif

      if (imo.eq.2.and.ida.eq.29) ida = 28
c                                                                       
      print 10, imo,ida
   10 format(/'interpolated climate for month = ',i3,3x,'day =',i3)
c  check for bad date                                                   
      if (imo.lt.1.or.imo.gt.12.or.ida.lt.1.or.ida.gt.ndays(imo)) then
      print 222
  222 format (' warning interpolation error in dclim')
      ierr = 1
      return
      endif
      halfm1 = real(ndays(imo))/2.
      if (ida.le.int(halfm1)) then
        if (imo.eq.1) then
          jmo = 12
        else
          jmo = imo-1
        endif
        d = halfm1 - real(ida) + .5
      else
        if (imo.eq.12) then
          jmo = 1
        else
          jmo = imo+1
        endif
        d = real(ida) - halfm1 - .5
      endif
      halfm2 = real(ndays(jmo))/2.
      span = halfm1+halfm2
      do 80 j=1,180
        do 70 i=1,360
          t1 = float(iclim(imo,i,j))*.01
          t2 = float(iclim(jmo,i,j))*.01
          dclim(i,j) = ((t2-t1)/span)*d + t1
   70   continue
         if(ii2.gt.360)then
           do i=361,ii2
             dclim(i,j)=dclim(i-360,j)
           enddo
         endif
   80 continue
      return
      end
