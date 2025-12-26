      function conmc(cname)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    conmc       provide nmc*s value for physical const.
!   prgmmr: parrish          org: w/nmc22    date: 88-09-09
!
! abstract: return nmc handbook value for requested physical const.
!   note that some values have been modified based on bill collins
!   suggestion.  (g, rd, cp)
!
! program history log:
!   88-09-09  parrish
!
! usage:    const=conmc(cname)
!   input argument list:
!     cname    - character array containing name of constant as
!              - suggested in nmc handbook, sec. 3.4.2.
!              - must end in a "$" sign.
!
! attributes:
!   language: cft77
!   machine:  cray
!
!$$$
      character*1 cname(*),dollar
      data dollar/'$'/
      character*8  tabone(15)
      data  tabone/ &
        'rerth   ','g       ','omega   ','rd      ','cp      ', &
        'cv      ','rv      ','cvap    ','cliq    ','hvap    ', &
        'hfus    ','psat    ','sbc     ','solr    ','pi      '/
      character*1 tabnam(8,15)
      integer lnam(15)
      data lnam/5,1,5,2,2,2,2,4,4,4,4,4,3,4,2/
      equivalence (tabone,tabnam)
!******
!****** values from nmc handbook
!******
!     double precision consts(15)
!     data consts/ &
!       6.3712e6,    9.8062, 7.2921e-5,  2.8704e2,  1.0046e3, &
!       7.1760e2,  4.6150e2,  1.8460e3,  4.1855e3,  2.5000e6, &
!       3.3358e5,  6.1078e2, 5.6730e-8,  1.3533e3,    3.1416/
!******
!****** modified values, based on bill collins suggestions.
!******
       dimension consts(15)
       data consts/ &
         6.3712e6,    9.8000, 7.2921e-5,  2.8705e2,  1.0045e3, &
         7.1760e2,  4.6150e2,  1.8460e3,  4.1855e3,  2.5000e6, &
         3.3358e5,  6.1078e2, 5.6730e-8,  1.3533e3,    3.1416/
!--------
!-------- first find number of characters in cname
!--------
      ii=0
      do 100 i=2,9
        ii=ii+1
        if(cname(i).eq.dollar) go to 200
100   continue
      go to 500
200   continue
!--------
!-------- now find a match
!--------
      do 400 k=1,15
        jj=lnam(k)
        if(ii.eq.jj) then
          match=0
          do 300 i=1,ii
            if(cname(i).eq.tabnam(i,k)) match=match+1
300       continue
          if(match.eq.ii) then
            conmc=consts(k)
            return
          end if
        end if
400   continue
500   continue
!--------
!-------- here for trouble only
!--------
      print *,'trouble in conmc'
      stop 56
      end
