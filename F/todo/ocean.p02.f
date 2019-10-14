!From bhuber@ldgo.columbia.edu Sun Oct 24 10:58:20 EDT 1993
!Article: 738 of sci.geo.fluids
!Newsgroups: sci.geo.fluids
!Path: digex.net!news.intercon.com!udel!darwin.sura.net!europa.eng.gtefsd.com!avdms8.msfc.nasa.gov!sol.ctr.columbia.edu!news.columbia.edu!lamont!bhuber
!From: bhuber@ldgo.columbia.edu (bruce huber)
!Subject: Re: Looking for Dissolved Oxygen Saturation in Seawater Subroutine
!Message-ID: <1993Oct22.134453.10886@lamont.ldgo.columbia.edu>
!Keywords: Dissolved Oxygen Subroutine
!Sender: news@lamont.ldgo.columbia.edu
!Organization: Lamont-Doherty Geological Observatory
!References: <2a7u8r$jvm@news.u.washington.edu>
!Date: Fri, 22 Oct 1993 13:44:53 GMT
!Lines: 146
!
!In article <2a7u8r$jvm@news.u.washington.edu> bethp@carson.u.washington.edu (Beth Plotkin) writes:
!>Does anyone have a subroutine (in BASIC, FORTRAN or C) to compute
!>Dissolved Oxygen (DO) Saturation (mg/L) in Seawater as a function of
!>Salinity, Temperature AND DEPTH?  I have the algorithm that comes
!>with Seabird (CTD) software, but it doesn't include the depth
!>dependence (=f(T,S,P=1 atm)).  I could just assume an ideal gas
!>relationship, but I hope someone out there as a referenced routine
!>I could have instead (e.g. UNESCO).
!>
!>Thanks in advance,
!>
!>bethp@carson.u.washington.edu
!>
!
!Perhaps you mean a function which includes P=something other than
!1 atmosphere?  There is no depth dependence, since saturation is
!defined as the amount of gas which can dissolve in a liquid in
!equilibrium with the overlying gas (atmosphere).  For a reference, 
!see Weiss, R. F. (1970) The solubility of nitrogen, oxygen and
!    argon in water and seawater, Deep-Sea Res. 17:721-735.
!
!Hope the following will be helpful
!
!Bruce Huber
!Lamont-Doherty Earth Observatory
!
!
!
! For P=1 atmosphere,
!WOCE recommends the following:
![these were obtained from the Oceanic ftp site -  see the Sources of
!Meteorological and oceanographic data FAQ]
!

      FUNCTION O2SOL(S,TP)
C
C         COMPUTES OXYGEN SOLUBILITY AS  FUNCTION OF
C         SALINITY AND TEMPERATURE
C         EQUATION FROM WEISS,D.S.R.17,P721(1970)
C
C         SELECT REQUIRED UNITS BY REMOVING COMMENT C FROM
C         COLUMN 1
C         
      T=(TP+273.15)*.01
C
C         SOLUBILITY IN ML(STP)/L
C
      O2SOL=-173.4292+249.6339/T+143.3483*ALOG(T)-21.8492*T
     1+S*(-.033096+.014259*T-.0017*T*T)
C
      O2SOL=EXP(O2SOL)
      RETURN
      END


We have a modified version in C which corrects for atmospheric pressures
other than 1013:



/*
	function osatp

------------------------------------------------------
oxygen saturation (ml/l) weiss,1988, pers. comm.

 units: t = in situ temp C; s = salinity pss78
        pair = atmospheric pressure in mbar;
	osatp = Oxygen saturation in ml/l

  Compute the saturation value of oxygen in seawater
  for an atmospheric pressure other than 1 atmosphere.
  Uses the value for Bunsen Coefficient of solubility
  of oxygen as given in Weiss, 1970 (DSR).

 Uses function ph2o(t,s) (in libhydro.a) to compute partial pressure
 of water vapor.

 22 August 88  bah
-----------------------------------------------------
b huber
Physical Oceanography
Lamont-Doherty Earth
*/
#include <math.h>

/*  Constants for determining Bunsen coefficient
    of solubility for oxygen in seawater	 */

#define A1	-58.3877
#define A2	 85.8079
#define A3	 23.8439

#define B1	-0.034892
#define B2	 0.015568
#define B3	-0.0019387

#define MOLE_FRAC  0.20946	/* mole fraction of O2 in dry air */
#define TKELVIN	273.16		/* centigrade to kelvin 	  */
#define ATMOS	1013.246	/* 1 atmosphere = 1013.246 mbar   */

double osatp(t,s, pair)
double t,s, pair;
{
  double x, log(), exp();
  double ph2o();		/* partial pressure of H2O */
  double bunsen_coeff, oxy;

  x = (t+TKELVIN)/100.0;

 bunsen_coeff = exp (A1 + A2/x + A3*log(x) + s*(B1 + B2*x + B3*x*x) );
 oxy = (pair/ATMOS - ph2o(t,s))*bunsen_coeff*MOLE_FRAC;

 return( oxy * 1000.0 );
}


/*
	function ph2o

------------------------------------------------------
 Partial pressure of water vapor over seawater

 units: t = in situ temp C; s = salinity pss78
	ph2o = partial pressure in atmospheres

 As given by Weiss and Price 1980 (Mar. Chem. 8 pp347-359
 
  august 22 1988
-----------------------------------------------------
b huber
Physical Oceanography
Lamont-Doherty Earth Observatory
*/
#include <math.h>

#define  TKELVIN   273.16 	/* centigrade to kelvin */

double ph2o(t,s)
double t,s;
{
  double x, log(), exp();

  x = (t+TKELVIN)/100.0;
  return( exp( 24.4543 - 67.4509/x - 4.8489*log(x) - 0.000544*s) );
}


