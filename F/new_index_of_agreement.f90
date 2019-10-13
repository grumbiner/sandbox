function d_r(n,o,p)
! Fortran 90 function to calculate the refined index of agreement
!
!   Willmott, C. J., S. M. Robeson, K. Matsuura (2011) "A refined
!   index of model performance," International Journal of Climatology,
!   DOI: 10.1002/joc2419
!
! Inputs:
!   n is the number of values in "o" and "p"
!   o is the array of observed values
!   p is the array of predicted or modeled values
!
! Output:
!   d_r is the refined index of agreement

  implicit none

  real::p,o,z,p_o,o_om,o_mean,d_r,c
  integer::n,i
  dimension::p(n),o(n),z(n)

  c=2.0
  o_mean=sum(o)/real(n)
  p_o=sum(abs(p-o))
  o_om=c*sum(abs(o-o_mean))

  if (p_o <= o_om) then
     d_r=1.0 - p_o/o_om
  else
     d_r=o_om/p_o - 1.0
  endif

  return
end function d_r
