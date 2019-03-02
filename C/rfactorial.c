 
double rfact(gamma)
double gamma;
/* trivial example of recursion */
  { double i;
    if (gamma > 1)
      { i = gamma*rfact(gamma-1) ;
        return(i); }
     else
      return(gamma);
  }
