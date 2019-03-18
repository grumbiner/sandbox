 
long power(alpha,beta)
long alpha, beta;
  { long i,j ;
    i = alpha;
    j = beta;
    for ( ; j > 1; --j)
      i = i * alpha;
    return(i);
  }
