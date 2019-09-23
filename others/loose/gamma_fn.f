C Gennady Moshkovich
C Purdue University
C Department of Civil Engineering
C moshkovi@ecn.purdue.edu
C  
C  
C The following simple routine is derived using an algorithm for the gamma
C function by Lanczos.   This differs from those of Stirling and de Moivre
C (the latter is usually erroneously called Stirling's formula), in that it
C is a convergent series not an asymptotic one.   It is also valid for complex
C arguments with positive real part, though the coding here is for a real
C argument.
C  
        double precision function lngamma(z)
c
c       Uses Lanczos-type approximation to ln(gamma) for z > 0.
c       Reference:
c            Lanczos, C. 'A precision approximation of the gamma
c                    function', J. SIAM Numer. Anal., B, 1, 86-96, 1964.
c       Accuracy: About 14 significant digits except for small regions
c                 in the vicinity of 1 and 2.
c
c       Programmer: Alan Miller
c                   1 Creswick Street, Brighton, Vic. 3187, Australia
c       Latest revision - 17 April 1988
c
        implicit none
        double precision a(9), z, lnsqrt2pi, tmp
        integer j
        data a/0.9999999999995183d0, 676.5203681218835d0,
     +         -1259.139216722289d0, 771.3234287757674d0,
     +         -176.6150291498386d0, 12.50734324009056d0,
     +         -0.1385710331296526d0, 0.9934937113930748d-05,
     +         0.1659470187408462d-06/
 
        data lnsqrt2pi/0.91893 85332 04672 7d0/
 
        if (z .le. 0.d0) return
 
        lngamma = 0.d0
        tmp = z + 7.d0
        do 10 j = 9, 2, -1
          lngamma = lngamma + a(j)/tmp
          tmp = tmp - 1.d0
   10   continue
        lngamma = lngamma + a(1)
        lngamma = log(lngamma) + lnsqrt2pi - (z+6.5d0) +
     +                               (z-0.5d0)*log(z+6.5d0)
        end
