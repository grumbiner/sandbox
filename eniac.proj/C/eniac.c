#include <stdio.h>
#include <math.h>
/* Robert Grumbine 28 Sep 1995 */

int main(void) {

/*    Emulate the first successful numerical weather prediction.
     It was run on the ENIAC during 1948 and 1949.
     The results and method were written up in Tellus 2, p. 237 1950.
       by Charney, Fjortoft, and Von Neumann.
      Language C version */

/*    Non-dimensional constants */
    int p, q, n, ip, jp;
    
/*    Declare Dimensional Constants */
    float g, radius, omega, ds, dt;
    
/*    Rescale feet (input dimension of Z's) to meters*/
    float fttom;
    float pi;
#define p_const 19
#define q_const 16
#define ip_const 9
#define jp_const 13
    
/*    Declare temporary arrays -
        These were used because of the memory constraint on the ENIAC*/
    float s[p_const][p_const], t[q_const][q_const];
    float v[p_const][q_const];
    float r[p_const][q_const], f[p_const][q_const], h[p_const][q_const];
    float a[p_const][q_const], b[p_const][q_const];
    float alpha[p_const][q_const],  beta[p_const][q_const];
    float    zp[p_const][q_const], zetap[p_const][q_const];
    
/*    Declare data, physical info. */
    float zeta[p_const][q_const], eta[p_const][q_const];
    
/*    Declare computational variables */
    int i, j, k, l, m;
    FILE *fout;
    
/*    Set up the initial conditions deck */
     
      float z[p_const ][q_const] ={
{910, 899, 900, 885, 880, 890, 895, 885, 895, 920, 910, 890, 870, 860, 850, 840},
{907, 900, 895, 880, 870, 885, 890, 890, 900, 925, 920, 900, 880, 870, 840, 830},
{905, 901, 890, 877, 865, 865, 880, 885, 900, 920, 930, 910, 885, 860, 820, 800},
{908, 901, 888, 870, 847, 830, 830, 850, 890, 900, 900, 890, 860, 800, 760, 720},
{910, 905, 888, 871, 840, 790, 760, 780, 860, 865, 830, 800, 750, 720, 680, 680},
{915, 910, 898, 880, 850, 795, 740, 740, 800, 830, 770, 720, 685, 670, 670, 670},
{918, 919, 915, 900, 865, 810, 750, 730, 750, 740, 730, 690, 655, 665, 665, 665},
{930, 931, 926, 915, 883, 825, 770, 725, 700, 680, 670, 660, 660, 655, 655, 660},
{932, 945, 942, 932, 900, 850, 780, 720, 660, 610, 600, 620, 640, 650, 655, 665},
{935, 950, 950, 942, 924, 890, 800, 710, 640, 600, 590, 580, 600, 645, 660, 675},
{933, 940, 945, 942, 926, 890, 810, 720, 650, 600, 580, 560, 620, 655, 670, 685},
{931, 934, 939, 935, 897, 880, 830, 750, 690, 620, 600, 600, 630, 670, 685, 700},
{925, 926, 925, 920, 890, 865, 830, 760, 710, 670, 620, 620, 650, 690, 700, 710},
{920, 919, 896, 903, 875, 845, 810, 760, 735, 730, 710, 720, 720, 740, 730, 730},
{915, 913, 889, 897, 860, 825, 790, 780, 790, 830, 840, 850, 820, 780, 760, 740},
{912, 910, 885, 891, 860, 830, 820, 840, 860, 890, 900, 900, 850, 790, 770, 755},
{908, 908, 885, 900, 880, 870, 880, 890, 910, 925, 920, 900, 870, 800, 780, 765},
{905, 907, 886, 905, 890, 900, 910, 920, 920, 920, 900, 890, 870, 840, 800, 780},
{903, 906, 886, 910, 900, 920, 920, 925, 915, 910, 890, 880, 860, 850, 820, 810}
     };
 p  = p_const - 1;
 q  = q_const - 1;
 n  = 24;
 ip = ip_const;
 jp = jp_const;;
 g      = 9.81;
 radius = 6.37E6;
 omega  = 7.292E-5;
 ds     = 7.36E5;
 dt     = omega*86400./(float)n;
 fttom  = 3.048;
 pi     = 3.141592654;
 
/*    BEGIN THE EXECUTION HERE
C    This file is not comparable to anything in the original program. */
     //fout = fopen("ENIAC.OUT", "w");
    
/*    Rescale the heights to metric */
    for ( j = 0; j <= q; j++) {
      for ( i = 0; i <= p; i++) {
        z[i][j] = z[i][j]/fttom;  
      }
    }
    /* initialize zeta to zero */
    for ( j = 0; j <= q; j++) {
      for ( i = 0; i <= p; i++) {
        v[i][j] = 0.0;
        r[i][j] = 0.0;
        f[i][j] = 0.0;
        h[i][j] = 0.0;
        zeta[i][j] = 0.0;
         eta[i][j] = 0.0;
        zetap[i][j] = 0.0;
        a[i][j] = 0.0;
        b[i][j] = 0.0;
         beta[i][j] = 0.0;
        alpha[i][j] = 0.0;
      
      }
    }
 
/*    Prepare the data decks - used due to memory constraints */
    for (i = 1; i <= p-1; i++) {
      for (l = 1; l <= p-1; l++) {
        s[l][i] = sin(pi*i*l/p);  
      }
    }
   
     for (j = 1; j <= q-1; j++) {
       for (m = 1; m <= q-1; m++) {
         t[m][j] = sin(pi*m*j/q);  
       }
     }
   
     for (i = 0; i <= p; i++) {
       for (j = 0;j <= q; j++) {
         v[i][j] = p*q*(sin(pi*i/(2*p))*sin(pi*i/(2*p))
                      + sin(pi*j/(2*q))*sin(pi*j/(2*q)) );  
       }
     }
   
     for (i = 0; i <= p; i++) {
       for (j = 0; j <= q; j++) {
         r[i][j] = (ds/(2*radius))*(ds/(2*radius))
                     *((i-ip)*(i-ip)+(j-jp)*(j-jp));  
       }
     }
   
     for (i = 0; i <= p; i++) {
       for (j = 0; j <= q; j++) {
         f[i][j] = (1.-r[i][j])/(1.+r[i][j]); 
       }
     }
   
     for (i = 0; i <= p; i++) {
        for (j = 0; j <= q; j++) {
          h[i][j] = g/(2*omega*ds)/(2*omega*ds) 
               * (1+r[i][j])*(1+r[i][j])*(1+r[i][j])/(1.-r[i][j]);  
        }
     }

    for (i = 1; i <= p-1; i++) {
      for (j = 1; j <= q-1; j++) {
        zeta[i][j] = z[i+1][j]+z[i][j+1]+z[i-1][j]+z[i][j-1] - 4.*z[i][j];  
      }
    }
   
    for (j = 0; j <= q; j++) {
      zeta[0][j] = 2.*zeta[1][j]  - zeta[2][j];
      zeta[p][j] = 2.*zeta[p-1][j]- zeta[p-2][j];  
    }
    for (i = 0; i <= p; i++) {
      zeta[i][0] = 2.*zeta[i][1]   - zeta[i][2]; 
      zeta[i][q] = 2.*zeta[i][q-1] - zeta[i][q-2];  
    }
   
    for (j = 0; j <= q; j++) {
    for (i = 0; i <= p; i++) {
        eta[i][j] = f[i][j]+h[i][j]*zeta[i][j];  
    }
    }

   
/*     END OF THE PRELIMINARY SET UP SECTION
    BEGIN THE ITERATIVE SOLUTION OF THE EQUATIONS             */

    for ( k = 0; k <= 3*n-1; k++) {
    
      for (j = 1; j <= q-1; j++) {
        for (i = 1; i <= p-1; i++) {
          eta[i][j] = f[i][j]+h[i][j]*zeta[i][j]; 
        }
      }
 
//RG: beware j vs. q or 0
      for (i = 1; i <= p-1; i++) {
        j = 0;
        if ( z[i+1][0] < z[i-1][0]) eta[i][j] = f[i][j]+h[i][j]*zeta[i][j];
        j = q;
        if ( z[i+1][q] > z[i-1][q]) eta[i][j] = f[i][j]+h[i][j]*zeta[i][j];
      }
//RG: beware i vs. p or 0
      for (j = 1; j <= q-1; j++) {
        i = 0;
        if (z[0][j+1] > z[0][j-1]) eta[i][j] = f[i][j]+h[i][j]*zeta[i][j];
        i = p;
        if (z[p][j+1] < z[p][j-1]) eta[i][j] = f[i][j]+h[i][j]*zeta[i][j];
      }

     for (j = 1; j <= q-1; j++) {
       for (i = 1; i <= p-1; i++) {
         zetap[i][j] = 0.5*( (eta[i+1][j]-eta[i-1][j] )
                 *( z[i][j+1]-z[i][j-1]   )
               -  ( eta[i][j+1]-eta[i][j-1] )
                 *( z[i+1][j]-z[i-1][j])  ); 
        }
     }
 
     for (j = 1; j <= q-1; j++) {
       for (i = 1; i <=  p-1; i++) {
         alpha[i][j] = 0.0;
         for (l = 1; l <= p-1; l++) {
           alpha[i][j]=alpha[i][j]+s[l][i]*zetap[l][j];  
         }
       }  
     }

    for (j = 1; j <= q-1; j++) {
      for (i = 1; i <=  p-1; i++) {
        a[i][j] = 0.0;
        for (m = 1; m <= q-1; m++) {
          a[i][j] = a[i][j] + t[m][j]*alpha[i][m];  
        }
      }
    }

    for (j = 1; j <= q-1; j++) {
      for (i = 1; i <=  p-1; i++) {
        b[i][j] = -a[i][j]/v[i][j]; 
      }
    }
     
    for (j = 1; j <= q-1; j++) {
      for (i = 1; i <= p-1; i++) {
        beta[i][j] = 0.0;
        for (m = 1; m <= q-1; m++) {
          beta[i][j] = beta[i][j]+t[m][j]*b[i][m];  
        }
      }  
    }
 
   for (j = 1; j <= q-1; j++) {
     for (i = 1; i <= p-1; i++) {
       zp[i][j] = 0.0;
       for (l = 1; l <= p-1; l++) {
         zp[i][j] = zp[i][j]+s[l][i]*beta[l][j];  
       }
     }
   }
          
/*    SECTION FOR CARRYING FORWARD THE EXTRAPOLATION */
     if (k == 0) {
       for (j = 1; j <= q-1; j++)  {
       for (i = 1; i <= p-1; i++) {
            z[i][j] =    z[i][j] + dt*zp[i][j]   ;
         zeta[i][j] = zeta[i][j] + dt*zetap[i][j];   
       }
       }
     }
     else {
       for (j = 1; j <= q-1; j++) {
       for (i = 1; i <= p-1; i++) {
            z[i][j] =    z[i][j] + 2.*dt*   zp[i][j]; 
         zeta[i][j] = zeta[i][j] + 2.*dt*zetap[i][j];  
       }
       }
     }

/*    Apply the inflow/outflow conditions    */
    for (j = 1; j <= q-1; j++) {
      if (z[0][j+1] > z[0][j-1]) { 
             z[0][j] = z[0][j] ;
          zeta[0][j] = 2.*zeta[1][j]-zeta[2][j];
      }
      else { 
         z[0][j]    = z[0][j];
         zeta[0][j] = zeta[0][j]; 
      }

      if (z[p][j+1] < z[p][j-1]) { 
         z[p][j]    = z[p][j];
         zeta[p][j] = 2.*zeta[p-1][j]-zeta[p-2][j]; 
      }
      else {
        z[p][j]    = z[p][j];
        zeta[p][j] = zeta[p][j]; 
      }
    }
        
    for (i = 1; i <= p-1; i++) {
      if (z[i+1][0] < z[i-1][0]) {
        z[i][0]    = z[i][0];
        zeta[i][0] = 2.*zeta[i][1]-zeta[i][2]; 
      }
      else {
         z[i][0]    = z[i][0];
         zeta[i][0] = zeta[i][0]; 
      }

      if (z[i+1][q] > z[i+1][q]) {
         z[i][q]    = z[i][q];
         zeta[i][q] = 2.*zeta[i][q-1]-zeta[i][q-2]; 
      }
     }
     
     /* output */
     printf("%3d\n",k); fflush(stdout);
     for (i = 0; i <= p; i++) {
       for (j = 0; j <= q; j++) {
         printf("%7.2f",z[i][j]);
       }
       printf("\n");
     }
     printf("\n");

  } /* end of k (time step) loop */   

  return 0;

}
