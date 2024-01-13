#include <stdio.h>
#include <math.h>

//This is the start of framing a parameters class
//The idea is to make it easier to use NCEP standard values
//  for standard physical parameters.
//Values are taken from Gill, A. E., Atmosphere-Ocean Dynamics, Academic
//  press, 1982, 662 pgs.

//Note that although the variables are public, the const declaration
//  prevents users from altering the values.

//Robert Grumbine 18 April 2000

class parameters {
  private:
     static const float a = 6371.0e3; //radius of the earth, in m
 
  public:
  // Physical parameters -- Dry and moist air
     static const float cp_air               = 1004.0;
     static const float latent_fusion        = 3.32e5;
     static const float latent_vaporization  = 2.5e6;
     static const float molecular_mass_air   = 28.966;
     static const float molecular_mass_water = 18.016;
     static const float molecular_ratio      = 0.62197;

  // Physical parameters -- universal
     static const float universal_gas_constant = 8.31436; //  J/mol/K 
     static const float stefan_boltzman        = 5.67E-8; //  W/m^2/K^4

  // Physical parameters -- the earth
     static const float g_bar                  = 9.7976;  //  m/s^2
     float g(float lat) { return 9.78032  
                         + 0.005172*sin(lat)*sin(lat) 
                         - 0.00006*sin(2.*lat)*sin(2.*lat) ; } //lat in radians
     float g(float lat, float height) {return g(lat)*pow((1+height/a), -2); }
     static const float omega = 7.292e-5;  // s^-1

  // Physical parameters -- water
     float tfreeze(float S) { return -0.0575*S + 1.710523e-3*pow(S,1.5) - 
                                     2.154996e-4*S*S; }
     float tfreeze(float S, float P) { return tfreeze(S) - 7.53e-3*P ; }

  // Unit conversions
     static const float nmtokm = 1.85318;
     
     parameters() {};
};
