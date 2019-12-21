class Fluid:
# 3d
  attributes: density, temperature, pressure
  attributes: u, v, w
# constants
  attributes: viscosity, thermal_conductivity

#atmosphere also has moisture
#ocean, sea ice --  also has salinity

  def state_p(temperature, density):

  def state_t(pressure, density):

  def state_rho(temperature, pressure):

  def rate_of_strain(u,v,w, Metric):

#Fluid types: 
#  newtonian
#  glacial (Nye)
#  sea ice (?)
#  reiner-rivlin

