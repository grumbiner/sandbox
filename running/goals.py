## python
import sys

from running import *
#----------------------------------==--------------------------------------
# Given time and distance, look for goal times at that distance
#print(elite_male.t_ref)
#print(elite_female.t_ref)

distance = str_to_distance(sys.argv[1])
tau      = str_to_time(sys.argv[2])
x        = runner(distance, tau) # Implicitly computes x.t_ref
km    = distance / 1000.
miles = distance / mi

times = [] #declare an empty list

# by time, integer minutes
for t in range (int(from_reference(distance, elite_male.t_ref)/60.), 
                int(1+ tau/60.)      ):
  times.append(t*60.)

# by time, integer minutes of improvement:
nnn = tau - from_reference(distance, elite_male.t_ref)
for imp in range (1,int(nnn/60.)):
  times.append(tau-imp*60.)

# by km pace
for p in range(int(from_reference(distance, elite_male.t_ref)/km/60.), 
               int(1+tau/km/60.) ):
  times.append(p*60.*km)

# by mile pace
for p in range(int(from_reference(distance, elite_male.t_ref)/miles/60.), 
               int(1+tau/miles/60.) ):
  times.append(p*60.*miles)

#----------------------------------------------------------
# Now show list:
times.sort(key=None, reverse=True)    #default is ascending order
#times.reverse()
for t in times:
  if (t > from_reference(distance, elite_male.t_ref)):
    indent = ""
    if (int(t/60./miles) == t/60./miles):
      indent += "****"
    if (int(t/60./km) == t/60./km):
      indent += "****"
    if (int(t/60./5) == t/60./5):
      indent += "*"
    if (int(t/60./10) == t/60./10):
      indent += "**"
    if (int(t/60./15) == t/60./15):
      indent += "***"
    print(indent, t_to_string(t), t_to_string(t/km), t_to_string(t/miles)  )


