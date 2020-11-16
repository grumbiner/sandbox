import matplotlib

albedo = float(0.80)
sigma = 5.67e-8
sc = 1366.0

def t(albedo):
  return pow( (1-albedo)/4./sigma*sc , 0.25)

print("te for "+"{:8.2f}".format(albedo)+" is " ,t(albedo))
