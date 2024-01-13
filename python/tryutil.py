from utility import *
salinity = 34.7
t = 30.0
rh = 30.0
albedo = 0.30
lat = 30.0
lon = 30.0

print(tfreeze(salinity))
print(dew_point(t, rh))
print(t_black_body(albedo))
print(oiv2(lat, lon))
print(rg12th(lat, lon))

xs = "75S"
print( llparse(xs))
x = llparse(xs)
print( lon_standards(x, lonmin = -360.0, lonmax = 360.0))
lat1 = 80.0
lon1 = -160.0
lat2 = 70.0
lon2 = -120.0
print( arcdis(lat1, lon1, lat2, lon2))
#def harmonic_pred(date, ampl, phase))
#
#def score(obs, pred, delta, start, end, metric = 0, tolerance = 0):
#def score_rms(delta, start, end, tolerance = 0):
#def score_mean(delta, start, end, tolerance = 0):
#def score_mae(delta, start, end, tolerance = 0):
#def score_mean3(delta, start, end, tolerance = 0):
#def score_mean4(delta, start, end, tolerance = 0):
#def score_loss(obs, pred, delta, start, end, tolerance):
#
primes = []
k = int(2)
primes.append(k)
k = 3
primes.append(k)
x = 91
print(trivial_prime(x, primes))
print(factor(x, primes))
print(primes)

#def strprec(x):
#def parse_8digits(tag):
