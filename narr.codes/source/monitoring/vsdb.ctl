dset ^out5.grd
undef 9.999E+6
title 4x daily flx file from NCEP/DOE Reanalysis (R-2)
xdef 1 linear 1 1
ydef 1 linear 1 1
zdef 1 linear 1 1
tdef 365 linear 01jan1988 24hr
vars 29
t850f   0 99 mean [f] of T (p=850)
t850o   0 99 mean [o] of T (p=850)
t850fo  0 99 mean [f*o] of T (p=850)
t850f2  0 99 mean [f**2] of T (p=850)
t850o2  0 99 mean [o**2] of T (p=850)
t200f   0 99 mean [f] of T (p=200)
t200o   0 99 mean [o] of T (p=200)
t200fo  0 99 mean [f*o] of T (p=200)
t200f2  0 99 mean [f**2] of T (p=200)
t200o2  0 99 mean [o**2] of T (p=200)
rh850f  0 99 mean [f] of RH (p=850)
rh850o  0 99 mean [o] of RH (p=850)
rh850fo 0 99 mean [f*o] of RH (p=850)
rh850f2 0 99 mean [f**2] of RH (p=850)
rh850o2 0 99 mean [o**2] of RH (p=850)
u850f   0 99 mean [uf] (p=850)
v850f   0 99 mean [vf] (p=850)
u850o   0 99 mean [uo] (p=850)
v850o   0 99 mean [vo] (p=850)
uv850fo 0 99 mean[uf*uo+vf*vo] (p=850)
uv850f2 0 99 mean[uf**2+vf**2] (p=850)
uv850o2 0 99 mean[uo**2+vo**2] (p=850)
u200f   0 99 mean [uf] (p=200)
v200f   0 99 mean [vf] (p=200)
u200o   0 99 mean [uo] (p=200)
v200o   0 99 mean [vo] (p=200)
uv200fo 0 99 mean[uf*uo+vf*vo] (p=200)
uv200f2 0 99 mean[uf**2+vf**2] (p=200)
uv200o2 0 99 mean[uo**2+vo**2] (p=200)
ENDVARS
