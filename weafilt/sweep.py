#----------------------------------------------------------------
def delta(x,y):
  return (x-y)/(x+y)

#----------------------------------------------------------------
def bayes(xvec, xcrit, label, unknown, fout = sys.stdout ):
  warm = ma.masked_array(xvec > xcrit)
  warm = ma.logical_and(warm, unknown)
  nwarm = len(warm.nonzero()[0])
  lmask = np.logical_and(landmask, warm)
  imask = np.logical_and(icemask, warm)
  omask = np.logical_and(watermask, warm)
  pwarm = float(nwarm)/float(nobs)
  pover_land  = len(lmask.nonzero()[0])/nlandpts
  pover_water = len(omask.nonzero()[0])/nwaterpts
  pover_ice   = len(imask.nonzero()[0])/nicepts
  if (pwarm > 0):
    print(label, "hot ", xcrit,
      "{:5.3f}".format(pover_ice * pice / pwarm) ,
      "{:5.3f}".format(pover_land * pland / pwarm) ,
      "{:5.3f}".format(pover_water * pwater / pwarm), nwarm, file = fout )

  cold = ma.masked_array(xvec < xcrit)
  cold = ma.logical_and(cold, unknown)
  ncold = len(cold.nonzero()[0])
  lmask = np.logical_and(landmask, cold)
  imask = np.logical_and(icemask, cold)
  omask = np.logical_and(watermask, cold)
  pcold = float(ncold)/float(nobs)
  pover_land  = len(lmask.nonzero()[0])/nlandpts
  pover_water = len(omask.nonzero()[0])/nwaterpts
  pover_ice   = len(imask.nonzero()[0])/nicepts
  if (pcold > 0):
    print(label,"cold ",xcrit,
      "{:5.3f}".format(pover_ice * pice / pcold) ,
      "{:5.3f}".format(pover_land * pland / pcold) ,
      "{:5.3f}".format(pover_water * pwater / pcold), ncold, file = fout )

#----------------------------------------------------------------
def dr(x, y, label, unknown, fout = sys.stdout):
  ratio = delta(x,y)
  tc = np.linspace(ratio.min(), ratio.max(), num=100)
  for i in range(0,len(tc)):
    bayes(ratio, tc[i], label, unknown, fout)
  del ratio

#----------------------------------------------------------------
def sweep(fname):
  fout = open(fname, "w")
  for thot in range (95, 320):
    bayes(t19v, thot, "t19v", unknown, fout)
    bayes(t19h, thot, "t19h", unknown, fout)
    bayes(t22v, thot, "t22v", unknown, fout)
    bayes(t37v, thot, "t37v", unknown, fout)
    bayes(t37h, thot, "t37h", unknown, fout)
    bayes(t85v, thot, "t85v", unknown, fout)
    bayes(t85h, thot, "t85h", unknown, fout)
  
  dr(t19v, t19h, "drt19vt19h", unknown, fout)
  dr(t19v, t22v, "drt19vt22v", unknown, fout)
  dr(t19v, t37v, "drt19vt37v", unknown, fout)
  dr(t19v, t37h, "drt19vt37h", unknown, fout)
  dr(t19v, t85v, "drt19vt85v", unknown, fout)
  dr(t19v, t85h, "drt19vt85h", unknown, fout)
  dr(t19h, t22v, "drt19ht22v", unknown, fout)
  dr(t19h, t37v, "drt19ht37v", unknown, fout)
  dr(t19h, t37h, "drt19ht37h", unknown, fout)
  dr(t19h, t85v, "drt19ht85v", unknown, fout)
  dr(t19h, t85h, "drt19ht85h", unknown, fout)
  dr(t22v, t37v, "drt22vt37v", unknown, fout)
  dr(t22v, t37h, "drt22vt37h", unknown, fout)
  dr(t22v, t85v, "drt22vt85v", unknown, fout)
  dr(t22v, t85h, "drt22vt85h", unknown, fout)
  dr(t37v, t37h, "drt37vt37h", unknown, fout)
  dr(t37v, t85v, "drt37vt85v", unknown, fout)
  dr(t37v, t85h, "drt37vt85h", unknown, fout)
  dr(t37h, t85v, "drt37ht85v", unknown, fout)
  dr(t37h, t85h, "drt37ht85h", unknown, fout)
  dr(t85v, t85h, "drt85vt85h", unknown, fout)
  print(flush=True, file=fout)
  fout.close()

#-----------------------------------------------
