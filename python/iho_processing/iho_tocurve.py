import shapefile

#def bbarea(bbox):

sf = shapefile.Reader("World_Seas_IHO_v3.shp")

 
for k in range(0, len(sf)):
  x = sf.shape(k) 
  name = sf.record(k)[0]
  name = name.replace(" ","_")

  file = open(name+".curve","w")
  print(name, file=file)
  print(x.bbox[0], x.bbox[1], file=file)
  print(x.bbox[2], x.bbox[1], file=file)
  print(x.bbox[2], x.bbox[3], file=file)
  print(x.bbox[0], x.bbox[3], file=file)
  print(x.bbox[0], x.bbox[1], file=file)
  file.close()

  if (abs(x.bbox[0] - x.bbox[2]) > 360):
    print(name, x.bbox)

  area = sf.record(k)[8]
  #print(x.bbox[2], x.bbox[3])
  #print(x.bbox[0], x.bbox[1])
