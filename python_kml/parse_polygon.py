from fastkml import kml
from fastkml import styles
from fastkml import base
from fastkml import atom
from fastkml import config

from fastkml.config import etree

from fastkml.geometry import Point, LineString, Polygon
from fastkml.geometry import MultiPoint, MultiLineString, MultiPolygon
from fastkml.geometry import LinearRing, GeometryCollection
from fastkml.geometry import Geometry

k = kml.KML()
f = open("fred.kml","r").read()
k.from_string(f)
print(k)

#x = k.Document()
#print(x)
#
#print(k.Placemark[0])

for alpha in k.features() :
  print("in features loop")
  print(alpha)
  for beta in alpha.features():
    print("beta = ",beta)
    print(".placemark.name ",beta.name)
    print(".placemark.id ",beta.id )

#point, polygon, etc., including the location(s)
    print(".placemark geometry ",beta.geometry)
    print("type ",beta.geometry.type)
    if (isinstance(beta.geometry, Polygon)):
      print("is a polygon")
      x = beta.geometry
      print("x = ",x)
      y = Polygon(x)
      for k in range(0,len(y.exterior.coords)):
        print("y = ",y.exterior.coords[k] )
