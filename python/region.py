import geopandas as gpd
import pandas as pd

fp = "Arctic_Regions.shp"
hr = gpd.read_file(fp)
orig = hr

print(orig.crs, flush=True)
print("area\n",hr.area,flush=True)
print("centroid\n",hr.centroid,flush=True)
print("number of regions",len(hr))

print("bounding curves in orig")
for i in range(0, len(hr)):
  print(hr.geometry[i])

#print(hr.count_geometries(), hr.count_coordinates() )

# Get on to lat-long points by using a crs
# EPSG, 4326 == WGS84
anew = hr.to_crs("EPSG:4326")
coords = anew.count_coordinates()
print("bounding curves in ll space")
for i in range(0, len(anew)):
    print(i, coords[i]) 
    print(anew.geometry[i])

df = pd.DataFrame(
    { "Latitude" : [ 75.0 ],
      "Longitude" : [ 120 ],
      }
    )
tmp = gpd.GeoDataFrame(df, geometry = gpd.points_from_xy(df.Longitude, df.Latitude) )
tmp.set_crs("EPSG:4326")
print(tmp)
print(anew.contains(nwp))
