import numpy as np


import pandas as pd
import geopandas
import matplotlib.pyplot as plt

lats = np.linspace(0,90,90)
lons = np.linspace(15,105,90)

# Make a Pandas data frame:
df = pd.DataFrame(
    {
        #"Latitude": [-34.58, -15.78, -33.45, 4.60, 10.48],
        #"Longitude": [-58.66, -47.91, -70.66, -74.08, -66.86],
        "Latitude": lats,
        "Longitude": lons,
    }
)

# Make a geopandas geodataframe from the pandas dataframe
gdf = geopandas.GeoDataFrame(
    df, geometry=geopandas.points_from_xy(df.Longitude, df.Latitude), crs="EPSG:4326"
)

# Write out:
gdf.to_file("try1")
# -- will make a directory with .cpg, .prj, .shx, .shp, .dbf files

