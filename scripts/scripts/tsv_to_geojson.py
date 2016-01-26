
import json
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point



# df = pd.read_csv("/home/userz/Desktop/wb_vfm_work/wb_pc1.tsv", sep='\t')
df = pd.read_csv("/home/userz/Desktop/wb_vfm_work/wb_pc1_model_03/wb_pc1_data_sectorsx.csv")

gdf = gpd.GeoDataFrame(df)


def buildPoint(lon, lat):
    tmp_pnt = Point(lon, lat)
    return tmp_pnt

gdf.geometry = gdf.apply(lambda x: buildPoint(x.longitude, x.latitude), axis=1)

geo_json = gdf.to_json()
# geo_file = open("/home/userz/Desktop/wb_vfm_work/wb_pc1.geojson", "w")
geo_file = open("/home/userz/Desktop/wb_vfm_work/wb_pc1_model_03/wb_pc1_data_sectorsx.geojson", "w")
json.dump(json.loads(geo_json), geo_file, indent = 4)
