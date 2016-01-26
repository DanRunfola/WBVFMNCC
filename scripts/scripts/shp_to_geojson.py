import os
import json

import geopandas as gpd

path = "/home/userz/Desktop/wb_vfm_work/matching_iba/matching_ibas_proj.shp"

geo_df = gpd.GeoDataFrame.from_file(path)
# geo_df["ad_id"] = range(len(geo_df))

# del geo_df['ID']

geo_json = geo_df.to_json()
geo_file = open(os.path.splitext(path)[0] + ".geojson", "w")
json.dump(json.loads(geo_json), geo_file, indent = 4)



