import pandas as pd
import geopandas as gpd
import json

vector = "/home/userz/Desktop/wb_vfm_work/wb_pc1/wb_pc1.geojson"
extract_merge = "/home/userz/Desktop/wb_vfm_work/wb_pc1_extract_merge.csv"
output = "/home/userz/Desktop/wb_vfm_work/wb_pc1_data.geojson"
join_id = "project_location_id"


# vector = "/home/userz/Desktop/wb_vfm_work/wb_ibas/matching_iba/matching_ibas_proj.shp"
# extract_merge = "/home/userz/Desktop/wb_vfm_work/wb_ibas_extract_merge.csv"
# output = "/home/userz/Desktop/wb_vfm_work/wb_ibas_data.geojson"
# join_id = "SitRecID"



gdf = gpd.GeoDataFrame.from_file(vector)
df = pd.DataFrame.from_csv(extract_merge)


years = range(1982,2015)

drop_data = []

for y in years:

    at41_data = []
    pc41_data = []

    for m in range(1,13):
        m = str(m)
        if len(m) == 1:
            m = "0" + m

        at41_col = "at41_" + str(y) + m + "e"
        at41_data.append(at41_col)

        pc41_col = "pc41_" + str(y) + m + "e"
        pc41_data.append(pc41_col)


    # min
    df["at41_"+str(y)+"m"] = df[at41_data].min(axis=1)
    df["pc41_"+str(y)+"m"] = df[pc41_data].min(axis=1)

    # mean
    df["at41_"+str(y)+"e"] = df[at41_data].mean(axis=1)
    df["pc41_"+str(y)+"e"] = df[pc41_data].mean(axis=1)

    # max
    df["at41_"+str(y)+"x"] = df[at41_data].max(axis=1)
    df["pc41_"+str(y)+"x"] = df[pc41_data].max(axis=1)

    drop_data += at41_data + pc41_data
    

df.drop(drop_data, axis=1, inplace=True)



# create new geodataframe by merging just geometry from gdf to df
merge = df.merge(gdf[[join_id,'geometry']], on=join_id)
gdfx = gpd.GeoDataFrame(merge)



geo_json = gdfx.to_json()
geo_file = open(output, "w")
json.dump(json.loads(geo_json), geo_file, indent = 4)

