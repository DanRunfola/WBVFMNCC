import geopandas as gpd
import pandas as pd
from shapely.geometry import Point
import json
import pyproj

iba = pd.DataFrame.from_csv('/home/userz/Desktop/wb_vfm_work/iba_data/IBA monitoring data 4 Nov 2015.csv')

iba_valid = iba.loc[iba['StateScore'] != 5]

iba_2010 = iba_valid.loc[iba_valid['MonitoringYear'] >= 2010]

iba_2010_sort = iba_2010.sort('MonitoringYear', ascending=1)

iba_2010_unique = iba_2010_sort.groupby('SiteID').last()

iba_gdf = gpd.GeoDataFrame(iba_2010_unique)

iba_gdf.geometry = iba_gdf.apply(lambda z: Point(z.Longitude, z.Latitude), axis=1)


wb_gdf = gpd.GeoDataFrame.from_file('/home/userz/Desktop/wb_pc_model_04/biodiversity_output/bio_par_test_1_merge.geojson')

wb_gdf['iba_area'] = -999

iba_area = []

for i in range(len(wb_gdf)):
    print i
    x = iba_gdf.geometry.distance(Point(wb_gdf.geometry[i]))
    x.sort()

    val = x.iloc[0]
    print val
    ind = x.index[0]
    area = iba_gdf.loc[ind]['Area']

    iba_area.append(area)


wb_gdf['iba_area'] = iba_area

geo_json = wb_gdf.to_json()
geo_file = open("/home/userz/Desktop/wb_pc_model_04/biodiversity_output/bio_par_test_1_merge_area.geojson", "w")
json.dump(json.loads(geo_json), geo_file, indent = 4)
geo_file.close()

