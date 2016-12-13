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


# proj_utm = pyproj.Proj('+proj=utm +ellps=WGS84 +datum=WGS84 +units=m +no_defs ')
# proj_wgs = pyproj.Proj(init="epsg:4326")

# utm_pnt_raw = pyproj.transform(proj_wgs, proj_utm, tmp_pnt.x, tmp_pnt.y)
# utm_pnt_act = Point(utm_pnt_raw)

# # create buffer in meters
# utm_buffer = utm_pnt_act.buffer(tmp_int)

# # reproject back
# buffer_proj = partial(pyproj.transform, proj_utm, proj_wgs)
# tmp_buffer = transform(buffer_proj, utm_buffer)


wb_gdf = gpd.GeoDataFrame.from_file('/home/userz/Desktop/wb_pc_model_04/wb_pc1_data_sectors_short_carbon.geojson')

wb_gdf['iba_statescore'] = 0
wb_gdf['iba_distance'] = -999

iba_distance = []
iba_statescore = []
for i in range(len(wb_gdf)):
    print i
    x = iba_gdf.geometry.distance(Point(wb_gdf.geometry[i]))
    x.sort()

    val = x.iloc[0]
    print val
    ind = x.index[0]
    score = iba_gdf.loc[ind]['StateScore']
    # print val
    # print score
    # print '-'
    iba_distance.append(val)
    iba_statescore.append(score)


wb_gdf['iba_distance'] = iba_distance
wb_gdf['iba_statescore'] = iba_statescore

geo_json = wb_gdf.to_json()
# geo_file = open("/home/userz/Desktop/wb_vfm_work/wb_pc1.geojson", "w")
geo_file = open("/home/userz/Desktop/wb_pc_model_04/wb_pc1_data_sectors_iba1.geojson", "w")
json.dump(json.loads(geo_json), geo_file, indent = 4)
geo_file.close()


# import matplotlib.pyplot as plt
# import pylab

# plot = gdf.geometry.plot(colormap='Set1', alpha=0.5, axes=None)
# pylab.show()



import numpy as np

clist = ['geometry', 'project_id', 'geoname_id', 'project_location_id', 'sector_split_id', 'longitude', 'latitude', 
'place_name', 'project_title', 'start_actual_isodate', 'end_actual_isodate', 
'ad_sector_names', 'ad_sector_codes', 'sector_group', 'total_commitments', 'even_split_commitments', 
'sector_split_aid', 'iba_distance', 'iba_statescore', #'iba_statescore_cutoff', 
'gpw3_2000e', 'at41_1999e', 'pc41_1999e', 'lnye_1999e', 'alp4_1999e', 'am50_e', 'sslp_e', 'selv_e', 'dari_e', 'droa_e', 
'pre_avg_NDVI_max', 'pre_avg_temp_mean', 'pre_avg_precip_mean', 'post_avg_NDVI_max', 'post_avg_temp_mean', 'post_avg_precip_mean', 
'pre_trend_temp_mean', 'pre_trend_precip_mean', 'post_trend_temp_mean', 'post_trend_precip_mean']

colbool = [i in clist for i in list(wb_gdf.columns)]

wb_gdf_short = wb_gdf.drop(wb_gdf.columns[[not i for i in colbool]], axis=1)


# wb_gdf_short['iba_statescore_cutoff'] = wb_gdf_short['iba_statescore']
# wb_gdf_short.loc[wb_gdf_short['iba_distance'] > 1, 'iba_statescore_cutoff'] = np.nan

wb_gdf_short = wb_gdf_short.loc[wb_gdf_short['iba_distance'] < 1]

short_json = wb_gdf_short.to_json()
short_file = open('/home/userz/Desktop/wb_pc_model_04/wb_pc1_data_sectors_short_iba1.geojson', 'w')
json.dump(json.loads(short_json), short_file, indent=4)
short_file.close()
