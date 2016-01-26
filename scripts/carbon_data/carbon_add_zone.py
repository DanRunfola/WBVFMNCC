import geopandas as gpd
import pandas as pd
from shapely.geometry import Point
import json
import numpy as np
from shapely.prepared import prep

# carbon zones
zones = gpd.GeoDataFrame.from_file('/home/userz/Desktop/wb_vfm_work/carbon_data/carbon_zones/carbon_zones.shp')

# carbon related project location data
points = gpd.GeoDataFrame.from_file('/home/userz/Desktop/wb_pc1_model_04/wb_pc1_data_sectors_short_carbon.geojson')


# # filter points
# iba_valid = iba.loc[iba['StateScore'] != 5]
# iba_2010 = iba_valid.loc[iba_valid['MonitoringYear'] >= 2010]
# iba_2010_sort = iba_2010.sort('MonitoringYear', ascending=1)


# group projects by unique location 
# to reduce calculations
points_unique = points.groupby(['longitude', 'latitude'])['longitude','latitude'].last()


points_unique = gpd.GeoDataFrame(points_unique)

points_unique.geometry = points_unique.apply(lambda z: Point(z.longitude, z.latitude), axis=1)



# find carbon zone each location exists in

points_unique['carbon_code'] = -999
points_unique['carbon_type'] = -999
points_unique['carbon_region'] = -999

# carbon_code = []
# carbon_type = []
# carbon_region = []

print len(zones)

for i in range(len(zones)):

    print i

    z = zones.loc[i]
    
    points_unique['within'] = 0
    
    # points_unique.within = points_unique.geometry.within(z.geometry)
    
    geom = prep(z.geometry)
    points_unique.within = [geom.contains(i) for i in points_unique.geometry]


    if not np.isnan(z.CODE): 
        points_unique.loc[points_unique.within == 1, 'carbon_code'] = z.CODE
        points_unique.loc[points_unique.within == 1, 'carbon_type'] = z.GEZ_TERM
        points_unique.loc[points_unique.within == 1, 'carbon_region'] = z.REGION


#     carbon_code.append(val)
#     carbon_type.append(score)
#     carbon_region.append(score)


# points_unique['carbon_code'] = carbon_code
# points_unique['carbon_type'] = carbon_type
# points_unique['carbon_region'] = carbon_region



# output points_unique to serve as lookup table
geo_json = points_unique.to_json()
geo_file = open("/home/userz/Desktop/wb_vfm_work/carbon_data/wb_pc1_carbon_with_zones_ndvi.geojson", "w")
json.dump(json.loads(geo_json), geo_file, indent = 4)
geo_file.close()


