import geopandas as gpd
import rasterstats as rs
import json


vector = "/home/userz/Desktop/wb_vfm_work/carbon_data/wb_pc1_carbon_with_zones.geojson"
carbon_raster = "/home/userz/Desktop/wb_vfm_work/carbon_data/carbon_map.tif"
ndvi_raster = "/home/userz/Desktop/wb_vfm_work/carbon_data/lnye_2000.tif"


gdf = gpd.GeoDataFrame.from_file(vector)

# if needed
gdf.drop(['stats', 'within'], axis=1, inplace=True)


# extract
carbon_stats = rs.zonal_stats(vector, carbon_raster, stats="mean")
ndvi_stats = rs.zonal_stats(vector, ndvi_raster, stats="mean")

# add stats to geodataframe
gdf['carbon_extract'] = [i['mean'] for i in carbon_stats]
gdf['lnye_2000e'] = [i['mean']/10000 for i in ndvi_stats]


# gdfx = gdf.copy(deep=True)

# add field to indicate usable carbon zone data
gdf['has_data'] = map(int, ~gdf.carbon_code.isin([-999]))

# gdf['valid_carbon_type'] =  map(int, ~gdf.carbon_type.isin([None, '-999.0', 'No data', 'Water']))
# gdf['valid_carbon_code'] =  map(int, ~gdf.carbon_code.isin([-999]))


# create dataframe for all unique points with usable carbon zone and extract extract data
tmp_gdf = gdf.loc[~gdf.carbon_type.isin([None, '-999.0', 'No data', 'Water']) & ~gdf.carbon_code.isin([-999]) & gdf.carbon_extract.notnull()]






# fill in extracts for points without raster data based on zone type

gdf_out = gdf.copy(deep=True)

# create new field
gdf_out['carbon_extract_type_fill'] = gdf_out['carbon_extract']
gdf_out['carbon_extract_code_fill'] = gdf_out['carbon_extract']

# aggregate by carbom zone type and take mean
type_agg = tmp_gdf.groupby('carbon_type')['carbon_extract'].mean()
code_agg = tmp_gdf.groupby('carbon_code')['carbon_extract'].mean()

# update all points where no carbon data exists using aggregated data
gdf_out.loc[gdf_out.carbon_extract.isnull() & gdf_out.carbon_type.isin(type_agg.index), 'carbon_extract_type_fill'] = list(gdf_out.loc[gdf_out.carbon_extract.isnull() & gdf_out.carbon_type.isin(type_agg.index), 'carbon_type'].apply(lambda z: type_agg.loc[z]))
gdf_out.loc[gdf_out.carbon_extract.isnull() & gdf_out.carbon_code.isin(code_agg.index), 'carbon_extract_code_fill'] = list(gdf_out.loc[gdf_out.carbon_extract.isnull() & gdf_out.carbon_code.isin(code_agg.index), 'carbon_code'].apply(lambda z: code_agg.loc[z]))

# identify points that could not be filled
gdf_out['valid_carbon_type'] =  map(int, ~gdf_out.carbon_extract_type_fill.isnull())
gdf_out['valid_carbon_code'] =  map(int, ~gdf_out.carbon_extract_code_fill.isnull())


gdf_out = gdf_out.loc[~gdf_out.lnye_2000e.isnull()]


gdf_out = gdf_out.loc[~gdf.carbon_type.isin([None, '-999.0', 'No data', 'Water']) & ~gdf.carbon_extract_type_fill.isnull()]

# output
json.dump(json.loads(gdf_out.to_json()), open('/home/userz/Desktop/wb_vfm_work/carbon_data/wb_pc1_carbon_with_zones_and_extract_fill_type.geojson','w'), indent=4)





# # fill in extracts for points without raster data based on zone type

# gdf_type = gdf.copy(deep=True)

# # create new field
# gdf_type['carbon_extract_type_fill'] = gdf_type['carbon_extract']

# # aggregate by carbom zone type and take mean
# type_agg = tmp_gdf.groupby('carbon_type')['carbon_extract'].mean()
 
# # update all points where no carbon data exists using aggregated data
# gdf_type.loc[gdf_type.carbon_extract.isnull() & gdf_type.carbon_type.isin(type_agg.index), 'carbon_extract_type_fill'] = list(gdf_type.loc[gdf_type.carbon_extract.isnull() & gdf_type.carbon_type.isin(type_agg.index), 'carbon_type'].apply(lambda z: type_agg.loc[z]))

# # identify points that could not be filled
# gdf_type['valid_carbon_type'] =  map(int, ~gdf_type.carbon_extract.isnull())


# # output
# json.dump(json.loads(gdf_type.to_json()), open('/home/userz/Desktop/wb_pc1_carbon_with_zones_and_type_fill_extract.geojson','w'), indent=4)





# # fill in extracts for points without raster data based on zone code

# gdf_code = gdf.copy(deep=True)

# # create new field
# gdf_code['carbon_extract_code_fill'] = gdf_code['carbon_extract']
 
# # aggregate by carbom zone code and take mean
# code_agg = tmp_gdf.groupby('carbon_code')['carbon_extract'].mean()
 
# # update all points where no carbon data exists using aggregated data
# gdf_code.loc[gdf_code.carbon_extract.isnull() & gdf_code.carbon_code.isin(code_agg.index), 'carbon_extract_code_fill'] = list(gdf_code.loc[gdf_code.carbon_extract.isnull() & gdf_code.carbon_code.isin(code_agg.index), 'carbon_code'].apply(lambda z: code_agg.loc[z]))

# # identify points that could not be filled
# gdf_code['valid_carbon_code'] =  map(int, ~gdf_code.carbon_extract.isnull())



# # output
# json.dump(json.loads(gdf_code.to_json()), open('/home/userz/Desktop/wb_pc1_carbon_with_zones_and_code_fill_extract.geojson','w'), indent=4)

