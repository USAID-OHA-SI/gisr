# gisr 0.2.1

* Improvement and documentation of utility functions
* Adding vignettes and package site
* `attributes()` extract attributes from sf object
* `get_attributes()` extract OU/Country orgunits attributes for VcPolygons
* `extract_boundaries()` extract specific boundaries from VcPolygons
* `extract_roads` extract road networks from osm repository
* `export_spdf()` export sf objects to shapefiles
* `zip_shapefiles()` compress all shapefile component into a zipped file
* `download_shapefeles()` download compressed shapefiles from googledrive


# gisr 0.2.0

* Improvement to basemap functions: crop neighbor countries, add labels on demand
* Sample code on how to extract country boundaries from PEPFAR Polygons
* Read tiff file as RasterLayer, `get_raster()`
* Identify org levels for country/orgunit, `get_ouorglevel()`
* Get list of uids for ou level x, `get_ouleveluids()`
* Get list of orgunits uids, `get_orguids()`

# gisr 0.1.0

* Improvement to basemap functions: crop neighbor countries
* Parse out PEPFAR OrgHierarchy features
* Adoption of SI Style Guide

# gisr 0.0.0.9000

* Initial set of geosptial utility functions
* Get country admin 1 boundaries sf features, `get_admin1(countries = "Togo")`
* Create basemap from terrain raster data, `terrain_map()`

