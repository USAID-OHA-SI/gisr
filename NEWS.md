# gisr 0.4.0

* Soft depreciation of extact_locations - recommend using grabr::datim_pull_hierarchy
* Update extract_facilities - dropping mer_sites parameter
* Update extract_boundaries - leveraging grabr::get_ouorgs + clean up
* Update get_attributes - switched to datim_orgunits and adding new params: search, baseurl
* Update extract_attributes - switching to datim_orgunits with the use of prefix
* Update zip_shapefiles - switch to folderpath instead of dest_folder
* Adding `check_columns()` for shapefile column names restrictions
* Update get_terrain, extract_raster, get_raster to use folderpath and be required
* Replaced `raster` with `terra` per maintenance recommendation
* Removed `sp` in favor of `sf` per maintenance recommendation
* Removed `get_adm_boundaries` given depreciation of `raster`
* Updated `geo_neighbors` to require source as an external dataset
* Adding `get_nepolygons` as a common way of extracting naturalearth database

# gisr 0.3.0

* Removed duplicative functions from `grabr` [2023-11-30]
* Update functions to use `baseurl` and `folderpath` as parameter
* General clean up and documentation

# gisr 0.2.2
* Change instructions to install from rOpenSci [2024-01-04]
* Add in package status check from `gagglr` on load [2022-08-15]
* Improvement and documentation of utility functions
* Updated vignette for Geodata extraction
* Updated `attributes()` to convert all geodata to `sf` object before extracting data
* `cntry_polygons()` extract all boundaries for specific country as a named list
* `spdf_points()` generate point spatial data frame
* `spdf_export()` a redirect to `export_spdf()` with column length checks

# gisr 0.2.1

* Improvement and documentation of utility functions
* Adding vignettes and package site
* `attributes()` extract attributes from sf object
* `get_raster()` read terrain raster file and other raster file
* `get_attributes()` extract OU/Country orgunits attributes for VcPolygons
* `extract_boundaries()` extract specific boundaries from VcPolygons
* `extract_roads` extract road networks from osm repository
* `export_spdf()` export sf objects to shapefiles
* `extract_raster` extract raster for AOI
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

