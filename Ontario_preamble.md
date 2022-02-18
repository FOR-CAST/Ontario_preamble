---
title: "Ontario_preamble"
author: "Alex Chubaty"
date: "18 February 2022"
output:
  html_document:
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
---



# Overview

Define the AOU and ROF study areas, subsetting ROF by ecozone, and prepare species equivalency table for running LandR-fireSense simulations.

# Parameters

Provide a summary of user-visible parameters.


|paramName        |paramClass |default |min |max |paramDesc                                                                                                                                                                                                                                                                                                       |
|:----------------|:----------|:-------|:---|:---|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|.plotInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first plot event should occur.                                                                                                                                                                                                                                       |
|.plotInterval    |numeric    |NA      |NA  |NA  |Describes the simulation time interval between plot events.                                                                                                                                                                                                                                                     |
|.saveInitialTime |numeric    |NA      |NA  |NA  |Describes the simulation time at which the first save event should occur.                                                                                                                                                                                                                                       |
|.saveInterval    |numeric    |NA      |NA  |NA  |This describes the simulation time interval between save events.                                                                                                                                                                                                                                                |
|.useCache        |logical    |FALSE   |NA  |NA  |Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant                                                                                                                                                        |
|.resolution      |numeric    |250     |NA  |NA  |raster pixel size, in m, to use for simulation. Either 250 or 125.                                                                                                                                                                                                                                              |
|runName          |character  |AOU     |NA  |NA  |Should include one of 'AOU' or 'ROF' to identify the studyArea (if 'ROF', then 'shield' or 'plain' should be specified too, to identify whether to run in the Boreal Shield or Hudson Plains ecozone); as well as 'CanESM5_SSP370' or 'CNRM-ESM2-1_SSP370' (or SSP585) to identify the climate scenario to use. |
|useAgeMapkNN     |logical    |FALSE   |NA  |NA  |if TRUE, use kNN age maps, corrected with fire polygons data; if FALSE, use Raquel's predicted age map from ROF_age.                                                                                                                                                                                            |

# Events

This module consists of a single `init` event that performs all the data preparation.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData(...)` may be sufficient.


|objectName |objectClass |desc                          |sourceURL |
|:----------|:-----------|:-----------------------------|:---------|
|targetCRS  |character   |Geospatial projection to use. |NA        |

## Output data

Description of the module outputs.


|objectName             |objectClass              |desc                                                                                                                                                                                            |
|:----------------------|:------------------------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|ageMap                 |RasterLayer              |Age (time since disturbance) map, derived from national kNN product and ON FRI data.                                                                                                            |
|LCC                    |RasterLayer              |Land cover classification map, derived from national LCC 2005 product and ON FRI data.                                                                                                          |
|nontreeClasses         |integer                  |vector of LCC classes considered to be non-forested/treed.                                                                                                                                      |
|nonTreePixels          |integer                  |pixel indices indicating non-treed pixels                                                                                                                                                       |
|rasterToMatch          |RasterLayer              |Raster to match, based on study area.                                                                                                                                                           |
|rasterToMatchLarge     |RasterLayer              |Raster to match (large) based on studyAreaLarge.                                                                                                                                                |
|rasterToMatchReporting |RasterLayer              |Raster to match based on studyAreaReporting.                                                                                                                                                    |
|speciesTable           |data.table               |Species parameter table.                                                                                                                                                                        |
|sppColorVect           |character                |Species colour vector.                                                                                                                                                                          |
|sppEquiv               |data.table               |Species equivalency table.                                                                                                                                                                      |
|sppEquivCol            |character                |name of column to use in sppEquiv                                                                                                                                                               |
|standAgeMap2001        |RasterLayer              |raster of time since disurbance for year 2001.                                                                                                                                                  |
|standAgeMap2011        |RasterLayer              |raster of time since disurbance for year 2011.                                                                                                                                                  |
|studyArea              |SpatialPolygons          |Buffered study area in which to run simulations.                                                                                                                                                |
|studyAreaLarge         |SpatialPolygons          |Buffered study area used for parameterization/calibration.                                                                                                                                      |
|studyAreaPSP           |SpatialPolygonsDataFrame |this area will be used to subset PSP plots before building the statistical model. Currently PSP datasets with repeat measures exist only for Saskatchewan, Alberta, and Boreal British Columbia |
|studyAreaReporting     |SpatialPolygons          |Unbuffered study area used for reporting/post-processing.                                                                                                                                       |
|treeClasses            |integer                  |vector of LCC classes considered to be forested/treed.                                                                                                                                          |

# Links to other modules

LandR Biomass suite of modules and the fireSense suite of modules.
