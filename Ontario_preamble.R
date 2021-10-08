defineModule(sim, list(
  name = "Ontario_preamble",
  description = "Study area definition and preparation of data required to run fireSense and LandR in the Ontario AOU/ROF.",
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", role = c("aut", "cre"), email = "achubaty@for-cast.ca")
  ),
  childModules = character(0),
  version = list(SpaDES.core = "1.0.4.9004", Ontario_preamble = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Ontario_preamble.Rmd")),
  reqdPkgs = list("httr", "raster", "rgeos", "reproducible", "sf", "sp",
                  "PredictiveEcology/fireSenseUtils@development",
                  "PredictiveEcology/LandR@development"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant")),
    defineParameter("climateGCM", "numeric", "CNRM-ESM2-1", NA, NA,
                    paste("Global Circulation Model to use for climate projections:",
                          "currently '13GCMs_ensemble', 'CanESM5', 'CNRM-ESM2-1', or 'CCSM4'.")),
    defineParameter("climateSSP", "numeric", 370, NA, NA,
                    "SSP emissions scenario for `climateGCM`: one of 245, 370, or 585.",
                    "[If using 'climateGCM = CCSM4', climateSSP must be one of 45 or 85.]"),
    defineParameter("historicalFireYears", "numeric", default = 1991:2020, NA, NA,
                    desc = "range of years captured by the historical climate data"),
    defineParameter("projectedFireYears", "numeric", default = 2011:2100, NA, NA,
                    "range of years captured by the projected climate data"),
    defineParameter("runName", "character", "AOU", NA, NA,
                    paste("Should include one of 'AOU' or 'ROF' to identify the studyArea,",
                          "as well as 'CCSM4_RCP45' or 'CCSM4_RCP85' to identify the climate scenario to use.")),
    defineParameter("treeClassesLCC", "numeric", c(1:15, 20, 32, 34:35), 0, 39,
                    "The classes in the LCC2005 layer that are considered 'trees' from the perspective of LandR-Biomass"),
    defineParameter("treeClassesToReplace", "numeric", c(34:35), 0, 39,
                    "The transient classes in the LCC2005 layer that will become 'trees' from the perspective of LandR-Biomass (e.g., burned)")
  ),
  inputObjects = bindrows(
    expectsInput("targetCRS", "character", desc = "Geospatial projection to use.", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("ageMap", "RasterLayer", desc = "Age (time since disturbance) map, derived from national kNN product and ON FRI data."),
    createsOutput("historicalClimateRasters", "list", desc = "list of RasterStacks of historical MDC calculated from ClimateNA data."),
    createsOutput("projectedClimateRasters", "list", desc = "list of RasterStacks of projected MDC calculated from ClimateNA data."),
    createsOutput("LCC", "RasterLayer", desc = "Land cover classification map, derived from national LCC 2005 product and ON FRI data."),
    createsOutput("nonTreePixels", "integer", desc = "pixel indices indicating non-treed pixels"),
    createsOutput("rasterToMatch", "RasterLayer", desc = "Raster to match, based on study area."),
    createsOutput("rasterToMatchLarge", "RasterLayer", desc = "Raster to match (large) based on studyAreaLarge."),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = "Raster to match based on studyAreaReporting."),
    createsOutput("speciesTable", "data.table", desc = "Species parameter table."),
    createsOutput("sppColorVect", "character", desc = "Species colour vector."),
    createsOutput("sppEquiv", "data.table", desc = "Species equivalency table."),
    createsOutput("standAgeMap2001", "RasterLayer", desc = "raster of time since disurbance for year 2001."),
    createsOutput("standAgeMap2011", "RasterLayer", desc = "raster of time since disurbance for year 2011."),
    createsOutput("studyArea", "SpatialPolygons", desc = "Buffered study area in which to run simulations."),
    createsOutput("studyAreaLarge", "SpatialPolygons", desc = "Buffered study area used for parameterization/calibration."),
    createsOutput("studyAreaReporting", "SpatialPolygons", desc = "Unbuffered study area used for reporting/post-processing.")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.Ontario_preamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Ontario_preamble", "plot", .last())
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Ontario_preamble", "save", .last())
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      ON <- as_Spatial(mod$ON)
      Plot(ON, col = "Greens")
      Plot(sim$studyArea, addTo = "ON", col = "Accent")

      Plot(sim$rasterToMatch)
      Plot(sim$rasterToMatchLarge)

      Plot(sim$ageMap2001)
      Plot(sim$ageMap2011)

      Plot(sim$LCC)

      # schedule future event(s)
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "Ontario_preamble", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "Ontario_preamble", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  cacheTags <- c(P(sim)$runName, currentModule(sim))
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")

  studyAreaName <- if (grepl("AOU", runName)) {
    "AOU"
  } else if (grepl("ROF", runName)) {
    "ROF"
  } else {
    stop("runName must contain one of 'AOU' or 'ROF'.")
  }

  ## provincial boundary
  # canProvs <- Cache(prepInputs,
  #                   "GADM",
  #                   fun = "base::readRDS",
  #                   dlFun = "raster::getData",
  #                   country = "CAN", level = 1, path = dPath,
  #                   #targetCRS = sim$targetCRS, ## TODO: fails on Windows
  #                   targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
  #                   overwrite = TRUE,
  #                   destinationPath = dPath) %>% ## TODO: why is this failing?
  canProvs <- getData("GADM", path = dPath, country = "CAN", level = 1, type = "sf")

  mod$ON <- canProvs[canProvs$NAME_1 == "Ontario", ] %>%
    st_transform(., crs = sim$targetCRS)

  ## STUDY AREA
  if (grepl("AOU", toupper(P(sim)$runName))) {
    studyAreaReporting <- prepInputs(
      url = "https://drive.google.com/file/d/1Idtreoo51hGBdfJXp0BmN83bOkKHTXvk",
      destinationPath = dPath,
      targetFile = "CEON_def.shp",
      alsoExtract = "similar",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      fun = "sf::st_read",
      overwrite = TRUE,
      team_drive = TRUE
    ) %>%
      as_Spatial(.)

    studyArea <- buffer(studyAreaReporting, 20000) ## 20 km buffer

    studyAreaLarge <- prepInputs(
      url = "https://drive.google.com/file/d/1ngQshBgoyLjkjuXnloXPg7IUMdLmppkB",
      destinationPath = dPath,
      targetFile = "CEON_def_50km_buff.shp",
      alsoExtract = "similar",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      fun = "sf::st_read",
      overwrite = TRUE,
      team_drive = TRUE
    ) %>%
      as_Spatial(.)
  } else if (grepl("ROF", toupper(P(sim)$runName))) {
    studyAreaReporting <- prepInputs(
      url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
      fun = "sf::st_read", destinationPath = dPath,
      filename2 = "ROF_RA_def", overwrite = TRUE
    ) %>%
      as_Spatial(.)

    studyArea <- buffer(studyAreaReporting, 20000) ## 20 km buffer

    studyAreaLarge <- prepInputs(
      url = "https://drive.google.com/file/d/1iOXXIkvY-YaR9BTG_SRd5R_iLstk99n0",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      targetFile = "ROF_RA_def_50km_buff.shp", alsoExtract = "similar",
      fun = "sf::st_read", destinationPath = dPath,
      filename2 = "ROF_RA_def_50km_buff", overwrite = TRUE
    ) %>%
      as_Spatial(.)
  }

  ## define test study area
  if (grepl("test", runName)) {
    studyArea <- randomStudyArea(rgeos::gCentroid(studyArea), size = 1e10, seed = NULL)
    studyAreaLarge <- buffer(studyArea, 10000)
    shapefile(sim$studyArea, file.path(outputPath(sim), "studyArea_AOU_test.shp"), overwrite = TRUE)
  }

  sim$studyArea <- studyArea
  sim$studyAreaLarge <- studyAreaLarge
  sim$studyAreaReporting <- studyAreaReporting

  ## RASTERS TO MATCH
  sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                             year = 2005, ## TODO: use 2010
                             studyArea = sim$studyArea,
                             destinationPath = dPath,
                             useCache = P(sim)$.useCache,
                             filename2 = paste0(studyAreaName, '_rtm.tif'))
  sim$rasterToMatchLarge <- Cache(LandR::prepInputsLCC,
                                  year = 2005, ## TODO: use 2010
                                  studyArea = sim$studyAreaLarge,
                                  destinationPath = dPath,
                                  useCache = P(sim)$.useCache,
                                  filename2 = paste0(studyAreaName, '_rtml.tif'))
  sim$rasterToMatchReporting <- Cache(LandR::prepInputsLCC,
                                      year = 2005, ## TODO: use 2010
                                      studyArea = sim$studyAreaReporting,
                                      destinationPath = dPath,
                                      useCache = P(sim)$.useCache,
                                      filename2 = paste0(studyAreaName, '_rtmr.tif'))

  if (P(sim)$.resolution == 125L) {
    sim$rasterToMatch <- Cache(raster::disaggregate, x = sim$rasterToMatch, fact = 2)
    sim$rasterToMatchLarge <- Cache(raster::disaggregate, x = sim$rasterToMatchLarge, fact = 2)
  }

  ## CLIMATE DATA (used by fireSense)
  historicalClimateUrl <- "https://drive.google.com/file/d/1CHSaBUwi_DAdygRQ032yOQWt1yKeSw0s"
  historicalMDC <- prepInputs(url = historicalClimateUrl, ## TODO: put all 3 steps into a single prepInputs call
                              destinationPath = dPath,
                              # rasterToMatch = sim$rasterToMatch,
                              # studyArea = sim$studyArea,
                              fun = "raster::stack",
                              filename2 = paste0(studyAreaName, "_histMDC_lonlat.grd"),
                              overwrite = TRUE,
                              useCache = P(sim)$.useCache,
                              userTags = c("histMDC", cacheTags))
  historicalMDC <- Cache(raster::projectRaster, historicalMDC, to = sim$rasterToMatch,
                         datatype = "INT2U",
                         filename2 = paste0(studyAreaName, "_histMDC_lcc.grd"),
                         overwrite = TRUE,
                         userTags = c("reprojHistoricClimateRasters", cacheTags))
  historicalMDC <- raster::stack(historicalMDC)
  historicalMDC <- Cache(raster::mask, historicalMDC, sim$studyArea,
                         userTags = c("maskHistoricClimateRasters"),
                         filename = file.path(dPath, paste0(studyAreaName, "_histMDC_lcc_masked.grd")),
                         overwrite = TRUE)
  historicalMDC <- raster::stack(historicalMDC)
  historicalMDC <- updateStackYearNames(historicalMDC, Par$historicalFireYears)
  sim$historicalClimateRasters <- list("MDC" = historicalMDC)

  ## lookup table to get projectedClimateURL based on studyArea, GCM, and SSP
  dt <- data.table::fread(file = file.path(dataPath(sim), "climateDataURLs.csv"))
  projectedClimateUrl <- dt[studyArea == "ON" &
                              GCM == P(sim)$climateGCM &
                              SSP == P(sim)$climateSSP, GID]

  projectedMDC <- prepInputs(url = projectedClimateUrl, ## TODO: put all 3 steps into a single prepInputs call
                             destinationPath = dPath,
                             # rasterToMatch = sim$rasterToMatch,
                             # studyArea = sim$studyArea,
                             fun = "raster::stack",
                             filename2 = paste0(studyAreaName, "_projMDC_lonlat.grd"),
                             overwrite = TRUE,
                             useCache = P(sim)$.useCache,
                             userTags = c("histMDC", cacheTags))
  projectedMDC <- Cache(raster::projectRaster, projectedMDC, to = sim$rasterToMatch,
                        datatype = "INT2U",
                        filename = file.path(dPath, paste0(studyAreaName, "_projMDC_lcc.grd")),
                        overwrite = TRUE,
                        userTags = c("reprojProjectedMDC", cacheTags))
  projectedMDC <- stack(projectedMDC)
  projectedMDC <- Cache(raster::mask, projectedMDC, sim$studyArea,
                        userTags = c("maskProjectedClimateRasters"),
                        filename = file.path(dPath, paste0(studyAreaName, "_projMDC_lcc_masked.grd")),
                        overwrite = TRUE)
  projectedMDC <- stack(projectedMDC)
  projectedMDC <- updateStackYearNames(projectedMDC, Par$projectedFireYears)
  sim$projectedClimateRasters <- list("MDC" = projectedMDC)

  ## SPECIES STUFF
  data("sppEquivalencies_CA", package = "LandR")
  sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                               EN_generic_full = "Pine",
                                               Leading = "Pine leading")]

  ## 'ONFRI' used for Ontario forest resource inventory layers
  sppEquivalencies_CA[, ONFRI := c(Abie_bal = "Abie_bal",
                                   Betu_pap = "Betu_pap",
                                   Lari_lar = "Lari_lar",
                                   Pice_gla = "Pice_gla",
                                   Pice_mar = "Pice_mar",
                                   Pinu_ban = "Pinu_ban", #Pinu_res = "Pinu_res", ## TODO: double check Red Pine
                                   Popu_bal = "Popu_bal", Popu_tre = "Popu_tre",
                                   Thuj_occ = "Thuj_spp")[LandR]]

  ## 'ON' used for simulations (i.e., sppEquivCol)
  sppEquivalencies_CA[, ON := c(Abie_bal = "Abie_bal",
                                Betu_pap = "Betu_pap",
                                Lari_lar = "Lari_lar",
                                Pice_gla = "Pice_gla",
                                Pice_mar = "Pice_mar",
                                Pinu_ban = "Pinu_ban", #Pinu_res = "Pinu_res", ## TODO: double check Red Pine
                                Popu_bal = "Popu_sp", Popu_tre = "Popu_sp",
                                Thuj_occ = "Thuj_sp")[LandR]]

  sppEquivalencies_CA[ON == "Abie_sp", EN_generic_full := "Fir"]
  sppEquivalencies_CA[ON == "Abie_sp", EN_generic_short := "Fir"]

  sppEquivalencies_CA[ON == "Betu_sp", EN_generic_full := "Birch"]
  sppEquivalencies_CA[ON == "Betu_sp", EN_generic_short := "Birch"]

  sppEquivalencies_CA[ON == "Lari_lar", EN_generic_full := "Tamarack"]
  sppEquivalencies_CA[ON == "Lari_lar", EN_generic_short := "Tamarack"]

  sppEquivalencies_CA[ON == "Popu_sp", EN_generic_full := "Poplar"]
  sppEquivalencies_CA[ON == "Popu_sp", EN_generic_short := "Poplar"]

  sppEquivalencies_CA[ON == "Thuj_sp", `:=`(EN_generic_full = "Cedar",
                                            EN_generic_short = "Cedar",
                                            LANDIS_traits = "THUJ.SPP.ALL",
                                            Leading = "Cedar leading")]

  sim$sppEquiv <- sppEquivalencies_CA[!is.na(ON), ]

  sim$sppColorVect <- sppColors(sppEquivalencies_CA, sppEquivCol, newVals = "Mixed", palette = "Accent")

  sim$speciesTable <- getSpeciesTable(dPath = dPath) ## uses default URL

  ## LANDCOVER AND AGE MAPS (kNN and ON FRI)
  LCC2005 <- prepInputsLCC(year = 2005, studyArea = sim$studyAreaLarge, destinationPath = dPath) ## TODO: use LCC2010
  if (P(sim)$.resolution == 125L) {
    LCC2005 <- Cache(raster::disaggregate, x = LCC2005, fact = 2)
  }

  ## NOTE: there are 10 LCC classes for ON (see Benoit's README); we want class 10 - FOR)
  if (studyAreaName == "AOU") {
    LCC_FRI <- prepInputs(url = "https://drive.google.com/file/d/1eg9yhkAKDsQ8VO5Nx4QjBg4yiB0qyqng",
                          destinationPath = dPath, filename2 = "lcc_fri_ceon_250m.tif",
                          fun = "raster::raster", method = "ngb",
                          rasterToMatch = sim$rasterToMatchLarge)
  } else if (studyAreaName == "ROF") {
    if (P(sim)$.resolution == 125L) {
      LCC_FRI <- prepInputs(url = "https://drive.google.com/file/d/1JouBj0iJOPB1qQeXkRRePMN6MZSX_R_q",
                            destinationPath = dPath, filename2 = "lcc_fri_rof_125m.tif",
                            fun = "raster::raster", method = "ngb",
                            rasterToMatch = sim$rasterToMatchLarge)
    } else if (P(sim)$.resolution == 250L) {
      LCC_FRI <- prepInputs(url = "https://drive.google.com/file/d/1-2XSrSp_WrZCnqUhHTaj0rQpzOcSLrfS",
                            destinationPath = dPath, filename2 = "lcc_fri_rof_250m.tif",
                            fun = "raster::raster", method = "ngb",
                            rasterToMatch = sim$rasterToMatchLarge)
    }
  }
  LCC_FRI <- setMinMax(LCC_FRI)

  ###### FRI LANDCOVER CLASSES
  ## 1 - water
  ## 2 - developed agricultural land
  ## 3 - grass and meadow
  ## 4 - small island
  ## 5 - unclassified
  ## 6 - brush and alder
  ## 7 - rock
  ## 8 - treed wetland
  ## 9 - open wetland
  ## 10 - forested
  nontreeClassesFRI <- 1:9
  treeClassesFRI <- 10
  treePixelsFRI_TF <- LCC_FRI[] %in% treeClassesFRI
  LandTypeFRI_NA <- is.na(LCC_FRI[])
  noDataPixelsFRI <- LandTypeFRI_NA
  treePixelsCC <- which(treePixelsFRI_TF)

  uniqueLCCclasses <- na.omit(unique(LCC2005[]))
  nontreeClassesLCC <- sort(uniqueLCCclasses[!uniqueLCCclasses %in% P(sim)$treeClassesLCC])

  ## for each LCC2005 + LCC_FRI class combo, define which LCC2005 code should be used
  ## remember, setting a pixel to NA will omit it entirely (i.e., non-vegetated)
  remapDT <- as.data.table(expand.grid(LCC2005 = c(NA_integer_, sort(uniqueLCCclasses)),
                                       LCC_FRI = c(NA_integer_, 1:10)))
  remapDT[LCC2005 == 0, newLCC := NA_integer_]
  remapDT[is.na(LCC_FRI), newLCC := LCC2005]
  remapDT[LCC_FRI %in% c(1, 5, 7), newLCC := NA_integer_]
  remapDT[LCC_FRI %in% c(2, 3, 6, 8, 9, 10), newLCC := LCC2005]
  remapDT[is.na(LCC2005) & LCC_FRI %in% 10, newLCC := 99] ## reclassification needed
  remapDT[LCC2005 %in% P(sim)$treeClassesToReplace, newLCC := 99] ## reclassification needed

  sim$LCC <- Cache(overlayLCCs,
                   LCCs = list(LCC_FRI = LCC_FRI, LCC2005 = LCC2005),
                   forestedList = list(LCC_FRI = 10, LCC2005 = P(sim)$treeClassesLCC),
                   outputLayer = "LCC2005",
                   remapTable = remapDT,
                   classesToReplace = c(P(sim)$treeClassesToReplace, 99),
                   availableERC_by_Sp = NULL)
  treePixelsLCC <- which(sim$LCC[] %in% P(sim)$treeClassesLCC)
  nonTreePixels <- which(sim$LCC[] %in% nontreeClassesLCC)

  sim$nonTreePixels <- nonTreePixels

  standAgeMapURL <- paste0(
    "http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
    "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/",
    "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"
  )
  standAgeMapFileName <- basename(standAgeMapURL)

  standAgeMap2001 <- Cache(
    LandR::prepInputsStandAgeMap,
    ageUrl = standAgeMapURL,
    rasterToMatch = sim$rasterToMatchLarge,
    studyArea = sim$studyAreaLarge,
    destinationPath = dPath,
    startTime = 2001,
    filename2 = .suffix("standAgeMap_2001.tif", paste0("_", P(sim)$studyAreaName)),
    userTags = c("stable", currentModule(sim), P(sim)$studyAreaname)
  )

  standAgeMap2011 <- Cache(
    LandR::prepInputsStandAgeMap,
    ageURL = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                    "canada-forests-attributes_attributs-forests-canada/",
                    "2011-attributes_attributs-2011/",
                    "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"),
    rasterToMatch = sim$rasterToMatchLarge,
    studyArea = sim$studyAreaLarge,
    destinationPath = dPath,
    startTime = 2011,
    filename2 = .suffix("standAgeMap_2011.tif", paste0("_", P(sim)$studyAreaName)),
    userTags = c("stable", currentModule(sim), P(sim)$studyAreaname)
  )
#browser()
if (FALSE) {
  ## overlay age from FRI. These are assembled from multiple years, so will adjust ages accordingly.
  if (studyAreaName == "AOU") {
    standAgeMapFRI <- prepInputs(url = "https://drive.google.com/file/d/1NGGUQi-Un6JGjV6HdIkGzjPd1znHFvBi",
                                 destinationPath = dPath, filename2 = "age_fri_ceon_250m.tif",
                                 fun = "raster::raster", method = "bilinear", datatype = "INT2U",
                                 rasterToMatch = sim$rasterToMatchLarge,
                                 userTags = c("standAgeMapFRI", studyAreaName, currentModule(sim)))
    refYearMapFRI <- prepInputs(url = "",
                                destinationPath = dPath, filename2 = "",
                                fun = "raster::raster", method = "bilinear", datatype = "INT2U",
                                rasterToMatch = sim$rasterToMatchLarge,
                                userTags = c("refYearMapFRI", studyAreaName, currentModule(sim)))
  } else if (studyAreaName == "ROF") {
    if (P(sim)$.resolution == 125L) {
      standAgeMapFRI <- prepInputs(url = "https://drive.google.com/file/d/1l0_tx4_fwFZ5RExspBr08ETQDtr0HrXr",
                                   destinationPath = dPath, filename2 = "age_fri_rof_125m.tif",
                                   fun = "raster::raster", method = "bilinear", datatype = "INT2U",
                                   rasterToMatch = sim$rasterToMatchLarge,
                                   userTags = c("standAgeMapFRI", studyAreaName, currentModule(sim)))
    } else if (P(sim)$.resolution == 250L) {
      standAgeMapFRI <- prepInputs(url = "https://drive.google.com/file/d/1AIjLN9V80ln23hr_ECsEqWkcP0UNQetl",
                                   destinationPath = dPath, filename2 = "age_fri_rof_250m.tif",
                                   fun = "raster::raster", method = "bilinear", datatype = "INT2U",
                                   rasterToMatch = sim$rasterToMatchLarge,
                                   userTags = c("standAgeMapFRI", studyAreaName, currentModule(sim)))
    }
  }
  standAgeMapFRI <- setMinMax(standAgeMapFRI)
  standAgeMapFRI[standAgeMapFRI < 0] <- 0L

  # TODO:
  #   1. adjust each age by reference year, for 2001 and 2011 to get age in 2001 and 2011
  #   2. any values < 0, set as NA. we will fall back to kNN values for these

  ## 2001 age map
  standAgeMap2001[noDataPixelsFRI] <- standAgeMapFRI[noDataPixelsFRI]
  standAgeMap2001[sim$nonTreePixels] <- NA ## TODO: i think we need ages on non-treed for fS

  ## 2011 age map
  standAgeMap2011[noDataPixelsFRI] <- standAgeMapFRI[noDataPixelsFRI]
  standAgeMap2011[sim$nonTreePixels] <- NA ## TODO: i think we need ages on non-treed for fS
}
  sim$standAgeMap2001 <- as.integer(standAgeMap2001)
  sim$standAgeMap2011 <- as.integer(standAgeMap2011)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere(targetCRS)) {
    sim$targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                           "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
