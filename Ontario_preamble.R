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
  reqdPkgs = list(),
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
    defineParameter("runName", 'character', 'AOU', NA, NA,
                    paste("Should include one of 'AOU' or 'ROF' to identify the studyArea,",
                          "as well as 'RCP45' or 'RCP85' to identify the climate scenario to use."))
  ),
  inputObjects = bindrows(
    expectsInput("targetCRS", "character", desc = "Geospatial projection to use.", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("climateDataHistoric", "list", desc = "list of RasterStacks of historical MDC calculated from ClimateNA data."),
    createsOutput("climateDataProjected", "list", desc = "list of RasterStacks of projected MDC calculated from ClimateNA data."),
    createsOutput("rasterToMatch", "RasterLayer", desc = "Raster to match, based on study area."),
    createsOutput("rasterToMatchLarge", "RasterLayer", desc = "Raster to match (large) based on studyAreaLarge."),
    createsOutput("speciesTable", "data.table", desc = "Species parameter table."),
    createsOutput("sppColorVect", "character", desc = "Species colour vector."),
    createsOutput("sppEquiv", "data.table", desc = "Species equivalency table."),
    createsOutput("studyArea", "SpatialPolygons", desc = "Buffered study area in which to run simulations."),
    createsOutput("studyAreaLarge", "SpatialPolygons", desc = "Buffered study area in which to run simulations.")
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
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Ontario_preamble", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "Ontario_preamble", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event


      # schedule future event(s)

      # e.g.,
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

  ## provincial boundary
  canProvs <- Cache(prepInputs,
                    "GADM",
                    fun = "base::readRDS",
                    dlFun = "raster::getData",
                    country = "CAN", level = 1, path = inputDir,
                    #sim$targetCRS = targetCRS, ## TODO: fails on Windows
                    targetFile = "gadm36_CAN_1_sp.rds", ## TODO: this will change as GADM data update
                    cacheRepo = cacheDir,
                    destinationPath = inputDir) %>%
    st_as_sf(.)

  ON <- canProvs[canProvs$NAME_1 == "Ontario", ] %>%
    st_transform(., crs = sim$targetCRS)

  ## STUDY AREA
  if (grepl("AOU", toupper(P(sim)$runName))) {
    studyArea <- prepInputs(
      url = "https://drive.google.com/file/d/1Idtreoo51hGBdfJXp0BmN83bOkKHTXvk",
      destinationPath = inputDir,
      targetFile = "CEON_def.shp",
      alsoExtract = "similar",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      fun = "sf::st_read",
      overwrite = TRUE,
      team_drive = TRUE
    ) %>%
      as_Spatial(.)

    studyAreaLarge <- prepInputs(
      url = "https://drive.google.com/file/d/1ngQshBgoyLjkjuXnloXPg7IUMdLmppkB",
      destinationPath = inputDir,
      targetFile = "CEON_def_50km_buff.shp",
      alsoExtract = "similar",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      fun = "sf::st_read",
      overwrite = TRUE,
      team_drive = TRUE
    ) %>%
      as_Spatial(.)
  } else if (grepl("ROF", toupper(P(sim)$runName))) {
    studyArea <- prepInputs(
      url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
      fun = "sf::st_read", destinationPath = dPath, filename2 = "ROF_RA_def", overwrite = TRUE
    ) %>%
      as_Spatial(.)

    studyAreaLarge <- prepInputs(
      url = "https://drive.google.com/file/d/1iOXXIkvY-YaR9BTG_SRd5R_iLstk99n0",
      targetCRS = targetCRS, ## TODO: fails on Windows
      targetFile = "ROF_RA_def_50km_buff.shp", alsoExtract = "similar",
      fun = "sf::st_read", destinationPath = dPath, filename2 = "ROF_RA_def_50km_buff", overwrite = TRUE
    ) %>%
      as_Spatial(.)
  }

  ## RASTERS TO MATCH
  sim$rasterToMatch <- LandR::prepInputsLCC(studyArea = sim$studyArea,
                                            destinationPath = dPath,
                                            useCache = P(sim)$.useCache,
                                            filename2 = paste0(P(sim)$studyAreaName, '_rtm.tif'))
  sim$rasterToMatchLarge <- LandR::prepInputsLCC(studyArea = sim$studyAreaLarge,
                                                 destinationPath = dPath,
                                                 useCache = P(sim)$.useCache,
                                                 filename2 = paste0(P(sim)$studyAreaName, '_rtml.tif'))

  ## CLIMATE DATA (used by fireSense)
  historicalClimateUrl <- "https://drive.google.com/file/d/1ZsppgYeIYsrcScvetoLJNjuA0E5IwhkM"
  historicalMDC <- prepInputs(url = historicalClimateUrl,
                              destinationPath = dPath,
                              # rasterToMatch = sim$rasterToMatch,
                              # studyArea = sim$studyArea,
                              fun = 'raster::stack',
                              filename2 = paste0(P(sim)$studyAreaName, '_histClim.grd'),
                              useCache = TRUE,
                              userTags = c("histMDC", cacheTags))
  historicalMDC <- Cache(raster::projectRaster, historicalMDC, to = sim$rasterToMatch,
                         datatype = 'INT2U',
                         userTags = c("reprojHistoricClimateRasters"))
  historicalMDC <- Cache(raster::mask, historicalMDC, sim$studyArea,
                         userTags = c("maskHistoricClimateRasters"),
                         filename = file.path(dPath, paste0(P(sim)$studyAreaName, '_histMDC.grd')),
                         overwrite = TRUE)
  names(historicalMDC) <- paste0('year', P(sim)$historicalFireYears)
  sim$historicalClimateRasters <- list('MDC' = historicalMDC)

  projectedClimateUrl <- if (grepl("RCP45", runName)) {
    "https://drive.google.com/file/d/1xgTS-BHd3Rna5C2svqBdneQMEWjD5q5Z/"
  } else if (grepl("RCP85", runName)) {
    "https://drive.google.com/file/d/1haj15Jf7HhEWxRU52_mTp3VIcqfWYQ5K/"
  }

  projectedMDC <- prepInputs(url = projectedClimateUrl,
                             destinationPath = dPath,
                             # rasterToMatch = sim$rasterToMatch,
                             # studyArea = sim$studyArea,
                             fun = 'raster::stack',
                             filename2 = paste0(P(sim)$studyAreaName, '_projClim.grd'),
                             useCache = TRUE,
                             userTags = c("histMDC", cacheTags))

  projectedMDC <- Cache(raster::projectRaster, projectedMDC, to = sim$rasterToMatch,
                        datatype = 'INT2U',
                        userTags = c("reprojProjectedMDC"))

  projectedMDC <- Cache(raster::mask, projectedMDC, sim$studyArea,
                        userTags = c("maskProjectedClimateRasters"),
                        filename = file.path(dPath, paste0(P(sim)$studyAreaName, '_projMDC.grd')),
                        overwrite = TRUE)
  names(projectedMDC) <- paste0('year', P(sim)$projectedFireYears)
  sim$projectedClimateRasters <- list('MDC' = projectedMDC)

  ## SPECIES STUFF
  data("sppEquivalencies_CA", package = "LandR")
  sppEquivalencies_CA[grep("Pin", LandR), `:=`(EN_generic_short = "Pine",
                                               EN_generic_full = "Pine",
                                               Leading = "Pine leading")]

  sppEquivalencies_CA[, ON := c(Abie_bal = "Abie_sp", Abie_las = "Abie_sp", Abie_sp = "Abie_sp",
                                Betu_pap = "Betu_sp",
                                Lari_lar = "Lari_lar",
                                Pice_gla = "Pice_gla",
                                Pice_mar = "Pice_mar",
                                Pinu_ban = "Pinu_sp",
                                Popu_bal = "Popu_sp", Popu_tre = "Popu_sp",
                                Thuj_occ = "Thuj_sp", Thuj_sp = "Thuj_sp")[LandR]]

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

  sim$sppColorVect <- sppColors(sppEquivalencies_CA, sppEquivCol, palette = "Accent")

  sim$speciesTable <- getSpeciesTable(dPath = paths1$inputPath) ## uses default URL

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere(targetCRS)) {
    targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                       "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
