defineModule(sim, list(
  name = "Ontario_preamble",
  description = paste("this module prepares 2 sets of objects needed for LandR simulations in Ontario AOU/ROF:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions);",
                      "2. species equivalencies tables and the sppEquiv column.",
                      "Each is customized to the study area parameter passed as studyAreaName."),
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", role = c("aut", "cre"), email = "achubaty@for-cast.ca"),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(Ontario_preamble = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Ontario_preamble.Rmd")),
  reqdPkgs = list("archive", "httr", "raster", "rgeos", "reproducible", "sf", "sp",
                  "PredictiveEcology/reproducible@terraInProjectInputs (>= 1.2.8.9033)",
                  "PredictiveEcology/LandR@development (>= 1.0.7.9004)"),
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
    defineParameter(".resolution", "numeric", 250, NA, NA,
                    "raster pixel size, in m,  to use for simulation. Either 250 or 125."),
    defineParameter("runName", "character", "AOU", NA, NA,
                    paste("Should include one of 'AOU' or 'ROF' to identify the studyArea",
                          "(if 'ROF', then 'shield' or 'plain' should be specified too,",
                          "to identify whether to run in the Boreal Shield or Hudson Plains ecozone);",
                          "as well as 'CanESM5_SSP370' or 'CNRM-ESM2-1_SSP370' (or SSP585) to identify the climate scenario to use.")),
    defineParameter("useAgeMapkNN", "logical", FALSE, NA, NA,
                    paste("if TRUE, use kNN age maps, corrected with fire polygons data;",
                          "if FALSE, use Raquel's predicted age map from ROF_age."))
  ),
  inputObjects = bindrows(
    expectsInput("targetCRS", "character", desc = "Geospatial projection to use.", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("ageMap", "RasterLayer", desc = "Age (time since disturbance) map, derived from national kNN product and ON FRI data."),
    createsOutput("fireSenseForestLCC", "integer", desc = "vector of LCC classes considered to be forested by fireSEnse."),
    createsOutput("LandRforestLCC", "integer", desc = "vector of LCC classes considered to be forested by LandR."),
    createsOutput("LCC", "RasterLayer", desc = "Land cover classification map, derived from national LCC 2005 product and ON FRI data."),
    createsOutput("missingLCCgroup", "character", "the group in nonForestLCCGroups that describes forested pixels omitted by LandR"),
    createsOutput("nonflammableLCC", "integer", desc = "vector of LCC classes considered to be nonflammable"),
    createsOutput("nonForestLCCGroups", "list",desc = "named list of non-forested landcover groups for fireSense"),
    createsOutput("nontreeClasses", "integer", desc = "vector of LCC classes considered to be non-forested/treed."), #TODO what is this used for?
    createsOutput("nonTreePixels", "integer", desc = "pixel indices indicating non-treed pixels"), #TODO: what is this used for?
    createsOutput("rasterToMatch", "RasterLayer", desc = "Raster to match, based on study area."),
    createsOutput("rasterToMatchLarge", "RasterLayer", desc = "Raster to match (large) based on studyAreaLarge."),
    createsOutput("rasterToMatchReporting", "RasterLayer", desc = "Raster to match based on studyAreaReporting."),
    createsOutput("speciesTable", "data.table", desc = "Species parameter table."),
    createsOutput("sppColorVect", "character", desc = "Species colour vector."),
    createsOutput("sppEquiv", "data.table", desc = "Species equivalency table."),
    createsOutput("sppEquivCol", objectClass = "character", desc = "name of column to use in sppEquiv"),
    createsOutput("standAgeMap2001", "RasterLayer", desc = "raster of time since disurbance for year 2001."),
    createsOutput("standAgeMap2011", "RasterLayer", desc = "raster of time since disurbance for year 2011."),
    createsOutput("studyArea", "SpatialPolygons", desc = "Buffered study area in which to run simulations."),
    createsOutput("studyAreaLarge", "SpatialPolygons", desc = "Buffered study area used for parameterization/calibration."),
    createsOutput("studyAreaPSP", "SpatialPolygonsDataFrame",
                  paste("this area will be used to subset PSP plots before building the statistical model.",
                        "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                        "Alberta, and Boreal British Columbia")),
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
    if (grepl("boreal|shield", runName)) {
      "ROF_shield"
    } else if (grepl("plain", runName)) {
      "ROF_plain"
    }
  } else {
    stop("runName must contain one of 'AOU' or 'ROF'.")
  }

  stopifnot(P(sim)$.resolution %in% c(125, 250))

  ## provincial boundary
  canProvs <- raster::getData("GADM", path = dPath, country = "CAN", level = 1, type = "sf")
  st_crs(canProvs) <- st_crs(canProvs) ## fix "old-style crs" warning from sf

  mod$ON <- canProvs[canProvs$NAME_1 == "Ontario", ] %>%
    st_transform(., crs = sim$targetCRS)

  ## ECOZONES
  ez <- switch(studyAreaName,
               ROF_shield = "BOREAL SHIELD",
               ROF_plain = "HUDSON PLAIN",
               NULL)

  ecozones <- prepInputs(
    url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
    targetFile = "ecozones.shp",
    alsoExtract = "similar",
    fun = "sf::st_read",
    destinationPath = dPath,
    targetCRS = sim$targetCRS
  )
  ecozones[["ZONE_NAME"]] <- toupper(ecozones[["ZONE_NAME"]])
  ecozone <- ecozones[ecozones$ZONE_NAME == ez, ]
  rm(ecozones)

  ## STUDY AREA
  if (studyAreaName == "AOU") {
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
  } else if (grepl("ROF", studyAreaName)) {
    studyAreaReporting <- prepInputs(
      url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ",
      targetCRS = sim$targetCRS, ## TODO: fails on Windows
      targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
      fun = "sf::st_read", destinationPath = dPath,
      filename2 = "ROF_RA_def", overwrite = TRUE
    ) %>%
      st_intersection(., ecozone) %>%
      as_Spatial(.)

    studyArea <- buffer(studyAreaReporting, 20000) ## 20 km buffer

    studyAreaLarge <- studyArea
    # studyAreaLarge <- prepInputs(
    #   url = "https://drive.google.com/file/d/1iOXXIkvY-YaR9BTG_SRd5R_iLstk99n0",
    #   targetCRS = sim$targetCRS, ## TODO: fails on Windows
    #   targetFile = "ROF_RA_def_50km_buff.shp", alsoExtract = "similar",
    #   fun = "sf::st_read", destinationPath = dPath,
    #   filename2 = "ROF_RA_def_50km_buff", overwrite = TRUE
    # ) %>%
    #   st_intersection(., ecozone) %>%
    #   as_Spatial(.)
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
    sim$rasterToMatch <- raster::disaggregate(sim$rasterToMatch, fact = 2)
    sim$rasterToMatchLarge <- raster::disaggregate(sim$rasterToMatchLarge, fact = 2)
    sim$rasterToMatchReporting <- raster::disaggregate(sim$rasterToMatchReporting, fact = 2)
  }

  ## SPECIES STUFF
  sim$sppEquiv <- makeSppEquivON()
  sim$sppEquivCol <- "ON"

  sim$sppColorVect <- sppColors(sppEquivalencies_CA, sim$sppEquivCol, newVals = "Mixed", palette = "Accent")

  sim$speciesTable <- getSpeciesTable(dPath = dPath) ## uses default URL

  ## LANDCOVER MAPS (LCC2005 + FRI if AOU; Far North if ROF)
  if (studyAreaName == "AOU") {
    LCC2005 <- prepInputsLCC(year = 2005, studyArea = sim$studyAreaLarge, destinationPath = dPath) ## TODO: use LCC2010
    if (P(sim)$.resolution == 125L) {
      LCC2005 <- raster::disaggregate(LCC2005, fact = 2)
    }

    sim$nonflammableLCC  <- c(0, 25, 30, 33, 36:39)
    treeClassesLCC <- c(1:15, 20, 32, 34:35)
    uniqueLCCclasses <- na.omit(unique(LCC2005[]))
    nontreeClassesLCC <- sort(uniqueLCCclasses[!uniqueLCCclasses %in% treeClassesLCC])

    LCC_FRI <- prepInputs(url = "https://drive.google.com/file/d/1eg9yhkAKDsQ8VO5Nx4QjBg4yiB0qyqng",
                          destinationPath = dPath, filename2 = "lcc_fri_ceon_250m.tif",
                          fun = "raster::raster", method = "ngb",
                          rasterToMatch = sim$rasterToMatchLarge)

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

    ## for each LCC2005 + LCC_FRI class combo, define which LCC2005 code should be used
    ## remember, setting a pixel to NA will omit it entirely (i.e., non-vegetated)
    treeClassesToReplace <- c(34:35)
    remapDT <- as.data.table(expand.grid(LCC2005 = c(NA_integer_, sort(uniqueLCCclasses)),
                                         LCC_FRI = c(NA_integer_, 1:10)))
    remapDT[LCC2005 == 0, newLCC := NA_integer_]
    remapDT[is.na(LCC_FRI), newLCC := LCC2005]
    remapDT[LCC_FRI %in% c(1, 5, 7), newLCC := NA_integer_]
    remapDT[LCC_FRI %in% c(2, 3, 6, 8, 9, 10), newLCC := LCC2005]
    remapDT[is.na(LCC2005) & LCC_FRI %in% 10, newLCC := 99] ## reclassification needed
    remapDT[LCC2005 %in% treeClassesToReplace, newLCC := 99] ## reclassification needed

    sim$LCC <- Cache(overlayLCCs,
                     LCCs = list(LCC_FRI = LCC_FRI, LCC2005 = LCC2005),
                     forestedList = list(LCC_FRI = 10, LCC2005 = treeClassesLCC),
                     outputLayer = "LCC2005",
                     remapTable = remapDT,
                     classesToReplace = c(treeClassesToReplace, 99),
                     availableERC_by_Sp = NULL)

    nontreeClassesLCC <- (1:39)[!(1:39 %in% treeClassesLCC)]
    treePixelsLCC <- which(sim$LCC[] %in% treeClassesLCC) ## c(1:15, 20, 32, 34:35)
    nonTreePixels <- which(sim$LCC[] %in% nontreeClassesLCC)

    LandRforestClasses <- treeClassesLCC
    fireSenseForestClasses <- treeClassesLCC
    sim$nonForestClasses <- nontreeClassesLCC

    stop("figure out what nonForestedLCCGroups is with LCC2005...")
    sim$nonForestLCCGroups <- list(
      nonForest_highFlam = c(16:19, 22),
      nonForest_lowFlam = c(21, 23:24, 26:29, 31)
    )
    sim$missingLCCGroup <- "nonForest_highFlam"


  } else if (grepl("ROF", studyAreaName)) {

    ## FAR NORTH LANDCOVER (620 MB)
    ## unable to download directly b/c of SSL, time outs, and other server problems
    ##   https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/FarNorthLandCover.zip
    ##

    FarNorthLandCoverZip <- file.path(dPath, "FarNorthLandCover.zip")
    rFiles <- c("New folder/Class/FarNorth_LandCover_Class_UTM15.tif",
                "New folder/Class/FarNorth_LandCover_Class_UTM16.tif",
                "New folder/Class/FarNorth_LandCover_Class_UTM17.tif")

    if (!file.exists(FarNorthLandCoverZip)) {
      googledrive::drive_download(file = as_id("1_QeOAeSHjOjEU1iTqACRj8BYo3ysQKUi"),
                                  path = FarNorthLandCoverZip)
      archive::archive_extract(
        archive = file.path(dPath, "FarNorthLandCover.zip"),
        dir = file.path(dPath, "FarNorthLandCover"),
        files = "Version 1.4/TIF Format/ab1da0f2-7bba-430b-af11-d503865ff130-TIF.zip"
      )

      archive::archive_extract(
        archive = file.path(dPath, "FarNorthLandCover", "Version 1.4", "TIF Format",
                            "ab1da0f2-7bba-430b-af11-d503865ff130-TIF.zip"),
        dir = file.path(dPath, "FarNorthLandCover"),
        files = rFiles
      )

      vapply(file.path(dPath, "FarNorthLandCover", rFiles), file.copy, logical(1),
             to = file.path(dPath, "FarNorthLandCover"))
      unlink(file.path(dPath, "FarNorthLandCover", "New folder"), recursive = TRUE)
      unlink(file.path(dPath, "FarNorthLandCover", "Version 1.4"), recursive = TRUE)
    }

    r15 <- raster(file.path(dPath, "FarNorthLandCover", basename(rFiles))[1])
    r16 <- raster(file.path(dPath, "FarNorthLandCover", basename(rFiles))[2])
    r17 <- raster(file.path(dPath, "FarNorthLandCover", basename(rFiles))[3])

    e15 <- extent(r15)
    e16 <- extent(r16)
    e17 <- extent(r17)
    u <- raster::union(e15, e16) %>% raster::union(., e17)
    template <- raster(u, res = P(sim)$.resolution,  crs = crs(sim$rasterToMatch))

    f <- file.path(dPath, "FarNorthLandCover", paste0("FarNorth_LandCover_Class_UTM", 15:17, ".tif"))
    t15 <- raster(f[1]) %>% projectRaster(., template, alignOnly = TRUE)
    t16 <- raster(f[2]) %>% projectRaster(., template, alignOnly = TRUE)
    t17 <- raster(f[3]) %>% projectRaster(., template, alignOnly = TRUE)

    r15 <- raster(f[1]) %>% projectRaster(., t15, method = "ngb", datatype = "INT2U")
    r16 <- raster(f[2]) %>% projectRaster(., t16, method = "ngb", datatype = "INT2U")
    r17 <- raster(f[3]) %>% projectRaster(., t17, method = "ngb", datatype = "INT2U")

    LCC_FN <- raster::mosaic(r15, r16, r17, fun = min)
    LCC_FN[LCC_FN[] <= 0] <- NA_integer_ ## remove 0, -9, and -99
    LCC_FN[LCC_FN[] > 24] <- NA_integer_ ## remove >24
    LCC_FN <- Cache(postProcess, LCC_FN, method = "ngb", rasterToMatch = sim$rasterToMatchLarge)

    #LandR forest classes are distinct from fireSense forest classes, in that
    #fireSense assesses forest by the dominant species composition (i.e. fuel class) and not the landcover.
    #however, fire may be strongly influenced by landcover (e.g., wetland) therefore
    #fireSense forest classes are restricted to upland forest classes, unlike LandR
    #Here we reclassify disturbed to it's nearest valid (i.e. flammable) class using an expanding focal window
    #If no suitable pixel is found in the neighbouring 3-pixel locus, coniferous forest class is assigned by default
    #this would mean the pixel is at the center of a (7*125m^2) = 75 ha patch)


    ##### Far North cover classes described in `Far North Land Cover - Data Specification.pdf`
    # "Clear Open Water" = 1, "Turbid Water" = 2, "Intertidal Mudflat" = 3,
    # "Intertidal Marsh" = 4, "Supratidal Marsh" = 5, "Fresh Water Marsh" = 6,
    # "Heath" = 7, "Thicket Swamp" = 8, "Coniferous Swamp" = 9, "Deciduous Swamp" = 10,
    # "Open Fen" = 11, "Treed Fen" = 12, "Open Bog" = 13, "Treed Bog" = 14,
    # "Sparse Treed" = 15, "Deciduous Treed" = 16, "Mixed Treed" = 17, "Coniferous Treed" = 18,
    # "Disturbance - Non and sparse-woody" = 19, "Disturbance - Treed or shrub" = 20,
    # "Sand/Gravel/Mine Tailings" = 21, "Bedrock" = 22, "Community/Infrastructure" = 23,
    # "Agriculture" = 24, "Cloud/Shadow" = -9, "Other" = -99
    LCC_FN <- LandR::convertUnwantedLCC2(classesToReplace = 19:20, rstLCC = LCC_FN,
                                          nIterations = 3, defaultNewValue = 18,
                                          invalidClasses = c(1:5, 21:24))

    nontreeClassesLCC <- c(1:8, 11, 13, 21:24)
    LandRforestedLCC <- c(9:11, 14, 15:18)
    fireSenseForestedLCC <- c(15:18)
    # treeClassesLCC <- c(9:10, 12, 14:18, 19:20) ## NOTE: 19:20 are disturbed classes -- reclassify them
    sim$nonflammableLCC <- c(1:6, 21:23) #assumes agriculture is flammmable...

    treePixelsFN_TF <- LCC_FN[] %in% LandRforestedLCC
    LandTypeFN_NA <- is.na(LCC_FN[])
    noDataPixelsFN <- LandTypeFN_NA
    treePixelsCC <- which(treePixelsFN_TF)

    sim$LCC <- asInteger(LCC_FN)
    treePixelsLCC <- which(sim$LCC[] %in% LandRforestedLCC)
    nonTreePixels <- which(sim$LCC[] %in% nontreeClassesLCC)

    sim$nonForestLCCGroups <- list(
      "FenPlus" = c(7, 8, 10, 11, 12, 24), #heath, thicket swamp, deciduous swamp, open fen, treed fen, agriculture
      #ag/heath/deciduous swamp are present in trivial amounts
      "BogSwamp" = c(9, 13, 14)) #coniferous swamp, open bog, treed bog. These burn at ~2x the rate of other non-forest classes
    sim$missingLCCGroup <- "BogSwamp"
  }
  sim$LandRforestedLCC <- LandRforestedLCC
  sim$fireSenseForestedLCC <- fireSenseForestedLCC
  sim$nonTreePixels <- nonTreePixels
  sim$treeClasses <- sim$LandRforestedLCC #TODO review what this is used for
  sim$nontreeClasses <- nontreeClassesLCC

  ## STAND AGE MAP (TIME SINCE DISTURBANCE)
  if (isTRUE(P(sim)$useAgeMapkNN)) {
    standAgeMapURL <- paste0(
      "http://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
      "canada-forests-attributes_attributs-forests-canada/2001-attributes_attributs-2001/",
      "NFI_MODIS250m_2001_kNN_Structure_Stand_Age_v1.tif"
    )
    standAgeMapFileName <- basename(standAgeMapURL)

    fireURL <- "https://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_poly/current_version/NFDB_poly.zip"
    fireYear <- Cache(prepInputsFireYear,
                      earliestYear = 1950,
                      url = fireURL,
                      fun = "sf::st_read",
                      destinationPath = dPath,
                      rasterToMatch = sim$rasterToMatchLarge)
    fireYear <- postProcess(fireYear, rasterToMatch = sim$rasterToMatchLarge) ## needed cropping

    standAgeMap2001 <- Cache(
      LandR::prepInputsStandAgeMap,
      ageUrl = standAgeMapURL,
      rasterToMatch = sim$rasterToMatchLarge,
      studyArea = sim$studyAreaLarge,
      destinationPath = dPath,
      startTime = 2001,
      fireURL = fireURL,
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
      fireURL = fireURL,
      filename2 = .suffix("standAgeMap_2011.tif", paste0("_", P(sim)$studyAreaName)),
      userTags = c("stable", currentModule(sim), P(sim)$studyAreaname)
    )

    ## stand age maps already adjusted within fire polygons using LandR::prepInputsStandAgeMap.
    ## now, adjust pixels which are younger than oldest fires upward
    earliestFireYear <- as.integer(minValue(fireYear))
    minNonDisturbedAge2001 <- 2001L - earliestFireYear

    toChange2001 <- is.na(fireYear[]) & standAgeMap2001[] <= minNonDisturbedAge2001
    standAgeMap2001[toChange2001] <- minNonDisturbedAge2001 + 2L ## make it an even 40 years old instead of 39
    imputedPixID2001 <- which(toChange2001)
    attr(standAgeMap2001, "imputedPixID") <- unique(attr(standAgeMap2001, "imputedPixID"), imputedPixID2001)

    minNonDisturbedAge2011 <- 2011L - earliestFireYear
    toChange2011 <- is.na(fireYear[]) & standAgeMap2011[] <= minNonDisturbedAge2011
    standAgeMap2011[toChange2011] <- minNonDisturbedAge2011 + 2L ## make it an even 50 years old instead of 49
    imputedPixID2011 <- which(toChange2011)
    attr(standAgeMap2011, "imputedPixID") <- unique(attr(standAgeMap2011, "imputedPixID"), imputedPixID2011)

    ## TODO: compare kNN ages (adjusted with fire data) to the LCC_FN classes considered recently disturbed (9:10)
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
        # if (P(sim)$.resolution == 125L) {
        #   standAgeMapFRI <- prepInputs(url = "https://drive.google.com/file/d/1l0_tx4_fwFZ5RExspBr08ETQDtr0HrXr",
        #                                destinationPath = dPath, filename2 = "age_fri_rof_125m.tif",
        #                                fun = "raster::raster", method = "bilinear", datatype = "INT2U",
        #                                rasterToMatch = sim$rasterToMatchLarge,
        #                                userTags = c("standAgeMapFRI", studyAreaName, currentModule(sim)))
        # } else if (P(sim)$.resolution == 250L) {
        #   standAgeMapFRI <- prepInputs(url = "https://drive.google.com/file/d/1AIjLN9V80ln23hr_ECsEqWkcP0UNQetl",
        #                                destinationPath = dPath, filename2 = "age_fri_rof_250m.tif",
        #                                fun = "raster::raster", method = "bilinear", datatype = "INT2U",
        #                                rasterToMatch = sim$rasterToMatchLarge,
        #                                userTags = c("standAgeMapFRI", studyAreaName, currentModule(sim)))
        # }
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
  } else {
    ## NOTE: this new layer is bad:
    ##   - modage1: fairly uniform ~100 yrs; but at least it predicts entire ROF area;
    ##   - modage2: large areas w/o preds (will need to be rerun with a different bam model);
    ##              HUDSON PLAINS ecozone predictions look fine, but BOREAL SHIELD ones are garbage.
    ##              so, for now we will only use the predicted ages for the HUDSON PLAINS, overlaying on kNN-adj-ages
    modageMap <- prepInputs(
      url = "https://drive.google.com/file/d/1bfT5gUonIHVDAbgYIsDo2gE1qAUbMoli/", ## modage1,
      #url = "https://drive.google.com/file/d/139jxdQbzKjcUWLe87Ockqs8sMhsB_Jjz/", ## modage2
      fun = "raster::raster",
      destinationPath = dPath,
      rasterToMatch = sim$rasterToMatchLarge
    )
    imputedPixID <- which(!is.na(modageMap[])) ## all pixels imputed

    ## since Raquel's age layer is "2015", subtract 4 years to make it 2011; subtract 14 for 2001?
    standAgeMap2001 <- modageMap - 14L
    attr(standAgeMap2001, "imputedPixID") <- imputedPixID

    standAgeMap2011 <- modageMap - 4L
    attr(standAgeMap2011, "imputedPixID") <- imputedPixID
  }

  sim$standAgeMap2001 <- asInteger(standAgeMap2001)
  sim$standAgeMap2011 <- asInteger(standAgeMap2011)

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
