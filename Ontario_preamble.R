defineModule(sim, list(
  name = "Ontario_preamble",
  description = paste("this module prepares 2 sets of objects needed for LandR simulations in Ontario AOU/ROF:",
                      "1. study areas and corresponding rasterToMatch (as well as large versions);",
                      "2. species equivalencies tables and the sppEquiv column.",
                      "Each is customized to the study area parameter passed via `studyAreaName`."),
  keywords = "",
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre")),
    person("Ian", "Eddy", email = "ian.eddy@nrcan-rncan.gc.ca", role = "ctb")
  ),
  childModules = character(0),
  version = list(Ontario_preamble = "1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.md", "Ontario_preamble.Rmd")),
  reqdPkgs = list("archive", "geodata", "httr", "sf", "terra",
                  "PredictiveEcology/reproducible@development (>= 2.0.8)",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9067)"),
  parameters = rbind(
    defineParameter("studyAreaName", "character", "ON_AOU_6.2", NA, NA,
                    paste("Should include one of 'ON_AOU' or 'ON_ROF' to identify the study area.",
                          "May also include the ecoprovince ID in which to run the model.",
                          "Optionally, if 'ROF', then 'shield' or 'plain' can be specified too,",
                          "to identify whether to run only in the Boreal Shield or Hudson Plains ecozone.")),
    defineParameter("useAgeMapkNN", "logical", FALSE, NA, NA,
                    paste("if TRUE, use kNN age maps, corrected with fire polygons data;",
                          "if FALSE, use Raquel's predicted age map from ROF_age.")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".resolution", "numeric", 250, NA, NA,
                    "raster pixel size, in m,  to use for simulation. Either 250 or 125."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    paste("Should this entire module be run with caching activated?",
                          "This is generally intended for data-type modules, where stochasticity",
                          "and time are not relevant"))
  ),
  inputObjects = bindrows(
    expectsInput("targetCRS", "character", desc = "Geospatial projection to use.", sourceURL = NA)
  ),
  outputObjects = bindrows(
    createsOutput("ageMap", "SpatRaster", desc = "Age (time since disturbance) map, derived from national kNN product and ON FRI data."),
    createsOutput("fireSenseForestedLCC", "integer", desc = "vector of LCC classes considered to be forested by fireSense."),
    createsOutput("flammableRTM", "SpatRaster", desc = "RTM without ice/rocks/urban/water. Flammable map with 0 and 1."),
    createsOutput("LCC", "SpatRaster", desc = "Land cover classification map, derived from national LCC 2005 product and ON FRI data."),
    createsOutput("missingLCCgroup", "character", "the group in `nonForestLCCGroups` that describes forested pixels omitted by LandR"),
    createsOutput("nonflammableLCC", "integer", desc = "vector of LCC classes considered to be non-flammable"),
    createsOutput("nonForestLCCGroups", "list",desc = "named list of non-forested landcover groups for fireSense"),
    createsOutput("nontreeClasses", "integer", desc = "vector of LCC classes considered to be non-forested/treed."), ## TODO what is this used for?
    createsOutput("nonTreePixels", "integer", desc = "pixel indices indicating non-treed pixels"), ## TODO: what is this used for?
    createsOutput("rasterToMatch", "SpatRaster", desc = "Raster to match, based on study area."),
    createsOutput("rasterToMatchLarge", "SpatRaster", desc = "Raster to match (large) based on `studyAreaLarge.`"),
    createsOutput("rasterToMatchReporting", "SpatRaster", desc = "Raster to match based on `studyAreaReporting.`"),
    createsOutput("speciesTable", "data.table", desc = "Species parameter table."),
    createsOutput("sppColorVect", "character", desc = "Species colour vector."),
    createsOutput("sppEquiv", "data.table", desc = "Species equivalency table."),
    createsOutput("sppEquivCol", "character", desc = "name of column to use in `sppEquiv`."),
    createsOutput("standAgeMap2001", "SpatRaster", desc = "raster of time since disurbance for year 2001."),
    createsOutput("standAgeMap2011", "SpatRaster", desc = "raster of time since disurbance for year 2011."),
    createsOutput("studyArea", "sf", desc = "Buffered study area in which to run simulations."),
    createsOutput("studyAreaLarge", "sf", desc = "Buffered study area used for parameterization/calibration."),
    createsOutput("studyAreaPSP", "sf",
                  paste("this area will be used to subset PSP plots before building the statistical model.",
                        "Currently PSP datasets with repeat measures exist only for Saskatchewan,",
                        "Alberta, Boreal British Columbia, and Ontario.")),
    createsOutput("studyAreaReporting", "sf", desc = "Unbuffered study area used for reporting/post-processing.")
  )
))

## event types
#   - type `init` is required for initialization

doEvent.Ontario_preamble = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # do stuff for this event
      fullStudyAreaName <- P(sim)$studyAreaName
      if (isFALSE(grepl("^ON_", fullStudyAreaName))) {
        fullStudyAreaName <- paste0("ON_", fullStudyAreaName)
      }
      mod$studyAreaNameShort <- gsub("^ON_(AOU|ROF_plain|ROF_shield|ROF).*", "\\1", fullStudyAreaName)

      chunks <- strsplit(fullStudyAreaName, "_")[[1]]
      mod$ecoprov <- chunks[which(!is.na(suppressWarnings(as.integer(chunks))))] ## keep as character
      mod$ecoprov <- if (length(mod$ecoprov) > 0) mod$ecoprov else NULL

      sim <- InitSpecies(sim)
      sim <- InitStudyAreaRTM(sim)
      sim <- InitStudyAreaLCC(sim)
      sim <- InitAge(sim)

      ## check that rasters all match
      compareGeom(sim$rasterToMatchLarge, sim$LCC, sim$standAgeMap2001, sim$standAgeMap2011)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "Ontario_preamble", "plot", .last())
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
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

InitSpecies <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  cacheTags <- c(P(sim)$studyAreaName, currentModule(sim))
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")

  ## SPECIES STUFF
  sim$sppEquiv <- makeSppEquivON()
  sim$sppEquivCol <- "ON"

  sim$sppColorVect <- sppColors(sim$sppEquiv, sim$sppEquivCol, newVals = "Mixed", palette = "Accent")

  sim$speciesTable <- getSpeciesTable(dPath = dPath) ## uses default URL

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

InitStudyAreaRTM <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  cacheTags <- c(P(sim)$runName, currentModule(sim))
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")

  stopifnot(P(sim)$.resolution %in% c(125, 250))

  ## provincial boundary
  mod$ON <- geodata::gadm(country = "CAN", level = 1, path = dPath) |>
    sf::st_as_sf() |>
    subset(x = _, NAME_1 == "Ontario") |>
    sf::st_transform(sim$targetCRS)

  ## ECOPROVINCES
  ecoprov <- prepInputs(
    url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/province/ecoprovince_shp.zip",
    targetFile = "ecoprovinces.shp",
    alsoExtract = "similar",
    fun = "sf::st_read",
    purge = TRUE,
    destinationPath = dPath
  ) |>
    subset(x = _, ECOPROVINC %in% mod$ecoprov) |>
    st_transform(crs = sim$targetCRS)

  ## ECOZONES
  ez <- switch(mod$studyAreaNameShort,
               ROF = c("BOREAL SHIELD", "HUDSON PLAIN"),
               ROF_shield = "BOREAL SHIELD",
               ROF_plain = "HUDSON PLAIN",
               NULL)

  if (!is.null(ez)) {
    ecozones <- prepInputs(
      url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
      targetFile = "ecozones.shp",
      alsoExtract = "similar",
      fun = "sf::st_read",
      destinationPath = dPath
    ) |>
      st_transform(crs = sim$targetCRS)
    ecozones[["ZONE_NAME"]] <- toupper(ecozones[["ZONE_NAME"]])
    ecozone <- ecozones[ecozones$ZONE_NAME %in% ez, ]
    rm(ecozones)
  } else {
    ecozone <- NULL
  }

  ## STUDY AREA
  if (mod$studyAreaNameShort == "AOU") {
    studyAreaReporting <- prepInputs(
      url = "https://drive.google.com/file/d/1Idtreoo51hGBdfJXp0BmN83bOkKHTXvk",
      destinationPath = dPath,
      targetFile = "CEON_def.shp",
      alsoExtract = "similar",
      fun = "sf::st_read",
      overwrite = TRUE,
      team_drive = TRUE
    ) |>
      st_transform(crs = sim$targetCRS)

    if (is.null(ecoprov)) {
      studyArea <- st_buffer(studyAreaReporting, 20000)
    } else {
      studyAreaReporting <- st_intersection(studyAreaReporting, ecoprov)
      studyArea <- st_buffer(studyAreaReporting, 20000) |>
        st_union() |>
        st_convex_hull() |>
        st_as_sf()
    }

    studyAreaLarge <- prepInputs(
      url = "https://drive.google.com/file/d/1ngQshBgoyLjkjuXnloXPg7IUMdLmppkB",
      destinationPath = dPath,
      targetFile = "CEON_def_50km_buff.shp",
      alsoExtract = "similar",
      fun = "sf::st_read",
      overwrite = TRUE,
      team_drive = TRUE
    ) |>
      st_transform(crs = sim$targetCRS)

    if (!is.null(ecoprov)) {
      studyAreaLarge <- st_intersection(studyAreaLarge, ecoprov) |>
        st_buffer(50000) |>
        st_union() |>
        st_convex_hull() |>
        nngeo::st_remove_holes() |>
        st_as_sf()
    }
  } else if (grepl("ROF", mod$studyAreaNameShort)) {
    studyAreaReporting <- prepInputs(
      url = "https://drive.google.com/file/d/1DzVRglqJNvZA8NZZ7XKe3-6Q5f8tlydQ",
      targetFile = "ROF_RA_def.shp", alsoExtract = "similar",
      fun = "sf::st_read", destinationPath = dPath,
      filename2 = "ROF_RA_def", overwrite = TRUE
    ) |>
      st_transform(crs = sim$targetCRS) |>
      st_intersection(y = ecozone)

    studyArea <- st_buffer(studyAreaReporting, 20000) ## 20 km buffer

    studyAreaLarge <- studyArea ## TODO: temporary workaround downstream raster mismatches
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

  sim$studyArea <- studyArea
  sim$studyAreaLarge <- studyAreaLarge
  sim$studyAreaReporting <- studyAreaReporting

  ## RASTERS TO MATCH
  sim$rasterToMatch <- Cache(LandR::prepInputsLCC,
                             year = 2005,
                             studyArea = sim$studyArea,
                             destinationPath = dPath,
                             useCache = P(sim)$.useCache,
                             filename2 = NULL)

  sim$rasterToMatchLarge <- Cache(LandR::prepInputsLCC,
                                  year = 2005,
                                  studyArea = sim$studyAreaLarge,
                                  destinationPath = dPath,
                                  useCache = P(sim)$.useCache,
                                  filename2 = NULL)

  sim$rasterToMatchReporting <- Cache(LandR::prepInputsLCC,
                                      year = 2005,
                                      studyArea = sim$studyAreaReporting,
                                      destinationPath = dPath,
                                      useCache = P(sim)$.useCache,
                                      filename2 = NULL)

  if (P(sim)$.resolution == 125L) {
    sim$rasterToMatch <- terra::disagg(sim$rasterToMatch, fact = 2)
    sim$rasterToMatchLarge <- terra::disagg(sim$rasterToMatchLarge, fact = 2)
    sim$rasterToMatchReporting <- terra::disagg(sim$rasterToMatchReporting, fact = 2)
  }

  writeRaster(sim$rasterToMatch, file.path(dPath, paste0(P(sim)$studyAreaName, "_rtm.tif")),
              datatype = "INT1U", overwrite = TRUE)
  writeRaster(sim$rasterToMatchLarge,  file.path(dPath, paste0(P(sim)$studyAreaName, "_rtml.tif")),
              datatype = "INT1U", overwrite = TRUE)
  writeRaster(sim$rasterToMatchReporting,  file.path(dPath, paste0(P(sim)$studyAreaName, "_rtmr.tif")),
              datatype = "INT1U", overwrite = TRUE)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

InitStudyAreaLCC <- function(sim) {
  cacheTags <- c(P(sim)$runName, currentModule(sim))
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  allClasses <- if (grepl("ROF", mod$studyAreaNameShort)) {
    c(1:18, 21:24) ## classes 19 and 20 reclassified
  } else {
    1:39
  }

  ## LANDCOVER MAPS (LCC2005 + FRI if AOU; Far North if ROF)
  if (mod$studyAreaNameShort == "AOU") {
    LCC2005 <- prepInputsLCC(year = 2005, ## TODO: use LCC2010
                             to = sim$rasterToMatchLarge,
                             destinationPath = dPath)

    uniqueLCCclasses <- na.omit(unique(as.vector(values(LCC2005))))

    treeClassesLCC <- c(1:15, 20, 32, 34:35)
    nontreeClassesLCC <- (1:39)[!(1:39 %in% treeClassesLCC)]

    LCC_FRI <- prepInputs(url = "https://drive.google.com/file/d/1eg9yhkAKDsQ8VO5Nx4QjBg4yiB0qyqng",
                          destinationPath = dPath,
                          filename1 = "lcc_fri_ceon_250m.tif",
                          filename2 = paste0("lcc_fri_ceon_", P(sim)$studyAreaName, "_250m.tif"),
                          fun = "terra::rast",
                          method = "near",
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
    treePixelsFRI_TF <- as.vector(values(LCC_FRI)) %in% treeClassesFRI
    LandTypeFRI_NA <- is.na(as.vector(values(LCC_FRI)))
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

    treePixelsLCC <- which(as.vector(values(sim$LCC)) %in% treeClassesLCC) ## c(1:15, 20, 32, 34:35)
    nonTreePixels <- which(as.vector(values(sim$LCC)) %in% nontreeClassesLCC)

    fireSenseForestedLCC <- LandRforestedLCC <- treeClassesLCC
    sim$nonForestClasses <- nontreeClassesLCC

    nonflammableLCC  <- c(0, 25, 30, 33, 36:39)
    nonForestLCCGroups <- list(
      nonForest_highFlam = c(16:19, 22),
      nonForest_lowFlam = c(21, 23:24, 26:29, 31)
    )
    sim$missingLCCGroup <- "nonForest_highFlam"
  } else if (grepl("ROF", mod$studyAreaNameShort)) {
    ## FAR NORTH LANDCOVER (620 MB)
    ## unable to download directly b/c of SSL, time outs, and other server problems
    ##   https://ws.gisetl.lrc.gov.on.ca/fmedatadownload/Packages/FarNorthLandCover.zip
    ##

    LCC_FN <- Cache(prepInputsFarNorthLCC, dPath = dPath)
    LCC_FN[as.vector(values(LCC_FN)) <= 0] <- NA_integer_ ## remove 0, -9, and -99
    LCC_FN[as.vector(values(LCC_FN)) > 24] <- NA_integer_ ## remove >24

    LCC_FN <- Cache(
      postProcess,
      LCC_FN,
      method = "ngb",
      to = sim$rasterToMatchLarge,
      destinationPath = dPath, #this isn't working
      filename2 = file.path(dPath, paste0("FarNorth_LandCover_Class_", P(sim)$studyAreaName, ".tif"))
    )
    #dPath ignored?

    ## LandR forest classes are distinct from fireSense forest classes, in that fireSense assesses
    ## forest by the dominant species composition (i.e. fuel class) and not the landcover.
    ## however, fire may be strongly influenced by landcover (e.g., wetland) therefore
    ## fireSense forest classes are restricted to upland forest classes, unlike LandR
    ## Here we reclassify disturbed to it's nearest valid (i.e. flammable) class using an expanding
    ## focal window.
    ## If no suitable pixel is found in the neighbouring 3-pixel locus, coniferous forest class is
    ## assigned by default.
    ## this would mean the pixel is at the center of a (7*125m^2) = 75 ha patch)

    ## Far North cover classes described in `Far North Land Cover - Data Specification.pdf`
    # "Clear Open Water" = 1, "Turbid Water" = 2, "Intertidal Mudflat" = 3,
    # "Intertidal Marsh" = 4, "Supratidal Marsh" = 5, "Fresh Water Marsh" = 6,
    # "Heath" = 7, "Thicket Swamp" = 8, "Coniferous Swamp" = 9, "Deciduous Swamp" = 10,
    # "Open Fen" = 11, "Treed Fen" = 12, "Open Bog" = 13, "Treed Bog" = 14,
    # "Sparse Treed" = 15, "Deciduous Treed" = 16, "Mixed Treed" = 17, "Coniferous Treed" = 18,
    # "Disturbance - Non and sparse-woody" = 19, "Disturbance - Treed or shrub" = 20,
    # "Sand/Gravel/Mine Tailings" = 21, "Bedrock" = 22, "Community/Infrastructure" = 23,
    # "Agriculture" = 24, "Cloud/Shadow" = -9, "Other" = -99

    nIterations <- ifelse(grepl("ROF_shield", P(sim)$studyAreaName), 3, 3) ## TODO: revisit this
    LCC_FN <- Cache(LandR::convertUnwantedLCC2,
                    classesToReplace = 19:20,
                    rstLCC = LCC_FN,
                    nIterations = nIterations,
                    defaultNewValue = 18,
                    invalidClasses = c(1:5, 21:24),
                    userTags = c("convertUnwantedLCC2", studyAreaName))

    sim$LCC <- LCC_FN
    nontreeClassesLCC <- c(1:8, 11, 13, 21:24)
    LandRforestedLCC <- c(9:10, 12, 14, 15:18)

    treePixelsFN_TF <- as.vector(values(LCC_FN)) %in% LandRforestedLCC
    LandTypeFN_NA <- is.na(as.vector(values(LCC_FN)))
    noDataPixelsFN <- LandTypeFN_NA
    treePixelsCC <- which(treePixelsFN_TF)

    treePixelsLCC <- which(as.vector(values(sim$LCC)) %in% LandRforestedLCC)
    nonTreePixels <- which(as.vector(values(sim$LCC)) %in% nontreeClassesLCC)

    fireSenseForestedLCC <- c(15:18)
    nonflammableLCC <- c(1:6, 21:23, 24) ## TODO: reassess agriculture class 24
    nonForestLCCGroups <- list(
      "FenPlus" = c(7:8, 10:12),
      "BogSwamp" = c(9, 13:14)
    )
    sim$missingLCCGroup <- "BogSwamp"
  }

  sim$LCC <- setValues(sim$LCC, asInteger(as.vector(values(sim$LCC))))
  sim$LandRforestedLCC <- LandRforestedLCC
  sim$fireSenseForestedLCC <- fireSenseForestedLCC
  sim$nonForestLCCGroups <- nonForestLCCGroups
  sim$nonflammableLCC <- nonflammableLCC

  sim$nonTreePixels <- nonTreePixels
  sim$treeClasses <- sim$LandRforestedLCC
  sim$nontreeClasses <- nontreeClassesLCC
  sim$flammableRTM <- defineFlammable(crop(sim$LCC, sim$rasterToMatch),
                                      nonFlammClasses = sim$nonflammableLCC,
                                      mask = sim$rasterToMatch)

  # check that all LCC classes accounted for in forest, nonForest, and non flamm classes for fS
  fS_classes <- sort(unique(c(sim$fireSenseForestedLCC, unlist(sim$nonForestLCCGroups), sim$nonflammableLCC)))
  if (!all(allClasses %in% fS_classes)) {
    stop("Some LCCs not accounted for:\n",
         "Expected: ", allClasses, "\n",
         "Assigned to fireSense classes: ", fS_classes)
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

InitAge <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  cacheTags <- c(P(sim)$runName, currentModule(sim))
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath\n '", dPath, "'.")

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
                      destinationPath = dPath,
                      rasterToMatch = sim$rasterToMatchLarge)

    fireYear <- postProcess(fireYear, to = sim$rasterToMatchLarge) ## needed cropping

    standAgeMap2001 <- Cache(
      LandR::prepInputsStandAgeMap,
      ageURL = standAgeMapURL,
      ageFun = "terra::rast",
      rasterToMatch = sim$rasterToMatchLarge,
      studyArea = sim$studyAreaLarge,
      destinationPath = dPath,
      startTime = 2001,
      fireFun = "terra::vect",
      fireURL = fireURL,
      filename2 = .suffix("standAgeMap_2001.tif", paste0("_", P(sim)$studyAreaName)),
      userTags = c("stable", currentModule(sim), P(sim)$studyAreaName)
    )

    standAgeMap2011 <- Cache(
      LandR::prepInputsStandAgeMap,
      ageURL = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                      "canada-forests-attributes_attributs-forests-canada/",
                      "2011-attributes_attributs-2011/",
                      "NFI_MODIS250m_2011_kNN_Structure_Stand_Age_v1.tif"),
      ageFun = "terra::rast",
      rasterToMatch = sim$rasterToMatchLarge,
      studyArea = sim$studyAreaLarge,
      destinationPath = dPath,
      startTime = 2011,
      fireFun = "terra::vect",
      fireURL = fireURL,
      filename2 = .suffix("standAgeMap_2011.tif", paste0("_", P(sim)$studyAreaName)),
      userTags = c("stable", currentModule(sim), P(sim)$studyAreaName)
    )

    ## stand age maps already adjusted within fire polygons using LandR::prepInputsStandAgeMap.
    ## now, adjust pixels which are younger than oldest fires upward
    earliestFireYear <- as.integer(minmax(fireYear)[1])

    minNonDisturbedAge2001 <- 2001L - earliestFireYear
    toChange2001 <- is.na(as.vector(values(fireYear))) &
      as.vector(values(standAgeMap2001)) <= minNonDisturbedAge2001
    standAgeMap2001[toChange2001] <- minNonDisturbedAge2001 + 2L ## make it an even 40 years old instead of 39
    imputedPixID2001 <- unique(attr(standAgeMap2001, "imputedPixID"), which(toChange2001))

    minNonDisturbedAge2011 <- 2011L - earliestFireYear
    toChange2011 <- is.na(as.vector(values(fireYear))) &
      as.vector(values(standAgeMap2011)) <= minNonDisturbedAge2011
    standAgeMap2011[toChange2011] <- minNonDisturbedAge2011 + 2L ## make it an even 50 years old instead of 49
    imputedPixID2011 <- unique(attr(standAgeMap2011, "imputedPixID"), which(toChange2011))

    ## overlay age from FRI. These are assembled from multiple years, so will adjust ages accordingly.
    if (mod$studyAreaNameShort == "AOU") {
      standAgeMapFRI <- prepInputs(
        url = "https://drive.google.com/file/d/1NGGUQi-Un6JGjV6HdIkGzjPd1znHFvBi",
        destinationPath = dPath,
        filename1 = "age_fri_ceon_250m.tif",
        filename2 = paste0("age_fri_ceon_", P(sim)$studyAreaName, "_250m.tif"),
        fun = "terra::rast", method = "bilinear", datatype = "INT2U",
        to = sim$rasterToMatchLarge,
        userTags = c("standAgeMapFRI", P(sim)$studyAreaName, currentModule(sim))
      )
      refYearMapFRI <- prepInputs(
        url = "https://drive.google.com/file/d/1hl3NDAdA9qcLMUlVrWvszdmHy48d2L1O",
        destinationPath = dPath,
        filename1 = "reference_year_fri_ceon_250m.tif",
        filename2 = paste0("reference_year_fri_ceon_", P(sim)$studyAreaName, "_250m.tif"),
        fun = "terra::rast", method = "bilinear", datatype = "INT2U",
        to = sim$rasterToMatchLarge,
        userTags = c("refYearMapFRI", P(sim)$studyAreaName, currentModule(sim))
      )
    } else if (mod$studyAreaNameShort == "ROF") {
      ## TODO: compare kNN ages (adjusted with fire data) to the LCC_FN classes considered recently disturbed (9:10)

      if (P(sim)$.resolution == 125L) {
        standAgeMapFRI <- prepInputs(
          url = "https://drive.google.com/file/d/1l0_tx4_fwFZ5RExspBr08ETQDtr0HrXr",
          destinationPath = dPath,
          filename1 = "age_fri_rof_125m.tif",
          filename2 = paste0("age_fri_rof_", P(sim)$studyAreaName, "_125m.tif"),
          fun = "terra::rast", method = "bilinear", datatype = "INT2U",
          to = sim$rasterToMatchLarge,
          userTags = c("standAgeMapFRI", P(sim)$studyAreaName, currentModule(sim))
        )
        refYearMapFRI <- prepInputs(
          url = "https://drive.google.com/file/d/1ApBDUW4nei6pl19IQh5pe8UfalTH4xgs",
          destinationPath = dPath,
          filename1 = "reference_year_fri_rof_125m.tif",
          filename2 = paste0("reference_year_fri_rof_", P(sim)$studyAreaName, "_125m.tif"),
          fun = "terra::rast", method = "bilinear", datatype = "INT2U",
          to = sim$rasterToMatchLarge,
          userTags = c("refYearMapFRI", P(sim)$studyAreaName, currentModule(sim))
        )
      } else if (P(sim)$.resolution == 250L) {
        standAgeMapFRI <- prepInputs(
          url = "https://drive.google.com/file/d/1AIjLN9V80ln23hr_ECsEqWkcP0UNQetl",
          destinationPath = dPath,
          filename1 = "age_fri_rof_250m.tif",
          filename2 = paste0("age_fri_rof_", P(sim)$studyAreaName, "_250m.tif"),
          fun = "terra::rast", method = "bilinear", datatype = "INT2U",
          to = sim$rasterToMatchLarge,
          userTags = c("standAgeMapFRI", P(sim)$studyAreaName, currentModule(sim))
        )
        refYearMapFRI <- prepInputs(
          url = "https://drive.google.com/file/d/1OaioZX4ZEVJPfeqg9BNbl9N1avHFY82F",
          destinationPath = dPath,
          filename1 = "reference_year_fri_rof_250m.tif",
          filename2 = paste0("reference_year_fri_rof_", P(sim)$studyAreaName, "_250m.tif"),
          fun = "terra::rast", method = "bilinear", datatype = "INT2U",
          to = sim$rasterToMatchLarge,
          userTags = c("refYearMapFRI", P(sim)$studyAreaName, currentModule(sim))
        )
      }
      standAgeMapFRI <- setMinMax(standAgeMapFRI)
      standAgeMapFRI[as.vector(values(standAgeMapFRI)) < 0] <- 0L

      #   1. adjust each age by reference year, for 2001 and 2011 to get age in 2001 and 2011
      standAgeMapFRI2001 <- standAgeMapFRI - (refYearMapFRI - 2001L)
      standAgeMapFRI2011 <- standAgeMapFRI - (refYearMapFRI - 2011L)

      #   2. any values < 0, fall back to kNN values for these
      noDataPixelsFRI2001 <- which(as.vector(values(standAgeMapFRI2001)) < 0)
      standAgeMapFRI2001[noDataPixelsFRI2001] <- standAgeMap2001[noDataPixelsFRI2001]
      standAgeMap2001 <- standAgeMapFRI2001
      imputedPixID2001 <- unique(imputedPixID2001, noDataPixelsFRI2001)

      noDataPixelsFRI2011 <- which(as.vector(values(standAgeMapFRI2011)) < 0)
      standAgeMapFRI2011[noDataPixelsFRI2011] <- standAgeMap2011[noDataPixelsFRI2011]
      standAgeMap2011 <- standAgeMapFRI2011
      imputedPixID2011 <- unique(imputedPixID2011, noDataPixelsFRI2011)
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
      fun = "terra::rast",
      destinationPath = dPath,
      rasterToMatch = sim$rasterToMatchLarge
    )
    modageMap <- as.int(modageMap)
    imputedPixID <- which(!is.na(as.vector(values(modageMap)))) ## all pixels imputed

    ## TODO: it doesn't look like Raquel's layer properly accounts for wildfire raster,
    ## so we will manually adjust it here. But the age map needs to be fixed in ROF_age and rebuilt.
    wildfires <- Cache(
      prepInputs,
      url = "https://drive.google.com/file/d/1WcxdP-wyHBCnBO7xYGZ0qKgmvqvOzmxp/",
      targetFile = "wildfire_ROF.tif",
      destinationPath = dPath,
      rasterToMatch = sim$rasterToMatch
    )
    wildfires <- wildfires2001 <- wildfires2011 <- as.int(wildfires)
    wildfires2001[as.vector(values(wildfires)) > 2001] <- NA
    wildfires2011[as.vector(values(wildfires)) > 2011] <- NA
    tsf2001 <- 2001L - wildfires2001
    tsf2011 <- 2011L - wildfires2011

    ## since Raquel's age layer is "2015", subtract 4 years to make it 2011; subtract 14 for 2001.
    standAgeMap2001 <- modageMap - 14L - tsf2001
    imputedPixID2001 <- imputedPixID

    standAgeMap2011 <- modageMap - 4L - tsf2011
    imputedPixID2011 <- imputedPixID
  }

  sim$standAgeMap2001 <- as.int(standAgeMap2001)
  attr(sim$standAgeMap2001, "imputedPixID") <- imputedPixID2001

  sim$standAgeMap2011 <- as.int(standAgeMap2011)
  attr(sim$standAgeMap2011, "imputedPixID") <- imputedPixID2011

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
