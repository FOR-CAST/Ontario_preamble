#'
#' @param dPath the directory to download the raw data
#'
#' @return the FarNorth Land Cover Classification as a single raster
#'
#' @author Alex Chubaty
#' @export
#' @importFrom googledrive drive_download
#' @importFrom raster raster union extent mosaic projectRaster res crs
#' @importFrom archive archive_extract
prepInputsFarNorthLCC <- function(dPath) {
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
  template <- raster(u, res = res(r16),  crs = crs(r16))

  f <- file.path(dPath, "FarNorthLandCover", paste0("FarNorth_LandCover_Class_UTM", 15:17, ".tif"))
  t15 <- raster(f[1]) %>% projectRaster(., template, alignOnly = TRUE)
  t16 <- raster(f[2]) %>% projectRaster(., template, alignOnly = TRUE)
  t17 <- raster(f[3]) %>% projectRaster(., template, alignOnly = TRUE)

  r15 <- raster(f[1]) %>% projectRaster(., t15, method = "ngb", datatype = "INT2U")
  r16 <- raster(f[2]) %>% projectRaster(., t16, method = "ngb", datatype = "INT2U")
  r17 <- raster(f[3]) %>% projectRaster(., t17, method = "ngb", datatype = "INT2U")

  LCC_FN <- raster::mosaic(r15, r16, r17, fun = min, filename = file.path(dPath, "FarNorth_LandCover_Class_UTM17_mosaic.tif"))
  return(LCC_FN)
}
