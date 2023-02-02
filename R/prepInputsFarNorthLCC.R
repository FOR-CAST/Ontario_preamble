#'
#' @param dPath the directory to download the raw data
#'
#' @return the FarNorth Land Cover Classification as a single raster
#'
#' @author Alex Chubaty
#' @export
#' @importFrom googledrive drive_download
#' @importFrom raster raster
#' @importFrom archive archive_extract
#' @importFrom terra rast union ext mosaic project res crs
prepInputsFarNorthLCC <- function(dPath) {
  ff <- file.path(dPath, "FarNorth_LandCover_Class_UTM17_mosaic.tif")
  if (!file.exists(ff)) {
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

    r15 <- rast(file.path(dPath, "FarNorthLandCover", basename(rFiles))[1])
    r16 <- rast(file.path(dPath, "FarNorthLandCover", basename(rFiles))[2])
    r17 <- rast(file.path(dPath, "FarNorthLandCover", basename(rFiles))[3])

    e15 <- ext(r15)
    e16 <- ext(r16)
    e17 <- ext(r17)
    u <- terra::union(e15, e16) |> terra::union(x = _, e17)
    template <- rast(u, res = res(r16),  crs = crs(r16))

    tempExtent1 <- tempfile(fileext = ".tif")
    tempExtent2 <- tempfile(fileext = ".tif")
    tempExtent3 <- tempfile(fileext = ".tif")


    f <- file.path(dPath, "FarNorthLandCover", paste0("FarNorth_LandCover_Class_UTM", 15:17, ".tif"))
    t15 <- rast(f[1]) |>  project(x = _, template, align = TRUE, filename = tempExtent2)
    t16 <- rast(f[2]) |>  project(x = _, template, align = TRUE, filename = tempExtent2)
    t17 <- rast(f[3]) |> project(x = _, template, align = TRUE, filename = tempExtent3)

    r15 <- rast(f[1]) |> project(x = _, t15, method = "ngb", datatype = "INT2U", )
    r16 <- rast(f[2]) |> project(x = _, t16, method = "ngb", datatype = "INT2U")
    r17 <- rast(f[3]) |> project(x = _, t17, method = "ngb", datatype = "INT2U")

    LCC_FN <- terra::mosaic(r15, r16, r17, fun = min, filename = ff) # overwrite = TRUE
    LCC_FN <- raster::raster(ff)

    unlink(c(tempExtent1, tempExtent2, tempExtent3))
    gc()
  } else {
    LCC_FN <- raster::raster(ff)
  }

  return(LCC_FN)
}
