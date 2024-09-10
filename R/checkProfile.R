#' Verify position of ring transitions of a density profile
#'
#' @param profile_with_borders xRing profile with transitions between rings located
#' @param totRings Total number of rings of the disk
#'
#' @return xRing profile with corrected ring location
#' @export
#'
#' @import xRing
#'
#' @examples
#' library(oro.dicom)
#' file_path <- system.file("extdata", "disk.dcm", package = "CTRing")
#' dcm <-  readDICOM(file_path)
#' hdr_df <- dcm$hdr[[1]]
#' image_info <- getImageInfo(hdr = hdr_df)
#'
#' im <- imageToMatrix(dcm$img)
#' im_8bit <- xBitTo8Bit(im, image_info$grayScale)
#' image_info <- getImageInfo(hdr = hdr_df)
#' im_dens <- grayToDensity(im_8bit)
#'
#' pith_coord <- detect_pith(im_dens,
#'                           n_segments = 12,
#'                           pixel = TRUE,
#'                           toPlot = FALSE)
#'
#' endPath <- c(472, 284)
#'
#' densPath <- extractProfile(im_dens,
#'                            image_info,
#'                            pith_coord,
#'                            endPath,
#'                            k = 2, r = 5,
#'                            threshold = 0.002)
#'
#' newPath <- checkProfile(densPath, 26)
checkProfile <- function(profile_with_borders, totRings) {
  numRingsFound <- length(profile_with_borders$ring_limits)
  deltaRingNumber <- totRings - numRingsFound

  # if no rings are missing
  if (deltaRingNumber == 0 ){
    message("Check profile")
    plotProfile(profile_with_borders)
  }

  # if rings are missing
  if (deltaRingNumber > 0 ){
    plotProfile(profile_with_borders)
    profile_with_borders <- addRingFromProfile(n = deltaRingNumber, profile_with_borders)
  }

  # if too many rings
  if (deltaRingNumber < 0 ){
    plotProfile(profile_with_borders)
    profile_with_borders <- deleteRingFromProfile(n = abs(deltaRingNumber), profile_with_borders)
  }

  return(profile_with_borders)
}
