#' Add years to series
#'
#' @param lastYear Last year of series
#' @param densProfile Density profile
#'
#' @return Density profile with years
#' @export
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
#' im_dens <- grayToDensity(im_8bit)
#'
#' pith_coord <- detect_pith(im_dens, n_segments = 12, pixel = TRUE, toPlot = FALSE)
#'
#' endPath <- c(472, 284) # manual
#' # not run - endPath <- locatePathEnd(im_dens, pith_coord) # using the image
#'
#' path <- extractProfile(im_dens, image_info, pith_coord, endPath, k = 2, r = 5, threshold = 0.002)
#'
#' path <- addYears(2021, path)
addYears <- function(lastYear, densProfile) {
  densProfile$years <- seq(from = lastYear-length(densProfile$ring_limits),
                           to = lastYear)
  return(densProfile)
}

