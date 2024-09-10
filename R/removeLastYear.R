#' Remove the last year of a profile
#'
#' @param densProfile Density profile
#'
#' @return Density profile with the last year removed
#' @export
#'
#' @examples
#'
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
#' path_last_year_2021 <- addYears(2021, path)
#' path_last_year_2020 <- removeLastYear(path_last_year_2021)
removeLastYear <- function(densProfile) {
  densProfile$xx <- densProfile$xx[-c(max(densProfile$ring_limits):length(densProfile$xx))]
  densProfile$yy <- densProfile$yy[-c(max(densProfile$ring_limits):length(densProfile$yy))]
  densProfile$dens <- densProfile$dens[-c(max(densProfile$ring_limits):length(densProfile$dens))]
  densProfile$distFromPith <- densProfile$distFromPith[-c(max(densProfile$ring_limits):length(densProfile$distFromPith))]
  densProfile$ring_limits <- densProfile$ring_limits[-length(densProfile$ring_limits)]
  densProfile$distRingChange <- densProfile$distRingChange[-length(densProfile$distRingChange)]
  densProfile$transitionType <- densProfile$transitionType[-length(densProfile$transitionType)]


  if ("ew_limits" %in% names(densProfile))
    densProfile$ew_limits <- densProfile$ew_limits[-length(densProfile$ew_limits)]
  if ("avgDens" %in% names(densProfile))
    densProfile$avgDens <- densProfile$avgDens[-length(densProfile$avgDens)]
  if ("avgDensEw" %in% names(densProfile))
    densProfile$avgDensEw <- densProfile$avgDensEw[-length(densProfile$avgDensEw)]
  if ("avgDensLw" %in% names(densProfile))
    densProfile$avgDensLw <- densProfile$avgDensLw[-length(densProfile$avgDensLw)]
  if ("years" %in% names(densProfile))
    densProfile$years <- densProfile$years[-length(densProfile$years)]

  return(densProfile)
}
