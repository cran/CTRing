#' Add ring to pith to bark profile from profile plot
#'
#' @param n Number of rings to add
#' @param densProfile Density profile
#'
#' @return Corrected density profile with new ring(s) added and blue bar in plot of added ring
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
#' plotProfile(path)
#' newPath <- addRingFromProfile(n = 1, path)
#'
addRingFromProfile <- function(n = 1, densProfile) {

  message(paste("Clic", n, "time(s) on graph to position new ring(s)"))
  newCoord <- locator(n)

  for (i in c(1:n)){
    newIndex <- min(which.min(abs(densProfile$distFromPith - newCoord$x[i])))
    newDist <- densProfile$distFromPith[newIndex]
    densProfile$ring_limits <- sort(c(densProfile$ring_limits, newIndex))
    densProfile$distRingChange <- sort(c(densProfile$distRingChange, newDist))
    abline(v = newDist, col = 'blue')
  }
  return(densProfile)
}
