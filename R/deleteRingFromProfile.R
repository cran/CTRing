#' Delete ring from a pith to bark profile
#'
#' @param n Number of rings to remove
#' @param densProfile Density profile
#'
#' @return Corrected density profile with ring(s) removed and red bar in plot of deleted ring
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
#' densPath <- extractProfile(im_dens,
#'                            image_info,
#'                            pith_coord,
#'                            endPath,
#'                            k = 2, r = 5,
#'                            threshold = 0.002)
#'
#' plotProfile(densPath)
#' newPath <- addRingFromProfile(n = 1, densPath)
#' oldPath <- deleteRingFromProfile(n = 1, newPath)
deleteRingFromProfile <- function(n = 1, densProfile) {

  message(paste("Select", n, "ring(s) on graph delete"))
  newCoord <- locator(n)

  for (i in c(1:n)){
    newIndex <- min(which.min(abs(densProfile$distRingChange - newCoord$x[i])))
    newDist <- densProfile$distRingChange[newIndex]
    densProfile$ring_limits <- densProfile$ring_limits[-newIndex]
    densProfile$distRingChange <- densProfile$distRingChange[-newIndex]
    abline(v = newDist, col = 'red')
  }
  return(densProfile)
}
