#' Add ring to pith to bark profile from CT scan image
#'
#' @param n Number of rings to add
#' @param densProfile Density profile
#' @param im Density matrix
#'
#' @return Corrected density profile with new ring(s) added and blue bar in plot of added ring
#' @export
#'
#' @import oro.dicom
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
#' newPath2 <- addRingFromImage(n = 1, densPath, im_dens)
#'
addRingFromImage <- function(n = 1, densProfile, im) {
  plotImageProfile(densProfile, im)
  segXY <- data.frame(x = densProfile$xx,
                      y = densProfile$yy)

  message(paste("Clic", n, "time(s) on graph to position new ring(s)"))
  newCoord <- locator(n)
  newCoord$x <- newCoord$x * dim(im)[1]
  newCoord$y <- newCoord$y * dim(im)[2]

  for (i in 1:n){
    segXY$dist <- sqrt((segXY$x - newCoord$x[i])^2 + (segXY$y - newCoord$y[i])^2)
    newIndex <- which.min(segXY$dist)
    newDist <- densProfile$distFromPith[newIndex]
    densProfile$ring_limits <- sort(c(densProfile$ring_limits, newIndex))
    densProfile$distRingChange <- sort(c(densProfile$distRingChange, newDist))
    graphics::points(x = densProfile$xx[newIndex]/dim(im)[1], y = densProfile$yy[newIndex]/dim(im)[2],
           pch = 10, col = 'blue')
  }

  return(densProfile)
}
