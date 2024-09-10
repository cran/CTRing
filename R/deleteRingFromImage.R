
#' Add ring to pith to bark profile from CT scan image
#'
#' @param n Number of rings to remove
#' @param densProfile Density profile
#' @param im Density matrix
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
#'
#' densPath <- extractProfile(im_dens,
#'                            image_info,
#'                            pith_coord,
#'                            endPath,
#'                            k = 2, r = 5,
#'                            threshold = 0.002)
#'
#' newPath2 <- addRingFromImage(n = 1, densPath, im_dens)
#' oldPath2 <- deleteRingFromImage(n = 1, densPath, im_dens)
deleteRingFromImage <- function(n = 1, densProfile, im) {
  plotImageProfile(densProfile, im)
  ringXY <- data.frame(x = densProfile$xx[densProfile$ring_limits],
                       y = densProfile$yy[densProfile$ring_limits])

  message(paste("Select", n, "ring(s) on graph to delete"))
  newCoord <- locator(n)
  newCoord$x <- newCoord$x * dim(im)[1]
  newCoord$y <- newCoord$y * dim(im)[2]

  for (i in 1:n){
    ringXY$dist <- sqrt((ringXY$x - newCoord$x[i])^2 + (ringXY$y - newCoord$y[i])^2)
    newIndex <- which.min(ringXY$dist)
    deletedRingIndex <- densProfile$ring_limits[newIndex]
    densProfile$ring_limits <- densProfile$ring_limits[-newIndex]
    densProfile$distRingChange <- densProfile$distRingChange[-newIndex]
    graphics::points(x = densProfile$xx[deletedRingIndex]/dim(im)[1], y = densProfile$yy[deletedRingIndex]/dim(im)[2],pch = 10, col = 'red')
  }

  return(densProfile)
}
