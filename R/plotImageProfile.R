#' Plot scan image, profile path and ring limits
#'
#' @param densProfile Density profile
#' @param im Density matrix
#'
#' @return Plot
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
#' path <- extractProfile(im_dens, image_info, pith_coord, endPath, k = 2, r = 5, threshold = 0.002)
#'
#' plotProfile(path)
#'
#' plotImageProfile(path, im_dens)
#'
plotImageProfile <- function(densProfile, im) {
  xSeg <- c(densProfile$xx[1], densProfile$xx[length(densProfile$xx)])/dim(im)[1]
  ySeg <- c(densProfile$yy[1], densProfile$yy[length(densProfile$xx)])/dim(im)[2]

  ringXY <- data.frame(x = densProfile$xx[densProfile$ring_limits],
                       y = densProfile$yy[densProfile$ring_limits])
  #### plot image
  dev.new()
  graphics::image(im)
  graphics::points(x = xSeg, y = ySeg, pch = 19)
  graphics::segments(xSeg[1], ySeg[1], xSeg[2], ySeg[2])
  graphics::points(x = ringXY$x/dim(im)[1], y = ringXY$y/dim(im)[2], pch = 10)
}
