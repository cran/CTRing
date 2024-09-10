#' Plot density profile
#'
#' @param densProfile Density profile
#'
#' @return Figure
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
#'

plotProfile <- function(densProfile) {
  dev.new()
  plot(x = densProfile$distFromPith,
       y = densProfile$dens,
       type = 'l',
       xlab = "Distance from pith",
       ylab = "Density")
  abline(v = densProfile$distRingChange)

  if ("ew_limits" %in% names(densProfile)) {
    points(x = densProfile$distFromPith[densProfile$ew_limits],
           y = densProfile$dens[densProfile$ew_limits],
           pch = 10,
           col = 'blue')
  }
}
