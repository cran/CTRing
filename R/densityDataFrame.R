#' Convert to dataframe
#'
#' @param densProfile Density profile
#' @param sampleID Sample ID
#' @param addTransitionType add transition type to dataframe
#'
#' @return Dataframe with cambial age, density, years, transition type
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
#' pathEwLw <- getEwLw(path)
#' plotProfile(pathEwLw)
#' path_avgDens <- calcAvgDens(pathEwLw)
#' densityDf <- densityDataFrame(pathEwLw)
#'
densityDataFrame <- function(densProfile, sampleID = "NoID", addTransitionType = FALSE) {

  if ("avgDens" %in% names(densProfile)){
    dens <- densProfile$avgDens
    ringAge <- c(1:length(dens))
    ID = rep(sampleID, length(dens))

    out <- data.frame(ID = ID,
                      cambialAge = ringAge,
                      avgDens = dens)

    if ("avgDensEw" %in% names(densProfile))
      out$avgDensEw <- densProfile$avgDensEw
    if ("avgDensLw" %in% names(densProfile))
      out$avgDensLw <- densProfile$avgDensLw
    if ("years" %in% names(densProfile))
      out$years <- densProfile$years
    if (addTransitionType == TRUE)
      out$transitionType = densProfile$transitionType

    return(out)
  } else {
    message("The function 'calcAvgDens' was used to calculate\naverage density before generating the dataframe.")
    path_avgDens <- calcAvgDens(densProfile)
    return(densityDataFrame(path_avgDens, sampleID, addTransitionType))
  }
}
