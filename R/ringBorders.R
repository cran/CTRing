#' Wrapper to use getBorders of xRing package to identify transition points between rings
#'
#' @param t Dataframe with density profiles
#' @param name Name of the series
#' @param thershold Thershold used by getBorders to identify transistion between rings
#' @param minTrw Width of the narrowest ring in number of measurements
#'
#' @return xRing list for the profile
#'
#' @import xRing
#'
#' @export
#'
#' @examples
# ringBorders <- function(t, name = "t1", thershold = 0.1, minTrw = 1) {
#   out <- getBorders(toxRing(t, seriesName = name),
#                     thershold,
#                     minTrw)
#   return(out)
# }
