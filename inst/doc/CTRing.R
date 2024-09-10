## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
#  library(CTRing)

## -----------------------------------------------------------------------------
#  library(oro.dicom)
#  file_path <- system.file("extdata", "disk.dcm", package = "CTRing")
#  dcm <-  readDICOM(file_path)

## -----------------------------------------------------------------------------
#  hdr_df <- dcm$hdr[[1]]
#  image_info <- getImageInfo(hdr = hdr_df)

## -----------------------------------------------------------------------------
#  im <- imageToMatrix(dcm$img)
#  im_8bit <- xBitTo8Bit(im, image_info$grayScale)
#  im_dens <- grayToDensity(im_8bit)

## -----------------------------------------------------------------------------
#  pith_coord <- detect_pith(im_dens, pixel = TRUE, toPlot = FALSE)
#  pith_coord_checked <- verifyPith(im_dens, pith_coord)

## -----------------------------------------------------------------------------
#  endPath <- c(472, 284) # manual
#  endPath <- locatePathEnd(im_dens, pith_coord_checked) # using the image

## -----------------------------------------------------------------------------
#  path <- extractProfile(im_dens, image_info, pith_coord, endPath, k = 2, r = 5, threshold = 0.002)

## -----------------------------------------------------------------------------
#  plotImageProfile(path, im_dens)
#  
#  newPath2 <- addRingFromImage(n = 1, path, im_dens)
#  oldPath2 <- deleteRingFromImage(n = 1, path, im_dens)
#  

## -----------------------------------------------------------------------------
#  path <- getEwLw(path)
#  plotProfile(path)

## -----------------------------------------------------------------------------
#  path <- getEwLw(path)
#  path <- calcAvgDens(path)
#  path <- addYears(2021, path)
#  path <- removeLastYear(path)

## -----------------------------------------------------------------------------
#  densityDf <- densityDataFrame(path)

