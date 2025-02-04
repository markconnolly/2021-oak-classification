## make subsection maps
library(magrittr)
library(maps)
library(openxlsx)
# get data used in 2018 New Phyt paper -- slow download!

addNewDat <- TRUE
newDatVec <- c('Coccineae')
columnsKeep <- c('Species', 'use', 'latitude', 'longitude')
columnsNew <- c('specificEpithet', 'use', 'decimalLatitude', 'decimalLongitude')
if(!exists('dat.specimen')) {
  dat.specimen <-
    read.xlsx('https://nph.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fnph.14773&file=nph14773-sup-0004-TableS2.xlsx', 2)
  # dat.specimen.extras <-
  #   read.csv('https://raw.githubusercontent.com/andrew-hipp/oak-convergence-2017/master/data/tables/all.eco.data.exportedFromR.2016-02-03.csv', as.is = T)
  dat.specimen$Species <-
    sapply(strsplit(dat.specimen$Species, "_", fixed = T), '[', 2)
  dat.specimen <- dat.specimen[dat.specimen$use, ]
  dat.specimen <-
    dat.specimen[-which(dat.specimen$Species == 'havardii' &
                        dat.specimen$longitude < -105), ]
  dat.specimen <- dat.specimen[columnsKeep]
  }

if(addNewDat) {
  newDatList <- lapply(newDatVec, function(x) {
    print(dir(paste('../DATA', x, sep = '/'), full = TRUE))
    temp <- lapply(dir(paste('../DATA', x, sep = '/'), full = TRUE), read.csv, as.is = T)
    temp <- do.call('rbind', temp)
    temp <- temp[!is.na(temp$decimalLatitude), ]
    temp$use <- TRUE
    temp <- temp[grep('North Carolina', temp$stateProvince), columnsNew]
    names(temp) <- columnsKeep
    return(temp)
  }) # close lapply
  newDatList <- do.call('rbind', newDatList)
  newDatSpecimen <- rbind(dat.specimen[columnsKeep], newDatList)
}

dat.spClass <- read.xlsx('../DATA/spClassification.xlsx', 1)
dat.spClass <- dat.spClass[!is.na(dat.spClass$map), ]

mapEm <- function(x = 'all', outName = NA,
                  dat = newDatSpecimen, col = "Species",
                  lat = 'latitude', lon = 'longitude',
                  basemap = 'usa', overplot = 'state',
                  base.col = 'black', base.lwd = 1,
                  overplot.col = 'gray85', overplot.lwd = 0.1,
                  pt.col = 'gray', pt.pch = 19, pt.cex = 1,
                  mapTitle = NA) {
  if(x == 'all') {
    use <- seq(dim(dat)[1])
  } else use <- which(dat[[col]] %in% x)
  if(!is.na(outName)) pdf(outName)
  map(basemap, col = base.col, lwd = base.lwd) # close map
  points(dat[use, c(lon, lat)],
         pch = pt.pch, col = pt.col, cex = pt.cex)
  if(!is.na(overplot)) map(overplot, add = TRUE,
                            col = overplot.col, lwd = overplot.lwd)
  map(basemap, col = base.col, lwd = base.lwd, add = TRUE)
  if(!is.na(mapTitle)) title(mapTitle)
  if(!is.na(outName)) {
    dev.off()
    missing <- x[which(!x %in% dat[[col]])]
    if(length(missing) > 0) {
      logfile <- paste(outName, '.log.txt', sep = '')
      warning(paste('spp missing; check logfile', logfile))
      writeLines(missing, logfile)
    } # close if length(missing) > 0
  } # close if is.na
  return(ifelse(is.na(outName), 0, paste('saved', outName)))
}

# looping
for(i in dat.spClass$map %>% unique) {
  spp <- dat.spClass$sp[dat.spClass$map == i] %>% unique
  mapEm(spp, pt.col = colLab[i],  
        outName = paste('../OUT/MAP.PDFS/', i, '.pdf', sep = ''))
}

pdf('../OUT/mapLayout.pdf', 8.5, 11)
layout(matrix(c(1:10), 5, 2))
for(i in (dat.spClass$map %>% unique)) {
  spp <- dat.spClass$sp[dat.spClass$map == i] %>% unique
  mapEm(spp, mapTitle = i, pt.col = colLab[i], pt.cex = 0.3)
}
dev.off()
