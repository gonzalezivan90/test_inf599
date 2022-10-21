raster_count <- structure(
  function( # Count the pixels in a given raster
    ### This function generate a frequency table for a given raster dataset
    layer = '',      ##<<\code{character}. Raster object or raster path
    del0 = FALSE,    ##<<\code{boolean}. Determines if 0 count  categories should me removed
    n256 = FALSE      ##<<\code{boolean}. Determines if the raster contains less than 256 unique values, with
  ) {
    
    # requires: raster, rasterDT, gdalUtilities, 
    
    if (!class(layer) %in% c('RasterLayer', 'character')){
      stop('Class not RasterLayer or character')
    }
    
    if (class(layer) %in% c('character')){
      if (!file.exists(layer)){
        stop('File not found')
      }
    }
    
    if (n256){
      gdalLog <- capture.output(gdalUtilities::gdalinfo(datasetname = layer, hist = TRUE))
      
      (bucxml <- as.numeric(sub('buckets.+', '', grep('buckets ', gdalLog, value = TRUE))))
      (minxml <- as.numeric(gsub('.+from | to.+', '', grep('buckets ', gdalLog, value = TRUE)) ))
      (maxxml <- as.numeric(gsub('.+to |:', '', grep('buckets ', gdalLog, value = TRUE))))
      (histxml <- as.numeric(strsplit(split = '[[:space:]]', gsub("^ |^  ", "", 
                                                                  gdalLog[grep('buckets', gdalLog)+1]))[[1]]))
      
      labs <- seq(from = minxml, to = maxxml, length.out = bucxml)
      length(histxml)
      
      df2 <- data.frame(labs, nwlab = c(ceiling(labs[1]),
                                        round(labs[2:(bucxml-1)]),
                                        floor(labs[bucxml])), 
                        val = histxml)
      hist2 <- aggregate(df2$val, by = list(df2$nwlab), sum)
      result <- data.frame(id = hist2$Group.1, count = hist2$x, stringsAsFactors = FALSE)
      
    } else {
      if (class(layer) %in% c('character')){
        layer <- tryCatch(raster(layer), error = function (e) stop( "Can't open raster layer"))
      }
      freqTable <-  rasterDT::freqDT(layer)
      result <- data.frame(id = freqTable$ID, count = freqTable$freq, stringsAsFactors = FALSE )
    }
    
    if(del0){
      return(subset(result, count > 0) )
    } else {
      return(result)
    }
    
    ### \code{data.frame}.
  } , ex = function() {
    ## \donttest{
    ## raster_count(raster(volcano), n256 = FALSE)
    ## }
  })