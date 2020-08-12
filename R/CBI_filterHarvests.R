
#' @description Filter timber harvest results based on Year and ID
#' 
#' @param Year either 2016, 2017, 2018, or 2019 designating the year(s) of interest
#' @param ID what type of harvest was it. Either thp, ntmps, fs, or exempt
#' @param shp shapefile of timber harvests, acts as a key
#' 
#' @return The same polys param with filtered results based on criteria
#' 

cbi_filterHarvests <- function(Year, ID, shp) {
  if(!ID %in% shp$id) {cat("ID must be one of:", unique(shp$id)); return()}
  
  # remove geometry (much faster)
  dt <- as.data.frame(shp)
  
  # filter based on criteria
  dt <- subset(dt, dt$year %in% Year)
  dt <- subset(dt, dt$id == ID)
    
  # get object IDs
  nums <- as.character(dt$OBJECTID)

  # bound the values for exact matches
  # maybe clearer to use this outside of this function
  #nums <- paste0("\\b", nums, ".rds\\b")
    
  return(nums)
}

#' @example 

# get all 2016 exempt harvests 
# filtered_object_ids <- cbi_filterHarvests(2016, ID = "exempt", shp)
# 
# # find all output files
# strs <- dir("CBREC_output_directory/path_to_results", full.names = T)
# 
# # filter file path strings to object IDs of interest
# grep(pattern = paste0("\\b", filtered_object_ids, "\\b", collapse = "|"), strs, value = T)

# collapse is important to match all values from a character vector
# \\b bounds the regular expression to exact matches

