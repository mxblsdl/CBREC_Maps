#####################################################################################
# %notin%: converse of "%in%" function, identifies when one item is not in a target
# object. Use: "a" %notin% c("b","c","d") will yieled TRUE.
#####################################################################################
"%notin%" <- function(x, table) match(x, table, nomatch = 0) == 0


#' @author max.blasdel@humboldt.edu
#' @description Functions to load and process cbrec results
#' 
#' @DEVELOPMENT
#' 
#' @concept Develop functions to deal with specific file structure of cbrec outputs
#' 


#' @name readOutputs
#' 
#' @param filepath to results
#' 
#' @return list of outputs from each polygon in file directory
#' 
#' @TODO build in a throttle of some kind as the full dataset will not be able to be read into memory at once

cbi_readOutputs <- function(filepath) { # soft deprecated in rin-climate-metric.R function 
  # not used as the function loops on each poly
  f <- dir(filepath, full.names = T)
  
  #f <- gsub(".*/", "", f)
  # get names of polygons
  n <- dir(filepath)
  n <- tools::file_path_sans_ext(n)
  
  # read all outputs
  f <- lapply(f, readRDS)
  
  # set names
  names(f) <- n
  
  return(f)
}

#' @name sortUseRef
#' @concept Separate the cases from a given polygon into use and reference lists
#' 
#' @param poly a single output file from CBREC in the form of lists of cases {list}
#' 
#' @return two lists of use and reference cases returned as a single list object {list}

cbi_sortUseRef <- function(poly) {
  
  # declare length of output lists
  use.list <- vector("list", length = length(poly)) 
  ref.list <- vector("list", length = length(poly))
  
  # apply to each item in list
  for(i in 1:length(poly)){
    
    # check for the use case specific outputs
    # if residues removed is greater than zero, this is a use case
    if(poly[[i]]$treatment$field.residue.removed_tonnes == 0) {
      ref.list[[i]] <- poly[[i]]
      # names(use.list[i]) <- names(poly[i])
      
    } else {
      use.list[[i]] <- poly[[i]]
      # names(ref.list[i]) <- names(poly[i])
    }
  }
  
  # name list items
  names(use.list) <- names(poly)
  names(ref.list) <- names(poly)
  
  # filter each list to remove NULL values
  # a NULL value corresponds to the data going to the other list
  use.list <- Filter(length, use.list)
  ref.list <- Filter(length, ref.list)
  
  # Each list will be the length of full cases
  return(list("use" = use.list, "ref" = ref.list))
}

#' @name sepPathways
#'
#' @concept filters case pathways based on user inputs. This way power plant type and collection and processing can be isolated 
#'
#' @param use_cases a list of values as returned by `cbi_sortUseRef` that correlates to the use cases {list}
#' @param power_plant string value as one of {cur_gen, ig, next_gen, one_mw, nearest}
#' @param CandP string value as one of {green_chip, green_grind, dry_grind, low_volume}
#' @param haulKM float or integer represent one-way on-road haul distance in km if you don't want to use the distance to the nearest power plant
#'
#' @return list output with {treatment} and {postTreatment} for the chosen pathways
#'

cbi_sepUseCaseSources <- function(use_cases, power_plant = "nearest", CandP = "dry_grind", haulKM = NULL) {
  # subset list based on inputs
  # this is only relevant for the use cases
  
  # simplify list structure
  all_results <- unlist(use_cases, recursive = F)
  
  # loop through each case and filter based on user specifications
  cp_res <-
    lapply(all_results, function(x) {
      # Grab CandP results
      if(CandP == "dry_grind") {
        cp <- x$treatment$CandP$"CandP_dry_grind"
      } else if(CandP == "green_chip") {
        cp <- x$treatment$CandP$"CandP_green_chip"
      } else if(CandP == "green_grind") {
        cp <- x$treatment$CandP$"CandP_green_grind"
      } else if(CandP == "low_volume") {
        cp <- x$treatment$CandP$"CandP_low"
      } else {return(warning("Must select: green_chip, green_grind, dry_grind, or low_volume for CandP"))}
      
      # Correct on-road transportation emissions to haulKM distance if a value is specified for haulKM
      if(power_plant != "nearest") {
        if(is.null(haulKM)) {
          warning("No haul distance specified. Using distance to nearest power plant.")
          # Do nothing
        } else {
          nearestDistance <- cp[,grepl("*distance*",names(cp)),with=F][[1]] # Grab distance_km value for scaling on-road transportation emissions
          lapply(names(cp), function(n) {
            if(grepl("*onroad*",n)) {
              value <- cp[,(n),with=F][[1]]
              cp[,(n) := value * (haulKM / nearestDistance)] # Note this also changes the distance value in the data table
            }
          })
        }
      }
      return(cp)
    })
  
  pp_res <-
    lapply(all_results, function(y) {
      if(power_plant == "cur_gen") {
        cbind("field.residue.removed_tonnes" = y$treatment$field.residue.removed_tonnes, 
              "total.biomass.mobilized_tonnesAcre" = y$treatment$total.biomass.mobilized_tonnesAcre,
              y$treatment$PP$"CurrentGenCombustionPlantDefault")
        
      } else if(power_plant == "ig") {
        cbind("field.residue.removed_tonnes" = y$treatment$field.residue.removed_tonnes, 
              "total.biomass.mobilized_tonnesAcre" = y$treatment$total.biomass.mobilized_tonnesAcre,
              y$treatment$PP$"CurrentGenIG/CombustionPlantDefault")
        
      } else if(power_plant == "next_gen") {
        cbind("field.residue.removed_tonnes" = y$treatment$field.residue.removed_tonnes, 
              "total.biomass.mobilized_tonnesAcre" = y$treatment$total.biomass.mobilized_tonnesAcre,
              y$treatment$PP$"NextGenThermochemicalPlantDefault")
        
      } else if(power_plant == "one_mw") {
        cbind("field.residue.removed_tonnes" = y$treatment$field.residue.removed_tonnes, 
              "total.biomass.mobilized_tonnesAcre" = y$treatment$total.biomass.mobilized_tonnesAcre,
              y$treatment$PP$"LessThan1MWPlant")
        
      } else if(power_plant == "nearest") {
        cbind("field.residue.removed_tonnes" = y$treatment$field.residue.removed_tonnes, 
              "total.biomass.mobilized_tonnesAcre" = y$treatment$total.biomass.mobilized_tonnesAcre,
              y$treatment$PP$"Nearest")
        
      } else {return(warning("Must select: cur_gen, ig, next_gen, one_mw, or nearest for power_plant"))}
    })
  
  # each residue bin is a list
  # combine power plant and collection and processing emissions to a single list
  pathways <- mapply(cbind, pp_res, cp_res, SIMPLIFY = F)
  
  # isolate broadcast burn emissions and bind
  broadcastburntype <- lapply(all_results, function(z) {z$treatment$BroadcastBurn})
  pathways <- mapply(cbind, pathways, broadcastburntype, SIMPLIFY = F)
  pathways <- lapply(pathways, function(x) x[,!(names(x) %in% c("V2")),with=F]) # Remove zero broadcast burn, which shows up as column name "V2"
  
  # isolate pile burn emissions and bind
  pileburntype <- lapply(all_results, function(z) {z$treatment$PileBurn})
  pathways <- mapply(cbind, pathways, pileburntype, SIMPLIFY = F)
  pathways <- lapply(pathways, function(x) x[,!(names(x) %in% c("V2")),with=F]) # Remove zero pile burn, which shows up as column name "V2"
  
  # trim out pathways options and keep time series
  all_results <- lapply(all_results, function(r) {
    r$postTreatment
  })
  
  # bind specific pathways data with the time series data
  # include names of lists
  out <-
    mapply(list, # function to apply
           "postTreatment" = all_results, # list one
           "treatment" = pathways, # list two
           SIMPLIFY = F) # option to return list
  
  return(out)
}


#' @concept reorganize the reference cases to match the climate metric function
#'
#' @param refs reference cases in list sttructure
#'
#' @return reference cases in reorganized list strucutre
#'
#'

cbi_processRefs <- function(refs) {
  
  # apply to each list item in reference cases
  t <-
    lapply(refs, function(x) {
      # replace treatment list with unlisted values
      x$treatment <- as.data.table(unlist(x$treatment, recursive = F))
      
      # re-order list of values (needed for climate metric function structure)
      x <- list("postTreatment" = x$postTreatment,
                "treatment" = x$treatment)
      
      return(x)
    })
  
  # the list items in reference need to be reordered
  
  # return input with new treatment column
  return(t)
}

#' @concept Unload all non-base packages without restarting the R session
#' @source Answer by petzi at https://stackoverflow.com/questions/55655162/unload-all-loaded-packages
#' 
unloadPackages <- function() {
  lapply(names(sessionInfo()$otherPkgs), function(pkgs) {
    detach(
      paste0('package:', pkgs),
      character.only = T,
      unload = T,
      force = T
    )
  })
}
