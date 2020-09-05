

# bind data into data table function; works with specific list structure of outputs
bindDataTable <- function(data) {
  require(data.table)
  tr <-
    lapply(data, function(d) {
      # simplify list structure
      d <- unlist(d, recursive = F)
      # bind together as data table
      d <- rbindlist(d)
      return(d)
    })
  return(tr)
}

# for use in subsetting the scenario matrix based on map specifications
scenario_function <- function(scenario_matrix, 
                              piled, # will be the same for use and reference 
                              use_collection, 
                              use_burn,
                              ref_collection = "No",
                              ref_burn,
                              negate_ref = F){
  
  use <- subset(scenario_matrix, Fraction_Piled == piled & 
                  Biomass_Collection == use_collection &
                  Burn_Type == use_burn)
  
  # allow for no reference case burn to be explicitly defined
  if(negate_ref) {
    ref <- subset(scenario_matrix, Fraction_Piled == piled & 
                    Biomass_Collection == ref_collection &
                    Burn_Type != ref_burn)
  } else {
    ref <- subset(scenario_matrix, Fraction_Piled == piled & 
                    Biomass_Collection == ref_collection & 
                    Burn_Type == ref_burn)
  }
  
  return(list("use_id" = use$ID, 
              "ref_id"= ref$ID)) 
}

## ggplot theme
mxblsdl <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        #legend.title = element_blank(),
        strip.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12))

