# scenario definition
scenario_name <<- "GLOBAL"
reference_scenario <<- 'Baseline'

compute_mode <<- 'constant' # constant parameters from the given parameters
############################### No need to change the following ##################################
# keep record when code started:
starttime <- Sys.time()

# define min and max age to be considered
# min_age <-15
# max_age <-69
# # set age ranges for outcome statistics
# outcome_age_min <-c(15,50)
# outcome_age_max <-c(49,69)
# outcome_age_groups <-c('15-49','50-69')
# 

all_inputs <- read_excel(input_parameter_file, sheet = "all_city_parameter_inputs")
all_inputs[is.na(all_inputs)] <- ""
all_inputs <- as.data.frame(all_inputs)

# get input parameters into correct format
parameter_names <- all_inputs$parameter
parameter_starts <- which(parameter_names!='')
parameter_stops <- c(parameter_starts[-1] - 1, nrow(all_inputs)) 
parameter_names <- parameter_names[parameter_names!='']
parameter_list <- list()

for(i in 1:length(parameter_names)){
  parameter_list[[parameter_names[i]]] <-list()
  parameter_index <-which(all_inputs$parameter==parameter_names[i]) 
  if(all_inputs[parameter_index,2]=='')  { 
    parameter_list[[parameter_names[i]]] <-lapply(cities,function(x) {
      city_index <-which(colnames(all_inputs)==x)
      val <-all_inputs[parameter_index,city_index]
      ifelse(val%in%c('T','F'),val,ifelse(is.numeric(val), as.numeric(val), as.character(val)))
    })
    names(parameter_list[[parameter_names[i]]]) <-cities
  }else if(all_inputs[parameter_index,2]=='constant'){
    if (compute_mode != 'sample'){
      indices <-0
      parameter_list[[parameter_names[i]]] <-lapply(cities,function(x) {
        city_index <-which(colnames(all_inputs)==x)
        val <-all_inputs[parameter_index+indices,city_index]
        ifelse(val=='',0,as.numeric(val))
      })
    }
    if(compute_mode=='sample'){ # if sampling from distribution, check that distribution parameters exist
      parameter_list[[parameter_names[i]]] <-lapply(cities,function(x) {
        indices <-1:2
        city_index <-which(colnames(all_inputs)==x)  
        val <-all_inputs[parameter_index+indices,city_index] 
        if (val[1] == '' & val[2]==''){  # if no distribution parameters given in input file, read in constant value instead
          indices <<-0
          city_index <-which(colnames(all_inputs)==x) 
          val <-all_inputs[parameter_index+indices,city_index]} 
        val <-as.numeric(val)
      })
    }
    names(parameter_list[[parameter_names[i]]]) <-cities
  }else{
    parameter_list[[parameter_names[i]]] <-lapply(cities,function(x) {
      city_index <-which(colnames(all_inputs)==x)
      if(any(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')){
        sublist_indices <-which(all_inputs[parameter_starts[i]:parameter_stops[i],city_index]!='')
        thing <-as.list(as.numeric(c(all_inputs[parameter_starts[i]:parameter_stops[i],city_index])[sublist_indices]))
        names(thing) <-c(all_inputs[parameter_starts[i]:parameter_stops[i],2])[sublist_indices]
        thing
      }
    }
    )
    names(parameter_list[[parameter_names[i]]]) <-cities
  }
}

list2env(parameter_list, environment()) 

# read in global parameters

all_global_inputs <-read_excel(input_parameter_file, sheet = "all_global_parameter_inputs")
all_global_inputs[is.na(all_global_inputs)] <-""
all_global_inputs <-as.data.frame(all_global_inputs)

# get input parameters into correct format
global_parameter_names <-all_global_inputs$parameter
global_parameter_starts <-which(global_parameter_names!='')
global_parameter_stops <-c(global_parameter_starts[-1] - 1, nrow(all_global_inputs)) 
global_parameter_names <-global_parameter_names[global_parameter_names!='']
global_parameter_list <-list()

for(i in 1:length(global_parameter_names)){
  global_parameter_list[[global_parameter_names[i]]] <-list()
  global_parameter_index <-which(all_global_inputs$parameter==global_parameter_names[i]) 
  if(all_global_inputs[global_parameter_index,2]=='')  { 
    
    global_parameter_list[[global_parameter_names[i]]] <-all_global_inputs[global_parameter_index,'global']
    
  }else if(all_global_inputs[global_parameter_index,2]=='constant'){
    if (compute_mode != 'sample'){
      global_parameter_list[[global_parameter_names[i]]] <-ifelse(all_global_inputs[global_parameter_index,'global']=='',
                                                                   0,as.numeric(all_global_inputs[global_parameter_index,'global']))
    }
    else if(compute_mode=='sample'){ # if sampling from distribution, check that distribution parameters exist
      indices <-1:2
      val <-all_global_inputs[global_parameter_index+indices,'global'] 
      if (val[1] == '' & val[2]==''){  # if no distribution parameters given in input file, read in constant value instead
        val <-all_global_inputs[global_parameter_index,'global']} 
      val <-as.numeric(val)
      global_parameter_list[[global_parameter_names[i]]] <-val
    }
  }
}

list2env(global_parameter_list, environment()) 

dist_cat <<- unlist(strsplit(gsub(" ", "", dist_cat, fixed = TRUE), "\\,"))

outcome_age_min <<- as.numeric(unlist(strsplit(gsub(" ", "", outcome_age_min, fixed = TRUE), "\\,")))
outcome_age_max <<- as.numeric(unlist(strsplit(gsub(" ", "", outcome_age_max, fixed = TRUE), "\\,")))
outcome_age_groups <<- unlist(strsplit(gsub(" ", "", outcome_age_groups, fixed = TRUE), "\\,"))

min_age <<- as.numeric(min_age)
max_age <<- as.numeric(max_age)

day_to_week_scalar <<- as.numeric(day_to_week_scalar)

################################### Start running the the actual analysis

# logical for PA dose response: set F - use quantile 0.5
pa_dr_quantile <<-  F
# logical for AP dose response: set F - use quantile 0.5
ap_dr_quantile <<-  F

ithim_objects <<- outcome <<- outcome_pp <<- yll_per_hundred_thousand <<- list()
