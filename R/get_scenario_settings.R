#' Get values for max mode share scenario
#' 
#' Computes the maximum mode share for specified mode types and specified distance categories across specified (stored) cities.
#' Used for max mode share scenario generation.
#' 
#' @param cities which cities to use
#' @param modes which modes to use
#' @param distances which distance categories to use
#' @param speeds named list of mode speeds (to be applied to all cities)
#' 
#' @return data frame of proportions by mode and distance category
#' 
#' @export
get_scenario_settings <- function(cities = c('accra', 'bangalore', 'belo_horizonte', 'bogota', 'buenos_aires', 'cape_town',
                                             'delhi', 'mexico_city', 'santiago', 'sao_paulo', 'vizag'),
                                  modes=c("pedestrian","cycle","car","motorcycle","bus"),     
                                  distances=c('0-2 km','2-6 km','6+ km'),
                                  speeds = list( bus = 10, 
                                                 bus_driver = 10, 
                                                 minibus = 10, 
                                                 minibus_driver = 10, 
                                                 car = 14.4, 
                                                 car_driver = 14.4,
                                                 taxi = 12.6, 
                                                 pedestrian = 2.5, 
                                                 walk_to_pt = 2.5, 
                                                 cycle = 7.2, 
                                                 motorcycle = 15.2, 
                                                 truck = 10, 
                                                 van = 14.4, 
                                                 subway = 18.1, 
                                                 rail = 21.9, 
                                                 auto_rickshaw = 4, 
                                                 shared_auto = 14.4, 
                                                 shared_taxi = 12.6, 
                                                 cycle_rickshaw = 4,
                                                 other = 9.1
                                  )){
  
  min_distances <- as.numeric(sapply(distances,function(x)strsplit(x, "[^0-9.]+")[[1]][1]))
  mode_proportions <- mode_proportions_by_distance <- list()
  
  for(city in cities){
    tripset_path <- file.path(find.package('ithimr',lib.loc=.libPaths()), paste0('extdata/local/',city,'/trips_',city,'.csv')) 
    trip_set <- read_csv(tripset_path,col_types = cols())
    if('main_mode_name'%in%colnames(trip_set)){
      names(trip_set)[which(names(trip_set)=='trip_mode')] <- 'stage_mode'
      names(trip_set)[which(names(trip_set)=='main_mode_name')] <- 'trip_mode'
      names(trip_set)[which(names(trip_set)=='total_distance')] <- 'trip_distance'
    }
    ## copy over as required
    mode_cols <- c('trip_mode','stage_mode')
    if(sum(mode_cols%in%colnames(trip_set))==0) stop(paste0('Please include a column labelled "trip_mode" or "stage_mode" in ', filename))
    if('stage_mode'%in%colnames(trip_set)&&!'trip_mode'%in%colnames(trip_set)) 
      trip_set$trip_mode <- trip_set$stage_mode
    if('stage_distance'%in%colnames(trip_set)&&!'trip_distance'%in%colnames(trip_set)) 
      trip_set$trip_distance <- trip_set$stage_distance
    ## use specified words for key modes
    walk_words <- c('walk','walked','pedestrian')
    cycle_words <- c('bike','cycle','cycling')
    mc_words <- c('motorcycle','mcycle','mc','mtw')
    subway_words <- c('metro','underground')
    rail_words <- c('train')
    ## lower case mode names
    trip_set[['trip_mode']] <- tolower(trip_set[['trip_mode']])
    ## replaces spaces with _
    trip_set[['trip_mode']] <- sapply(trip_set[['trip_mode']],function(x)gsub(' ','_',as.character(x)))
    trip_set[['trip_mode']][trip_set[['trip_mode']]=='private_car'] <- 'car'
    trip_set[['trip_mode']][trip_set[['trip_mode']]%in%walk_words] <- 'pedestrian'
    trip_set[['trip_mode']][trip_set[['trip_mode']]%in%cycle_words] <- 'cycle'
    trip_set[['trip_mode']][trip_set[['trip_mode']]%in%mc_words] <- 'motorcycle'
    trip_set[['trip_mode']][trip_set[['trip_mode']]%in%subway_words] <- 'subway'
    trip_set[['trip_mode']][trip_set[['trip_mode']]%in%rail_words] <- 'rail'
    
    trip_set <- drop_na(trip_set)
    
    ## get distances
    if(!'trip_distance'%in%colnames(trip_set)){
      #trip_set <- subset(trip_set,trip_mode%in%names(speeds))
      mode_speeds <- sapply(trip_set$trip_mode,function(x)ifelse(x%in%names(speeds),speeds[[x]],0))
      trip_set$trip_distance <- mode_speeds * trip_set$trip_duration / 60
    }
    
    ## assign distance categories
    # AA: DON'T REMOVE 'OTHER' MODE
    trip_set <- subset(trip_set,!duplicated(trip_id))
    trip_set$trip_distance_cat <- sapply(trip_set$trip_distance,function(x)last(distances[which(min_distances<=x)]))
    ## get distance profiles
    if(!"rail" %in% unique(trip_set$trip_mode)){ # Conditional to sum rail propensity to bus
      mode_proportions_by_distance[[city]] <- sapply(distances,function(y) sapply(modes,function(x)sum(trip_set$trip_mode==x&trip_set$trip_distance_cat==y)/sum(trip_set$trip_distance_cat==y)))
      ## get total mode shares
      mode_proportions[[city]] <- sapply(modes,function(x)sum(trip_set$trip_mode==x)/nrow(trip_set))
    } else {
      new_modes <- c(modes, "rail")
      # Compute proportion in each distance band
      mode_proportions_by_distance[[city]] <- sapply(distances,function(y) sapply(new_modes,function(x)sum(trip_set$trip_mode==x&trip_set$trip_distance_cat==y)/sum(trip_set$trip_distance_cat==y)))
      ## Identify rows for bus and rail
      row_bus <- which(rownames(mode_proportions_by_distance[[city]]) == "bus")
      row_rail <- which(rownames(mode_proportions_by_distance[[city]]) == "rail")
      ## Sum rail to bus
      mode_proportions_by_distance[[city]][row_bus,] <- mode_proportions_by_distance[[city]][row_bus,] + mode_proportions_by_distance[[city]][row_rail,]
      ## Delete rail proportion
      mode_proportions_by_distance[[city]] <- mode_proportions_by_distance[[city]][-row_rail,]
      
      # get total mode shares
      mode_proportions[[city]] <- sapply(new_modes,function(x)sum(trip_set$trip_mode==x)/nrow(trip_set))
      ## Identify rows for bus and rail
      col_bus <- which(names(mode_proportions[[city]]) == "bus")
      col_rail <- which(names(mode_proportions[[city]]) == "rail")
      ## Sum rail to bus
      mode_proportions[[city]][col_bus] <- mode_proportions[[city]][col_bus] +
        mode_proportions[[city]][col_rail]
      ## Delete rail proportion
      mode_proportions[[city]] <- mode_proportions[[city]][-col_rail]
      new_modes <- NULL
    } # End else
    
  }
  ## write as %
  mode_proportions_tab <- sapply(mode_proportions,function(x)x*100)
  mode_proportions_list <- lapply(mode_proportions_by_distance,function(x)x*100)
  mode_proportions_tab <- t(sapply(1:length(modes),function(x)apply(sapply(mode_proportions_list,function(y)y[x,]),1,max)))
  rownames(mode_proportions_tab) <- modes
  #{cat(  paste0('||',(paste0(colnames(mode_proportions_tab),collapse='|')),'|\n|---|---|---|---|\n'))
  #for(i in 1:nrow(mode_proportions_tab)) cat('|',rownames(mode_proportions_tab)[i],'|',paste0(sapply(mode_proportions_tab[i,],function(x)sprintf('%.1f',x)),collapse='|'),'|\n')
  #}
  for(j in 1:length(mode_proportions_list)){
    {
      cat("\n")
      cat(  paste0('|',names(mode_proportions_list)[j],'|',(paste0(colnames(mode_proportions_list[[j]]),collapse='|')),'|\n|---|---|---|---|\n'))
      for(i in 1:nrow(mode_proportions_list[[j]])) cat('|',rownames(mode_proportions_list[[j]])[i],'|',paste0(sapply(mode_proportions_list[[j]][i,],function(x)sprintf('%.1f',x)),collapse='|'),'|\n')
    }
  }
  
  ## find max mode share city for each mode
  #mode_cities <- max.col(mode_proportions_tab)
  ## copy the right city's mode row into matrix to return
  #scenario_proportions <- mode_proportions_list[[1]]
  #for(i in 1:length(mode_cities)) {
  #  scenario_proportions[i,] <- mode_proportions_list[[mode_cities[i]]][i,]
  #  scenario_proportions[i,][is.na(scenario_proportions[i,])] <- 0
  #}
  scenario_proportions <- mode_proportions_tab
  return(scenario_proportions)
}