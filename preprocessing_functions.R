average_points <- function(points_received, column){
  years <- unique(points_received$year)
  
  points <- 0
  for(i in years){
    tmp_data <- points_received %>% filter(year == i)
    
    if(i==2016){participants <- 42}
    if(i==2017){participants <- 42}
    if(i==2018){participants <- 43}
    if(i==2019){participants <- 41}
    
    points <- points + sum(tmp_data[[column]]) / participants
    
  }
  
  points <- points/length(years)
  
  return(points)
}

points_from_neighbours <- function(country, neighbours, data) {
  
  # get the points given to the input country
  points_received <- data %>% filter(to_country_id == country)
  
  n_years <- length(unique(points_received$year))
  
  average_sum_jury <- average_points(points_received, "jury_points")
  average_sum_tele <- average_points(points_received, "tele_points")
  
  
  # get the points given by the neighbours
  points <- points_received %>% filter(from_country_id %in% neighbours)
  
  n_nbs <- length(neighbours)
  # get sum and subtract the average number of points 
  points_sum_jury <- (sum(points$jury_points) / n_years / n_nbs) - average_sum_jury
  points_sum_tele <- (sum(points$tele_points) / n_years / n_nbs) - average_sum_tele
  
  cbind(points_sum_jury, points_sum_tele)
}

return_shortname <- function(longname, mapping_data) {
  shortname <- mapping_data[which(mapping_data$country == longname), 1]
  
  shortname[1,1]
}

average_nbh_points <- function(data, points_col) {
  data$avg_nb <- 0
  
  for (i in 1:nrow(data)) {
    neighbours <- data[i,]$countries_similar
    neighbours <- strsplit(neighbours, " ")[[1]]
    
    # the points the country itself got
    points <- data[i,][[points_col]]
    
    # points the neighbours got
    neighbourhood_votes <- data %>% filter(country_sn %in% neighbours)
    
    data[i,]$avg_nb <- mean(c(neighbourhood_votes[[points_col]], points), na.rm = TRUE)
  }
  
  return(data)
}