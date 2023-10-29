#'visualize airport delays
#' @name visualize_airport_delays

#' @import nycflights13
#' @import magrittr


#' @description
#' plot flight data.
#' @export visualize_airport_delays
visualize_airport_delays<-function(){

  airports_df<-dplyr::as_tibble(nycflights13::airports)
  flights_df<-dplyr::as_tibble(nycflights13::flights)
  flights_airport<-dplyr::left_join(flights_df, airports_df, by = c("dest" = "faa"))

  # declare dummy variables to skip warnings
  arr_delay <-list()
  dest<-list()
  lat<-list()
  lon<-list()
  long<-list()
  group<-list()

  flights_airport <- flights_airport %>% dplyr::mutate(arr_delay = ifelse(is.na(arr_delay), 0, arr_delay))
  delay_mean<-flights_airport %>%
    dplyr::group_by(dest) %>%
    dplyr::summarise(delay_mean = mean(arr_delay))

  flights_airport_with_mean<-dplyr::left_join(flights_airport, delay_mean, by = c("dest" = "dest"))

  plot_data<-dplyr::select(flights_airport_with_mean, dest,lat, lon, delay_mean)
  plot_data<-dplyr::filter(plot_data, lat > 0)
  plot_data<-dplyr::filter(plot_data, !dest %in% c("HNL","ANC"))

  states_map <- map_data("state")
  ggplot()+
    geom_polygon(data=states_map, aes(long, lat, group = group),color = "green", fill= "lightyellow")+
    geom_point(data=plot_data, aes(x=lon, y=lat,group=dest,colour = delay_mean))

}

