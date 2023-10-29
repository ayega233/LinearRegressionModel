#' @import caret


root_mean_squared_error <- function(error)
{
  sqrt(mean(error^2))
}

flight_delay <- function(){

  # declare dummy variables to skip warnings
  wind_speed <-list()
  arr_delay <-list()
  pressure<-list()


  weather_df<-dplyr::as_tibble(nycflights13::weather)
  flights_df<-dplyr::as_tibble(nycflights13::flights)
  flights_weather<-dplyr::left_join(flights_df, weather_df, by = c("origin" = "origin","time_hour" = "time_hour"))

  flights_data <- dplyr::select(flights_weather, arr_delay,wind_speed,pressure)
  flights_data <- flights_data %>% dplyr::mutate(arr_delay = ifelse(is.na(arr_delay), 0, arr_delay))
  flights_data <- flights_data %>% dplyr::mutate(pressure = ifelse(is.na(pressure), 0, pressure))
  flights_data <- flights_data %>% dplyr::mutate(wind_speed = ifelse(is.na(wind_speed), 0, wind_speed))
  flights_data<-dplyr::filter(flights_data, wind_speed > 0 , pressure>0)

  inTraining <- caret::createDataPartition(flights_data$arr_delay, p = .8, list = FALSE)
  training <- flights_data[ inTraining,]
  remaining  <- flights_data[-inTraining,]
  inTesting <- caret::createDataPartition(remaining$arr_delay, p = .25, list = FALSE)
  testing <- remaining[ inTesting,]
  validation <- remaining[ -inTesting,]

  ridgeregModelInfo <- caret::getModelInfo(model = "lm", regex = FALSE)[[1]]

  ridgeregModelInfo$fit <-function(x,y, wts, param, lev, last, weights, classProbs, ...) {

     #need to use our own ridgereg but parameters are different
    #TODO:
  }
  ridgeregModelInfo$parameters <- data.frame(parameter = 'lambda',
                                                    class = 'numeric',
                                                    label = '# lambda')
  ridgeGrid <- data.frame(lambda = 10)
  control<-caret::trainControl(method = "cv",number=2)

  set.seed(825)
  ridgeFit_new <- caret::train(arr_delay~wind_speed+pressure, data = as.data.frame(training),
                   method = ridgeregModelInfo,
                   tuneGrid =ridgeGrid,
                   trControl = control)


}


