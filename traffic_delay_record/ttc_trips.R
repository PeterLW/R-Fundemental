# ### A2: TTC - Part 1 Script ###
# Please write your full name(s) here:
# Please write your UTORid(s) here:
###

# Sets the working directory as the one this script is in (you must source this
# script in RStudio for this to work.)
# setwd(getSrcDirectory(function(dummy) {}))

#Peter:remember to change the directory to the one contains ur script and data folders
setwd("C:\\Users\\PeterLin\\Desktop\\a2")

# Filname constants
kRoutesFilename <- "routes.csv"
kTripsFilename <- "trips.csv"
kStopsFilename <- "stops.csv"
kStopTimesFilename <- "stop_times.csv"

# ############# File Input/Output Functions #####################################
# All files are in CSV format.
# Your functions must check if the file exists before reading from it.

GetDataFromFile <- function(filename, flag) {
  
  if(flag==1){
    file = paste(".\\ttc_data_files\\",filename, sep = "", collapse = NULL);
    MyData <- read.csv(file=file, header=TRUE, sep=",");
  }
  else{
    file = paste(".\\subway_delay_data\\",filename, sep = "", collapse = NULL);
    MyData <- read.csv(file=file, header=TRUE, sep=",");
  }
  return(MyData);
}

########### Data functions ####################################################
## You *must* use *all* of these data helper functions in 
## your TripDetails() function.
## These are the only functions in which you can use subset().

GetStopNameFromStopId <- function(stopId) {
  stops = GetDataFromFile("stops.csv", 1);
  ids = stops["stop_id"];
  names = stops["stop_name"];
  index = 1;
  temp = 1;
  for(num in ids$stop_id){
    if(num == stopId){
      index = temp;
    }
    temp = temp + 1;
  }
  name = as.character(names$stop_name[index]);

  return(name);
}

GetStopTimesForTrip <- function(tripId) {
  stop_times = GetDataFromFile("stop_times.csv", 1);
  trip_id_ = stop_times["trip_id"];
  stop_time_ = stop_times["stop_time"];
  stop_ids_ = stop_times["stop_id"];
  time = list()
  stop_id = list()
  
  index = 1;
  for(num in trip_id_$trip_id){
    if(num == tripId){
      stop_time_temp = as.character(stop_time_$stop_time[index]);
      stop_id_temp = as.character(stop_ids_$stop_id[index]);
      time = c(time, stop_time_temp);
      stop_id = c(stop_id, stop_id_temp)
    }
    index = index + 1;
  }
  
  result = do.call(rbind, Map(data.frame, A=time, B=stop_id))
  return(result);
}

GetRouteNumberFromRouteId <- function(routeId) {
  routes = GetDataFromFile("routes.csv", 1);
  route_numbers = routes["route_number"];
  route_ids = routes["route_id"];
  index = 1;
  temp = 1;
  for(num in route_ids$route_id){
    if(num == routeId){
      index = temp;
    }
    temp = temp + 1;
  }
  route_number = route_numbers$route_number[index];
  # print(route_number)
  return(route_number);
}

GetRouteNameFromRouteId <- function(routeId) {
  routes = GetDataFromFile("routes.csv", 1);
  route_names = routes["route_name"];
  route_ids = routes["route_id"];
  index = 1;
  temp = 1;
  for(num in route_ids$route_id){
    if(num == routeId){
      index = temp;
    }
    temp = temp + 1;
  }
  route_name = as.character(route_names$route_name[index]);
  return(route_name);
}

GetRouteIdFromTripId <- function(tripId) {
  trips = GetDataFromFile("trips.csv", 1);
  trip_ids = trips["trip_id"];
  route_ids = trips["route_id"];
  index = 1;
  temp = 1;
  for(num in trip_ids$tripId){
    if(num == tripId){
      index = temp;
    }
    temp = temp + 1;
  }
  route_id = route_ids$route_id[index];
  return(route_id);
}

GetTotalTripTime <- function(tripId) {
  stop_times = GetStopTimesForTrip(tripId);
  num = length(stop_times$A);
  start_temp = as.character(stop_times$A[1]);
  end_temp = as.character(stop_times$A[num]);
  start = as.POSIXct(start_temp, format="%H:%M:%S");
  end = as.POSIXct(end_temp, format="%H:%M:%S");
  period = difftime(end, start, units = "mins")
  return(period);
}


##### Print functions that use above helper functions ########################
TripDetails <- function(tripId, printDetailedStops = FALSE) {
  # Outputs to the console various details about the trip with id 'tripID'.
  # If 'printDetailedStops' is TRUE, then prints the stop name and 
  # stop time for each stop on the trip.
  
  # Note1: Use *all* of your data helper functions above in this function.
  # Note2: You must use sprintf() to format your strings in all of your calls to
  # cat().  You should not use the 'sep = ' argument.
  
  if(printDetailedStops){
    output = paste("Trip details for trip # ", as.character(tripId));
    print(output);
    temp1 = GetRouteIdFromTripId(tripId);
    temp2 = GetRouteNumberFromRouteId(temp1);
    temp3 = GetRouteNameFromRouteId(temp1);
    output = paste("Route: ", as.character(temp2), " ", temp3);
    print(output)
    temp1 = GetStopTimesForTrip(tripId);
    temp2 = length(temp1$A)
    output = paste("Total Number of Stops: ", temp2)
    print(output)
    writeLines("\n")
    
    stop_times = GetStopTimesForTrip(tripId);
    stop_time = stop_times[1];
    stop_id = stop_times[2];
    index = 1;
    for(num in stop_id$B){
      stop_name = as.character(GetStopNameFromStopId(num));
      output = paste("stop ", as.character(index), ": ", stop_name,as.character(num));
      print(output);
      index = index + 1;
    }
    
    writeLines("\n")
    
    stop_times = GetStopTimesForTrip(tripId);
    num_stops = length(stop_times$A);
    start_temp = as.character(stop_times$A[1]);
    
    end_temp = as.character(stop_times$A[num_stops]);
    total_time = GetTotalTripTime(tripId);
    hours = floor(total_time/60);
    minutes = total_time-60*hours;
    average = formatC(total_time/num_stops, digits = 2, format = "f");
    
    print(paste("Trip start time: ", start_temp));
    print(paste("Trip end time: ", end_temp));
    print(paste("Total time for trip: ", as.character(hours), " hours ", as.character(minutes), " minutes "));
    print(paste("Average minutes between stops: ", as.character(average)));
    
    writeLines("\n")
  }
  else{
    output = paste("Trip details for trip # ", as.character(tripId));
    print(output);
    temp1 = GetRouteIdFromTripId(tripId);
    temp2 = GetRouteNumberFromRouteId(temp1);
    temp3 = GetRouteNameFromRouteId(temp1);
    output = paste("Route: ", as.character(temp2), " ", temp3);
    print(output)
    temp1 = GetStopTimesForTrip(tripId);
    temp2 = length(temp1$A)
    output = paste("Total Number of Stops: ", temp2)
    print(output)

    stop_times = GetStopTimesForTrip(tripId);
    num_stops = length(stop_times$A);
    start_temp = as.character(stop_times$A[1]);
    
    end_temp = as.character(stop_times$A[num_stops]);
    total_time = GetTotalTripTime(tripId);
    hours = floor(total_time/60);
    minutes = total_time-60*hours;
    average = formatC(total_time/num_stops, digits = 2, format = "f");
    
    print(paste("Trip start time: ", start_temp));
    print(paste("Trip end time: ", end_temp));
    print(paste("Total time for trip: ", as.character(hours), " hours ", as.character(minutes), " minutes "));
    print(paste("Average minutes between stops: ", as.character(average)));
    
    writeLines("\n")
  }
  
}

############### Main Program ##################################################

cat("Reading files...\n")

# Global Environment Variables which can be used in any function.
# NEVER change the values of these variables in your functions, only extract
# data from them and save the extracted data to a new variable.
stops <- GetDataFromFile(kStopsFilename,1)
stopTimes <- GetDataFromFile(kStopTimesFilename,1)
trips <- GetDataFromFile(kTripsFilename,1)
routes <- GetDataFromFile(kRoutesFilename,1)

runningProgram <- TRUE
while(runningProgram) {

  if (!(stops == -1 || stopTimes == -1 || trips == -1 || routes == -1)) {
    tripId <- readline("Please enter a trip ID or Q to quit: ")
    if (tripId == "Q") {
      runningProgram <- FALSE
    } else {
      showDetails <- readline("Show stop details? y/n: ")
      if (showDetails == "y") {
        TripDetails(as.numeric(tripId), TRUE)
      } else {
        TripDetails(as.numeric(tripId))
      }
    }
  } else {
    cat("\nFile Error detected, quitting program.")
    runningProgram <- FALSE
  }

}



