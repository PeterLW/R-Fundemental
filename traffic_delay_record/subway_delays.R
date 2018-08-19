
setwd("C:\\Users\\PeterLin\\Desktop\\a2")


GetDataFromFile <- function(filename) {
  
  file = paste(".\\subway_delay_data\\",filename, sep = "", collapse = NULL);
  MyData <- read.csv(file=file, header=TRUE, sep=",");
  
  return(MyData);
}

#1
day_delay_plot <- function(filename) {
  data = GetDataFromFile(filename);
  days = data["Day"];
  delays = data["Delay"]
  Sun_dealy = 0;
  Mon_dealy = 0;
  Tue_dealy = 0;
  Wed_dealy = 0;
  Thu_dealy = 0;
  Fri_dealy = 0;
  Sat_dealy = 0;
  
  index = 1;
  
  for(num in days$Day){
    if(num == "Sunday"){
      Sun_dealy = Sun_dealy + delays$Delay[index];
    }
    else if(num == "Monday"){
      Mon_dealy = Mon_dealy + delays$Delay[index];
    }
    else if(num == "Tuesday"){
      Tue_dealy = Tue_dealy + delays$Delay[index];
    }
    else if(num == "Wednesday"){
      Wed_dealy = Wed_dealy + delays$Delay[index];
    }
    else if(num == "Thursday"){
      Thu_dealy = Thu_dealy + delays$Delay[index];
    }
    else if(num == "Friday"){
      Fri_dealy = Fri_dealy + delays$Delay[index];
    }
    else if(num == "Saturday"){
      Sat_dealy = Sat_dealy + delays$Delay[index];
    }
    index = index + 1;
  }
  
  x = list(Sun_dealy, Mon_dealy, Tue_dealy, Wed_dealy, Thu_dealy, Fri_dealy, Sat_dealy);
  y = list("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday");
  df = do.call(rbind, Map(data.frame, A=x, B=y))
  barplot(df$A, name = df$B, xlab = "Day", ylab = "Total Dealy", main = "day_delays", col = "lightblue");
}

#2
week_top_five_plot <- function(filename, plot) {
  data = GetDataFromFile(filename);
  station = data["Station"];
  delays = data["Delay"];
  
  df = do.call(rbind, Map(data.frame, A=station, B=delays));
  index = order(df$A);
  df = df[index,];
  df = subset(df, B!=0)
  station = df["A"];
  delays = df["B"];
  
  result_A = list();
  result_B = list();
  
  s_name = as.character(station$A[1]);
  delay_temp = 0;
  index = 1;
  for(num in station$A){
    
    if(num != s_name){
      
      result_A = c(result_A, s_name);
      result_B = c(result_B, delay_temp)
      s_name = num;
      delay_temp = 0;
    }
    delay_temp = delay_temp + delays$B[index];
    index = index + 1;
  }
  
  result = df = do.call(rbind, Map(data.frame, A=result_A, B=result_B));
  index = order(result$B, decreasing = TRUE);
  result = result[index,];
  result = result[c(1,2,3,4,5),];
  if(plot == 1){
    barplot(result$B, name = result$A, xlab = "Station", ylab = "Total Dealy", main = "week_top_five", col = "lightblue");
  }
  return(result);
}


#3
hour_delays_plot <- function(filename) {
  data = GetDataFromFile(filename);
  
  
  time_list_str = list();
  delay_list= list();
  index = 1;
  for(num in data$Delay){
    if(num != 0){
      time_list_str = c(time_list_str, as.character(data$Time[index]));
      delay_list = c(delay_list, num);
    }
    index = index + 1;
  }
  
  #remove minutes, convert to int
  time_list_int = list();
  for(num in time_list_str){
    temp = strtoi(vapply(strsplit(num,":"), `[`, 1, FUN.VALUE=character(1)));
    time_list_int = c(time_list_int, temp)
  }
  
  # result = list();
  # index = 1;
  # for(num in time_list_int){
  #   if(delay_list[index] == 1){
  #     result = c(result, num);
  #   }
  #   else{
  #     delay = delay_list[index];
  #     # print(delay);
  #     for(i in 1:delay[[1]]){
  #       result = c(result, num);
  #     }
  #   }
  #   
  #   index = index + 1;
  # }
  
  result <- as.numeric(time_list_int)
  hist(result, xlim = c(0,25), breaks = 23, ylim = c(0,40), main = "hour_delays", xlab = "number of delays", ylab = "Hours", col = "lightblue");
  
}


sum_list <- function(input_list) {
  len = length(input_list);
  sum = 0;
  for(i in 1:len){
    sum = sum + input_list[i];
  }
  return(sum);
}

month_minutes_plot <- function(){
  data_sept = GetDataFromFile("sept2017.csv");
  data_oct = GetDataFromFile("oct2017.csv");
  data_nov = GetDataFromFile("nov2017.csv");
  data_dec = GetDataFromFile("dec2017.csv");
  data_jan = GetDataFromFile("jan2018.csv");
  data_feb = GetDataFromFile("feb2018.csv");
  
  delay_sept = data_sept$Delay;
  delay_oct = data_oct$Delay;
  delay_nov = data_nov$Delay;
  delay_dec = data_dec$Delay;
  delay_jan = data_jan$Delay;
  delay_feb = data_feb$Delay;
  
  sum_sept = sum_list(delay_sept);
  sum_oct = sum_list(delay_oct);
  sum_nov = sum_list(delay_nov);
  sum_dec = sum_list(delay_dec);
  sum_jan = sum_list(delay_jan);
  sum_feb = sum_list(delay_feb);
  
  delays = list(sum_sept, sum_oct, sum_nov, sum_dec, sum_jan, sum_feb);
  months = list("sept", "oct", "nov", "dec", "jan", "feb");
  
  result = df = do.call(rbind, Map(data.frame, A=delays, B=months));
  barplot(result$A, name = result$B, xlab = "month", ylab = "delays", main = "month_minutes", col = "lightblue");
}

find_delay_num <- function(input_list) {
  index = 1;
  len = length(input_list);
  result = list();
  for(num in input_list){
    if(num != 0){
      result = c(result, num);
    }
    index = index + 1;
  }
  return(length(result));
}

month_delays_plot <- function() {
  data_sept = GetDataFromFile("sept2017.csv");
  data_oct = GetDataFromFile("oct2017.csv");
  data_nov = GetDataFromFile("nov2017.csv");
  data_dec = GetDataFromFile("dec2017.csv");
  data_jan = GetDataFromFile("jan2018.csv");
  data_feb = GetDataFromFile("feb2018.csv");
  
  delay_sept = find_delay_num(data_sept$Delay);
  delay_oct = find_delay_num(data_oct$Delay);
  delay_nov = find_delay_num(data_nov$Delay);
  delay_dec = find_delay_num(data_dec$Delay);
  delay_jan = find_delay_num(data_jan$Delay);
  delay_feb = find_delay_num(data_feb$Delay);
  
  delay_num = list(delay_sept, delay_oct, delay_nov, delay_dec, delay_jan, delay_feb);
  months = list("sept", "oct", "nov", "dec", "jan", "feb");
  
  result = df = do.call(rbind, Map(data.frame, A=delay_num, B=months));
  barplot(result$A, name = result$B, xlab = "month", ylab = "number of delays", main = "month_delays", col = "lightblue");
}

month_top_five_plot <- function() {
  
  top_five_sept = week_top_five_plot("sept2017.csv", 0);
  top_five_oct = week_top_five_plot("oct2017.csv", 0);
  top_five_nov = week_top_five_plot("nov2017.csv", 0);
  top_five_dec = week_top_five_plot("dec2017.csv", 0);
  top_five_jan = week_top_five_plot("jan2018.csv", 0);
  top_five_feb = week_top_five_plot("feb2018.csv", 0);
  
  station_list = list();
  delay_list = list();
  
  for(i in 1:5){
    station_list = c(station_list, as.character(top_five_sept$A[i]));
    station_list = c(station_list, as.character(top_five_oct$A[i]));
    station_list = c(station_list, as.character(top_five_nov$A[i]));
    station_list = c(station_list, as.character(top_five_dec$A[i]));
    station_list = c(station_list, as.character(top_five_jan$A[i]));
    station_list = c(station_list, as.character(top_five_feb$A[i]));
    
    delay_list = c(delay_list, as.numeric(top_five_sept$B[i]));
    delay_list = c(delay_list, as.numeric(top_five_oct$B[i]));
    delay_list = c(delay_list, as.numeric(top_five_nov$B[i]));
    delay_list = c(delay_list, as.numeric(top_five_dec$B[i]));
    delay_list = c(delay_list, as.numeric(top_five_jan$B[i]));
    delay_list = c(delay_list, as.numeric(top_five_feb$B[i]));
  }
  
  station = station_list;
  delays = delay_list;
  
  df = do.call(rbind, Map(data.frame, A=station, B=delays));
  index = order(df$A);
  df = df[index,];
  df = subset(df, B!=0)
  station = df["A"];
  delays = df["B"];
  
  result_A = list();
  result_B = list();
  
  s_name = as.character(station$A[1]);
  delay_temp = 0;
  index = 1;
  for(num in station$A){
    
    if(num != s_name){
      
      result_A = c(result_A, s_name);
      result_B = c(result_B, delay_temp)
      s_name = num;
      delay_temp = 0;
    }
    delay_temp = delay_temp + delays$B[index];
    index = index + 1;
  }
  
  result = df = do.call(rbind, Map(data.frame, A=result_A, B=result_B));
  index = order(result$B, decreasing = TRUE);
  result = result[index,];
  result = result[c(1,2,3,4,5),];
  barplot(result$B, name = result$A, xlab = "Station", ylab = "Total Dealy", main = "month_top_five", col = "lightblue");
  
}

{
  
  par(mfrow=c(4,3))
  
  filename = "sept2017.csv";
  day_delay_plot(filename);
  a = week_top_five_plot(filename, 1);
  hour_delays_plot(filename);
  month_minutes_plot();
  month_delays_plot();
  month_top_five_plot();
  
  filename = "oct2017.csv";
  day_delay_plot(filename);
  a = week_top_five_plot(filename, 1);
  hour_delays_plot(filename);
  month_minutes_plot();
  month_delays_plot();
  month_top_five_plot();
  
}
