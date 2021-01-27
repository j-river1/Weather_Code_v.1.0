#Check_TotalData_Day checks if data per day has total data enough for processing as day. Each day must have at least 80% possible data.
# Arguments:  -weatherdata after hourlycontrol. It has to be in days.
#             -Percentage for an admissible day. If data per day has less data than that percentage then
#              day is delete.
#             -weatherfile : Weather file
#                      
# Return:     1 or 0.  If the day meets with condition, return 1.
#                      If the day dont meet with condition, return 0.   

Check_TotalData_Day <- function(weatherdataperday, percentageday, weatherfile, long=LONG[1], lat=LAT[1], tz=TZ[1])
{
  #Delay time
  delaytime <- time_step(weatherdataperday)
  
  #Convert delaytime in number 
  delaytime_number <- delaytime/60
  
  #Days with only a data
  if(delaytime_number == 0)
  {
    check_day = 0
    warning('There is only a data in the station : ', weatherfile ,' in the day: ', unique(weatherdataperday$Date))
  }
  
  else
  {
    
    #Format
    weatherdataperday <- as.data.frame(weatherdataperday)
    colnames(weatherdataperday) = c("Date", "Hour", "Value", "HourDecimal")    
    
    #Different scale
    #Extract part decimal
    extract_partDeci <- unlist(weatherdataperday$HourDecimal)%%1
    is_naturalnumber <-  round(extract_partDeci/delaytime_number, digits = 2)%%1
    change_hour <- unlist(weatherdataperday$HourDecimal[which(is_naturalnumber!=0)])
    
    if(is.null(change_hour)== TRUE)
    {
      #Day divide by delaytime
      HourDecimal_Allday <- seq(0,23.999, by = delaytime_number)
      All_data <- data.frame(HourDecimal_Allday)
      All_data$Value <- NA
    }
    else  
    {
      #Day divide by delaytime
      HourDecimal_Allday <- Table_NA(change_hour, delaytime_number)
      All_data <- data.frame(HourDecimal_Allday)
      All_data$Value <- NA
      
    }
    colnames(All_data) <- c("HourDecimal","Value")
    
    #Exception when station has more delay time
    count_true <- All_data$HourDecimal %in% weatherdataperday$HourDecimal
    if(sum(count_true, na.rm=TRUE) != length(weatherdataperday$Value)){stop('There is a problem with delay time of station : ', weatherfile ,' in the day: ', unique(weatherdataperday$Date))}
    
    #Match values
    All_data[All_data$HourDecimal %in% weatherdataperday$HourDecimal,]$Value <- weatherdataperday$Value
    
    #Control para SR
    if(split_name(weatherfile)[2]=='SR')
    {
      sunrise_hour <- hour_solarnoon_sunrise(unique(weatherdataperday$Date), lat=lat, long=long, timezo=tz, typeofhour= "sunrise") 
      noon_hour <- hour_solarnoon_sunrise(unique(weatherdataperday$Date), lat=lat, long=long, timezo=tz, typeofhour= "solarnoon") 
      
      sunrise_hour <- hour_to_number(sunrise_hour)
      noon_hour <- hour_to_number(noon_hour)
      
      time_solar <- 2*(noon_hour - sunrise_hour) 
      time_solar <- c(sunrise_hour, sunrise_hour + time_solar)
      
      All_data <- subset (All_data, HourDecimal > time_solar[1] & HourDecimal < time_solar[2] ) 
    }
    
    
    #Count NA
    count_NA <- sum(is.na(All_data$Value))
    
    
    check_day <- ifelse(length(All_data$Value)*(1-percentageday) > count_NA, 1, 0 )
    
  }
  return (check_day)
  
}

#Check_Day_Station returns days that meet with condition refers to number of data. 
#Arguments: - data after hourly control
#         : - percentage_data.  Percentage for an admissible day
#Return:    - fill data           

Check_Day_Station <- function(weatherdata, percentage_data, LONG=LONG, LAT=LAT, TZ=TZ)
{
  #Read
  weather_data <- put_format(weatherdata, date_format="%Y-%m-%d" )
  
  #Divide per day
  divi_day <-  divide_by_day(weather_data)
  names_day <- unique(names(divi_day))
  
  
  #Check day 
  check_day <- lapply(divi_day, function (x) Check_TotalData_Day (x, percentageday =percentage_data, weatherfile = weatherdata))
  
  admissible_day <- data.frame(names_day, unlist(check_day))
  colnames(admissible_day) <- c("Date", "CheckDay")
  
  
  days_per_station <- admissible_day[which(admissible_day$CheckDay==1),]$Date
  
  return(days_per_station)
  
}  

#Hour_to_Day converts hours to days.
#Arguments:   - Weather data with hourly control
#             - percentage_data.  Percentage for an admissible day

Hour_to_Day <- function(weather_data, percentage)
{
  
  #Dates
  dates <- Check_Day_Station(weather_data, percentage)
  
  #Divide per day
  divi_day <-  divide_by_day(put_format(weather_data, date_format = "%Y-%m-%d"))
  days_aux <- divi_day[which(names(divi_day) %in% dates)]
  days <- do.call(rbind.data.frame, days_aux)
  
  
  if(split_name (weather_data)[2]== 'P')
  {
    if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
    hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=sum)
    hours_day <- subset(hours_day, hours_day$x <= 1000)
  }
  
  if(split_name (weather_data)[2]== 'SR')
  {
    if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
    #hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=sum)
    SR_day <- lapply(days_aux, SR_hourlytodaily)
    SR_names <- names(SR_day)
    hours_day <- data.frame(SR_names, as.double(unlist(SR_day)))
  }
  
  if(split_name (weather_data)[2]== 'RH')
  {
    if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
    hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=mean)
  }
  
  if(split_name (weather_data)[2]== 'TX')
  {
    if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
    hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=max)
  }
  
  if(split_name (weather_data)[2]== 'TM')
  {
    if((length(days$Value)==0)){stop('None day has minimum amount of data. The station is ', weather_data, ' with percentage is :',percentage)}
    hours_day <- aggregate(days$Value, by=list(Date=days$Date), FUN=min)
  }
  
  colnames(hours_day) <- c("Date","Value")
  
  
  name <- as.character(weather_data)
  weather_data <- paste0("..", "/", "AfterDailyControl_Data", "/", name )            
  write.table (hours_day, file = weather_data, row.names= FALSE, sep = "\t", col.names = TRUE)
  #setwd('..')
  #return(hours_day)
  
}

#Table_NA makes a table witn NA
#Arguments  - HourBreaks. Hours with breaks
#           - timedelay.  Time of reception signal
#return table with NA

Table_NA <- function(HourDecimal,timedelay)
{
  SeqNumbers <- list()
  for (i in 1:length(HourDecimal))
  { 
    
    if(i < length(HourDecimal))
    {
      SeqNumbers[[i]] <-  seq(HourDecimal[i],HourDecimal[i+1], by = timedelay)
    }
    
    
  }
  
  sequ_ini <- seq(0,HourDecimal[1], by = timedelay)
  sequ_final <- seq(HourDecimal[length(HourDecimal)],23.999, by = timedelay)
  listNumbers <- unlist(SeqNumbers)
  
  tabla_na <- c(sequ_ini, listNumbers, sequ_final)
  tabla_na <- unique(tabla_na)
  return(tabla_na)
}

#SR_hourlytodaily converts hourly data into daily data.
#Arguments  - Data_SR : Data SR per day
#           - Hour_Decimal: Hour in decimal
#           - k : elements for mobile average 
#Return     - Data of SR per day with units energy (Wh/m^2) 

#info http://www.trisolar.ca/Content/Insolation.php
#     http://solarelectricityhandbook.com/solar-irradiance.html


SR_hourlytodaily <- function (data_perday, k=3)
{
  #order
  data_perday <- data_perday[order(data_perday$Hour),]
  
  #Values
  data_SR <- data_perday$Value
  
  Hour_Decimal <- data_perday$HourDecimal
  
  #Fill values with moving average with 3 values
  data_SR <- na.ma(data_SR, k = k, weighting = "simple")
  
  
  #Area under curve
  values_SR <- data_SR
  hour_SR <- unlist(Hour_Decimal)
  AUC <- sum(diff(hour_SR)*rollmean(values_SR,2))
  
  #kWh/m^2/day 
  AUC <- AUC/1000
  
  #kWh/m^2/day to calories/cm^2/day
  #kWh/m^2/day = 859824 calories/m^2/day = 85.98 * calories/cm^2/day
  
  AUC <- AUC*85.98
  
  
  return(AUC)
}

#info_station compute the overall results after daily data 
#Arguments      file. Hourly data 
#               percentage. percentage for acceptable day
#               time. If data is hourly so time = 1. If data is daily so time = 0

info_station<- function(file, percentage, time, sepa )
{
  station_name <- split_name(file)[1]
  variable <- split_name(file)[2]
  
  read_file <- read.table(paste0(here::here(), "/AfterDailyControl_Data/",file), header = T)
  read_file$Value <- as.double(read_file$Value)
  read_file$Date  <- as.Date(as.character(read_file$Date ), format = "%Y-%m-%d" )
  
  
  days <- sort(read_file$Date)
  
  star_day<- days[1]
  end_day <- days[length(days)]
  
  numbe_days <- as.Date(as.character(end_day), format="%Y-%m-%d")-
    as.Date(as.character(star_day), format="%Y-%m-%d")
  
  numbe_days <- as.double(numbe_days)
  
  
  if(time == 1)
  {
    acceptable_days <- Check_Day_Station(file, percentage)
    acceptable_days <- length(acceptable_days)
    
    result <- data.frame(station_name, variable, star_day, end_day, numbe_days, acceptable_days, percentage)
  }
  if (time == 2)
  {
    
    result <- data.frame(station_name, variable, star_day, end_day)
  }
  
  
  return(result)
}



daily_control <- function (daily_restric, file, sepa, date_format )
{
  
  #Daily Restrictions
  daily_res <- daily_restric
  
  #Variable
  splitname <- split_name(file)
  variable <- splitname[2]  
  
  #ReadFile
  #convert_units <- function(weatherdata, date_format="%Y%m%d", typefile, sepa)
  read_file <- convert_units(weatherdata=file , date_format=date_format, sepa= sepa )
  

  
  
  
  if( anyNA(read_file$Date)== TRUE)
  {
    stop('There is a problem with orginal or input of date format: ', file)
  }
  
  if(variable == "RH")
  {
    
    
    file_naomit <- na.omit(read_file)
    mod <- lm(Value ~ ., data=file_naomit)
    cooksd <- cooks.distance(mod)
    umbral <- 9*mean(cooksd, na.rm=T)  
    index <- which(cooksd>umbral)
    values <- file_naomit[index, ]
    
    
    #Color 
    read_file$Colour <- "Standard"
    read_file$Colour[read_file$Value %in% values$Value]="Outliers"

    
    ggplot(read_file, aes(x=Date, y= Value, color= Colour)) + geom_point() + ggtitle(paste0("Grafica de Outliers","\n", splitname[1], "\n", "Variable ", variable)) + theme(plot.title = element_text(hjust = 0.5))
    ggsave(paste0(here::here(), "/Outliers/", splitname[1], "_", variable, ".pdf"))
    
    #Put NA
    read_file$Value[read_file$Value %in% values$Value] <- NA  
    
    #Delete column Colour
    read_file$Colour <- NULL 
    
    
    values_out <- which(read_file$Value < daily_res$RH[2] || read_file$Value > daily_res$RH[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "NE", ".txt")
    

    
    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }
    
  }
  
  if(variable == "TX")
  {
    
    file_naomit <- na.omit(read_file)
    mod <- lm(Value ~ ., data=file_naomit)
    cooksd <- cooks.distance(mod)
    umbral <- 9*mean(cooksd, na.rm=T)  
    index <- which(cooksd>umbral)
    values <- file_naomit[index, ]
    
    
    #Color 
    read_file$Colour <- "Standard"
    read_file$Colour[read_file$Value %in% values$Value]="Outliers"

    
    ggplot(read_file, aes(x=Date, y= Value, color= Colour)) + geom_point() + ggtitle(paste0("Grafica de Outliers","\n", splitname[1], "\n", "Variable ", variable)) + theme(plot.title = element_text(hjust = 0.5))
    ggsave(paste0(here::here(), "/Outliers/", splitname[1], "_", variable, ".pdf"))
    
    
    #Put NA
    read_file$Value[read_file$Value %in% values$Value] <- NA
    
    
    #Delete column Colour
    read_file$Colour <- NULL 
    
    
    values_out <- which(read_file$Value < daily_res$TX[2] || read_file$Value > daily_res$TX[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "CD", ".txt")
    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }
    
  }
  
  
  if(variable == "TM")
  {
    
    
    file_naomit <- na.omit(read_file)
    mod <- lm(Value ~ ., data=file_naomit)
    cooksd <- cooks.distance(mod)
    umbral <- 9*mean(cooksd, na.rm=T)  
    index <- which(cooksd>umbral)
    values <- file_naomit[index, ]
    
    
    #Color 
    read_file$Colour <- "Standard"
    read_file$Colour[read_file$Value %in% values$Value]="Outliers"

    
    ggplot(read_file, aes(x=Date, y= Value, color= Colour)) + geom_point() + ggtitle(paste0("Grafica de Outliers","\n", splitname[1], "\n", "Variable ", variable)) + theme(plot.title = element_text(hjust = 0.5))
    ggsave(paste0(here::here(), "/Outliers/", splitname[1], "_", variable, ".pdf"))
  
    #Put NA
    read_file$Value[read_file$Value %in% values$Value] <- NA  
    
    #Delete column Colour
    read_file$Colour <- NULL 
    
    values_out <- which(read_file$Value < daily_res$TM[2] || read_file$Value > daily_res$TM[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "CD", ".txt")
    
    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }
    
  }
  
  if(variable == "SR")
  {
    
    
    file_naomit <- na.omit(read_file)
    mod <- lm(Value ~ ., data=file_naomit)
    cooksd <- cooks.distance(mod)
    umbral <- 9*mean(cooksd, na.rm=T)  
    index <- which(cooksd>umbral)
    values <- file_naomit[index, ]
    
    
    #Color 
    read_file$Colour <- "Standard"
    read_file$Colour[read_file$Value %in% values$Value]="Outliers"
    
    
    ggplot(read_file, aes(x=Date, y= Value, color= Colour)) + geom_point() + ggtitle(paste0("Grafica de Outliers","\n", splitname[1], "\n", "Variable ", variable)) + theme(plot.title = element_text(hjust = 0.5))
    ggsave(paste0(here::here(), "/Outliers/", splitname[1], "_", variable, ".pdf"))
    
    #Put NA
    read_file$Value[read_file$Value %in% values$Value] <- NA
    
    #Delete column Colour
    read_file$Colour <- NULL 
    
    
    values_out <- which(read_file$Value < daily_res$SR[2] || read_file$Value > daily_res$SR[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "CALCM2", ".txt")
    
    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }
    
  }
  
  if(variable == "P")
  {
    values_out <- which(read_file$Value < daily_res$P[2] || read_file$Value > daily_res$P[1])
    new_file <- paste0(splitname[1], "_", variable, "_", "MM", ".txt" )
    
    if(length(values_out)!=0)
    {
      read_file$Value[values_out] <- NA
      warning("There is a value out of limits ", read_file[values_out,] )
    }
    
  }
  
  
  #write.table(read_file, paste0("./AfterDailyControl_Data/", file), row.names = FALSE)
  write.table(read_file, paste0(here::here(), "/AfterDailyControl_Data/", new_file), row.names = FALSE)
  #write.table(read_file, file, row.names = FALSE)
  #write.table(read_file, file, row.names = FALSE)
  file.copy(from=new_file, to ="../AfterDailyControl_Data")
  
  
  
  
  return(read_file)
  
}

#check_amount_NA works counting number of NA per day if data is daily.
#Arguments    - File with data daily


check_amount_NA <- function(file, porcentage)
{
  #porcentage NA
  porcentage <- 1 - porcentage
  
  #Read table 
  table <- read.table(paste0("./Original_Data/", file), header = TRUE)
  
  #Count days
  number_length  <- as.numeric(as.Date(as.character(table$Date[length(table$Date)]), format = date_format)- as.Date(as.character(table$Date[1]), format = date_format))
  
  
  
  #Count the NA
  number_NA <- sum(is.na(table$Value))
  #number_length <- length(table$Value)
  
  if(number_NA/number_length > porcentage)
  {
    result <- data.frame(split_name(file)[1], split_name(file)[2])
    
  }
  
  else
  {
    result <- NULL
  }  
  
  return (result)
  
}


#Check_All_Station_NA works for choosing stations with meet with porcentage
#Arguments     - List of all stations 
#Porcentage    - porcentage amount of NA

Check_All_Station_NA  <- function (listfiles, porcentage)
{
  
  
  result <- lapply(listfiles, check_amount_NA, porcentage = porcentage)
  result <- result[!sapply(result, is.null)]
  final_results <- do.call("rbind", result)  
  colnames(final_results) <- c("Station_Name", "Variable_Name")
  
  write.table(final_results, "./Results/Stations_Delete.txt", row.names = FALSE)
  return (final_results)
}


#Choose_stations_Daily chooses station with meets the condition NA
#Arguments    - file
#             - names_station
Choose_station_Daily <- function(file, names_station)
{
  if(!any(split_name(file)[1] %in% names_station ))
  {
    file.copy(from=file, to ="../AfterDailyControl_Data")
  }
  
}

#few_NA choose stations with few NA and it could use moving average
#Arguments          -file
#Percentaje         -percentage minimun
few_NA <- function (file, percentage)
{
  #Read table 
  table <- read.table(paste0("./Original_Data/", file), header = TRUE)
  
  
  #Count the NA
  number_NA <- sum(is.na(table$Value))
  number_length <- length(table$Value)
  
  if(number_NA/number_length < percentage)
  {
    result <- data.frame(split_name(file)[1], split_name(file)[2])
    
  }
  
  else
  {
    result <- NULL
  }  
  
  return (result)
  
}

#Check_All_Station_Few_NA works for knowing which stations have few NA.  
#listfiles      - list of files 
#               - percentage of missing values.
Check_All_Station_Few_NA  <- function (listfiles, percentage)
{
  
  
  result <- lapply(listfiles, few_NA, percentage = percentage)
  result <- result[!sapply(result, is.null)]
  final_results <- do.call("rbind", result)  
  colnames(final_results) <- c("Station_Name", "Variable_Name")
  write.table(final_results, "./Results/Stations_Few_NA.txt")
  return (final_results)
}


#clusterstations_longlat is for doing the cluster of stations according to latitude and longitude
#parameters       -file. It has information of latitude and longitute of each station.
#
#return           -clusters of nearest stations  

#Results_DailyControl.csv

clusterstations_long_lat <- function(file)
{
  
  #Read the file Daily Information
  info_station <- read.csv(paste0(getwd(), "/Results/", file), header = T)
  
  #latitude and longitude  
  table <- subset(info_station, Variable_Name == 'P')
  table <- table[,c("Station_Name", "Latitude", "Longitude", "Altitude")]
  table <- as.data.frame(table)
  
  #Convert data to a SpatialPointsDataFrame object
  xy <- SpatialPointsDataFrame(matrix(c(table$Latitude ,table$Longitude), ncol=2), data.frame(Station_Name = table$Station_Name), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
  
  #use the distm for geodesic distance matrix
  mdist <- distm(xy)
  
  hc <- hclust(as.dist(mdist), method="complete")
  
  pdf("/Graphics/Cluster_Stations/Cluster.pdf")
  dev.off()
  
  return (xy)
  
}


#time_step calculates arrival time of signal to station.
#-Arguments: weatherdata = data per day with columns Date, Hour, Value and HourDecimal. This is output of put_format
#          : unit_hour = return hour format.  minutes = "mins", seconds= "secs". Default units = "mins" 
#-Return: delay time in minutes



time_step <- function(weatherdata, unit_hour = "mins")
{
  
  #The arrival time of signal is time more frequently of arrivals time.
  #Each time arrival is equal to diffence between t_i - t_i-1.
  
  weather_data <- as.data.frame(weatherdata)
  colnames(weather_data) <- c("Date","Hour","Value", "HourDecimal")
  
  
  #Convert Hour to Decimal
  decimal_hour <- as.difftime(weather_data$Hour, format="%H:%M:%S", units= unit_hour)
  
  
  #Time arrival of each signal 
  time_arrival <- diff(decimal_hour)
  
  #If there is only one data per day.
  if(length(time_arrival)==0)
  {
    time_more_frequ <- 0
  }
  else
  {
    #Time more frequently
    #frecuen <- sort(table(time_arrival), drecreasing = TRUE)
    time_more_frequ <- as.numeric(names(which.max((table(time_arrival)))))
    
    #time_more_frequ <- as.numeric(names(frecuen)[1])
  }
  return(time_more_frequ)
}  




#hour_solarnoon_sunrise function calculates solar noon and sunrise hour according to position (latitude and longitude) and day
#-Arguments: day, lat: latitude, long: longitude, timezo: time zone (ID) see OlsonNames(), typeofhour = "sunrise" or typeofhour ="solarnoo". 
#-Return:  A list with hour solarnoo and sunrise.


hour_solarnoon_sunrise <- function (day, lat, long, timezo, typeofhour) 
{
  #format
  long <- long[1]
  lat <- lat[1]
  timezo <- as.character(timezo)[1]
  day <- as.character(day)
  
  #Calculates sunrise and soloarnoon hour
  portsmouth <- matrix(c(long, lat), nrow=1)
  for_date <- as.POSIXct(day)
  time_sunrise <- sunriset(portsmouth, for_date, direction="sunrise", POSIXct.out=TRUE)
  hour_suns <- as.POSIXlt(time_sunrise$time, timezo)
  
  
  time_solarnoon <- solarnoon(portsmouth, for_date, POSIXct.out=TRUE)
  hour_noon <- as.POSIXlt(time_solarnoon$time, timezo)
  
  
  #Return hour
  hour_suns <-strsplit(as.character(hour_suns), split=" ", fixed = TRUE)[[1]][2]
  hour_noon <-strsplit(as.character(hour_noon), split=" ", fixed = TRUE)[[1]][2]
  
  if(typeofhour == "sunrise")
  {
    result_hour <- hour_suns
  }
  
  if(typeofhour == "solarnoon")
  {
    result_hour <- hour_noon
  }
  
  
  return(result_hour)
  
}

#The put_formatfunction  has two objectives. The first is to check that the file has a correct 
#format name nombredelaestacion_variable e.g. 12313_P.txt. 
#The second is to put format the variables, as follows: DATE tipo day, Hour tipo hora, Value tipo double.

#-Argument: is weather file.
#           typefile = Specific Folder
#           sepa = separator of file 
#-Return: weather file with format

#convert_units <- function(weatherdata, date_format="%Y%m%d", typefile, sepa)
put_format<- function(originalfile, date_format="%Y%m%d", sepa)
{
  
  #Check format name file. The name is composed by two parts. The first is 
  #name station that has only numbers, and the second name variable.
  split_name <- split_name(originalfile)
  
  
  #Those are variables for weather data.
  variablesnames <- c("TX","TM","RH","SR","P")
  
  #Those are units for variables.
  #CD = Celsius Degree
  #FD = Falherein Degree
  #MM = Milliliters 
  #NE = A number between 0 and 100
  #WAM2 = Watts per meter square
  #MJM2 = Megajoules per meter suare
  #CALCM2 = Calories per meter square
  
  variablesunits = c("CD", "FD","MM", "NE", "WAM2","MJM2", "CALCM2", "KWHM2", "WHM2") 
  
  if(all(str_detect(variablesnames, fixed(split_name[2]))== FALSE)== TRUE){stop('Not valid variable name : ', originalfile)}
  if(all(str_detect(variablesunits, fixed(split_name[3]))== FALSE)== TRUE){stop('Not valid unit : ', originalfile)}
  
  
  #Read file
  fileoriginal <- read.table(paste0(here::here(),"/Original_Data/",originalfile), header= TRUE, sep=sepa)
  
  
  #Check if file is daily or hourly
  #if it has two columns it is daily 
  #if it has three columns it is hourly
  
  #Define the variables names for hourly and daily
  variablesnames_hourly <- c("Date", "Hour", "Value")
  variablesnames_daily <- c("Date", "Value")
  
  # Check if the file has the correct number of columns and their names
  if(ncol(fileoriginal)==2)
  {
    
    if(all(colnames(fileoriginal) == variablesnames_daily)== FALSE){stop('There is a problem with name of columns :', originalfile)}
    
  }
  
  if(ncol(fileoriginal)==3)
  {
    
    if(all(colnames(fileoriginal) == variablesnames_hourly)== FALSE){stop('There is a problem with name of columns :', originalfile)}
    
    fileoriginal$HourDecimal  <- lapply(fileoriginal$Hour, function (x) hour_to_number (x))
    
    #Control of the hour format: if am or pm are detected, 
    #the code converts the hour to 24h format. The target format includes seconds.
    
    if(any(grepl("m",fileoriginal$Hour)))
    {
      fileoriginal$Hour <- format(strptime(fileoriginal$Hour,"%I:%M %p"),'%H:%M:%S')
    }
    
    else
    {
      #To convert 24 hour format  
      fileoriginal$Hour <- format(strptime(fileoriginal$Hour,"%H:%M"),'%H:%M:%S')
      
    }
  }
  
  
  fileoriginal$Date <- as.Date(as.character(fileoriginal$Date), format= date_format)
  fileoriginal$Value <- as.double(as.character(fileoriginal$Value))
  
  
  fileoriginal <- as.data.frame(fileoriginal)
  return(fileoriginal)
}



#The convert_units function converts units of the original data set to standard units. 
#-Arguments: weather data
#-Return: weather data with standerized units
#lapply(list.files(here("Original_Data")), daily_control, daily_restric = Daily_restric, typefile = 1, sepa = separt, date_format = date_format )
convert_units <- function(weatherdata, date_format="%Y%m%d", sepa)
{
  #Read file
  data <- put_format(weatherdata,date_format, sepa)
  
  #Extract variable names and units
  split_name <- split_name(weatherdata) 
  
  #SR 
  if(split_name[3]=='MJM2')
  {
    data$Value <- data$Value*23.88
  }
  
  #Temperatures: target units is celcius degrees
  if(split_name[3]=='FD')
  {
    data$Value <- (data$Value-32)/1.8
  }
  
  #If the units are kWHM2 (kilowatts per meter square) 
  if(split_name[3]=='KWHM2')
  {
    data$Value <- (1000*data$Value)*0.0858
  }
  
  #If the units are WHM2 (watts per meter square) 
  if(split_name[3]=="WHM2")
  {
    data$Value <- (data$Value)*0.0858
  }
  return(data)
  
}

#Read files of HR and SR
#Arguments listfiles is a list of files with pattern txt.
read_files <- function (listfiles)    
{
  
  files_merge <- lapply(listfiles,merge_tables)
  names_filemerge <- lapply(listfiles, function(x) {names = split_name(x) 
  return (names)})
  names(files_merge) <- names_filemerge
  variable_names <- names(files_merge)
  
  save_files <- lapply(seq_along(files_merge), function(y,n,i){
    name <- n[[i]]
    write.table(y[[i]], file = paste0("../Final_Data/", name, ".txt" ))
  }, y=files_merge, n=names(files_merge))
  
}



merge_tables <- function(namefile)
{
  #incomplete name
  file <- read.table(namefile, header= TRUE) 
  file$SR <- NULL
  file$RH <- NULL
  
  
  
  #File SR
  filename <- split_name(namefile)
  files_SR <- read.table(paste0("./SR/",filename, "_SR.txt"), header= TRUE)
  names(files_SR) = c("Date", "SR" ) 
  files_SR$Date <- NULL
  
  #File RH
  filename <- split_name(namefile)
  files_RH <- read.table(paste0("./RH/",filename, "_RH.txt"), header= TRUE)
  names(files_RH) = c("Date", "RH" ) 
  files_RH$Date <- NULL
  
  
  
  file_total <- cbind(file, files_SR, files_RH)                                                         
  
  return(file_total)
  
  
}


#match_files merge the files according their names
#Arguments      -listFiles list of files

match_files  <- function(type)
{
  
  slitingnames <- split_name(list.files(here::here("Rmawgen", "Files_By_Station")))
  length <- seq(1, length(slitingnames ), by = 3)
  names_stations <- unique(split_name(list.files(here::here("Rmawgen", "Files_By_Station")))[length])
  
  files  <- lapply(names_stations, merge_files, listfiles = list.files(here::here("Rmawgen", "Files_By_Station")), type = type)
  return (files)
  
}

#merge_files merges files according to names
merge_files <- function (name, listfiles, type)
{
  name <- as.character(name)
  
  files <-  list.files(here::here("Rmawgen", "Files_By_Station"), pattern= name)
  read_files <- lapply(files,label_for_files)
  tablaPerday <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = "Date", all = TRUE),read_files)
  
  namec <- paste0(name,".txt" )
  
  if(type == "RandomForest")
  {
    write.table(tablaPerday, file = paste0(here::here("RandomForest"), "/", namec), row.names = FALSE, quote = FALSE, col.names = TRUE)
  }
  if(type == "Final_Data")
  {
    write.table(tablaPerday, file = paste0(here::here("Final_Data", namec)), row.names = FALSE, quote = FALSE, col.names = TRUE)
  }
  
  return(tablaPerday)
}

#label_for_files puts label a files.

label_for_files <- function (name)
{
  file <- read.table(paste0(here::here("Rmawgen", "Files_By_Station"), "/", name), header=T)
  variable <-  split_name(name)[2]
  names(file) <- c("Date", variable)
  
  return(file) 
} 

#move_files_SR_HR works for copying data from AfterDailyControl_Data to  Files_By_Station

move_files_SR_HR <- function ()
{
  
  list_files_SR <- list.files(here::here("AfterDailyControl_Data"), pattern ="_SR_")
  list_files_RH <- list.files(here::here("AfterDailyControl_Data"), pattern ="_RH_")
  
  
  for  (i in 1: length(list_files_SR))
  {
    path_from <- paste0(here::here("AfterDailyControl_Data"), "/", list_files_SR[i])
    path_to <- paste0(here::here("Rmawgen", "Files_By_Station"), "/",list_files_SR[i])
    file.copy(path_from, path_to)
    
  }
  
  for  (i in 1: length(list_files_RH))
  {
    name_comp <- list_files_RH[i]
    path_from <- paste0(here::here("AfterDailyControl_Data"), "/", list_files_RH[i])
    path_to <- paste0(here::here("Rmawgen", "Files_By_Station"), "/",list_files_RH[i])
    file.copy(path_from, path_to)
    
  }
  
}




#The split_name function works split name file
#-Argument: name file
#-Return: split of name file 

split_name <- function(filename)
{
  split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
  split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )
  
  return(split_name)
}



#read_files chooses and reads files according to variable desired.
#Arguments     -files. List of files
#              -variable. Variable desired
#Return        -List of files 
#

read_files_form <- function (files, variable)
{
  #Check name file
  vari <- variable
  
  #Start and End Data
  file <- read.csv(paste0(here::here(),"/Results/","Results_DailyControl.csv"), header = T)
  file$Star_Data <- as.Date(as.character(file$Star_Data), format = "%Y-%m-%d")
  Start_date<- min(as.double(format(file$Star_Data, "%Y")))
  Start_date <- paste0(Start_date, "-1-1")
  
  
  file$End_Data <- as.Date(as.character(file$End_Data), format = "%Y-%m-%d")
  End_date<- max(as.double(format(file$End_Data, "%Y")))
  End_date <- paste0(End_date, "-12-31")
  
  if(split_name(files)[2] == vari)
  {
    #Read file
    namefile <- paste0(getwd(), "/AfterDailyControl_Data/", files)
    read_file <- read.table(namefile, header = T)
    
    #read_file <- read.table(files, header = T, sep = sepa )
    
    
    #Change colnames
    colnames(read_file) <- c("Date", as.character(split_name(files)[1]))
    read_file$Date <- as.Date(read_file$Date, format = "%Y-%m-%d")
    
    #Values NA
    seq_days <- seq(as.Date(Start_date), as.Date(End_date), by="days")
    tableALL <- data.frame(seq_days) 
    colnames(tableALL) <- c("Date")
    text_file <- merge (tableALL, read_file, all.x = TRUE)
    
    
  }
  else
  {
    text_file <- NULL
  }
  
  return(text_file)
}

#put_rmawgenformat puts to file with rmawgen format
#Arguments:       -files= List of files 
#                 -vari = variable match




put_rmawgenformat <- function(files, vari)
{
  
  #Read files
  files_reading <- lapply(files, read_files_form, variable = vari)
  
  #Remove Null
  files_reading = files_reading[-which(sapply(files_reading, is.null))]
  
  #Merge Columns
  merge_all <- do.call("cbind", files_reading)
  position_dates <- which(colnames(merge_all)== 'Date')
  merge_all <- merge_all[-position_dates[-1]]
  
  
  merge_all$year <- sapply(strsplit(as.character(merge_all$Date),'-'), "[", 1)
  merge_all$month <- sapply(strsplit(as.character(merge_all$Date),'-'), "[", 2)
  merge_all$day <- sapply(strsplit(as.character(merge_all$Date),'-'), "[", 3)
  merge_all$Date <- NULL
  
  #Ordercolumns
  merge_all<-merge_all[,c(length(merge_all), length(merge_all)-1, length(merge_all)-2, rep(1:(length(merge_all)-3)))]  
  
  name <- paste(vari, ".csv", sep="")
  weather_data <- paste0(".", "/", "Rmawgen", "/", name )            
  #write.csv(merge_all, file = paste(vari, ".csv", sep=""), row.names=FALSE)
  write.csv(merge_all, file = weather_data, row.names=FALSE)
}



#choose_stations chooses stations for applying rmwagen

choose_stations <- function()
{
  
  #Clustering for stations
  #Read file with longitude and latitude
  file_long_lat <- read.csv(paste0(here::here(),"/Results/","Results_DailyControl.csv"), header = T)
  
  #Read value of distance clustering
  dist_est <- read.csv(paste0(here::here(),"/SpatialInformation_InputVariables/","Input_Variables.csv"), header = T)
  
  
  file_station <- file_long_lat[, -c(4:7)]
  file_station <- unique(file_station) 
  
  long <- file_station$Longitude
  lati <- file_station$Latitude
  
  if(is.na(long)== TRUE || is.na(lati)== TRUE)
  {
    
    warning("Update longitud and latitude stations in the Information_Spatial_Stations file  in the folder SpatialInformation_InputVariables. Then run line 176")
  }
  
  else
  {
    long_lati <- SpatialPointsDataFrame(
      matrix(c(long,lati), ncol=2), data.frame(Station_Name=file_station$Station_Name),
      proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
    
    mdist <- distm(long_lati)
    hc <- hclust(as.dist(mdist), method="complete")
    
    
    # define the distance threshold, in this case 10000 m or 10Km
    d=dist_est$Distance_Cluster_Station
    
    # define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
    long_lati$clust <- cutree(hc, h=d)
    
    
    # expand the extent of plotting frame
    long_lati@bbox[] <- as.matrix(extend(extent(long_lati),0.001))
    
    # get the centroid coords for each cluster
    cent <- matrix(ncol=2, nrow=max(long_lati$clust))
    for (i in 1:max(long_lati$clust))
      # gCentroid from the rgeos package
      cent[i,] <- gCentroid(subset(long_lati, clust == i))@coords
    
    # compute circles around the centroid coords using a 40m radius
    # from the dismo package
    ci <- circles(cent, d=d, lonlat=T)
    
    
    jpeg(paste0(here::here(), "/Graphics/Clustering_Stations/Clustering_Stations.jpeg"))
    plot(ci@polygons, axes=T, main = paste("Clustering Stations to", "\n", d, " meters") )
    plot(long_lati, col=rainbow(4)[factor(long_lati$clust)], add=T)
    dev.off()
    
    
    #Start and end data 
    data_star_end <- file_long_lat[,c("Station_Name", "Star_Data", "End_Data","Variable_Name")]
    data_star_end  <- unique(data_star_end )
    total <- merge(long_lati@data, data_star_end, by= "Station_Name", all.x= TRUE)
    write.csv(total, paste0(here::here(), "/Results/Clustering_Stations.csv"))
    
  }
  
  
}  


#extract_names_data extracts names, star data and end data. 
#Arguments  -Station per year
extract_names_data <- function (stations)
{
  #Date minimun and maximun for station groups
  date_min <- stations$Star_Data[order(stations$Star_Data)][1]
  date_max <- stations$End_Data[order(stations$End_Data)][length(stations$End_Data)]
  
  #Station names
  station_names <- stations$Station_Name
  
  #Data frame
  group <- data.frame(date_min, date_max, station_names)
  colnames(group) <- c("date_min","date_max","station_names") 
  #FSFSAS
  
  return(group)
}



# Information about time zone https://en.wikipedia.org/wiki/List_of_tz_database_time_zones

#The split_name function works split name file
#-Argument: name file
#-Return: split of name file 

split_name <- function(filename)
{
  split_name <- unlist(strsplit(filename, split='_', fixed=TRUE))
  split_name <- gsub("txt","",gsub("[[:punct:]]","",split_name) )
  
  return(split_name)
}


#The put_formatfunction  has two objectives. The first is to check that the file has a correct 
#format name nombredelaestacion_variable e.g. 12313_P.txt. 
#The second is to put format the variables, as follows: DATE tipo day, Hour tipo hora, Value tipo double.

#-Argument: is weather file.
#           typefile = Specific Folder
#           sepa = separator of file 
#-Return: weather file with format

#convert_units <- function(weatherdata, date_format="%Y%m%d", typefile, sepa)
put_format<- function(originalfile, date_format="%Y%m%d", sepa)
{
  
  #Check format name file. The name is composed by two parts. The first is 
  #name station that has only numbers, and the second name variable.
  split_name <- split_name(originalfile)
  
  
  #Those are variables for weather data.
  variablesnames <- c("TX","TM","RH","SR","P")
  
  #Those are units for variables.
  #CD = Celsius Degree
  #FD = Falherein Degree
  #MM = Milliliters 
  #NE = A number between 0 and 100
  #WAM2 = Watts per meter square
  #MJM2 = Megajoules per meter suare
  #CALCM2 = Calories per meter square
  
  variablesunits = c("CD", "FD","MM", "NE", "WAM2","MJM2", "CALCM2", "KWHM2", "WHM2") 
  
  if(all(str_detect(variablesnames, fixed(split_name[2]))== FALSE)== TRUE){stop('Not valid variable name : ', originalfile)}
  if(all(str_detect(variablesunits, fixed(split_name[3]))== FALSE)== TRUE){stop('Not valid unit : ', originalfile)}
  
  
  #Read file
  fileoriginal <- read.table(paste0(here::here(),"/Original_Data/",originalfile), header= TRUE, sep=sepa)
  
  
  #Check if file is daily or hourly
  #if it has two columns it is daily 
  #if it has three columns it is hourly
  
  #Define the variables names for hourly and daily
  variablesnames_hourly <- c("Date", "Hour", "Value")
  variablesnames_daily <- c("Date", "Value")
  
  # Check if the file has the correct number of columns and their names
  if(ncol(fileoriginal)==2)
  {
    
    if(all(colnames(fileoriginal) == variablesnames_daily)== FALSE){stop('There is a problem with name of columns :', originalfile)}
    
  }
  
  if(ncol(fileoriginal)==3)
  {
    
    if(all(colnames(fileoriginal) == variablesnames_hourly)== FALSE){stop('There is a problem with name of columns :', originalfile)}
    
    fileoriginal$HourDecimal  <- lapply(fileoriginal$Hour, function (x) hour_to_number (x))
    
    #Control of the hour format: if am or pm are detected, 
    #the code converts the hour to 24h format. The target format includes seconds.
    
    if(any(grepl("m",fileoriginal$Hour)))
    {
      fileoriginal$Hour <- format(strptime(fileoriginal$Hour,"%I:%M %p"),'%H:%M:%S')
    }
    
    else
    {
      #To convert 24 hour format  
      fileoriginal$Hour <- format(strptime(fileoriginal$Hour,"%H:%M"),'%H:%M:%S')
      
    }
  }
  
  
  fileoriginal$Date <- as.Date(as.character(fileoriginal$Date), format= date_format)
  fileoriginal$Value <- as.double(as.character(fileoriginal$Value))
  
  
  fileoriginal <- as.data.frame(fileoriginal)
  return(fileoriginal)
}


#The convert_units function converts units of the original data set to standard units. 
#-Arguments: weather data
#-Return: weather data with standerized units
#lapply(list.files(here("Original_Data")), daily_control, daily_restric = Daily_restric, typefile = 1, sepa = separt, date_format = date_format )
convert_units <- function(weatherdata, date_format="%Y%m%d", sepa)
{
  #Read file
  data <- put_format(weatherdata,date_format, sepa)
  
  #Extract variable names and units
  split_name <- split_name(weatherdata) 
  
  #SR 
  if(split_name[3]=='MJM2')
  {
    data$Value <- data$Value/23.88
  }
  
  #Temperatures: target units is celcius degrees
  if(split_name[3]=='FD')
  {
    data$Value <- (data$Value-32)/1.8
  }
  
  #If the units are kWHM2 (kilowatts per meter square) 
  if(split_name[3]=='KWHM2')
  {
    data$Value <- (1000*data$Value)*0.0858
  }
  
  #If the units are WHM2 (watts per meter square) 
  if(split_name[3]=="WHM2")
  {
    data$Value <- (data$Value)*0.0858
  }
  return(data)
  
}


#time_step calculates arrival time of signal to station.
#-Arguments: weatherdata = data per day with columns Date, Hour, Value and HourDecimal. This is output of put_format
#          : unit_hour = return hour format.  minutes = "mins", seconds= "secs". Default units = "mins" 
#-Return: delay time in minutes


time_step <- function(weatherdata, unit_hour = "mins")
{
  
  #The arrival time of signal is time more frequently of arrivals time.
  #Each time arrival is equal to diffence between t_i - t_i-1.
  
  weather_data <- as.data.frame(weatherdata)
  colnames(weather_data) <- c("Date","Hour","Value", "HourDecimal")
  
  
  #Convert Hour to Decimal
  decimal_hour <- as.difftime(weather_data$Hour, format="%H:%M:%S", units= unit_hour)
  
  
  #Time arrival of each signal 
  time_arrival <- diff(decimal_hour)
  
  #If there is only one data per day.
  if(length(time_arrival)==0)
  {
    time_more_frequ <- 0
  }
  else
  {
    #Time more frequently
    #frecuen <- sort(table(time_arrival), drecreasing = TRUE)
    time_more_frequ <- as.numeric(names(which.max((table(time_arrival)))))
    
    #time_more_frequ <- as.numeric(names(frecuen)[1])
  }
  return(time_more_frequ)
}  

#The limits_TXTM_RH function works for creating limits from input file. 
#-Arguments: restricfile = restriction file 
#          : weatherdata = original data.
#-Return: weather data with limits.

limits_TXTM_RH  <- function(restricfile, weatherdata)
{
  #Read restriction file
  #data_restri <- read.csv(restricfile)
  data_restri <- restricfile
  
  #Read weather data
  data_weather <- convert_units(weatherdata) 
  
  
  #Delete NA
  data_weather <- delete_NA(data_weather, "Value")
  
  #Extract variable names 
  split_name <- split_name(weatherdata) 
  
  
  #TX
  if(split_name[2]=='TX')
  {
    data_weather <- data_weather[data_weather$Value >= data_restri$TX[2] & data_weather$Value <= data_restri$TX[1], ]
  }
  
  #TM
  if(split_name[2]=='TM')
  {
    data_weather <- data_weather[data_weather$Value >= data_restri$TM[2] & data_weather$Value <= data_restri$TM[1], ]
  }
  
  #RH
  if(split_name[2]=='RH')
  {
    data_weather <- data_weather[data_weather$Value>= data_restri$RH[2] & data_weather$Value<=data_restri$RH[1], ]
  }
  
  return(data_weather)
  
}

#limits_SR works puts the limits for solar irridance (SR) per hour.
#-Arguments: SR data,  restriction file.
#-Return: SR with limits.


limits_SR <- function (weatherdata, restricfile)
{
  #Extract data with units
  data_SR  <- convert_units (weatherdata)
  
  
  #Delete row with NA in Value  
  data_SR <- delete_NA(data_SR, "Value")
  
  #Extract Longititude, Latitude and Time Zone
  #data_Lon_Lati <- read.csv(restricfile)
  data_Lon_Lati  <- restricfile
  
  #Divide file per days
  divi_day <- divide_by_day(data_SR)
  
  #A list with date day 
  divi_day <- unique(names(divi_day))
  
  #Sunrise hours
  sunrise_hours <- lapply (divi_day, hour_solarnoon_sunrise, lat = data_Lon_Lati$LAT, long = data_Lon_Lati$LONG, timezo = data_Lon_Lati$TZ, "sunrise")  
  
  #Solarnoon hours
  solarno_hours <- lapply (divi_day, hour_solarnoon_sunrise, lat = data_Lon_Lati$LAT, long = data_Lon_Lati$LONG, timezo = data_Lon_Lati$TZ, "solarnoon") 
  
  
  #Match  days with hour sunrise 
  hourshine_day <- data.frame(divi_day,unlist(sunrise_hours))
  colnames(hourshine_day) <- c("Date", "HourShine")
  hourshine_day$Date <- as.Date(as.character(hourshine_day$Date), format="%Y-%m-%d")
  data_with_shinehour <- merge(data_SR,hourshine_day, by = "Date", all= TRUE ) 
  
  
  #Match  days with hour hour_solarnoon and sunshine 
  hoursolar_noo <- data.frame(divi_day,unlist(solarno_hours))
  colnames(hoursolar_noo) <- c("Date", "HourNoon")
  hoursolar_noo$Date <- as.Date(as.character(hoursolar_noo$Date), format="%Y-%m-%d")
  data_hourshine_noonhour <- merge(data_with_shinehour,hoursolar_noo, by = "Date", all= TRUE ) 
  
  #Convert from hour to decimal
  data_hourshine_noonhour$HourShine <- lapply(data_hourshine_noonhour$HourShine, function (x) hour_to_number (x))
  data_hourshine_noonhour$HourNoon  <- lapply(data_hourshine_noonhour$HourNoon, function (x) hour_to_number (x))
  
  #Find maximun radiation solar per hour
  data_hourshine_noonhour$MaxRadiac <- mapply(function (h, k , x, y, z) parabola_per_day(h, k , x, y, z), h = data_hourshine_noonhour$HourNoon, k=data_Lon_Lati$SR[1], x=data_hourshine_noonhour$HourShine, y=data_Lon_Lati$SR[2], z=data_hourshine_noonhour$HourDecimal)
  
  #Find values meet condition that Value less than MaxRadiac
  data_hourshine_noonhour <- data_hourshine_noonhour[data_hourshine_noonhour$Value <= data_hourshine_noonhour$MaxRadiac, ]
  
  
  #Return data with limits
  
  data_hourshine_noonhour <- data_hourshine_noonhour[, c("Date", "Hour", "Value", "HourDecimal")]
  data_hourshine_noonhour$Hour <- unlist(data_hourshine_noonhour$Hour)
  
  data_hourshine_noonhour <- as.data.frame(data_hourshine_noonhour)
  
  return(data_hourshine_noonhour)
  
}


#hour_solarnoon_sunrise function calculates solar noon and sunrise hour according to position (latitude and longitude) and day
#-Arguments: day, lat: latitude, long: longitude, timezo: time zone (ID) see OlsonNames(), typeofhour = "sunrise" or typeofhour ="solarnoo". 
#-Return:  A list with hour solarnoo and sunrise.


hour_solarnoon_sunrise <- function (day, lat, long, timezo, typeofhour) 
{
  #format
  long <- long[1]
  lat <- lat[1]
  timezo <- as.character(timezo)[1]
  day <- as.character(day)
  
  #Calculates sunrise and soloarnoon hour
  portsmouth <- matrix(c(long, lat), nrow=1)
  for_date <- as.POSIXct(day)
  time_sunrise <- sunriset(portsmouth, for_date, direction="sunrise", POSIXct.out=TRUE)
  hour_suns <- as.POSIXlt(time_sunrise$time, timezo)
  
  
  time_solarnoon <- solarnoon(portsmouth, for_date, POSIXct.out=TRUE)
  hour_noon <- as.POSIXlt(time_solarnoon$time, timezo)
  
  
  #Return hour
  hour_suns <-strsplit(as.character(hour_suns), split=" ", fixed = TRUE)[[1]][2]
  hour_noon <-strsplit(as.character(hour_noon), split=" ", fixed = TRUE)[[1]][2]
  
  if(typeofhour == "sunrise")
  {
    result_hour <- hour_suns
  }
  
  if(typeofhour == "solarnoon")
  {
    result_hour <- hour_noon
  }
  
  
  return(result_hour)
  
}

#divide_by_day  function divides file y
#-Arguments: weather file with hourly limits .
#-Retun: list with date and data daily

divide_by_day <- function (weather_data)
{
  split_data <- split(weather_data, weather_data$Date)
  return(split_data)
}

#parabola_per_day function calculates point in the parabola.   
#-Arguments: vertex (h,k) = (vertex_h, vertex_k ) 
#            point  (x,y) = (pointsunrise_x, pointsunshine_y). pointsunshine_y should be zero. Irradiacion solar is zero at sunshine  
#            pointval
#-Retun: Maximum irradiation solar according to hour.


parabola_per_day <- function (vertex_h, vertex_k, pointsunshine_x, pointsunshine_y, pointvalu)
{
  # The parabola equation is y = a(x - h)^2 + k
  # a = (y - k)/(x - h)^2
  
  a <- (pointsunshine_y - vertex_k)/(pointsunshine_x - vertex_h)^2
  y <- a*(pointvalu- vertex_h)^2 + vertex_k
  
  #Values before sunrise and after sunset will be close to zero
  y[y<=0] <- 2
  
  return(y)  
}  


#limits_P  puts the limits for precipation (P) per hour.
#-Arguments: P data,  restriction file.
#-Return: P with limits.

limits_P <- function (weatherdata)
{
  #Extract data with units
  data_P  <- convert_units (weatherdata)
  
  
  data_P <- delete_NA(data_P, "Value")
  
  #Divided by day
  divi_day <- divide_by_day(data_P)
  names_day <- unique(names(divi_day))
  
  #Compute  time step
  delaytime <- lapply(divi_day, function (x) time_step (x))
  
  #Date with date per day 
  date_delay <- data.frame(names_day,unlist(delaytime))
  colnames(date_delay) <- c("Date", "Time_Step")
  date_delay$Date <- as.Date(as.character(date_delay$Date), format="%Y-%m-%d")
  data_delaytime <- merge(data_P,date_delay, by = "Date", all= TRUE ) 
  
  #Compute max precipitation 
  data_delaytime$Max_prec <- lapply(data_delaytime$Time_Step, function (x) 40*x^(log(25)/log(1440)))
  
  
  data_delaytime <- data_delaytime[data_delaytime$Value < data_delaytime$Max_prec, ]
  data_delaytime <- data_delaytime[, c("Date", "Hour", "Value", "HourDecimal", "Time_Step")]
  data_delaytime <- as.data.frame(data_delaytime)
  
  return(data_delaytime)
}

#delete_NA delete rows that contains NA in an specif column (Variable)
#Arguments: -weatherdata, original weather data
#           -Variable, varible name
#Return   : Data without NA in Variable name.

delete_NA <- function (weatherdata, Variable)
{
  
  index <- complete.cases(weatherdata[, Variable])
  weatherdata <- weatherdata[index, ]
  return(weatherdata)
}

plot_original_aftercleaning <- function (originaldata, aftercleaningdata, title, units_variable, location_legend)
{
  
  plot(originaldata$HourDecimal, originaldata$Value, ylim=range(c(originaldata$Value,aftercleaningdata$Value)), col= "red", main = title, xlab= "Horas", ylab = units_variable)
  par(new = TRUE)
  plot(aftercleaningdata$HourDecimal, aftercleaningdata$Value, ylim=range(c(originaldata$Value,aftercleaningdata$Value)), axes = FALSE, xlab = "", ylab = "", col= "blue")
  legend(location_legend, c("Datos_Limpios","Datos_Originales"),lty=1, col=c("blue","red"), bty='n', cex=.75)
  
}



#hour_to_number converts hours into number between 0 and 23
#-Arguments: hourformat: Hour in format character 
#-Return: A number between 0 and 23.
hour_to_number <- function(hourformat)
{
  hourformat <- as.character(hourformat)
  split_hour <- strsplit(hourformat,":")
  hora_number <- as.numeric(split_hour[[1]])
  hora_number <- as.numeric(split_hour[[1]][1])+as.numeric(split_hour[[1]][2])/60
  return(hora_number)
  
} 

#results function shows results after and before of hourly control
#-Arguments: originalfil.    Original Data. 
#          : aftercleaning.  Data after hourly control 
#-Return: result.  A vector with station name, variable name, size original data,
#                  and size data after hourly control    

results <- function(originaldata, restricfile, typefile, sepa)
{
  station_name <- split_name(originaldata)[1]
  variable_names <- split_name(originaldata)[2]
  
  #Size original data
  original_size <- length(put_format(originaldata,typefile, sepa))$Value
  
  #Extract Variable Name
  if(variable_names =='P')
  {
    aftercleaning <- limits_P(originaldata)
    aftercleaning <- aftercleaning[,c("Date", "Hour", "Value")]
    aftercleaning_length <- length(limits_P(originaldata)$Value)
    diference <- original_size -  aftercleaning_length
    
  }
  if(variable_names =='SR')
  {
    aftercleaning <- limits_SR(originaldata, restricfile)
    aftercleaning <- aftercleaning[,c("Date", "Hour", "Value")]
    aftercleaning_length <- length(limits_SR(originaldata, restricfile)$Value)
    diference <- original_size -  aftercleaning_length
    
  }
  
  if(variable_names =='TX' | variable_names =='TM' | variable_names =='RH')
  {
    aftercleaning <- limits_TXTM_RH(restricfile, originaldata)
    aftercleaning <- aftercleaning[,c("Date", "Hour", "Value")]
    aftercleaning_length <- length(limits_TXTM_RH(restricfile, originaldata)$Value)
    diference <- original_size -  aftercleaning_length
  }
  
  result <- data.frame(station_name, variable_names, original_size, aftercleaning_length, diference)
  #colnames(results) <- c("Station_Name", "Variable_Name", "OriginalData_Size", "CleanData_Size", "ErrorData_Size")
  name <- as.character(originaldata)
  originaldata <- paste0("..", "/", "AfterHourlyControl_Data", "/", name )
  
  write.table (aftercleaning, file = originaldata, row.names = FALSE, quote = FALSE, sep = "\t", col.names = TRUE)
  
  return(result)
  
}

#move_files_txt moves txt files according to path 
#Arguments:   from = origin path 
#               to = destination path  
move_files_txt <- function(from, to, format ="\\.txt$")
{
  
  files_txt <- list.files(path = from,pattern = format)
  file.copy(from = files_txt, to= to) 
  file.remove(files_txt) 
  
}


#Spatial_information works for populate latitude and longitude information of station

Spatial_Information <- function(files = list.files(here::here("Original_Data")))
{
  #Name station
  files <- split_name(list.files(here::here("Original_Data")))
  
  if(length(files)==0)
  {
    warning("Please put the original weather files in the Original_Data folder ")
  }
  
  else
  {
    seque <- seq(1, length(files), by=3)
    
    #Unique Stations
    uni_station <- unique(files[seque])
    Info_spatial <- data.frame(Station_Name = uni_station )
    Info_spatial$Latitude <- NA
    Info_spatial$Longitude <- NA
    Info_spatial$Altitude <- NA
    write.csv(Info_spatial, paste0(here::here(),"/SpatialInformation_InputVariables/Information_Spatial_Stations.csv"), row.names = FALSE)
    
  }
  
}



#graph_station plots graph per variable
#- Arguments.   Station_table 

graph_station <- function (Station_table, variable)
{
  
  #   #Parameters
  real_data <- Station_table$Real_Data
  estimated_data <- Station_table$Estimated_Data
  dates <- Station_table$Date
  name <- unique(Station_table$Station_Names)
  
  #Units
  if(variable == "Temperatura_Maxima" || variable == 'Temperatura_Minima')
  {
    y = "Grados_Centigrados"
  }
  
  if(variable == 'Precipitacion')
  {
    
    y = "Mililitros"
  }
  
  if(variable == 'Radiacion_Solar')
  {
    y = "Calorias_cm2_diarios"
  }
  
  if(variable == 'Humedad_Relativa')
  {
    y = "Valor"
  }
  
  #Total Data
  dates <- as.Date(dates, format = "%Y-%m-%d")
  
  #Count NA
  NAs <-  which((is.na(real_data=="NA"))==TRUE)
  real_data[NAs] <- estimated_data[NAs]
  
  
  real_dat <- cbind.data.frame(dates, real_data)
  estimated_dat <- cbind.data.frame(dates, estimated_data)
  
  colnames(real_dat) <- c("Dates", "Value")
  colnames(estimated_dat) <- c("Dates", "Value")
  
  #Variable for plot
  grafica <- real_dat
  grafica$Datos <- c(rep("Datos_Reales", nrow(real_dat)))
  grafica$Datos[NAs] <- c("Datos_Estimados")
  
  #Graph
  graph <- ggplot(data=grafica, aes(x=Dates, y=Value, col=Datos)) + geom_point() +ggtitle(paste0(name,"\n",variable)) + theme(plot.title = element_text(hjust = 0.5)) + ylab(y) + xlab("Dias")
  name_grap <- paste0(paste(name,variable, sep="_"),".pdf")
  nameFile <-  paste0(".", "/", "Graphics", "/", name_grap)
  ggsave(nameFile, plot=graph)
  
  
  if(variable == 'Temperatura_Maxima' )
  {
    
    namefile = "TX"
  }
  
  if(variable == 'Temperatura_Minima' )
  {
    
    namefile = "TM"
  }
  
  
  if(variable == 'Precipitacion')
  {
    
    namefile = "P"
  }
  
  if(variable == 'Radiacion_Solar')
  {
    namefile = "SR"
  }
  
  if(variable == 'Humedad_Relativa')
  {
    namefile = "RH"
  }
  
  #Plots for Random Forest
  
  if(namefile == "SR" )
  {
    name_file <- paste0(paste(name,namefile, "CALCM2",sep="_"),".txt")
    #weather_data <- paste0(".", "/", "SR", "/", name_file )   
    weather_data <- paste0(".", "/Rmawgen/", "Files_By_Station", "/", name_file )
  }
  else if (namefile == "RH")
  {
    name_file <- paste0(paste(name,namefile, "NE", sep="_"),".txt")
    #weather_data <- paste0(".", "/", "RH", "/", name_file )   
    weather_data <- paste0(".", "/Rmawgen/", "Files_By_Station", "/", name_file )
  } 
  else if (namefile == "P")
  {
    name_file <- paste0(paste(name,namefile, "MM",  sep="_"),".txt")
    weather_data <- paste0(".", "/Rmawgen/", "Files_By_Station", "/", name_file )   
    
  }
  else if (namefile == "TX" || namefile == "TM")
  {
    name_file <- paste0(paste(name,namefile, "CD",  sep="_"),".txt")
    weather_data <- paste0(".", "/Rmawgen/", "Files_By_Station", "/", name_file )   
  }
  
  
  
  write.table(real_dat, file = weather_data, row.names = FALSE, quote = FALSE, sep = "\t", col.names = TRUE)
  
}

#generate_missing_values <- function (listFiles, resumefile, variable)
#graph_ graphs all stations estimated and real data
#Arguments    -listFiles. List of files with format rmawgen 
#             -variable_rmw. variable for using in rmwagen
#             -variable_plot. variable for plot
#Return graphs

graph_all <- function(variable_rmw, variable_plot, choose_station)
{
  
  #Data
  data_all <- generate_missing_values (variable_rmw, choose_station)
  
  #Table per station
  #table_station <- lapply(data_all, table_graph)
  table_station <- table_graph(data_all)
  #lapply(table_station, function(x) lapply(x, graph_station, variable = variable_plot))
  #(Station_table, variable)
  #graph_station (table_station)
  lapply(table_station, graph_station, variable =  variable_plot)
  
}



#table_graph makes tables for plotting

table_graph <- function(list)
{
  
  #List with data
  data_real  <- list$real_data
  data_estimated <- list$estimated_data
  date <- list$date
  
  #Number of stations
  num_stations <- length(colnames(data_real)) 
  
  
  
  station_table <- list()
  name_station <- list()
  
  #Station 
  for ( i in 1 :num_stations)
  {
    name_station[[i]]  <- rep(colnames(data_real)[i], length(data_real[,i]))
    station_table[[i]] <- data.frame(date, data_real[,i], data_estimated[,i],name_station[[i]])
    names(station_table[[i]]) <- c("Date", "Real_Data", "Estimated_Data", "Station_Names")
    #star_date <- subset(info_station, Station_Name %in% colnames(data_real)[i])
    #star <- star_date$Star_Data
    #end <- star_date$End_Data
    #station[[i]]  <- subset(station[[i]], Date >= star & Date <= end )
    
  }    
  
  names(station_table) <- colnames(data_real)
  
  return (station_table)
}

#paste_columns paste three columns  three columns
paste_columns <- function(column_date, colum_real, colum_estima)
{
  table <- data.frame(column_date, colum_real, colum_estima)
  names(table) <- c("Date","Real_Value","Estimated_Value")
  
  return(table)
  
}


#generate_missing_values generates values of all stations using rmawgen.
#Arguments    -ListFiles. Lisf of files with format for rmawgen 
#             -resumefile. Lsit with resumen all stations 
#
#generate_missing_values(list.files(), list.files()[1],  "PRECIPITATION")
generate_missing_values <- function (variable, choose_station)
{
  
  #station_info <- choose_stations(resumefile)
  #names
  name_TX <- paste0(here::here(), "/Rmawgen/", "TX.csv")
  TEMPERATURE_MAX <- read.csv(name_TX, header=T, check.names = FALSE)
  
  name_TM <- paste0(here::here(), "/Rmawgen/", "TM.csv")
  TEMPERATURE_MIN <- read.csv(name_TM, header =T, check.names = FALSE)
  
  name_P <- paste0(here::here(), "/Rmawgen/", "P.csv")
  PRECIPITATION <- read.csv(name_P, header =T, check.names = FALSE)
  
  
  if(variable=='TEMPERATURE_MAX')
  {
    #Name files
    #generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=1, manual=manual, choose_station = choose_station, year_min, year_max)
    generator_values <-  applying_rmwagen_2(TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=1, choose_station = choose_station)
  }
  
  if(variable=='TEMPERATURE_MIN')
  {
    #generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=2, manual=manual, choose_station = choose_station)
    generator_values <-  applying_rmwagen_2(TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=2, choose_station = choose_station)
  }    
  
  if(variable=='PRECIPITATION')
  {
    
    #generator_values <- lapply(station_info, applying_rmwagen, TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=3, manual=manual, choose_station = choose_station)
    generator_values <-  applying_rmwagen_2(TEMPERATURE_MAX = TEMPERATURE_MAX, TEMPERATURE_MIN= TEMPERATURE_MIN, PRECIPITATION = PRECIPITATION, menu=3, choose_station = choose_station) 
    
  }
  
  
  return(generator_values)
}

#Applying rmawagen works rmwagen function 
#Arguments - info_station: dataframe with station name, start and  end date
#          - manual. This is way for matching station then using rmwagen.
#                    If manual is 1. It is automatic
#                    If manual is 2. User must enter stations for matching
#          - choose stations. Choose the stations for matching. It must be a vector

#graph_all (list.files(pattern = "\\.csv$"), "./Results/Results_DailyControl.csv", "TEMPERATURE_MAX", 'Temperatura_M?xima', manual = 2, choose_station = c(24,7))
applying_rmwagen <- function (info_station, TEMPERATURE_MAX, TEMPERATURE_MIN, PRECIPITATION, n_GPCA_iter = 10, n_GPCA_iteration_residuals =10, lag=2, p_prec = 3, p_test =2,menu, manual, choose_station, year_min, year_max)
{
  if (manual == 1)
  {
    #Arguments for rmwagen
    station <- as.vector(info_station$station_names)
    year_min <- as.Date(info_station$date_min, "%Y-%m-%d")
    year_max <- as.Date(info_station$date_max, "%Y-%m-%d")
    
  }  
  
  if(manual == 2)
  {
    #Read Station 
    #stations <- read.csv("./Results/Results_DailyControl.csv", header=TRUE)
    #stations <- read.csv(paste0(here(), "/Results/Clustering_Stations.csv"), header=TRUE)
    
    #stations <- subset(stations, Station_Name %in% c("11045010"))
    stations <- choose_station
  }
  
  #year_min <- as.numeric(format(year_min[1], "%Y"))
  #year_max <- as.numeric(format(year_max[1], "%Y"))
  
  year_min <- year_min
  year_max <- year_max
  #Start and End Data
  # Start_Data_Sta <- unique(year_min)
  # End_Data_Sta <- unique(year_max)
  
  
  All_data <- seq(as.Date(paste0(year_min,"-1-1")), as.Date(paste0(year_max, "-12-31")), by="days")
  
  
  
  
  
  generationTemperature <- ComprehensiveTemperatureGenerator(
    station=station,
    Tx_all=TEMPERATURE_MAX,
    Tn_all=TEMPERATURE_MIN,
    year_min=year_min,
    year_max=year_max,
    p=5,
    n_GPCA_iteration=n_GPCA_iter,
    n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
    sample="monthly"
  )
  
  if(menu == 1 )
  { 
    
    real_data <- generationTemperature$input$Tx_mes
    fill_data <-generationTemperature$out$Tx_gen
  } 
  
  if(menu == 2)
  {
    real_data <- generationTemperature$input$Tn_mes
    fill_data <-generationTemperature$out$Tn_gen
  }
  
  if(menu == 3)
  {
    #Error distribution. Check if precipitation distribuition is biased to zero.  
    median <- lapply(PRECIPITATION[station], median, na.rm =T)
    test <- lapply(median, function(x) { if (x < 1) {result <- TRUE} else {result <- FALSE}})
    
    if(any (test == TRUE))
    {
      warning("There is a problem with distribution of Precipitation. This is very biased to zero")
      PRECIPITATION[station] <- PRECIPITATION[station] + 4
      generation_prec <- ComprehensivePrecipitationGenerator(
        station=station,
        prec_all=PRECIPITATION,
        year_min=year_min,
        year_max=year_max,
        p= 3,
        n_GPCA_iteration= 10,
        n_GPCA_iteration_residuals= 0,
        sample = "monthly",
        no_spline = FALSE,
        nscenario = 20)
      
      real_data <- generation_prec$prec_mes - 4
      fill_data <- generation_prec$prec_gen - 4
      
    }
    
    else 
    {
      generation_prec <- ComprehensivePrecipitationGenerator(
        station=station,
        prec_all=PRECIPITATION,
        year_min=year_min,
        year_max=year_max,
        p= 3,
        n_GPCA_iteration= 10,
        n_GPCA_iteration_residuals= 0,
        sample = "monthly",
        no_spline = FALSE,
        nscenario = 20)
      
      real_data <- generation_prec$prec_mes
      fill_data <- generation_prec$prec_gen 
      
    }
    
    
    
  }
  
  result <- list(real_data= real_data, estimated_data = fill_data, date=All_data)
  
  return(result)
  
  
}




#Applying rmawagen works rmwagen function 
#Arguments - info_station: dataframe with station name, start and  end date
#          - manual. This is way for matching station then using rmwagen.
#                    If manual is 1. It is automatic
#                    If manual is 2. User must enter stations for matching
#          - choose stations. Choose the stations for matching. It must be a vector




applying_rmwagen_2 <- function (TEMPERATURE_MAX, TEMPERATURE_MIN, PRECIPITATION, n_GPCA_iter = 10, n_GPCA_iteration_residuals =10, lag=2, p_prec = 3, p_test =2, choose_station, menu)
{
  
  station <- choose_station
  
  #Read year max and min of input
  year_min <- as.Date(as.character(variables$Star_date), format="%Y-%m-%d")
  year_min <- as.numeric(format(year_min, format="%Y"))
  
  
  year_max <- as.Date(as.character(variables$End_date), format="%Y-%m-%d")
  year_max <- as.numeric(format(year_max, format="%Y"))
  
  
  All_data <- seq(as.Date(paste0(year_min,"-1-1")), as.Date(paste0(year_max, "-12-31")), by="days")
  
  
  
  
  if(menu == 1 )
  {
    generationTemperature <- tryCatch(ComprehensiveTemperatureGenerator(
      station=station,
      Tx_all=TEMPERATURE_MAX,
      Tn_all=TEMPERATURE_MIN,
      year_min=year_min,
      year_max=year_max,
      p=5,
      n_GPCA_iteration=n_GPCA_iter,
      n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
      sample="monthly"),
      error = function(e)
        if(e$message == "row names supplied are of the wrong length")
          
          stop(paste("The station :", station," doesnt have any information on maximum or minimum temperature \n")), 
      warning=function(w) 
        w)
    
    real_data <- generationTemperature$input$Tx_mes
    fill_data <-generationTemperature$out$Tx_gen
  }
  
  if(menu == 2)
  {
    generationTemperature <- tryCatch(ComprehensiveTemperatureGenerator(
      station=station,
      Tx_all=TEMPERATURE_MAX,
      Tn_all=TEMPERATURE_MIN,
      year_min=year_min,
      year_max=year_max,
      p=5,
      n_GPCA_iteration=n_GPCA_iter,
      n_GPCA_iteration_residuals=n_GPCA_iteration_residuals,
      sample="monthly"),
      error = function(e)
        if(e$message == "row names supplied are of the wrong length")
          
          stop(paste("The stations :", station," doesnt have any information on maximum or minimum temperature \n")), 
      warning=function(w) 
        w)
    real_data <- generationTemperature$input$Tn_mes
    fill_data <-generationTemperature$out$Tn_gen
  }
  
  if(menu == 3)
  {
    #Error distribution. Check if precipitation distribuition is biased to zero.
    median <- lapply(PRECIPITATION[station], median, na.rm =T)
    test <- lapply(median, function(x) { if (x < 1) {result <- TRUE} else {result <- FALSE}})
    
    
    if(length(station)==1)
    {
      PRECIPITATION['fake_1']=PRECIPITATION[station]
      PRECIPITATION['fake_2']=PRECIPITATION[station]
      pstation = c(station, c('fake_1','fake_2'))
      
    }
    
    else
    {
      pstation = station
    }
    
    
    
    if(any (test == TRUE))
    {
      warning("There is a problem with distribution of Precipitation. This is very biased to zero")
      PRECIPITATION[station] <- PRECIPITATION[station] + 4
      generation_prec <- tryCatch(ComprehensivePrecipitationGenerator(
        station=pstation,
        prec_all=PRECIPITATION,
        year_min=year_min,
        year_max=year_max,
        p= 3,
        n_GPCA_iteration= 10,
        n_GPCA_iteration_residuals= 0,
        sample = "monthly",
        no_spline = FALSE,
        nscenario = 20), error = function(e)
        {
          if(e$message == "zero non-NA points" || e$message == "missing value where TRUE/FALSE needed")
          {
            matrix_aux <- PRECIPITATION[, c(station, "year")]
            selected_dates <- subset(matrix_aux, year >= year_min & year <= year_max)
            result <- summary(selected_dates)
            print(as.matrix(result))
            stop(paste("Check the number of NA per station. There are a lot of NA respect to total days."))
          }
          
        }, 
        warning=function(w) 
          w
      )
      real_data <- generation_prec$prec_mes - 4
      fill_data <- generation_prec$prec_gen - 4
      
      if(length(station)==1) 
      {
        real_data <- generation_prec$prec_mes[,station, drop=FALSE] - 4
        fill_data <- generation_prec$prec_gen[,station, drop=FALSE] - 4
      }
      
    }
    
    else
    {
      generation_prec <- ComprehensivePrecipitationGenerator(
        station=station,
        prec_all=PRECIPITATION,
        year_min=year_min,
        year_max=year_max,
        p= 3,
        n_GPCA_iteration= 10,
        n_GPCA_iteration_residuals= 0,
        sample = "monthly",
        no_spline = FALSE,
        nscenario = 20)
      
      real_data <- generation_prec$prec_mes
      fill_data <- generation_prec$prec_gen
      
    }
    
    
    
  }
  


  
  
  
  
  
  result <- list(real_data= real_data, estimated_data = fill_data, date=All_data)
  
  return( result )
  
  
}
#ControlHourlyDaily works limits for variables.
#Arguments:  type = 1 if is hourly
#                 = 2 if is daily

controlHourlyDaily <- function(type)
{
  if(type == 1)
  {
    
    #Change directory Original_Data
    #Hourly Control
    #final_results <- mclapply(list.files(), results, restricfile = Hourly_restric ,mc.cores=20)
    final_results <- lapply(list.files(here::here("Original_Data")), results, restricfile = Hourly_restric, typefile =2, sepa = separt)
    
    #Results of Hourly Control
    final_results <- do.call("rbind", final_results)
    final_results$Latitude <- NA
    final_results$Longitude <- NA
    final_results$Altitude <- NA
    colnames(final_results) <- c("Station_Name", "Variable_Name", "OriginalData_Size", "CleanData_Size", "ErrorData_Size", "Latitude", "Longitude", "Altitude")
    write.csv(final_results, file = "../Results/Results_HourlyControl.csv")
    
    
    
    #Hourly to Daily
    #The percentage is for checking if a station has enough data per day.   
    #mclapply (list.files(pattern = "\\.txt$"), Hour_to_Day, percentage = 0.8,mc.cores=20)
    lapply (list.files(here::here("AfterHourlyControl_Data")), Hour_to_Day, percentage = Percentage)
    
    #Results Daily Control
    results <- lapply(list.files(), info_station, percentage=Percentage)
    final_results <- do.call("rbind", results)
    colnames(final_results) <- c("Station_Name", "Variable_Name", "Star_Data", "End_Data", "Total_Days", "Acceptable_Days","Percentage" )
    write.csv(final_results, file = paste0(here::here("Results"), "/Results_DailyControl.csv") )
    
  }
  
  if(type == 2)
  {
    
    #Daily Control NA
    names_stations_NA <- Check_All_Station_NA(list.files(path = "./Original_Data"), variables$Approved_percentage)
    names_stations_few_NA <- Check_All_Station_Few_NA(list.files(path = "./Original_Data"), 1)
    lapply(list.files(here::here("Original_Data")), daily_control, daily_restric = Daily_restric, sepa = separt, date_format = date_format )
    
    results <- lapply(list.files(path= "./AfterDailyControl_Data"), info_station, percentage= variables$Approved_percentage, sepa = variables$separt, time =2)
    final_results <- do.call("rbind", results)
    colnames(final_results) <- c("Station_Name", "Variable_Name", "Star_Data", "End_Data")
    
    #Station number
    lat_Lon_El <- read.csv(paste0(here::here(),"/SpatialInformation_InputVariables/","Information_Spatial_Stations.csv"))
    lat_Lon_El$Station_Name <- as.character(lat_Lon_El$Station_Name )
    final_results$Station_Name <- as.character(final_results$Station_Name)
    
    total <-merge(lat_Lon_El,final_results, by = c("Station_Name"), all =TRUE)
    write.csv( total, file = paste0(here::here(), "/Results/","Results_DailyControl.csv"), row.names = FALSE)
    
    
  }
  
}  



create_folders <- function (mainDir)
{ 
dir.create(file.path(mainDir, "Original_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Outliers"), showWarnings = FALSE)
dir.create(file.path(mainDir, "SpatialInformation_InputVariables"), showWarnings = FALSE)
dir.create(file.path(mainDir, "AfterHourlyControl_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "AfterDailyControl_Data"), showWarnings = FALSE)
dir.create(file.path(mainDir, "RandomForest"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Rmawgen"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Graphics"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Results"), showWarnings = FALSE)
dir.create(file.path(mainDir, "Final_Data"), showWarnings = FALSE)
mainDir <- paste0(mainDir,"/", "Rmawgen" )
dir.create(file.path(mainDir, "Files_By_Station" ), showWarnings = FALSE)
mainDir <- getwd()
mainDir <-  paste0(mainDir,"/", "Graphics" )
dir.create(file.path(mainDir, "Clustering_Stations"), showWarnings = FALSE)
}


#RANDOM FOREST

#
library(randomForest)

#subDir <- "Final_Files_HR"
#subDir <- "With_SR"
#subDir <- "Total_Final"


#dir.create(file.path(getwd(), subDir), showWarnings = FALSE)
#joinfiles join all files.

join_file <- function (name)
{
    
    name <- as.character(name)
    files <-  list.files(pattern= name)
    
    if(length(files) != 2)
    {
        stop("There are more file with same name",name)
    }
    else
    {
        variable <-  split_name(files[2])[2]
        
        if(variable == "PTXTM")
        {
            file_first <- read.table(files[2], header = TRUE)
            j <- 1
        }
        else
        {
            file_first <- read.table(files[1], header = TRUE)
            j <-2
        }
        
        file_second <- read.table(files[j], header = TRUE)
        file_second <- file_second[,-which(colnames(file_second)%in% c("TM", "TX", "P"))]
    }
    
    
    result <- merge(file_first, file_second, by= "Date", all= TRUE)
    
    namec <- paste0(name,".txt" )
    
    
    
}




#All files join all files. 

#lapply (names_estaciones, join_file)

#radom_forest fill missing values for missing values
random_forest_SR <- function(station)
{
    #name
    name <- as.character(station)
    
    #Read table
    #station <- read.table(paste0("./Randomforest/",station), header = TRUE)
    station <- read.table(paste0(here::here("Randomforest"),"/", station), header = TRUE)
    
    #Real Data
    
    real_data <- station 
    
    #Diference between temperatura
    station$DifferenceTemperature <- station$TX - station$TM
    
    #Location NA's
    NAs= which((is.na(station$SR=="NA"))==TRUE)
    DataNas = station[NAs,]
    
    #Delete Columns "Date", "P", "TM"
    station_rf <- na.omit(station[,-which(colnames(station)%in% c("Date", "TM", "P"))])
    #DataNas <- na.omit(DataNas[,-which(colnames(station)%in% c("Date", "TM", "RH", "SR"))])
    
    
    #Random Forest
    randomforest <- randomForest(SR ~ ., data=station_rf)
    valPredic <- predict(randomforest, DataNas)
    
    #Put missing values into matrix
    DataNas$SR <- as.numeric(valPredic) 
    station[NAs,] <- DataNas
    
    #Predict
    predic <- station[,-which(colnames(station)=='DifferenceTemperature')]
    
    #Delete .txt
    name <- gsub('.txt', "", name)
    station_names <- c(rep(name, nrow(real_data)))
    result <- data.frame(real_data$Date, real_data$SR, predic$SR, station_names)
    colnames(result) <- c("Date", "Real_Data", "Estimated_Data", "Station_Names")
    
    #name <- paste0(name, ".txt")
    #write.table(predic, file = paste(getwd(),"Final_Files_SR", name, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
    

    
    
    return(result)
    
}

#lapply(list.files(), random_forest_SR)
#setwd("C:/Users/JCRIVERA/Documents/Codigo_Nuevo_Clima/Datos/Datos_Prueba/Hourly_Data/Daily_Data/Random_Forest/Final_Files/With_SR")

#radom_forest fill missing values for missing values
random_forest_RH <- function(station)
{
    #name
    name <- as.character(station)
    
    
    #Read table
    #station <- read.table(station, header = TRUE)
    station <- read.table(paste0(here::here("Randomforest"),"/",station), header = TRUE)
    
    #Real Data
    real_data <- station 
    
    #Location NA's
    NAs= which((is.na(station$RH=="NA"))==TRUE)
    DataNas = station[NAs,]
    
    #Delete Columns "Date", "P", "TM"  
    station_rf <- na.omit(station[,-which(colnames(station)%in% c("Date"))])
    #DataNas <- na.omit(DataNas[,-which(colnames(station)%in% c("Date", "TM", "RH", "SR"))])
    
    
    #Random Forest
    randomforest <- randomForest(RH ~ TX+TM+TX*TM, data=station_rf)
    valPredic <- predict(randomforest, DataNas)
    
    #Put missing values into matrix
    DataNas$RH <- as.numeric(valPredic) 
    station[NAs,] <- DataNas
    
    predic <- station
    
    name <- paste0(name, ".txt")
    #write.table(predic, file = paste(getwd(),"Final_Files_HR", name, sep =  "/"), row.names = FALSE, quote = FALSE, col.names = TRUE)
    
    
    #Delete .txt
    name <- gsub('.txt', "", name)
    station_names <- c(rep(name, nrow(real_data)))
    result <- data.frame(real_data$Date, real_data$RH, predic$RH, station_names)
    colnames(result) <- c("Date", "Real_Data", "Estimated_Data", "Station_Names")
    
    
    
    return(result)
    
}

#lapply(list.files(pattern ="\\.txt$"), random_forest_RH)


#graph_all_SR_HR graphs all stations.

graph_all_SR_RH <- function (listFiles, variable)
{
    if(variable == "Radiacion_Solar")
    {
        Data_Complete <- lapply(listFiles, random_forest_SR)
        
    }
    
    if(variable == "Humedad_Relativa")
    {
        #Aca hay un error      
        Data_Complete <- lapply(listFiles, random_forest_SR)
    }
    
    lapply(Data_Complete, graph_station, variable = variable)
    
}  
























