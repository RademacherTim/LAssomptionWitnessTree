#========================================================================================
# Hourly meteorological data from the Canadian Centre for Climate Services for the 
# meteorological station in l'Assomption stared on the 1994-09-07T08:00:00 EST and daily 
# records stretch back to 1930-01-01. updateClimateAssomption.sh downloads the latest data
# and archives it. This script reads in the archived data and reads it into the witness 
# tree format.
#
# Metadata and data is available at:
#----------------------------------------------------------------------------------------

# load dependencies ---------------------------------------------------------------------
if(!existsFunction("%>%")) library("tidyverse")

# loop over daily files and append them -------------------------------------------------
for (y in 1930:2022) {  
  filename <- paste0("./data/daily/en_climate_daily_QC_7014160_", y, "_P1D.csv")
  tmp <- read_csv(file = filename, col_types = "ddciDiiicdcdcdcdcdcdcdcdcdcdcdc")
  
  # append the climate data -------------------------------------------------------------
  if (y == 1930) {
    dailyData <- tmp
  } else {
    dailyData <- full_join(dailyData, tmp, 
                           by = c("Longitude (x)", "Latitude (y)", "Station Name", 
                                  "Climate ID", "Date/Time", "Year", "Month", "Day", 
                                  "Data Quality", "Max Temp (°C)", "Max Temp Flag", 
                                  "Min Temp (°C)", "Min Temp Flag", "Mean Temp (°C)", 
                                  "Mean Temp Flag", "Heat Deg Days (°C)", 
                                  "Heat Deg Days Flag", "Cool Deg Days (°C)", 
                                  "Cool Deg Days Flag", "Total Rain (mm)", 
                                  "Total Rain Flag", "Total Snow (cm)", 
                                  "Total Snow Flag", "Total Precip (mm)", 
                                  "Total Precip Flag", "Snow on Grnd (cm)", 
                                  "Snow on Grnd Flag", "Dir of Max Gust (10s deg)", 
                                  "Dir of Max Gust Flag", "Spd of Max Gust (km/h)", 
                                  "Spd of Max Gust Flag"))
  }
}

# simplify data -------------------------------------------------------------------------
dailyData <- dailyData %>% 
  select(6:8, seq(10, 30, by = 2)) %>% 
  rename(year = Year, month = Month, day = Day, 
         tmax = `Max Temp (°C)`, tmin = `Min Temp (°C)`, temp = `Mean Temp (°C)`, 
         GDD = `Heat Deg Days (°C)`, CDD = `Cool Deg Days (°C)`,
         rain = `Total Rain (mm)`, snow = `Total Snow (cm)`, prec = `Total Precip (mm)`,
         snoP = `Snow on Grnd (cm)`, 
         winD = `Dir of Max Gust (10s deg)`, winS = `Spd of Max Gust (km/h)`)

# delete empty rows from the future -----------------------------------------------------
dailyData <- dailyData %>% 
  mutate(date = lubridate::as_date (paste(year, month, day)), .before = 1) %>%
  filter(date < Sys.Date())
  
# loop over hourly files and append them ------------------------------------------------
for (y in 1994:2022) {
  for (m in 1:12) {
    if ((y == 1994 & m < 10) |
        (y == 2022 & m > 1)) next
    month <- ifelse(m < 10, paste0 ("0",m), as.character (m))  
    filename <- paste0("./data/hourly/en_climate_hourly_QC_7014160_", month, "-", y, 
                       "_P1H.csv")
    string <- ifelse(y < 2001, "ddciTiiitdcdcdcdcdcdcdcdcdcc", 
                     "ddciTiiitdcdcdcdcdcdcdcdcdcdcc")
    tmp <- read_csv(file = filename, col_types = string)
  
    # add precipitation column before 2001 ----------------------------------------------
    if (y < 2001) tmp <- tmp %>% 
      add_column(`Precip. Amount (mm)` = NA, `Precip. Amount Flag` = NA)
    
    # append the climate data -----------------------------------------------------------
    if (y == 1994 & m == 10) {
      hourlyData <- tmp
    } else {
      hourlyData <- full_join(hourlyData, tmp, 
                              by = c("Longitude (x)", "Latitude (y)", "Station Name", 
                                     "Climate ID", "Date/Time (LST)", "Year", "Month", 
                                     "Day", "Time (LST)", "Temp (°C)", "Temp Flag", 
                                     "Dew Point Temp (°C)", "Dew Point Temp Flag", 
                                     "Rel Hum (%)", "Rel Hum Flag", "Wind Dir (10s deg)", 
                                     "Wind Dir Flag", "Wind Spd (km/h)", "Wind Spd Flag", 
                                     "Visibility (km)", "Visibility Flag", 
                                     "Stn Press (kPa)", "Stn Press Flag", "Hmdx", 
                                     "Hmdx Flag", "Wind Chill", "Wind Chill Flag",
                                     "Weather", "Precip. Amount (mm)", 
                                     "Precip. Amount Flag"))
    }
  }
}

# simplify data -------------------------------------------------------------------------
hourlyData <- hourlyData %>% 
  select(5:10, seq(12, 27, by = 2), 29) %>% 
  rename(year = Year, month = Month, day = Day, time = `Time (LST)`,
         temp = `Temp (°C)`, dewp = `Dew Point Temp (°C)`, rehu = `Rel Hum (%)`, 
         winD = `Wind Dir (10s deg)`, winS = `Wind Spd (km/h)`, visb = `Visibility (km)`,
         pres = `Stn Press (kPa)`, hmdx = `Hmdx`, wich = `Wind Chill`, 
         prec = `Precip. Amount (mm)`, datetime = `Date/Time (LST)`)

# daily data has missing data in the end of 1999 and beginning of 2000 ------------------
# here we fill the gap from aggregated hourly data
tmpData <- hourlyData %>% select(-c(datetime, time)) %>% 
  group_by(year, month, day) %>% 
  summarise(tmax = max(temp, na.rm = TRUE),
            tmin = min(temp, na.rm = TRUE),
            temp = mean(temp, na.rm = TRUE),
            prec = sum(prec, na.rm = TRUE), .groups = "drop") %>%
  mutate(date = lubridate::as_date(paste(year,month,day)), .before = 1)
# concatenate three dataframes to get dataframe of same length as averaged hourlyData ---
tmp2Data <- dailyData %>% filter(year ==  1994 & month >= 10)
tmp3Data <- dailyData %>% filter(year >=  1995 & year < 2022)
tmp4Data <- dailyData %>% filter(year == 2022 & month == 1)
tmp5Data <- rbind(tmp2Data, tmp3Data, tmp4Data)

# plot daily temperatures versus daily mean hourly temperatures -------------------------
PLOT <- FALSE
if (PLOT) {
  plot(x = tmpData$temp, y = tmp5Data$temp, typ = "p", pch = 19, col = "#91b9a433",
       las = 1, xlab = expression (paste ("Daily mean temperature (",degree,"C)", sep = "")),
       ylab = expression (paste ("Daily mean of hourly temperature (",degree,"C)", sep = "")))
  abline(b = 1, a = 0, col = "#666666", lty = 2)
  summary(lm(tmpData$temp ~ tmp5Data$temp))
  abline(lm(tmpData$temp ~ tmp5Data$temp), col = "#106470")
  # R2 = 0.994, which is good enough for me to join the two dataframes
}

# get chunks of data to fill the daily data ---------------------------------------------
tmp1 <- dailyData %>% filter(date < "1999-08-10") # before gap
tmp2 <- dailyData %>% filter(date > "2000-10-09") # after gap
tmpData <- tmpData %>% 
  filter(date >= "1999-08-10" & date <= "2000-10-09") %>% # gap
  mutate(GDD = NA, CDD = NA, rain = NA, snow = NA, snoP = NA, winD = NA, winS = NA) # set non-existing variables to NA
dailyData <- rbind(tmp1, tmpData, tmp2) # bind data together to fill gap

# calculate annual mean temperature and annual total precipitation ----------------------
annualData <- dailyData %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  summarise(temp = mean(temp, na.rm = TRUE),
            prec = sum(prec , na.rm = TRUE))

# calculate some basic statistics -------------------------------------------------------
mean(dailyData %>% filter(year < 2022) %>% select(temp) %>% unlist(), na.rm = TRUE)
# mean and standard deviation of annual mean temperatures
mean(annualData %>% select(temp) %>% unlist(), na.rm = TRUE)
sd(annualData %>% select(temp) %>% unlist(), na.rm = TRUE)
# mean and standard deviation of total annual precipiation
mean(annualData %>% select(prec) %>% unlist(), na.rm = TRUE)
sd(annualData %>% select(prec) %>% unlist(), na.rm = TRUE)

# plot long-term temperature trend ------------------------------------------------------
if (PLOT) {
  par(mar = c(3, 5, 1, 5))
  plot(x = annualData$year, y = annualData$temp, typ = "l", col = "#901C3B",
       xlab = "", las = 1, xlim = c(1925, 2025), ylim = c(0, 9), axes = FALSE, 
       ylab = expression(paste("Mean annual temperature (",degree,")", sep = "")))
  axis(side = 1, seq(1925, 2025, by = 25))
  axis(side = 2, las = 1, seq(0, 8, by = 2))
  
  # plot long-term precipitation trend ----------------------------------------------------
  par (new = TRUE)
  plot (x = annualData$year, y = annualData$prec, typ = "l", col = "#0072cf", xlab = "", 
        las = 1, xlim = c (1925, 2025), ylim = c (0, 1300), axes = FALSE, ylab = "")
  axis (side = 4, seq (0, 1200, by = 400), las = 1)
  mtext (side = 4, line = 3, text = "Total annual precipitation (mm)")
  # TR - There might still be a problem with the precipitation data for 1999/2000 or that 
  # year was just very, very dry, like bone dry!!!
}
#========================================================================================