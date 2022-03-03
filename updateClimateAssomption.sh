#!/bin/bash
#----------------------------------------------------------------------------------------
# This script can download the lastest climate data from the weather station in 
# l'Assomption, Québec, Canada. The weather station provides hourly and daily weather
# variables. The up-to-date data for the current month is downloaded and added to 
# previously downloaded historic data (daily since 1930-01-01 and hourly since 
# 1994-09-07 08:00:00 EST).  
#----------------------------------------------------------------------------------------

# set path to directory with code and data
#----------------------------------------------------------------------------------------
WITNESSTREEPATH=$1

# set variables
#----------------------------------------------------------------------------------------
stationID="5237" # station ID for the l'Assomption meteorological station
DATE=$(date +%Y-%m-%d" "%H:%M:%S) # date for the losg
year=$(date +"%Y") # current year
month=$(date +"%-m") # current month
Month=$(date +"%m") # current month with leading zero

# download hourly data for the current month
#----------------------------------------------------------------------------------------
wget --content-disposition "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=${stationID}&Year=${year}&Month=${month}&Day=14&timeframe=1&submit= Download+Data"
if [ $? != 0 ]
then
   # write error message into log
   echo ${DATE} 'Error: Could not download hourly data.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# download daily data for the current month
#----------------------------------------------------------------------------------------
wget --content-disposition "https://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=${stationID}&Year=${year}&Month=${month}&Day=14&timeframe=2&submit= Download+Data"
if [ $? != 0 ]
then
   # write error message into log
   echo ${DATE} 'Error: Could not download daily data.' >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt 
   exit 1 # terminate script and indicate error
fi

# move updated hourly and daily file to the data directory
#----------------------------------------------------------------------------------------
mv en_climate_daily_QC_7014160_${year}_P1D.csv ${WITNESSTREEPATH}data/daily/
mv en_climate_hourly_QC_7014160_${Month}-${year}_P1H.csv ${WITNESSTREEPATH}data/hourly/

# write time and date into log file in the tmp/ folder
#----------------------------------------------------------------------------------------
echo ${DATE} >> ${WITNESSTREEPATH}logs/logFileDataUpdate.txt