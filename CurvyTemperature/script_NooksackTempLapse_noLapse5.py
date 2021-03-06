"""
Nooksack Temperature Lapse Rate
Created on Fri Sep 30 14:54:02 2016
@author: Claire Beveridge, University of Washington

This code analyzes temperature lapse rates for iButton sensors deployed in the 
 Nooksack River Basin:

ID	    Elevation (m) 	Data Type	     Date Start	Date End
Lapse2	664	        Air Temperature	12/4/2015	8/16/2016
Lapse3	664	        Air Temperature	12/4/2015	8/16/2016
Lapse4	1056	        Air Temperature	12/4/2015	8/16/2016
			        Ground Temperature	12/4/2015	8/16/2016
Lapse5	1287	        Air Temperature	12/28/2015	9/9/2016-- NOT INCLUDED
Lapse6	1575	        Air Temperature	12/4/2015	8/16/2016
			        Ground Temperature	12/4/2015	8/16/2016
Lapse7	1743	        Air Temperature	10/14/2015	8/16/2016
			        Ground Temperature	10/14/2015	8/16/2016
			        Relative Humidity	10/14/2015	8/16/2016

Important notes:
1. For all sensors, temperature or relative humidity data was collected every
3 hours.
2. Lapse 5 and Lapse 7 have data collected on slightly different dates. Only
the dates that overlap with the remaining sensors (i.e., in the range of 
12/4/2015-8/16/2016) are analyzed. Hence, Lapse 5 and Lapse 7 have adjusted 
data sets (indicated with "adj" in the variable name) that are used. 

Below is a description of the main variables used in this code:

Values for each sensor (Lapse2, Lapse3, Lapse4, Lapse4_ground, Lapse5, Lapse6, 
Lapse6_ground, Lapse7, Lapse7_ground, Lapse7_RH):
Shown for Lapse 2 and apply to all variables except Lapse7_RH. Special cases
noted below
    - ann_mean_daily_Tmean_Lapse2: Mean of daily temperature over year  
    - daily_Tmax_Lapse2: Array of daily maximum temperature (daily max of 3-hour data)
    - daily_Tmean_Lapse2: Array of daily mean temperature (average of 3-hour data)- See below for Lapse 5 and Lapse 7 adjusted varaible
    - daily_Tmin_Lapse2: Array of daily minimum temperature (daily min of 3-hour data)
    - date_Lapse2: date of original measurement
    - datetime_Lapse2: date and time of original measurement- See below for Lapse 5 and Lapse 7 adjusted varaible
    - elev_Lapse2: Elevation of sensor in [m]
    - end_date_Lapse2: End date of measurements for sensor
    - grouped_daily_data_Lapse2: Sub-daily measurments grouped by day
    - grouped_dates_Lapse2: array of days that measurements collected
    - ndays_Lapse2: Number of days that measurements collected
    - n_Lapse2: Number of original measurements for sensor (every 3 hours)
    - start_date_Lapse2: Start date of measurements for sensor
    - time_Lapse2: time of original measurement- See below for Lapse 5 and Lapse 7 adjusted varaible
    - temp_Lapse2: temperature of original measurement [deg C]- See below for Lapse 5 and Lapse 7 adjusted varaible

Special cases for individual variables:
- Lapse 5_ NOT INCLUDED IN THIS SCRIPT
    - ann_mean_daily_Tmean_Lapse5_adj: Mean of daily temperature over year, adjusted
    - daily_Tmean_Lapse5_adj: Adjusted array- Has "nan" for days that data not 
    collected at Lapse 5 but was collected at other sensors
    - datetime_Lapse5_adj: Adjusted array- Has "nan" for days that data not 
    collected at Lapse 5 but was collected at other sensors
    - grouped_dates_Lapse5_adj: Adjusted array- Has "nan" for days that data
    not collected at Lapse 5 but was collected at other sensors
    - ind_Lapse5_shift_daily: number of days to delay before first Lapse 5 data
     entry, as well as number of additional days that Lapse 5 extends beyond other
     sensors
    - ind_Lapse5_shift_subdaily: number of time steps to delay before first Lapse 5
     data entry, as well as number of additional days that Lapse 5 extends beyond 
     other sensors
     - temp_Lapse5_adj: Adjusted array- Has "nan" for days that data not 
     collected at Lapse 5 but was collected at other sensors
- Lapse 7 -Air Temperature
    - ann_mean_daily_Tmean_Lapse7_adj: Mean of daily temperature over year, adjusted
    - daily_Tmean_Lapse7_adj: Adjusted array- Has "nan" for days that data not 
    collected at Lapse 5 but was collected at other sensors
    - datetime_Lapse5_adj: Adjusted array- Has "nan" for days that data not 
    collected at Lapse 5 but was collected at other sensors
    - grouped_dates_Lapse5_adj: Adjusted array- Has "nan" for days that data 
    not collected at Lapse 5 but was collected at other sensors
    - ind_Lapse5_shift_daily: number of days to delay before first Lapse 5 data
     entry, as well as number of additional days that Lapse 5 extends beyond other
     sensors
    - ind_Lapse5_shift_subdaily: number of time steps to delay before first Lapse 5
     data entry, as well as number of additional days that Lapse 5 extends beyond 
     other sensors
     - temp_Lapse5_adj: Adjusted array- Has "nan" for days that data not 
     collected at Lapse 5 but was collected at other sensors
- Lapse 7:
    - daily_Tmean_Lapse7_adj: Adjusted array- starts same day as other sensors
    - datetime_Lapse7_adj: Adjusted array- starts same day as other sensors  
    - grouped_dates_Lapse7_adj: Adjusted array- starts same day as other sensors  
    - ind_Lapse7_start: Index to start Lapse 7 sub-daily data to align with other sensors
    - ind_Lapse7_daily_start: Index to start Lapse 7 daily average to align with other sensors
    - RH_Lapse7: RH of Lapse 7 
    - RH_thold: Array of 1 where RH> threshold value, 0 where RH< threshold value for 3-hour data
    - RH_thold_day: Array of 1 where RH> threshold value, 0 where RH< threshold value for daily data    
    - temp_Lapse7_adj: Adjusted array- starts same day as other sensors  

General variables:
    - ann_mean_LR_daily_Tmean: Annual mean- Daily temperature (mean) lapse rate
    - ann_mean_b_daily_Tmean: Annual mean- Daily temperature (mean) intercept
    - ann_daily_mean_LR_daily_Tmean: Annual mean- Daily temperature (mean) lapse rate
    - ann_daily_mean_LR_daily_Tmax: Annual mean- Daily temperature (max) lapse rate
    - ann_daily_mean_LR_daily_Tmin: Annual mean- Daily temperature (min) lapse rate
    - elevations: Elevations in [m] 
    - elevations_km: Elevations in [km] 
    - elevations_km_noLapse5:  Elevations in [km] excluding Lapse 5 (for calculating without Lapse 5 data)
    - grouped_month: Array of month that each measurement is colleced in
    - ind_dec, ind_jan, etc: indices of measurements collected in respective month  
    - LR_all_months: Array of each month lapse rate assuming constant w/elevation
    - LR_23_4_all_months, etc. : Array of each month lapse rate between Lapse 2/3 and 4, etc.
    - mean_LR: Mean lapse rate for 3-hour measurements
    - mean_b: Mean intercept for 3-hour measurements 
    - mean_r: mean correlation coefficient for 3-hour measurements 
    - mean_p: mean p-value for 3-hour measurements 
    - mean_SE: standard error for 3-hour measurements
    - mean_LR_4_5, etc.: Mean lapse rate for 3-hour measurements between Lapse 4 and Lapse 5
    - mean_b_LR_4_5, etc.: Mean intercept for 3-hour measurements between Lapse 4 and Lapse 5 
    - mean_r_LR_4_5, etc.: Mean correlation coefficient for 3-hour measurements  between Lapse 4 and Lapse 5
    - mean_p_LR_4_5, etc.: Mean p-value for 3-hour measurements  between Lapse 4 and Lapse 5
    - mean_SE_LR_4_5, etc.: Mean standard error for 3-hour measurements 
    - subdaily_b: Array of intercepts of 3-hour measurements   
    - subdaily_LR: Array of lapse rates of 3-hour measurements
    - subdaily_r: Array of correlation coefficient of 3-hour measurements
    - subdaily_p: Array of p-value of 3-hour measurements
    - subdaily_se: Array of standard error of 3-hour measurements
    - subdaily_b_4_5, etc.: Array of intercepts of 3-hour measurements between Lapse 4 and Lapse 5  
    - subdaily_LR_4_5, etc.: Array of lapse rates of 3-hour measurements between Lapse 4 and Lapse 5
    - subdaily_r_4_5, etc.: Array of correlation coefficient of 3-hour measurements between Lapse 4 and Lapse 5
    - subdaily_p_4_5, etc.: Array of p-value of 3-hour measurements between Lapse 4 and Lapse 5
    - subdaily_se_4_5, etc.: Array of standard error of 3-hour measurements between Lapse 4 and Lapse 5
"""

# Import modules, data, variables
from datetime import datetime
from scipy import stats
import matplotlib.pyplot as plt
import numpy as np
# import pandas
# import itertools as it
# import hs_utils
# establish a secure connection to HydroShare
# hs = hs_utils.hydroshare()

# Import elevation for each Lapse Rate sensor as a floating point number
Elevation= np.genfromtxt('Elevation.csv', delimiter=',',skip_header=1)
elev_Lapse2=np.array((Elevation[0][1]), dtype='float64')
elev_Lapse3=np.array((Elevation[1][1]), dtype='float64')
elev_Lapse4=np.array((Elevation[2][1]), dtype='float64')
elev_Lapse5=np.array((Elevation[3][1]), dtype='float64')
elev_Lapse6=np.array((Elevation[4][1]), dtype='float64')
elev_Lapse7=np.array((Elevation[5][1]), dtype='float64')

# Import temperature data from csv files
Lapse2= np.genfromtxt('Lapse2_8-16-16_2180.csv', delimiter=',',autostrip=True,skip_header=15,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse2=len(Lapse2) # n is number of samples in the record
datetime_Lapse2=np.empty(n_Lapse2,dtype=object)
date_Lapse2=np.empty(n_Lapse2,dtype=object)
time_Lapse2=np.empty(n_Lapse2,dtype=object)
temp_Lapse2=np.empty(n_Lapse2,dtype='float64')
for x in range(0,n_Lapse2): # Cycle through all days in sequence
    date_Lapse2[x]=datetime.date(Lapse2[x][0])
    time_Lapse2[x]=datetime.time(Lapse2[x][0])
    datetime_Lapse2[x]=Lapse2[x][0]
    temp_Lapse2[x]=Lapse2[x][2]
del(Lapse2)

Lapse3= np.genfromtxt('Lapse3_8-16-16_2180.csv', delimiter=',',autostrip=True,skip_header=15,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse3=len(Lapse3) # n is number of samples in the record
datetime_Lapse3=np.empty(n_Lapse3,dtype=object)
date_Lapse3=np.empty(n_Lapse3,dtype=object)
time_Lapse3=np.empty(n_Lapse3,dtype=object)
temp_Lapse3=np.empty(n_Lapse3,dtype='float64')
for x in range(0,n_Lapse3): # Cycle through all days in sequence
    date_Lapse3[x]=datetime.date(Lapse3[x][0])
    time_Lapse3[x]=datetime.time(Lapse3[x][0])
    datetime_Lapse3[x]=Lapse3[x][0]
    temp_Lapse3[x]=Lapse3[x][2]
del(Lapse3)
    
Lapse4= np.genfromtxt('Lapse4_8-16-16_3465.csv', delimiter=',',autostrip=True,skip_header=15,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse4=len(Lapse4) # n is number of samples in the record
datetime_Lapse4=np.empty(n_Lapse4,dtype=object)
date_Lapse4=np.empty(n_Lapse4,dtype=object)
time_Lapse4=np.empty(n_Lapse4,dtype=object)
temp_Lapse4=np.empty(n_Lapse4,dtype='float64')
for x in range(0,n_Lapse4): # Cycle through all days in sequence
    date_Lapse4[x]=datetime.date(Lapse4[x][0])
    time_Lapse4[x]=datetime.time(Lapse4[x][0])
    datetime_Lapse4[x]=Lapse4[x][0]
    temp_Lapse4[x]=Lapse4[x][2]
del(Lapse4)    
    
Lapse4_ground=np.genfromtxt('Lapse4_8-16-16_ground.csv', delimiter=',',autostrip=True,skip_header=15,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse4_ground=len(Lapse4_ground) # n is number of samples in the record
datetime_Lapse4_ground=np.empty(n_Lapse4_ground,dtype=object)
date_Lapse4_ground=np.empty(n_Lapse4_ground,dtype=object)
time_Lapse4_ground=np.empty(n_Lapse4_ground,dtype=object)
temp_Lapse4_ground=np.empty(n_Lapse4_ground,dtype='float64')
for x in range(0,n_Lapse4_ground): # Cycle through all days in sequence
    date_Lapse4_ground[x]=datetime.date(Lapse4_ground[x][0])
    time_Lapse4_ground[x]=datetime.time(Lapse4_ground[x][0])
    datetime_Lapse4_ground[x]=Lapse4_ground[x][0]
    temp_Lapse4_ground[x]=Lapse4_ground[x][2]
del(Lapse4_ground)
    

Lapse6= np.genfromtxt('Lapse6_8-16-16_5168.csv', delimiter=',',autostrip=True,skip_header=15,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse6=len(Lapse6) # n is number of samples in the record
date_Lapse6=np.empty(n_Lapse6,dtype=object)
time_Lapse6=np.empty(n_Lapse6,dtype=object)
datetime_Lapse6=np.empty(n_Lapse6,dtype=object)
temp_Lapse6=np.empty(n_Lapse6,dtype='float64')
for x in range(0,n_Lapse6): # Cycle through all days in sequence
    datetime_Lapse6[x]=Lapse6[x][0]
    date_Lapse6[x]=datetime.date(Lapse6[x][0])
    time_Lapse6[x]=datetime.time(Lapse6[x][0])
    temp_Lapse6[x]=Lapse6[x][2]
del(Lapse6)

Lapse6_ground=np.genfromtxt('Lapse6_8-16-16_ground.csv', delimiter=',',autostrip=True,skip_header=15,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse6_ground=len(Lapse6_ground) # n is number of samples in the record
date_Lapse6_ground=np.empty(n_Lapse6_ground,dtype=object)
time_Lapse6_ground=np.empty(n_Lapse6_ground,dtype=object)
datetime_Lapse6_ground=np.empty(n_Lapse6_ground,dtype=object)
temp_Lapse6_ground=np.empty(n_Lapse6_ground,dtype='float64')
for x in range(0,n_Lapse6_ground): # Cycle through all days in sequence
    datetime_Lapse6_ground[x]=Lapse6_ground[x][0]
    date_Lapse6_ground[x]=datetime.date(Lapse6_ground[x][0])
    time_Lapse6_ground[x]=datetime.time(Lapse6_ground[x][0])
    temp_Lapse6_ground[x]=Lapse6_ground[x][2]
del(Lapse6_ground)
   
Lapse7= np.genfromtxt('Lapse7_8-16-16_5719.csv', delimiter=',',autostrip=True,skip_header=20,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse7=len(Lapse7) # n is number of samples in the record
date_Lapse7=np.empty(n_Lapse7,dtype=object)
time_Lapse7=np.empty(n_Lapse7,dtype=object)
datetime_Lapse7=np.empty(n_Lapse7,dtype=object)
temp_Lapse7=np.empty(n_Lapse7,dtype='float64')
for x in range(0,n_Lapse7): # Cycle through all days in sequence
    datetime_Lapse7[x]=Lapse7[x][0]
    date_Lapse7[x]=datetime.date(Lapse7[x][0])
    time_Lapse7[x]=datetime.time(Lapse7[x][0])
    temp_Lapse7[x]=Lapse7[x][2]
del(Lapse7)
    
Lapse7_ground=np.genfromtxt('Lapse7_8-16-16_ground.csv', delimiter=',',autostrip=True,skip_header=15,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse7_ground=len(Lapse7_ground) # n is number of samples in the record
date_Lapse7_ground=np.empty(n_Lapse7_ground,dtype=object)
time_Lapse7_ground=np.empty(n_Lapse7_ground,dtype=object)
datetime_Lapse7_ground=np.empty(n_Lapse7_ground,dtype=object)
temp_Lapse7_ground=np.empty(n_Lapse7_ground,dtype='float64')
for x in range(0,n_Lapse7_ground): # Cycle through all days in sequence
    datetime_Lapse7_ground[x]=Lapse7_ground[x][0]
    date_Lapse7_ground[x]=datetime.date(Lapse7_ground[x][0])
    time_Lapse7_ground[x]=datetime.time(Lapse7_ground[x][0])
    temp_Lapse7_ground[x]=Lapse7_ground[x][2]
del(Lapse7_ground)
    
Lapse7_RH=np.genfromtxt('Lapse7_8-16-16_RH.csv', delimiter=',',autostrip=True,skip_header=20,
                      converters={0: lambda x: datetime.strptime(x.decode("utf-8"),"%m/%d/%y %I:%M:%S %p")})
n_Lapse7_RH=len(Lapse7_RH) # n is number of samples in the record
date_Lapse7_RH=np.empty(n_Lapse7_RH,dtype=object)
time_Lapse7_RH=np.empty(n_Lapse7_RH,dtype=object)
datetime_Lapse7_RH=np.empty(n_Lapse7_RH,dtype=object)
RH_Lapse7=np.empty(n_Lapse7_RH,dtype='float64')
for x in range(0,n_Lapse7_RH): # Cycle through all days in sequence
    datetime_Lapse7_RH[x]=Lapse7_RH[x][0]
    date_Lapse7_RH[x]=datetime.date(Lapse7_RH[x][0])
    time_Lapse7_RH[x]=datetime.time(Lapse7_RH[x][0])
    RH_Lapse7[x]=Lapse7_RH[x][2]
del(Lapse7_RH)     
#%% Plot sub-hourly data
# Create a figure, specifiying figure size
fig1, ax1=plt.subplots(1,1,figsize=(10, 5))
# Plot data and specify label of each line (for legend)
plt.plot(datetime_Lapse2,temp_Lapse2,'b--',label='Lapse 2 (664 m)')
plt.plot(datetime_Lapse3,temp_Lapse3,'r--',label='Lapse 3 (664 m)')
plt.plot(datetime_Lapse4,temp_Lapse4,'m--',label='Lapse 4 (1056 m)')
# plt.plot(datetime_Lapse5,temp_Lapse5,'k--',label='Lapse 5 (1287 m)')
plt.plot(datetime_Lapse6,temp_Lapse6,'c--',label='Lapse 6 (1575 m)')
plt.plot(datetime_Lapse7,temp_Lapse7,'g--',label='Lapse 7 (1743 m)')
# Set axes and figure titles
plt.title('Time Series of Air Temperature')
plt.xlabel('Date')
plt.xticks(rotation=40) # Rotate axis tick values as necessary
plt.ylabel('Air Temperature (deg C)')
# display a legend and specify the location (either 'best' or a value 1-10)
plt.legend(loc='best')

fig2, ax2=plt.subplots(1,1,figsize=(10, 5))
plt.plot(datetime_Lapse4_ground,temp_Lapse4_ground,'m--',label='Lapse 4 (1056 m)')
plt.plot(datetime_Lapse6_ground,temp_Lapse6_ground,'c--',label='Lapse 6 (1575 m)')
plt.plot(datetime_Lapse7_ground,temp_Lapse7_ground,'g--',label='Lapse 7 (1743 m)')
plt.title('Time Series of Ground Temperature')
plt.xlabel('Date')
plt.xticks(rotation=40)
plt.ylabel('Ground Temperature (deg C)')
plt.legend(loc='best')

fig3, ax3=plt.subplots(1,1,figsize=(10, 5))
plt.plot(datetime_Lapse7_RH,RH_Lapse7,'g--',label='Lapse 7 (1743 m)')
plt.title('Time Series of Relative Humidity')
plt.xlabel('Date')
plt.xticks(rotation=40)
plt.ylabel('Relative Humidity (%)')
plt.legend(loc='best')

#%% Group daily temperatures (daily mean, min, max) for computation of daily means
start_date_Lapse2 = date_Lapse2[0]
end_date_Lapse2 = date_Lapse2[-1]
ndays_Lapse2=(end_date_Lapse2-start_date_Lapse2).days+1
grouped_dates_Lapse2 = np.empty(ndays_Lapse2, dtype=object)
grouped_daily_data_Lapse2 = np.empty(ndays_Lapse2, dtype=object)
daily_Tmean_Lapse2 = np.empty(ndays_Lapse2, dtype=object)
daily_Tmax_Lapse2 = np.empty(ndays_Lapse2, dtype=object)
daily_Tmin_Lapse2 = np.empty(ndays_Lapse2, dtype=object)
ind_grouped_dates = 0 # starting index for grouping dates
ind_grouped_daily_data = 0 # starting index for grouping daily data
date=start_date_Lapse2 # starting date for grouping daily data
for j in range(0,ndays_Lapse2):
    temp_ind=np.where(date_Lapse2==date) # temporary indices of all measurements collected the same day
    grouped_dates_Lapse2[ind_grouped_dates]=date # Enter current day into grouped_dates_Lapse2 array
    grouped_daily_data_Lapse2[ind_grouped_daily_data]=[temp_Lapse2[temp_ind]]
    # Enter temperatures on the same day into grouped_daily_data_Lapse2
    daily_Tmean_Lapse2[ind_grouped_dates] = np.nanmean(temp_Lapse2[temp_ind])
    # Compute mean of daily temperatures and enter into daily mean array
    daily_Tmax_Lapse2[ind_grouped_dates] = np.nanmax(temp_Lapse2[temp_ind])
    # Compute max of daily temperatures and enter into daily max array
    daily_Tmin_Lapse2[ind_grouped_dates] = np.nanmin(temp_Lapse2[temp_ind])
    # Compute min of daily temperatures and enter into daily min array
    if j<ndays_Lapse2-1: # Makes the loop stop the day before the last day, so don't go out of bounds
        date=date_Lapse2[temp_ind[0][-1]+1]# go to first index of the next day (one day after the last index of previous data)
        ind_grouped_dates = ind_grouped_dates+1 # go to next index of array/next day
        ind_grouped_daily_data = ind_grouped_daily_data+1 # go to next index of array/next day

start_date_Lapse3 = date_Lapse3[0]
end_date_Lapse3 = date_Lapse3[-1]
ndays_Lapse3=(end_date_Lapse3-start_date_Lapse3).days+1
grouped_dates_Lapse3 = np.empty(ndays_Lapse3, dtype=object)
grouped_daily_data_Lapse3 = np.empty(ndays_Lapse3, dtype=object)
daily_Tmean_Lapse3 = np.empty(ndays_Lapse3, dtype=object)
daily_Tmax_Lapse3 = np.empty(ndays_Lapse3, dtype=object)
daily_Tmin_Lapse3 = np.empty(ndays_Lapse3, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse3
for j in range(0,ndays_Lapse3):
    temp_ind=np.where(date_Lapse3==date)
    grouped_dates_Lapse3[ind_grouped_dates]=date
    grouped_daily_data_Lapse3[ind_grouped_daily_data]=[temp_Lapse3[temp_ind]]
    daily_Tmean_Lapse3[ind_grouped_dates] = np.nanmean(temp_Lapse3[temp_ind])
    daily_Tmax_Lapse3[ind_grouped_dates] = np.nanmax(temp_Lapse3[temp_ind])
    daily_Tmin_Lapse3[ind_grouped_dates] = np.nanmin(temp_Lapse3[temp_ind])
    if j<ndays_Lapse3-1:
        date=date_Lapse3[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1

start_date_Lapse4 = date_Lapse4[0]
end_date_Lapse4 = date_Lapse4[-1]
ndays_Lapse4=(end_date_Lapse4-start_date_Lapse4).days+1
grouped_dates_Lapse4 = np.empty(ndays_Lapse4, dtype=object)
grouped_daily_data_Lapse4 = np.empty(ndays_Lapse4, dtype=object)
daily_Tmean_Lapse4 = np.empty(ndays_Lapse4, dtype=object)
daily_Tmax_Lapse4 = np.empty(ndays_Lapse4, dtype=object)
daily_Tmin_Lapse4 = np.empty(ndays_Lapse4, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse4
for j in range(0,ndays_Lapse4):
    temp_ind=np.where(date_Lapse4==date)
    grouped_dates_Lapse4[ind_grouped_dates]=date
    grouped_daily_data_Lapse4[ind_grouped_daily_data]=[temp_Lapse4[temp_ind]]
    daily_Tmean_Lapse4[ind_grouped_dates] = np.nanmean(temp_Lapse4[temp_ind])
    daily_Tmax_Lapse4[ind_grouped_dates] = np.nanmax(temp_Lapse4[temp_ind])
    daily_Tmin_Lapse4[ind_grouped_dates] = np.nanmin(temp_Lapse4[temp_ind])
    if j<ndays_Lapse4-1:
        date=date_Lapse4[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1

start_date_Lapse4_ground = date_Lapse4_ground[0]
end_date_Lapse4_ground = date_Lapse4_ground[-1]
ndays_Lapse4_ground=(end_date_Lapse4_ground-start_date_Lapse4_ground).days+1
grouped_dates_Lapse4_ground = np.empty(ndays_Lapse4_ground, dtype=object)
grouped_daily_data_Lapse4_ground = np.empty(ndays_Lapse4_ground, dtype=object)
daily_Tmean_Lapse4_ground = np.empty(ndays_Lapse4_ground, dtype=object)
daily_Tmax_Lapse4_ground = np.empty(ndays_Lapse4_ground, dtype=object)
daily_Tmin_Lapse4_ground = np.empty(ndays_Lapse4_ground, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse4_ground
for j in range(0,ndays_Lapse4_ground):
    temp_ind=np.where(date_Lapse4_ground==date)
    grouped_dates_Lapse4_ground[ind_grouped_dates]=date
    grouped_daily_data_Lapse4_ground[ind_grouped_daily_data]=[temp_Lapse4_ground[temp_ind]]
    daily_Tmean_Lapse4_ground[ind_grouped_dates] = np.nanmean(temp_Lapse4_ground[temp_ind])
    daily_Tmax_Lapse4_ground[ind_grouped_dates] = np.nanmax(temp_Lapse4_ground[temp_ind])
    daily_Tmin_Lapse4_ground[ind_grouped_dates] = np.nanmin(temp_Lapse4_ground[temp_ind])
    if j<ndays_Lapse4_ground-1:
        date=date_Lapse4_ground[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1


start_date_Lapse6 = date_Lapse6[0]
end_date_Lapse6 = date_Lapse6[-1]
ndays_Lapse6=(end_date_Lapse6-start_date_Lapse6).days+1
grouped_dates_Lapse6 = np.empty(ndays_Lapse6, dtype=object)
grouped_daily_data_Lapse6 = np.empty(ndays_Lapse6, dtype=object)
daily_Tmean_Lapse6 = np.empty(ndays_Lapse6, dtype=object)
daily_Tmax_Lapse6 = np.empty(ndays_Lapse6, dtype=object)
daily_Tmin_Lapse6 = np.empty(ndays_Lapse6, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse6
for j in range(0,ndays_Lapse6):
    temp_ind=np.where(date_Lapse6==date)
    grouped_dates_Lapse6[ind_grouped_dates]=date
    grouped_daily_data_Lapse6[ind_grouped_daily_data]=[temp_Lapse6[temp_ind]]
    daily_Tmean_Lapse6[ind_grouped_dates] = np.nanmean(temp_Lapse6[temp_ind])
    daily_Tmax_Lapse6[ind_grouped_dates] = np.nanmax(temp_Lapse6[temp_ind])
    daily_Tmin_Lapse6[ind_grouped_dates] = np.nanmin(temp_Lapse6[temp_ind])
    if j<ndays_Lapse6-1:
        date=date_Lapse6[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1

start_date_Lapse6_ground = date_Lapse6_ground[0]
end_date_Lapse6_ground = date_Lapse6_ground[-1]
ndays_Lapse6_ground=(end_date_Lapse6_ground-start_date_Lapse6_ground).days+1
grouped_dates_Lapse6_ground = np.empty(ndays_Lapse6_ground, dtype=object)
grouped_daily_data_Lapse6_ground = np.empty(ndays_Lapse6_ground, dtype=object)
daily_Tmean_Lapse6_ground = np.empty(ndays_Lapse6_ground, dtype=object)
daily_Tmax_Lapse6_ground = np.empty(ndays_Lapse6_ground, dtype=object)
daily_Tmin_Lapse6_ground = np.empty(ndays_Lapse6_ground, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse6_ground
for j in range(0,ndays_Lapse6_ground):
    temp_ind=np.where(date_Lapse6_ground==date)
    grouped_dates_Lapse6_ground[ind_grouped_dates]=date
    grouped_daily_data_Lapse6_ground[ind_grouped_daily_data]=[temp_Lapse6_ground[temp_ind]]
    daily_Tmean_Lapse6_ground[ind_grouped_dates] = np.nanmean(temp_Lapse6_ground[temp_ind])
    daily_Tmax_Lapse6_ground[ind_grouped_dates] = np.nanmax(temp_Lapse6_ground[temp_ind])
    daily_Tmin_Lapse6_ground[ind_grouped_dates] = np.nanmin(temp_Lapse6_ground[temp_ind])
    if j<ndays_Lapse6_ground-1:
        date=date_Lapse6_ground[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1

start_date_Lapse7 = date_Lapse7[0]
end_date_Lapse7 = date_Lapse7[-1]
ndays_Lapse7=(end_date_Lapse7-start_date_Lapse7).days+1
grouped_dates_Lapse7 = np.empty(ndays_Lapse7, dtype=object)
grouped_daily_data_Lapse7 = np.empty(ndays_Lapse7, dtype=object)
daily_Tmean_Lapse7 = np.empty(ndays_Lapse7, dtype=object)
daily_Tmax_Lapse7 = np.empty(ndays_Lapse7, dtype=object)
daily_Tmin_Lapse7 = np.empty(ndays_Lapse7, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse7
for j in range(0,ndays_Lapse7):
    temp_ind=np.where(date_Lapse7==date)
    grouped_dates_Lapse7[ind_grouped_dates]=date
    grouped_daily_data_Lapse7[ind_grouped_daily_data]=[temp_Lapse7[temp_ind]]
    daily_Tmean_Lapse7[ind_grouped_dates] = np.nanmean(temp_Lapse7[temp_ind])
    daily_Tmax_Lapse7[ind_grouped_dates] = np.nanmax(temp_Lapse7[temp_ind])
    daily_Tmin_Lapse7[ind_grouped_dates] = np.nanmin(temp_Lapse7[temp_ind])
    if j<ndays_Lapse7-1:
        date=date_Lapse7[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1

start_date_Lapse7_ground = date_Lapse7_ground[0]
end_date_Lapse7_ground = date_Lapse7_ground[-1]
ndays_Lapse7_ground=(end_date_Lapse7_ground-start_date_Lapse7_ground).days+1
grouped_dates_Lapse7_ground = np.empty(ndays_Lapse7_ground, dtype=object)
grouped_daily_data_Lapse7_ground = np.empty(ndays_Lapse7_ground, dtype=object)
daily_Tmean_Lapse7_ground = np.empty(ndays_Lapse7_ground, dtype=object)
daily_Tmax_Lapse7_ground = np.empty(ndays_Lapse7_ground, dtype=object)
daily_Tmin_Lapse7_ground = np.empty(ndays_Lapse7_ground, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse7_ground
for j in range(0,ndays_Lapse7_ground):
    temp_ind=np.where(date_Lapse7_ground==date)
    grouped_dates_Lapse7_ground[ind_grouped_dates]=date
    grouped_daily_data_Lapse7_ground[ind_grouped_daily_data]=[temp_Lapse7_ground[temp_ind]]
    daily_Tmean_Lapse7_ground[ind_grouped_dates] = np.nanmean(temp_Lapse7_ground[temp_ind])
    daily_Tmax_Lapse7_ground[ind_grouped_dates] = np.nanmax(temp_Lapse7_ground[temp_ind])
    daily_Tmin_Lapse7_ground[ind_grouped_dates] = np.nanmin(temp_Lapse7_ground[temp_ind])
    if j<ndays_Lapse7_ground-1:
        date=date_Lapse7_ground[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1
     
start_date_Lapse7_RH = date_Lapse7_RH[0]
end_date_Lapse7_RH = date_Lapse7_RH[-1]
ndays_Lapse7_RH=(end_date_Lapse7_RH-start_date_Lapse7_RH).days+1
grouped_dates_Lapse7_RH = np.empty(ndays_Lapse7_RH, dtype=object)
grouped_daily_data_Lapse7_RH = np.empty(ndays_Lapse7_RH, dtype=object)
daily_Tmean_Lapse7_RH = np.empty(ndays_Lapse7_RH, dtype=object)
daily_Tmax_Lapse7_RH = np.empty(ndays_Lapse7_RH, dtype=object)
daily_Tmin_Lapse7_RH = np.empty(ndays_Lapse7_RH, dtype=object)
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse7_RH
i=0
for j in range(0,ndays_Lapse7_RH):
    temp_ind=np.where(date_Lapse7_RH==date)
    grouped_dates_Lapse7_RH[ind_grouped_dates]=date
    grouped_daily_data_Lapse7_RH[ind_grouped_daily_data]=[RH_Lapse7[temp_ind]]
    daily_Tmean_Lapse7_RH[ind_grouped_dates] = np.nanmean(RH_Lapse7[temp_ind])
    daily_Tmax_Lapse7_RH[ind_grouped_dates] = np.nanmax(RH_Lapse7[temp_ind])
    daily_Tmin_Lapse7_RH[ind_grouped_dates] = np.nanmin(RH_Lapse7[temp_ind])
    if j<ndays_Lapse7_RH-1:
        date=date_Lapse7_RH[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1
        i=temp_ind[0][-1]+1  

#%%    
# Plots of Daily min, mean, max temperature
fig4, ax4=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse2,daily_Tmax_Lapse2,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse2,daily_Tmean_Lapse2,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse2,daily_Tmin_Lapse2,'g--',label='Min Daily T')
plt.title('Time Series of Daily Air Temperatures- Lapse 2')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')     

fig5, ax5=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse3,daily_Tmax_Lapse3,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse3,daily_Tmean_Lapse3,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse3,daily_Tmin_Lapse3,'g--',label='Min Daily T')
plt.title('Time Series of Daily Air Temperatures- Lapse 3')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')   

fig6, ax6=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse4,daily_Tmax_Lapse4,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse4,daily_Tmean_Lapse4,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse4,daily_Tmin_Lapse4,'g--',label='Min Daily T')
plt.title('Time Series of Daily Air Temperatures- Lapse 4')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')

fig7, ax7=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse4_ground,daily_Tmax_Lapse4_ground,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse4_ground,daily_Tmean_Lapse4_ground,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse4_ground,daily_Tmin_Lapse4_ground,'g--',label='Min Daily T')
plt.title('Time Series of Daily Ground Temperatures- Lapse 4')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')

fig9, ax9=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse6,daily_Tmax_Lapse6,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse6,daily_Tmean_Lapse6,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse6,daily_Tmin_Lapse6,'g--',label='Min Daily T')
plt.title('Time Series of Daily Air Temperatures- Lapse 6')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')

fig10, ax10=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse6_ground,daily_Tmax_Lapse6_ground,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse6_ground,daily_Tmean_Lapse6_ground,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse6_ground,daily_Tmin_Lapse6_ground,'g--',label='Min Daily T')
plt.title('Time Series of Daily Ground Temperatures- Lapse 6')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')

fig11, ax11=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse7,daily_Tmax_Lapse7,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse7,daily_Tmean_Lapse7,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse7,daily_Tmin_Lapse7,'g--',label='Min Daily T')
plt.title('Time Series of Daily Air Temperatures- Lapse 7')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')

fig12, ax12=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse7_ground,daily_Tmax_Lapse7_ground,'r--',label='Max Daily T')
plt.plot(grouped_dates_Lapse7_ground,daily_Tmean_Lapse7_ground,'b--',label='Mean Daily T')
plt.plot(grouped_dates_Lapse7_ground,daily_Tmin_Lapse7_ground,'g--',label='Min Daily T')
plt.title('Time Series of Daily Ground Temperatures- Lapse 7')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Temperature (deg C)')
plt.legend(loc='best')

fig13, ax13=plt.subplots(1,1,figsize=(10, 5))
plt.plot(grouped_dates_Lapse7_RH,daily_Tmax_Lapse7_RH,'r--',label='Max Daily RH')
plt.plot(grouped_dates_Lapse7_RH,daily_Tmean_Lapse7_RH,'b--',label='Mean Daily RH')
plt.plot(grouped_dates_Lapse7_RH,daily_Tmin_Lapse7_RH,'g--',label='Min Daily RH')
plt.title('Time Series of Daily Relative Humidity- Lapse 7')
plt.xlabel('Date')
plt.xticks(rotation=30)
plt.ylabel('Relative Humidity (%)')
plt.legend(loc='best')    
#%% Adjustments for Lapse rate calculations- Find a way to automate these!

# i.  Lapse 7 starts earlier (10/14/15) than other sensors, so need to adjust
# the data used to start on the same date/time as others (12/4/15 around 14:00)
ind_Lapse7_start=407 # This is the index of (12/4/15 at ~14:00) in 3-hour time series
ind_Lapse7_daily_start=51 # This is the index of (12/4/15) in grouped dates.
datetime_Lapse7_adj=datetime_Lapse7[(ind_Lapse7_start):-1:]
temp_Lapse7_adj=temp_Lapse7[(ind_Lapse7_start):-1:]
RH_Lapse7_adj=RH_Lapse7[(ind_Lapse7_start):-1:]

grouped_dates_Lapse7_adj=grouped_dates_Lapse7[(ind_Lapse7_daily_start)::]
daily_Tmean_Lapse7_adj=daily_Tmean_Lapse7[(ind_Lapse7_daily_start)::]

# Convert elevations to km
elevations_km=np.array([elev_Lapse2/1000, elev_Lapse3/1000, elev_Lapse4/1000, 
                        elev_Lapse6/1000, elev_Lapse7/1000]) 
                    # Convert to km to have consistent lapse  rates units. 

# Compute annual mean temperatures
ann_mean_daily_Tmean_Lapse2=np.nanmean(daily_Tmean_Lapse2)
ann_mean_daily_Tmean_Lapse3=np.nanmean(daily_Tmean_Lapse3)
ann_mean_daily_Tmean_Lapse4=np.nanmean(daily_Tmean_Lapse4)
ann_mean_daily_Tmean_Lapse6=np.nanmean(daily_Tmean_Lapse6)
ann_mean_daily_Tmean_Lapse7=np.nanmean(daily_Tmean_Lapse7)
ann_mean_daily_Tmean_Lapse7_adj=np.nanmean(daily_Tmean_Lapse7_adj)

#%% Compute Lapse Rates
# Temperature Lapse Rate - Sub-hourly (resolution of raw data)
# - Assuming constant at all elevations
subdaily_LR=np.empty([n_Lapse2,1], dtype=object) # array of daily mean lapse rates
subdaily_b=np.empty([n_Lapse2,1], dtype=object) # array of daily mean y-int
subdaily_r=np.empty([n_Lapse2,1], dtype=object) # array of correlation coefficient
subdaily_p=np.empty([n_Lapse2,1], dtype=object) # array of p-value
subdaily_se=np.empty([n_Lapse2,1], dtype=object) # array of standard error
for i in range(0,n_Lapse2):
    [subdaily_LR[i], subdaily_b[i], subdaily_r[i], subdaily_p[i],
     subdaily_se[i]] =stats.linregress(elevations_km, 
    [temp_Lapse2[i], temp_Lapse3[i], temp_Lapse4[i], temp_Lapse6[i], 
     temp_Lapse7_adj[i]])
mean_LR=np.nanmean(subdaily_LR)
mean_b=np.nanmean(subdaily_b)
mean_r=np.nanmean(subdaily_r)
mean_p=np.nanmean(subdaily_p)
mean_se=np.nanmean(subdaily_se) 

#%%
# Temperature Lapse Rate - Daily Mean Temperature
# - Assuming constant at all elevations
daily_Tmean_LR=np.empty([ndays_Lapse2-2,1],dtype=object) # array of daily mean lapse rates
daily_Tmean_b=np.empty([ndays_Lapse2-2,1], dtype=object) # array of daily mean y-int
daily_Tmean_r=np.empty([ndays_Lapse2-2,1], dtype=object) # array of correlation coefficient
daily_Tmean_p=np.empty([ndays_Lapse2-2,1], dtype=object) # array of p-value
daily_Tmean_se=np.empty([ndays_Lapse2-2,1], dtype=object) # array of standard error
for i in range(1,ndays_Lapse2-1): # Do not include first and last days since 
   [daily_Tmean_LR[i-1],daily_Tmean_b[i-1],daily_Tmean_r[i-1], 
     daily_Tmean_p[i-1], daily_Tmean_se[i-1]]=stats.linregress (elevations_km,
    [daily_Tmean_Lapse2[i], daily_Tmean_Lapse3[i], daily_Tmean_Lapse4[i], 
     daily_Tmean_Lapse6[i], daily_Tmean_Lapse7_adj[i]])
ann_mean_LR_daily_Tmean=np.nanmean(daily_Tmean_LR)
ann_mean_b_daily_Tmean=np.nanmean(daily_Tmean_b)
ann_mean_r_daily_Tmean=np.nanmean(daily_Tmean_r)
ann_mean_p_daily_Tmean=np.nanmean(daily_Tmean_p)
#%% Plot Lapse Rates
# Plot lapse rate on a single time-step

j=100 # input time step of interest
T_plot=[temp_Lapse2[j], temp_Lapse3[j], temp_Lapse4[j], temp_Lapse6[j],
        temp_Lapse7_adj[j]]
LR_plot=subdaily_LR[j]
b_plot=subdaily_b[j]
print(datetime_Lapse2[j])
print(LR_plot)

fig14=plt.figure(figsize=(8, 5))
plt.plot(T_plot, elevations_km,'ro',label='Observed')
plt.plot(elevations_km*LR_plot+b_plot, elevations_km,'b-',label='Modeled')
plt.xlabel('Temperature (deg C)')
plt.ylabel('Elevation (km)')
plt.legend(loc='best')
plt.title('Lapse Rate=-4.75 deg C/km, 12/17/15, 2:01 am')
# Interesting dates/times:
# j=100 (12/17/15)- closely follows lapse rate of -1.45
# j=1000 (4/7/16)- crazy inversions!

#%%
# Plot lapse rate on a single day
j=156 # input time step of interest
T_plot=[daily_Tmean_Lapse2[j], daily_Tmean_Lapse3[j], daily_Tmean_Lapse4[j], 
        daily_Tmean_Lapse6[j], daily_Tmean_Lapse7_adj[j]]
LR_plot=mean_LR[j]
b_plot=mean_b[j]
print(grouped_dates_Lapse2[j])
print(LR_plot)

fig14=plt.figure(figsize=(8, 5))
plt.plot(T_plot, elevations_km,'ro',label='Observed')
plt.plot(elevations_km*LR_plot+b_plot, elevations_km,'b-',label='Modeled')
plt.xlabel('Temperature (deg C)')
plt.ylabel('Elevation (km)')
plt.legend(loc='best')
plt.title('Lapse Rate Check for Individual Day')
# Interesting days:
# j=150 (5/2/16)- huge inversion!

#%%
# Plot with annual mean temperatures and lapse rate
fig15=plt.figure(figsize=(8, 5))
T_plot=[ann_mean_daily_Tmean_Lapse2, ann_mean_daily_Tmean_Lapse3, 
        ann_mean_daily_Tmean_Lapse4, ann_mean_daily_Tmean_Lapse6,
        ann_mean_daily_Tmean_Lapse7_adj]
LR_plot=mean_LR
b_plot=mean_b

plt.plot(T_plot, elevations_km,'ro',label='Observed- Mean Daily Temperature')
plt.plot(elevations_km*LR_plot+b_plot, elevations_km,'b-',label='Modeled- Mean Daily Temperature')
#plt.xlabel('Temperature (deg C)')
plt.ylabel('Elevation (km)')
plt.legend(loc='best')
plt.title('Average Annual Lapse Rate of Daily Mean Temperature = -4.45 deg C/km')
#%%
# Plot Time Series of Daily Temperature Lapse Rate
fig16=plt.figure(figsize=(8, 5))
plt.plot(datetime_Lapse2, subdaily_LR,'r',label='3-hour Lapse Rate')
plt.plot(grouped_dates_Lapse2[1:-1], daily_Tmean_LR,'b',label='Daily Lapse Rate')
#plt.xlabel('Date')
plt.ylabel('Lapse Rate (deg C/km)')
plt.legend(loc='best')
plt.title('Time Series of Lapse Rate')

#%%
# Temperature Lapse Rate - Sub-hourly (resolution of raw data) between elev 2180 and 3465
# - Assuming varies between elevation

subdaily_LR_23_4=np.empty([n_Lapse2,1], dtype=object) # array of daily mean lapse rates
subdaily_b_23_4=np.empty([n_Lapse2,1], dtype=object) # array of daily mean y-int
subdaily_r_23_4=np.empty([n_Lapse2,1], dtype=object) # array of correlation coefficient
subdaily_p_23_4=np.empty([n_Lapse2,1], dtype=object) # array of p-value
subdaily_se_23_4=np.empty([n_Lapse2,1], dtype=object) # array of standard error
elevations_23_4=np.array([elevations_km[0], elevations_km[1], elevations_km[2]],dtype='float64')
for i in range(0,n_Lapse2):
    [subdaily_LR_23_4[i], subdaily_b_23_4[i], subdaily_r_23_4[i], 
    subdaily_p_23_4[i],subdaily_se_23_4[i]] = stats.linregress(elevations_23_4, 
    [temp_Lapse2[i], temp_Lapse3[i], temp_Lapse4[i]])
mean_LR_23_4=np.nanmean(subdaily_LR_23_4)
mean_b_23_4=np.nanmean(subdaily_b_23_4) 
mean_r_23_4=np.nanmean(subdaily_r_23_4)
mean_p_23_4=np.nanmean(subdaily_p_23_4) 
mean_se_23_4=np.nanmean(subdaily_se_23_4) 

# Temperature Lapse Rate - Sub-hourly (resolution of raw data) between elev 3465 and 5168
# - Assuming varies between elevation
n_LR_4_6=len(datetime_Lapse4) # only look at days with both have data recorded
subdaily_LR_4_6=np.empty([n_LR_4_6,1], dtype=object) # array of daily mean lapse rates
subdaily_b_4_6=np.empty([n_LR_4_6,1], dtype=object) # array of daily mean y-int
subdaily_r_4_6=np.empty([n_LR_4_6,1], dtype=object) # array of correlation coefficient
subdaily_p_4_6=np.empty([n_LR_4_6,1], dtype=object) # array of p-value
subdaily_se_4_6=np.empty([n_LR_4_6,1], dtype=object) # array of standard error
elevations_4_6=np.array([elevations_km[2], elevations_km[3]])
T_plot_4_6=np.empty([n_LR_4_6,2])
for i in range(0,n_LR_4_6):
    [subdaily_LR_4_6[i], subdaily_b_4_6[i], subdaily_r_4_6[i], 
     subdaily_p_4_6[i],subdaily_se_4_6[i]] =stats.linregress(elevations_4_6, 
    [temp_Lapse4[i], temp_Lapse6[i]])
mean_LR_4_6=np.nanmean(subdaily_LR_4_6)
mean_b_4_6=np.nanmean(subdaily_b_4_6) 
mean_r_4_6=np.nanmean(subdaily_r_4_6)
mean_p_4_6=np.nanmean(subdaily_p_4_6) 
mean_se_4_6=np.nanmean(subdaily_se_4_6)

# Temperature Lapse Rate - Sub-hourly (resolution of raw data) between elev 2180 and 3465
# - Assuming varies between elevation
subdaily_LR_6_7=np.empty([n_Lapse2,1], dtype=object) # array of daily mean lapse rates
subdaily_b_6_7=np.empty([n_Lapse2,1], dtype=object) # array of daily mean y-int
subdaily_r_6_7=np.empty([n_Lapse2,1], dtype=object) # array of correlation coefficient
subdaily_p_6_7=np.empty([n_Lapse2,1], dtype=object) # array of p-value
subdaily_se_6_7=np.empty([n_Lapse2,1], dtype=object) # array of standard error
elevations_6_7=np.array([elevations_km[3], elevations_km[4]])
for i in range(0,n_Lapse2):
    [subdaily_LR_6_7[i], subdaily_b_6_7[i], subdaily_r_6_7[i], 
    subdaily_p_6_7[i],subdaily_se_6_7[i]]=stats.linregress(elevations_6_7,
    [temp_Lapse6[i], temp_Lapse7_adj[i]])
mean_LR_6_7=np.nanmean(subdaily_LR_6_7)
mean_b_6_7=np.nanmean(subdaily_b_6_7) 
mean_r_6_7=np.nanmean(subdaily_r_6_7)
mean_p_6_7=np.nanmean(subdaily_p_6_7) 
mean_se_6_7=np.nanmean(subdaily_se_6_7)

#%%
fig14=plt.figure(figsize=(12, 8))
# Plot with annual mean temperatures and lapse rate
T_plot=[np.nanmean(temp_Lapse6), np.nanmean(temp_Lapse7_adj)]
LR_plot=mean_LR_6_7
b_plot=mean_b_6_7
plt.plot(T_plot, elevations_6_7,'go',label='Observed, Average Temp, 1575m-1743m')
plt.plot(elevations_6_7*LR_plot+b_plot, elevations_6_7,'g-', label='Modeled, 1575m-1743m, LR=-7.17 C/km')

T_plot=[np.nanmean(temp_Lapse4), np.nanmean(temp_Lapse6)]
LR_plot=mean_LR_4_6
b_plot=mean_b_4_6
plt.plot(T_plot, elevations_4_6,'ko',label='Observed, Average Temp, 1056m-1575m')
plt.plot(elevations_4_6*LR_plot+b_plot, elevations_4_6,'k-',
         label='Modeled, 1287m-1575m, LR=-5.46 C/km')
         
T_plot=[np.nanmean(temp_Lapse2), np.nanmean(temp_Lapse3), np.nanmean(temp_Lapse4)]
LR_plot=mean_LR_23_4
b_plot=mean_b_23_4
plt.plot(T_plot, elevations_23_4,'ro',label='Observed, Average Temp, 664m -1056m')
plt.plot(elevations_23_4*LR_plot+b_plot, elevations_23_4,'r-',
         label='Modeled, 664m -1056m, LR=-2.25 C/km')
         
T_plot=[ann_mean_daily_Tmean_Lapse2, ann_mean_daily_Tmean_Lapse3, 
ann_mean_daily_Tmean_Lapse4, ann_mean_daily_Tmean_Lapse6, 
ann_mean_daily_Tmean_Lapse7_adj]
LR_plot=ann_mean_LR_daily_Tmean
b_plot=ann_mean_b_daily_Tmean
plt.plot(elevations_km*LR_plot+b_plot, elevations_km,'y-',label='Modeled, All Elevations, LR=-4.45 C/km')

#plt.xlabel('Mean Temperature (12/4/15-8/16/16) [deg C] ')
plt.ylabel('Elevation [km]')
plt.legend(loc='best')
plt.title('Lapse Rate Variation with Elevation- 3-hourly Measurements')
#%% Relative Humidity- Check difference between lapse rates when raining and not raining

# Plot Relative Humidity versus lapse rate
fig15=plt.figure(figsize=(10,5))
plt.plot(RH_Lapse7_adj,subdaily_LR,'bo')
plt.xlabel('Relative Humidity [%]')
plt.ylabel('Lapse Rate [deg C/km]')
plt.title('Lapse Rate vs. Relative Humidity')
#%%
RH_above_thold_ind=np.where(RH_Lapse7>=100) # Insert RH threshold value to be considered "rainy" day
RH_below_thold_ind=np.where(RH_Lapse7<100) # Insert RH threshold value to be considered "rainy" day
RH_thold=np.zeros(len(RH_Lapse7)) # Initialize matrix with all 0's for "not raining" time steps
RH_thold[RH_above_thold_ind]=1 # Where RH>=100%, insert 1 for "raining" time steps 
RH_thold_adj=RH_thold[(ind_Lapse7_start)::] # truncated matrix for dates when have data

start_date_Lapse7_RH = date_Lapse7_RH[0]
end_date_Lapse7_RH = date_Lapse7_RH[-1]
ndays_Lapse7_RH=(end_date_Lapse7_RH-start_date_Lapse7_RH).days+1
grouped_dates_Lapse7_RH = np.empty(ndays_Lapse7_RH, dtype=object)
grouped_daily_data_Lapse7_RH = np.empty(ndays_Lapse7_RH, dtype=object)
RH_thold_day= np.empty(ndays_Lapse7_RH, dtype=object)# array with 1's for days it rainined and 0's for days it didn't rain
ind_grouped_dates = 0
ind_grouped_daily_data = 0
date=start_date_Lapse7_RH
for j in range(0,ndays_Lapse7_RH):
    temp_ind=np.where(date_Lapse7_RH==date)
    grouped_dates_Lapse7_RH[ind_grouped_dates]=date
    grouped_daily_data_Lapse7_RH[ind_grouped_daily_data]=RH_Lapse7[temp_ind]
    if np.nansum(RH_thold[ind_grouped_daily_data])>0: # If it was raining for any of the 3 hour samples taht day, consider it a rainy day
        RH_thold_day[ind_grouped_daily_data]=1
    else: RH_thold_day[ind_grouped_daily_data]=0
    if j<ndays_Lapse7_RH-1:
        date=date_Lapse7_RH[temp_ind[0][-1]+1]
        ind_grouped_dates = ind_grouped_dates+1
        ind_grouped_daily_data = ind_grouped_daily_data+1

RH_above_thold_ind_adj=np.where(RH_Lapse7_adj>=100) # Insert RH threshold value to be considered "rainy" day
RH_below_thold_ind_adj=np.where(RH_Lapse7_adj<100) # Insert RH threshold value to be considered "rainy" day
LR_rain=np.nanmean(subdaily_LR[RH_above_thold_ind_adj])
b_rain=np.nanmean(subdaily_b[RH_above_thold_ind_adj])
LR_23_4_rain=np.nanmean(subdaily_LR_23_4[RH_above_thold_ind_adj])
b_23_4_rain=np.nanmean(subdaily_b_23_4[RH_above_thold_ind_adj])
LR_4_6_rain=np.nanmean(subdaily_LR_4_6[RH_above_thold_ind_adj])
b_4_6_rain=np.nanmean(subdaily_b_4_6[RH_above_thold_ind_adj])
LR_6_7_rain=np.nanmean(subdaily_LR_6_7[RH_above_thold_ind_adj])
b_6_7_rain=np.nanmean(subdaily_b_6_7[RH_above_thold_ind_adj])

LR_rain_array=[LR_rain, LR_23_4_rain, LR_4_6_rain, LR_6_7_rain]

LR_no_rain=np.nanmean(subdaily_LR[RH_below_thold_ind_adj])
b_no_rain=np.nanmean(subdaily_b[RH_below_thold_ind_adj])
LR_23_4_no_rain=np.nanmean(subdaily_LR_23_4[RH_below_thold_ind_adj])
b_23_4_no_rain=np.nanmean(subdaily_b_23_4[RH_below_thold_ind_adj])
LR_4_6_no_rain=np.nanmean(subdaily_LR_4_6[RH_below_thold_ind_adj])
b_4_6_no_rain=np.nanmean(subdaily_b_4_6[RH_below_thold_ind_adj])
LR_6_7_no_rain=np.nanmean(subdaily_LR_6_7[RH_below_thold_ind_adj])
b_6_7_no_rain=np.nanmean(subdaily_b_6_7[RH_below_thold_ind_adj])

LR_no_rain_array=[LR_no_rain, LR_23_4_no_rain, LR_4_6_no_rain, LR_6_7_no_rain]

fig14=plt.figure(figsize=(15, 10))

# Plot with lapse rates with and without rain
# Rain + No Rain
T_plot=[np.nanmean(temp_Lapse2), np.nanmean(temp_Lapse3), np.nanmean(temp_Lapse4)]
LR_plot=mean_LR_23_4
b_plot=mean_b_23_4
plt.plot(elevations_23_4*LR_plot+b_plot, elevations_23_4,'g-', label='Variable Lapse Rate, Rain + No Rain')

T_plot=[np.nanmean(temp_Lapse4), np.nanmean(temp_Lapse6)]
LR_plot=mean_LR_4_6
b_plot=mean_b_4_6
plt.plot(elevations_4_6*LR_plot+b_plot, elevations_4_6,'g-')

T_plot=[np.nanmean(temp_Lapse6), np.nanmean(temp_Lapse7_adj)]
LR_plot=mean_LR_6_7
b_plot=mean_b_6_7
plt.plot(elevations_6_7*LR_plot+b_plot, elevations_6_7,'g-')

T_plot=[ann_mean_daily_Tmean_Lapse2, ann_mean_daily_Tmean_Lapse3, 
        ann_mean_daily_Tmean_Lapse4, ann_mean_daily_Tmean_Lapse6, 
        ann_mean_daily_Tmean_Lapse7_adj]
LR_plot=mean_LR
b_plot=mean_b
plt.plot(elevations_km*LR_plot+b_plot, elevations_km,'g--',label='Constant LR (-4.45 C/km), Rain+ No Rain')

# Rain
plt.plot(elevations_23_4*LR_23_4_rain+b_23_4_rain, elevations_23_4,'b-', label='Variable LR, RH>=100')
plt.plot(elevations_4_6*LR_4_6_rain+b_4_6_rain, elevations_4_6,'b-')
plt.plot(elevations_6_7*LR_6_7_rain+b_6_7_rain, elevations_6_7,'b-')
plt.plot(elevations_km*LR_rain+b_rain, elevations_km,'b--', label='Constant LR (-5.07 C/km), RH>=100')

# No Rain
plt.plot(elevations_23_4*LR_23_4_no_rain+b_23_4_no_rain, elevations_23_4,'k-', label='Variable LR, RH<100')
plt.plot(elevations_4_6*LR_4_6_no_rain+b_4_6_no_rain, elevations_4_6,'k-')
plt.plot(elevations_6_7*LR_6_7_no_rain+b_6_7_no_rain, elevations_6_7,'k-')
plt.plot(elevations_km*LR_no_rain+b_no_rain, elevations_km,'k--', label='Constant LR (-3.08 C/km), RH<100')

#plt.xlabel('Mean Temperature (12/4/15-8/16/16) [deg C]')
plt.ylabel('Elevation (km)')
plt.legend(loc='best')
plt.title('Lapse Rate Variation with Elevation and Relative Humidity- 3-hourly Measurements')

#%% Monthly Time Series
# Each index below tell you the indices in the DAILY temperature arrays 
# (daily_Tmean_Lapse2, daily_Tmean_Lapse3, daily_Tmin_Lapse2, etc.) that apply
# for each month
grouped_month = np.empty(n_Lapse2, dtype=object)
for j in range(0,n_Lapse2):
    grouped_month[j]=datetime_Lapse2[j].month
    
# Find indices of measurements collected in respective month 
ind_dec=np.where(grouped_month==12)
ind_jan=np.where(grouped_month==1)
ind_feb=np.where(grouped_month==2)
ind_mar=np.where(grouped_month==3)
ind_apr=np.where(grouped_month==4)
ind_may=np.where(grouped_month==5)
ind_jun=np.where(grouped_month==6)
ind_jul=np.where(grouped_month==7)
ind_aug=np.where(grouped_month==8)

# Find indices of measurements collected in respective month 
LR_dec=np.nanmean(subdaily_LR[ind_dec])
LR_23_4_dec=np.nanmean(subdaily_LR_23_4[ind_dec])
LR_4_6_dec=np.nanmean(subdaily_LR_4_6[ind_dec])
LR_6_7_dec=np.nanmean(subdaily_LR_6_7[ind_dec])

LR_jan=np.nanmean(subdaily_LR[ind_jan])
LR_23_4_jan=np.nanmean(subdaily_LR_23_4[ind_jan])
LR_4_6_jan=np.nanmean(subdaily_LR_4_6[ind_jan])
LR_6_7_jan=np.nanmean(subdaily_LR_6_7[ind_jan])

LR_feb=np.nanmean(subdaily_LR[ind_feb])
LR_23_4_feb=np.nanmean(subdaily_LR_23_4[ind_feb])
LR_4_6_feb=np.nanmean(subdaily_LR_4_6[ind_feb])
LR_6_7_feb=np.nanmean(subdaily_LR_6_7[ind_feb])

LR_mar=np.nanmean(subdaily_LR[ind_mar])
LR_23_4_mar=np.nanmean(subdaily_LR_23_4[ind_mar])
LR_4_6_mar=np.nanmean(subdaily_LR_4_6[ind_mar])
LR_6_7_mar=np.nanmean(subdaily_LR_6_7[ind_mar])

LR_apr=np.nanmean(subdaily_LR[ind_apr])
LR_23_4_apr=np.nanmean(subdaily_LR_23_4[ind_apr])
LR_4_6_apr=np.nanmean(subdaily_LR_4_6[ind_apr])
LR_6_7_apr=np.nanmean(subdaily_LR_6_7[ind_apr])

LR_may=np.nanmean(subdaily_LR[ind_may])
LR_23_4_may=np.nanmean(subdaily_LR_23_4[ind_may])
LR_4_6_may=np.nanmean(subdaily_LR_4_6[ind_may])
LR_6_7_may=np.nanmean(subdaily_LR_6_7[ind_may])

LR_jun=np.nanmean(subdaily_LR[ind_jun])
LR_23_4_jun=np.nanmean(subdaily_LR_23_4[ind_jun])
LR_4_6_jun=np.nanmean(subdaily_LR_4_6[ind_jun])
LR_6_7_jun=np.nanmean(subdaily_LR_6_7[ind_jun])

LR_jul=np.nanmean(subdaily_LR[ind_jul])
LR_23_4_jul=np.nanmean(subdaily_LR_23_4[ind_jul])
LR_4_6_jul=np.nanmean(subdaily_LR_4_6[ind_jul])
LR_6_7_jul=np.nanmean(subdaily_LR_6_7[ind_jul])

LR_aug=np.nanmean(subdaily_LR[ind_aug])
LR_23_4_aug=np.nanmean(subdaily_LR_23_4[ind_aug])
LR_4_6_aug=np.nanmean(subdaily_LR_4_6[ind_aug])
LR_6_7_aug=np.nanmean(subdaily_LR_6_7[ind_aug])

LR_all_months=[LR_dec, LR_jan, LR_feb, LR_mar, LR_apr, LR_may, LR_jun, LR_jul, LR_aug]
LR_23_4_all_months=[LR_23_4_dec, LR_23_4_jan, LR_23_4_feb, LR_23_4_mar, LR_23_4_apr, LR_23_4_may, LR_23_4_jun, LR_23_4_jul, LR_23_4_aug]
LR_4_6_all_months=[LR_4_6_dec, LR_4_6_jan, LR_4_6_feb, LR_4_6_mar, LR_4_6_apr, LR_4_6_may, LR_4_6_jun, LR_4_6_jul, LR_4_6_aug]
LR_6_7_all_months=[LR_6_7_dec, LR_6_7_jan, LR_6_7_feb, LR_6_7_mar, LR_6_7_apr, LR_6_7_may, LR_6_7_jun, LR_6_7_jul, LR_6_7_aug]

months_plot = []
for year in range(2015, 2017):
    for month in range(1, 13):
        months_plot.append(datetime(year=year, month=month, day=15))
months_plot=months_plot[11:-4]
fig13, ax13a=plt.subplots(1,1,figsize=(12, 6))
ax13b=ax13a.twinx()
plt.setp(ax13a.xaxis.get_majorticklabels(), rotation=40 )
lns1=ax13a.plot(months_plot,LR_all_months,'c-o', label='Average All elevations')
lns2=ax13a.plot(months_plot,LR_6_7_all_months,'g-o', label='1575m-1743m')
lns4=ax13a.plot(months_plot,LR_4_6_all_months,'b-o', label='1056m- 1575m')
lns5=ax13a.plot(months_plot,LR_23_4_all_months,'r-o', label='664m -1056m')
lns6=ax13b.plot(datetime_Lapse2,RH_Lapse7_adj,'y--', label='RH')
lns=lns1+lns2+lns4+lns5
labs = [l.get_label() for l in lns]
ax13a.set_xlabel('Date')
ax13a.set_ylabel('Lapse Rate [deg C/km]')
ax13b.set_ylabel('Relative Humidity [%]')
ax13b.legend(lns, labs, loc=1)
plt.title('Monthly Lapse Rates at Varying Elevations')
