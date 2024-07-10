#Handling NetCDF files
#https://pjbartlein.github.io/REarthSysSci/netCDF.html
rm(list = ls())

#Reading a netCDF data set using the ncdf4 package

# load the ncdf4 package
library(ncdf4)

#https://confluence.ecmwf.int/pages/viewpage.action?pageId=197702790
# ERA5 tp :accumulated from the beginning of the forecast to the end of the forecast step 
# meaning  For example, runoff at day=D, step=12 will provide runoff accumulated from day=D,
# time=0 to day=D, time=12. The maximum accumulation is over 24 hours,


# compute daily sum of the hourly era5 nc dataset
require(cmsafops) # https://rdrr.io/cran/cmsafops/man/daysum.html
#dayavg!daymean   https://www.rdocumentation.org/packages/cmsafops/versions/1.2.4/topics/dayavg

#dayavg(), daymax(), daymean(), daymin(), daypctl(), dayrange(), daysd(), dayvar(), ydaymax(), daysum()
daysum(var = "ssrd", infile = "0-data/climat/noisy_dataset/ERA5_nc/era5_sn_surface_solar_radiation_downwards_2020.nc", 
       outfile = "0-data/climat/noisy_dataset/ERA5_nc/daily_era5_sn_surface_solar_radiation_downwards_2020.nc")

#unlink(c("0-data/climat/ERA5_nc/era5_sn_total_precipitation_2020.nc", 
#         "0-data/climat/ERA5_nc/daily_era5_sn_total_precipitation_2020.nc"))


#Open the netCDF file
ncin <- nc_open("0-data/climat/noisy_dataset/ERA5_nc/daily_era5_sn_max_temperature_2019.nc")
print(ncin)



#2.2 ---------------------------Get coordinate (including time) variables-----
# get longitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon) #get number of longitude values
head(lon)
# get latitude
lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)
print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time
tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time) #get the number of time steps 
nt

#The object tunits has two components hasatt (a logical variable), and tunits$value, the actual “time since” string
tunits

#Get a variable
#Get the the variable (tp) and its attributes, and verify the size of the array

# get Wind_Speed_at_2_Meters
dname <- "t2m"
tp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tp_array) #dim long, lat and timestep

# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")
ls()

# nc_close() # to Close the netCDF file

# 3------------------------------ Reshaping from raster to rectangular----

#NetCDF files or data sets are naturally 2-D raster slabs (e.g. longitude by latitude “slices”),
#3-D bricks (e.g. longitude by latitude by time), or 4-D arrays (e.g. longitude by latitude 
#by height by time), while most data analysis routines in R expect 2-D variable-by-observation 
#“tidy” data frames. (There is an exception to this expectation in some cases like principle 
#components analysis (PCA) in which variables are locations and the observations are times.) 
#Therefore, before analysis, a 3-D or 4-D array in the netCDF files must be “flattened” into a 2-D array.
#In addition, time is usually stored as the CF (Climate Forecast) “time since” format that is not usually 
#human-readable. Here are some example conversions:

# load some packages
library(chron)
library(lattice)
library(RColorBrewer)

# 3.1 Convert the time variable
# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
#tyear = 2020
chron(time,origin=c(tmonth, tday, tyear))

#3.2 Replace netCDF fillvalues with R NAs
# replace netCDF fill values with NA's
tp_array[tp_array==fillvalue$value] <- NA

#get The total number of non-missing grid cells 
length(na.omit(as.vector(tp_array[,,1])))

#3.3 Get a single time slice of the data, create an R data frame, and write a .csv file

#NetCDF variables are read and written as one-dimensional vectors (e.g. longitudes), 
#two-dimensional arrays or matrices (raster “slices”), or multi-dimensional arrays (raster “bricks”).

# get a single slice or layer (January)
m <- 1
tp_slice <- tp_array[,,m]

# quick map
image(lon,lat,tp_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
#cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tp_slice ~ lon * lat, data=grid, cuts=5, pretty=T, #add at=cutpts to reset the frame
          col.regions=(rev(brewer.pal(11,"RdBu"))))

# 3.3.2 Create a data frame
#To create a data frame, the expand.grid() and as.matrix() functions are used to create the 
#259200 pairs (i.e. rows) of values of longitude and latitude (the columns), and the as.vector() 
#function is used to “unstack” the slice of data into a long vector. The size of the objects that
# are created can be verified using the dim() and length() functions.

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# vector of `tp` values for the first slice or layer 01-01-2021
tp_vec <- as.vector(tp_slice)
length(tp_vec)

# create dataframe and add names
tp_df01 <- data.frame(cbind(lonlat,tp_vec))
names(tp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tp_df01), 10)

#3.3.3 Write out the data frame
#write.table(na.omit(tp_df01),"0-data/climat/Power_new_simu/meteo_all_sites/tp_df01.csv", 
#            row.names=FALSE, sep=",")

# 3.4 Convert the whole array to a data frame, and calculate MTWA, MTCO and the annual mean
#The idea here is to convert the nlon by nlat by nt 3-D array into a (nlon x nlat) by nt 
#2-D matrix. (This will work if the netCDF data set was written as a CF-compliant data 
#set, with arrays dimensioned as in Fortran, i.e. as nlon x nlat x nt arrays).

# 3.4.1 Reshape the whole array

# reshape the array into vector
tp_vec_long <- as.vector(tp_array)
length(tp_vec_long)

# reshape the vector into a matrix
tp_mat <- matrix(tp_vec_long, nrow=nlon*nlat, ncol=nt)

#tp in mm
#tp_mat <- tp_mat * 1000

#tp in deg C
tp_mat <- tp_mat - 273.15

#ssdr from j/m2 to MJ/m2
#tp_mat <- (tp_mat)/1000000

dim(tp_mat)
head(na.omit(tp_mat))


# Create the second data frame from the matrix.
lonlat <- as.matrix(expand.grid(lon,lat))
tp_df02 <- data.frame(cbind(lonlat,tp_mat))

Date <- seq.Date(as.Date("2019-01-01"), length.out = ncol(tp_df02)-2, by = "day", drop=FALSE)
names(tp_df02) <- c("lon","lat",paste0("tp_",Date))

#3.4.2 Get the annual mean
# get the anuual mean
dim(tp_df02)
  
tp_df02$annual_sum <- apply(tp_df02[which(names(tp_df02)=="tp_2019-01-01"):
                                        which(names(tp_df02)=="tp_2019-12-31")],1,mean) # sum, mean
head(na.omit(tp_df02))

#data = tp_df02[,c("lon","lat","annual_sum")]

dim(na.omit(tp_df02))


levelplot(tp_df02$annual_sum ~ tp_df02$lon * tp_df02$lat, cuts=7, pretty=T, 
          col.regions=((brewer.pal(10,"RdBu"))),               #(rev(brewer.pal(10,"RdBu")))
          main="era5 : max temperature 2019 (deg C)",
          xlab="",ylab="")

#transform a row into a colums in r
library(tidyr)
tp_df03 <- pivot_longer(tp_df02, cols=3:ncol(tp_df02), names_to = "Date", values_to = "temp_max")

tp_df03$Date= gsub("[tp_]", "",  tp_df03$Date) 
tail(tp_df03)



#3.4.3 Write out the second data frame
#Write the second data frame out as a .csv file, dropping NAs.

# write out the dataframe as a .csv file
write.table(na.omit(tp_df03),"0-data/climat/noisy_dataset/ERA5_nc/temp_max0.csv",row.names=FALSE, sep=",")

# create a dataframe without missing values
tp_df04 <- na.omit(tp_df03)
head(tp_df04)
ls()

# 4 Data frame-to-array conversion(rectangular to raster)

# time an R process
ptm <- proc.time() # start the timer
# ... some code ...
proc.time() - ptm # how long?

#4.1 Convert a “full” R data frame to an array

#4.1.1 Initial set up – create dimension variables

#1st approach
# copy lon, lat and time from the initial netCDF data set
lon2 <- lon
lat2 <- lat
time2 <- time
tunits2 <- tunits
nlon2 <- nlon; nlat2 <- nlat; nt2 <- nt

# generate lons, lats and set time
# site_id = read.csv("1-code/site_id.csv")
# 
# lon2 <- as.array(unique(site_id$x.x))
# nlon2 <- length(unique(site_id$x.x))
# lat2 <- as.array(unique(site_id$y.x))
# nlat2 <- length(unique(site_id$y.x))
# time2 <-as.array(seq.Date(as.Date("2020-01-01"), length.out = 366, by = "day", drop=FALSE))
# nt2 <- 366
# tunits2 <- "days since 2020-01-01 00:00:00.0 -0:00"

#4.1.2 Reshaping a “full” data frame to an array

ptm <- proc.time() # start the timer
# convert tmp_df02 back into an array
tp_mat2 <- as.matrix(tp_df02[3:(3+nt-1)])
dim(tp_mat2)

# then reshape the array
tp_array2 <- array(tp_mat2, dim=c(nlon,nlat,nt))
dim(tp_array2)

# convert annual_sum to arrays
annual_sum_array2 <- array(tp_df02$annual_sum, dim=c(nlon2,nlat2))
dim(annual_sum_array2)

proc.time() - ptm # how long?

#4.1.3 Check the conversion

# some plots to check creation of arrays
library(lattice)
library(RColorBrewer)

levelplot(tp_array2[,,1] ~ lon * lat, data=grid, cuts=5, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="Total precipitation 2020-01-01 (mm)")

levelplot(annual_sum_array2 ~ lon * lat, data=grid, cuts=10, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), main="ERA5 Total precipitation 2020 (mm)")




