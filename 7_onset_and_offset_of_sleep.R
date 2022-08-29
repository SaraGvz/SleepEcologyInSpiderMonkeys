### visualizing the onset and offset of sleep ###

library(hms)
library(lubridate)
library(plotrix)

################# Functions #########################


## function for plotting times from noon to noon. It will make correct 12:00 - 24:00 to be 00:00 - 12:00 and 00:00 - 12:00 to be 12:00 to 24:00

ts_func <- function( time_vec ){
  
  num_time <- as.numeric( as_hms( time_vec ) )
  
  corr_time <- ifelse( num_time < 12*60*60, num_time + 12*60*60, num_time - 12*60*60 )
  
  return( corr_time )
  
}
## function for setting transparency of a color while plotting

transp <- function(col, alpha=.5){

  res <- apply(col2rgb(col),2, function(c) rgb(c[1]/255, c[2]/255, c[3]/255, alpha))

  return(res)
}


lonlat_to_utm <- function( df, lon_name = 'location.long', lat_name = 'location.lat', crs_utm = CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # this is for Panama: "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
)){
  library( sp )
  library( rgdal )
  
  crs_longlat <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
  
  df_sp <- SpatialPointsDataFrame(coords = df[, c(lon_name, lat_name)], proj4string = crs_longlat, data = df)
  
  df_sp <- spTransform(df_sp, crs_utm)
  
  df <- as.data.frame(df_sp)
  
  names(df) [ names(df) == paste( lon_name, 1, sep = '.') ] <- 'x'
  names(df) [ names(df) == paste( lat_name, 1, sep = '.') ] <- 'y'
  
  return( df )
}

GPSdata = sdat #sdat was the name of my dataframe with the GPS data
GPSdata=lonlat_to_utm(GPSdata)

#########create sun_dat#########

## make an empty dataframe with each date of the study. For each date, we will fill in when sunset occurred, when the dark period began (the end of evening astronomical twilight), when the dark period ended the following morning (the beginning of morning astronomical twilight), and when sunrise the following morning occured
sun_dat <- data.frame( date = c(  seq( from=(min( as.Date( d1$local_timestamp ), na.rm = TRUE ) - 2 ),to = (max( as.Date( d1$local_timestamp ) , na.rm = TRUE) + 1 ), by="1 day"  ), sunset = NA, night_start = NA, night_end = NA, sunrise = NA ))

## fill in sunset time and night start time (dark period start time) on each date with the getsunlighttimes function
sun_dat[, c( 'sunset', 'night_start' ) ] <- getSunlightTimes( date = sun_dat$date, lon = ave_lon, lat = ave_lat )[, c( 'sunset', 'night' ) ]

## fill in rise time and night end time (dark period end time) on each date with those from the following date with the getsunlighttimes function. The reason we are using the following date is because we want to know when a night ended, which happens on the date following the start of that night
sun_dat[, c( 'sunrise', 'night_end' ) ] <- getSunlightTimes( date = ( sun_dat$date + 1 ), lon = ave_lon, lat = ave_lat )[, c( 'sunrise', 'nightEnd' ) ]

## put sun data in local time
#sun_dat[ , 2:5 ] <- sun_dat[ , 2:5 ] + 3*60*60
sun_dat[ , 2:5 ] <- lubridate::with_tz(sun_dat[ , 2:5 ], tzone = "America/Panama")
sun_dat=data.frame(na.omit(sun_dat))


############ Basic plots ##########

sleep_trim <- sleep_per[ !is.na( sleep_per$SPT ), ]

### Fig 1 ###

sun_trim <- sun_dat[sun_dat$date %in% unique( full_dat$night ), ]

ave_sunset <- as_hms( mean( as.numeric( as_hms( sun_trim$sunset ) ) ) )
ave_sunset <- ts_func( ave_sunset )

ave_sunrise <- as_hms( mean( as.numeric( as_hms( sun_trim$sunrise ) ) ) )
ave_sunrise <- ts_func( ave_sunrise )

ave_night_start <- as_hms( mean( as.numeric( as_hms( sun_trim$night_start ) ) ) )
ave_night_start <- ts_func( ave_night_start )

ave_night_end <- as_hms( mean( as.numeric( as_hms( sun_trim$night_end ) ) ) )
ave_night_end <- ts_func( ave_night_end )

par(mar=c(6,6,4,4))
layout(matrix(1:3, ncol = 1), widths = 1, heights = c(2,2,2.5), respect = FALSE)


## Fig 1A

par(mar = c(0, 5, 0.5, 1 ) )

sleep_per_func( tag = 2430, night = 3, x_axis = F, plot_waso = T, title = F, waso_block = waso_block, lwd = 0.5 )

# legend( x = 67000, y = 7.6, col = c( 'orange', transp( 'blue', 0.25 ) ), lty = c( 2, 0 ), pch = c( NA, 15 ), pt.cex = 2, legend = c( 'Sleep period time window', 'Wake after sleep onset'), bty = 'n', cex = 0.88 )

#legend( x = 67000, y = 6.9, col = c( 'orange', 'blue', transp( 'blue', 0.25 ) ), lty = c( 2, 0, 0 ), pch = c( NA, NA, 15 ), pt.cex = 2, legend = c( 'Sleep period time window', '', 'Wake after sleep onset'), bty = 'n', cex = 0.88 ) ##y was 7.6


#legend( 'topright', col = c( 'orange', transp( 'blue', 0.25 ) ), lty = c( 02, 0 ), pch = c( NA, 15 ), pt.cex = 2, legend = c( '', ''), bty = 'n' )


polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )


## Fig 1A


par(mar = c(0, 5, 0, 1))

ave_ved <- aggregate( d1$log_vedba, by = list( d1$local_time ), FUN = mean, na.rm = T ) 
names( ave_ved ) <- c( 'local_time', 'ave_VeDBA')

ave_ved <- ave_ved[ order( ts_func( ave_ved$local_time ) ), ]


se_ved <- aggregate( d1$log_vedba, by = list( d1$local_time ), FUN = std.error, na.rm = T ) 
names( se_ved ) <- c( 'local_time', 'se_VeDBA')

se_ved <- se_ved[ order( ts_func( se_ved$local_time ) ), ]

se_ved$se_VeDBA[ is.na( se_ved$se_VeDBA) ] <- 0

x_vals <- ts_func( ave_ved$local_time )

plot( x_vals , ave_ved$ave_VeDBA, type = 'l', xaxt = 'n', xlab = 'Time', ylab = '', las = 1, lwd = 0.5  )

title( ylab = "log VeDBA", line = 3.9 )


low_se <- ave_ved$ave_VeDBA - se_ved$se_VeDBA
high_se <- ave_ved$ave_VeDBA + se_ved$se_VeDBA


polygon( x = c( ( x_vals ) ,rev( x_vals ), x_vals[ 1 ] ), y = c( low_se, rev( high_se ), low_se[ 1 ] ), col=transp( "red" , 0.5 ), border=NA ) # fills area between the curves

polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )


## Fig 1C

par(mar = c(4.1, 5, 0, 1))

## make a density plot of the onset time and waking time
onset_dens <- density( ts_func( sleep_trim$onset_time ) )

wake_dens <- density(  ts_func( sleep_trim$waking_time ) )

waso_times <- full_dat[ !is.na( full_dat$sleep_per ) & full_dat$sleep_per == 1 & full_dat$sleep_bouts == 0, 'local_time' ]

waso_dens <- density( ts_func( waso_times ) )

x_range <- c( 0, 24*60*60 )

y_range <- c( min( onset_dens$y, wake_dens$y ), max( onset_dens$y, wake_dens$y ) )

plot( onset_dens$x, onset_dens$y, type = 'l', ylab = '', xlab = 'Time', xaxt = 'n', yaxt = "n", xlim = x_range, ylim = y_range, lty = 2, las = 1 )

#axis( 2, at = seq( 0, 0.0004, by = 0.0002 ), las = 1 )

title( ylab = "Probability Density", line = 3.9 )

lines( wake_dens$x, wake_dens$y, type = 'l', ylab = 'Density', xlab = 'Waking Time', xaxt = 'n', lty = 3 )


#legend( x = 'topright', cex = 0.9, col = c( 'red', 'blue' ), lty = c( 1, 1 ), legend = c( 'Sleep onset time', 'Waking time' ), bty = 'n' )

polygon( x = c( ave_sunset, ave_sunrise, ave_sunrise, ave_sunset ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

polygon( x = c( ave_night_start, ave_night_end, ave_night_end, ave_night_start ), y = c( -10, -10, 10, 10), col = transp('grey', .25), border = NA )

axis( 1, at = seq( 0, 60*24*60, 3*60*60), labels = c( as_hms( seq( 12*60*60, 60*23*60, 3*60*60) ), as_hms( seq( 0, 60*12*60, 3*60*60) ) ) ) 

axis( 1, at = seq( 0, 60*24*60, 1*60*60), labels = F ) 

axis( 2, at = c( 0.0, 0.0002, 0.0004 ), labels = c( '0.0', '0.0002', '0.0004' ), las = 2 )

#### End of Fig

