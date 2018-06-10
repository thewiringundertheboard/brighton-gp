# load the libraries
library(tidyverse)
library(ggmap)

# set the working directory to where you've downloaded this
setwd("/your/dir/here")

# load the months and years we have data for
# R doesn't have hashtable so we use an environment
dataMonthYear <- new.env()
dataMonthYear[['2013-04']] = 'April 2013'
dataMonthYear[['2014-01']] = 'January 2014'
dataMonthYear[['2015-01']] = 'January 2015'
dataMonthYear[['2016-01']] = 'January 2016'
dataMonthYear[['2017-01']] = 'January 2017'

# load valid B+H city postcodes
brightonPostcodeRange <- 'BN1 |BN2 |BN3 |BN41 '

# load postcode along with lat/long data, 
# taken from https://www.doogal.co.uk/UKPostcodes.php
postcodesLatLong <- read_csv('data/BNPostcodesWithLatLong.csv')

# get the google man for B+H
gMap <- get_map("Brighton and Hove", zoom=12)

# init an empty tibble for the final data
brightonDoctorData <- tibble()

# loop through the month and years, shape and plot the data
for (monthYear in ls(dataMonthYear)) {
    
    # get the year
    year = substr(monthYear, 0, 4)
    
    # read the GP practise size data from CSV
    # taken from https://digital.nhs.uk/data-and-information/publications/statistical/numbers-of-patients-registered-at-a-gp-practice
    doctors <- read_csv(paste('data/gp-reg-patients-', monthYear, '.csv', sep=''), guess_max=2000)

    # the data for 2013 is without postcodes 
    # so we take them from a different source and map them by practise code
    if (monthYear == '2013-04') {  
        
        # load doctor postcode and practise codes
        # taken from https://www.england.nhs.uk/wp-content/uploads/2012/05/ccg-practice-list.xls
        doctorPostcodes <- read_csv('data/GPWithPostcodes2012.csv', guess_max=2000)
        
        # select only B+H GPs from the list, add postcode then lat/long (for 2013 only)
        # add to the existsing tibble so we can plot a boxplot
        brightonDoctorData <- bind_rows(brightonDoctorData, doctors %>% 
          left_join(doctorPostcodes, by=c('GP_PRACTICE_CODE' = 'Practice')) %>% 
          left_join(postcodesLatLong, doctorPostcodes, by=c('Postcode' = 'Postcode')) %>% 
          filter(grepl(brightonPostcodeRange, Postcode)) %>% 
          select('Postcode', 'GP_PRACTICE_CODE', 'TOTAL_ALL', 'Longitude', 'Latitude') %>% 
          mutate('Year' = year))
        
    } else {
    
        # select only B+H GPs from the list and add lat/lon (after 2013)
        # add to the existsing tibble so we can plot a boxplot
        brightonDoctorData <- bind_rows(brightonDoctorData, doctors %>% 
          left_join(postcodesLatLong, by=c('POSTCODE' = 'Postcode')) %>% 
          filter(grepl(brightonPostcodeRange, POSTCODE)) %>% 
          select('POSTCODE', 'GP_PRACTICE_CODE', 'TOTAL_ALL', 'Longitude', 'Latitude') %>% 
          mutate('Year' = year))
    }
    
    # create a PNG file
    png(paste('BrightonGPSize-', monthYear, '.png', sep=''), width=800, height=800)

    # plot the map and data, and set the scale sizes 
    # so they are consistent between years
     print(
         ggmap(gMap) +
         geom_point(data=brightonDoctorData %>% filter(Year == year),
                aes(x=Longitude, y=Latitude, colour=TOTAL_ALL, size=TOTAL_ALL),
                inherit.aes = F) +
         scale_size_continuous(range=c(1,12), limits=c(1,26000)) +
         scale_colour_gradient(low='black', high='red', space="Lab", na.value="grey50", guide="colourbar", limits=c(1,26000)) +
         labs(colour='Total number of patients', size='Total number of patients', x='Longitude', y='Latitude') +
         ggtitle(paste('Total number of patients registered in Brighton and Hove GP surgeries (', dataMonthYear[[monthYear]], ')', sep=''))
     )

    # write to the file
    dev.off()
}

# create a PNG file for the boxplot
png('BrightonGPSizeBoxplot.png', width=800, height=800)

# plot the data by year
brightonDoctorData %>% 
    ggplot(aes(x=Year, y=TOTAL_ALL)) + 
    geom_boxplot() + 
    scale_y_continuous(breaks=pretty(brightonDoctorData$TOTAL_ALL, n=15)) +
    labs(x='Year', y='Total number of patients') + 
    ggtitle('Total number of patients registered by year in Brighton and Hove GP surgeries')

# write to the PDF
dev.off()