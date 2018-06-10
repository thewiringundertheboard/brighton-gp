# load the libraries
library(tidyverse)
library(ggmap)

# set the working directory
setwd("~/Desktop/BNNHS")

# load the months and years we have data for
# R doesn't have hashtable so we use an environment
dataMonthYear <- new.env()
dataMonthYear[['2016-01']] = 'January 2016'
dataMonthYear[['2017-10']] = 'October 2017'

# load postcode along with lat/long data, 
# taken from https://www.doogal.co.uk/UKPostcodes.php
postcodesLatLong <- read_csv('data/BNPostcodesWithLatLong.csv')

# get the google man for B+H
gMap <- get_map("Brighton and Hove", zoom=12)

# load valid B+H city postcodes
brightonPostcodeRange <- 'BN1 |BN2 |BN3 |BN41 '

# init an empty tibble for the final data
brightonDoctorData <- tibble()

# loop through the month and years, shape and plot the data
for (monthYear in ls(dataMonthYear)) {
    
    # set the title of the plots
    title = paste('Total number of patients, and doctor to patient ratio in Brighton and Hove GP surgeries (', dataMonthYear[[monthYear]], ')', sep='')
    
    # read the GP practise size and GP number data from CSV
    # taken from https://apps.nhsbsa.nhs.uk/infosystems/report/viewReportList.do?reportMenuItemId=211
    doctors <- read_csv(paste('data/GPPractiseSizeAndGPs-', monthYear,'.csv', sep=''), guess_max=2000)

    # select only B+H GPs from the list, add postcode, lat/long,
    # and calculate the doctor to patient ratio
    # add to the existsing tibble so we can plot a boxplot
    brightonDoctorData <- bind_rows(brightonDoctorData, doctors %>% 
      left_join(postcodesLatLong, by=c('PostCode' = 'Postcode')) %>% 
        filter(grepl(brightonPostcodeRange, doctors$PostCode), grepl('Yes', doctors$`GP practice`)) %>%
        mutate('Total' = as.numeric(gsub(',', '', Total)), 'Year' = substr(monthYear, 0, 4), 'DoctorToPatientRatio' = Total/`GP Count`) %>% 
      select('PostCode', 'PracticeCode', 'Total', 'GP Count', 'DoctorToPatientRatio', 'Longitude', 'Latitude', 'Year'))
    
    # create a PNG file
    png(paste('BrightonGPSizeAndRatio-', monthYear, '.png', sep=''), width=800, height=800)
    
    # plot the map and data
    print(
        ggmap(gMap) +
        geom_point(data=brightonDoctorData %>% filter(Year == substr(monthYear, 0, 4)),
                 aes(x=Longitude, y=Latitude, colour=DoctorToPatientRatio, size=Total),
                 inherit.aes = F) +
        scale_size_continuous(range=c(1,12), limits=c(1,26000)) +
        scale_colour_gradient(low='black', high='red', space="Lab", na.value="grey50", guide="colourbar", limits=c(1,4000)) +
        labs(colour='Doctor to patient ratio', size='Total number of patients', x='Longitude', y='Latitude') +
        ggtitle(title)
    )

    # write to the PDF
    dev.off()
    
    # plot a practise size only graphic to go with the other data sets
    if (monthYear == '2017-10') {
     
        # create a PNG file
        png(paste('BrightonGPSize-', monthYear, '.png', sep=''), width=800, height=800)
        
        # plot the map and data
        print(
            ggmap(gMap) +
                geom_point(data=brightonDoctorData  %>% filter(Year == substr(monthYear, 0, 4)),
                           aes(x=Longitude, y=Latitude, colour=Total, size=Total),
                           inherit.aes = F) +
                scale_size_continuous(range=c(1,12), limits=c(1,26000)) +
                scale_colour_gradient(low='black', high='red', space="Lab", na.value="grey50", guide="colourbar", limits=c(1,26000)) +
                labs(colour='Total number of patients', size='Total number of patients', x='Longitude', y='Latitude') +
                ggtitle(paste('Total number of patients registered in Brighton and Hove GP surgeries (', dataMonthYear[[monthYear]], ')', sep=''))
        )
        
        # write to the file
        dev.off()
    }
}

# create a PNG file for the boxplot
png(paste('BrightonGPSizeAndRatioBoxplot-', monthYear, '.png', sep=''), width=800, height=800)

# plot the data
brightonDoctorData %>% ggplot(aes(x=Year, y=DoctorToPatientRatio)) + 
    geom_boxplot() + 
    scale_y_continuous(breaks=pretty(brightonDoctorData$DoctorToPatientRatio, n=50)) + 
    ggtitle('Doctor to patient ratio by year in Brighton and Hove GP surgeries')

# write to the file
dev.off()

# calculate the change in doctor to patient ratio
brightonDoctorData <- brightonDoctorData %>% 
    group_by(PracticeCode) %>% 
    mutate(change = c(NA, diff(DoctorToPatientRatio)))

# calculate min and max change
changeMin = min(brightonDoctorData$change, na.rm = TRUE)
changeMax = max(brightonDoctorData$change, na.rm = TRUE)

# create a PNG file for the change data
png('BrightonGPSizeAndRatioChange.png', width=800, height=800)

# plot the map and data
print(
    ggmap(gMap) +
        geom_point(data=brightonDoctorData %>% filter(Year == 2017),
                   aes(x=Longitude, y=Latitude, colour=change, size=Total, alpha=change),
                   inherit.aes = F) +
        scale_size_continuous(range=c(1,12), limits=c(1,26000)) +
        scale_colour_gradient(low='white', high='red', space="Lab", na.value="grey50", guide="colourbar", limits=c(changeMin,changeMax)) +
        labs(colour='Change in doctor to patient ratio', size='Total number of patients', x='Longitude', y='Latitude', alpha='Change in doctor to patient ratio') +
        ggtitle('Change in doctor to patient ratio between 01/2016 and 10/2017 in Brighton and Hove GP surgeries')
)

# write to the PDF
dev.off()

