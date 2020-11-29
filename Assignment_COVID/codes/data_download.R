
# Initializing packages ---------------------------------------------------

library( tidyverse )
library( WDI )

# Downloading COVID data --------------------------------------------------

# source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports

# downloading the daily report on registered deaths and cases for 2020-09-15 from github
github_url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/09-15-2020.csv'
df_covid <- read_csv( github_url )

# saving the data frame to csv
my_path <- 'C:/CEU/Fall_Term/Github_Repos/Coding_1/Assignment_COVID/data/'
write_csv( df_covid, paste0( my_path, 'raw/covid_data.csv' ) )

# Downloading population data ---------------------------------------------

# searching for an indicator that contains total population for countries 
a <- WDIsearch('population, total')

# getting the data from the World Bank
df_population <- WDI( indicator=c( 'SP.POP.TOTL' ), country="all", start=2019, end=2019 )

# saving the data frame to csv
write_csv( df_population, paste0( my_path, 'raw/population_data.csv' ) )