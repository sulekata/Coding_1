
# Initializing packages ---------------------------------------------------
library( tidyverse )

rm( list=ls() )

# Importing data ----------------------------------------------------------
my_path <- 'C:/CEU/Fall_Term/Github_Repos/Coding_1/Assignment_COVID/data/'

df_covid <- read_csv( paste0( my_path, 'raw/covid_data.csv' ) )
df_pop <- read_csv( paste0( my_path, 'raw/population_data.csv' ) )

# Cleaning COVID data -----------------------------------------------------
# dropping unneccesary variables
df_covid <- df_covid %>% select( !c( FIPS, Admin2, Last_Update, Lat, Long_ , Combined_Key,
                                     Incidence_Rate, `Case-Fatality_Ratio` ) )
# summing up cases by countries
df_covid <- df_covid %>%
              group_by( Country_Region ) %>% 
              summarise( confirmed = sum( Confirmed ),
                         deaths = sum( Deaths ),
                         recovered = sum( Recovered ),
                         active = sum( Active ) )

# renaming the Country_Region variable
df_covid <- df_covid %>% 
              rename( country = Country_Region )


# Cleaning population data ------------------------------------------------
# non-country observations usually contain numbers
d1 <- df_pop %>% filter( grepl( "[[:digit:]]", df_pop$iso2c ) )
# filtering these out
df_pop <- df_pop %>% filter( !grepl( "[[:digit:]]", df_pop$iso2c ) )

# some grouping observations are still left e.g. EU, High income
# all that start with X, except XK which is Kosovo
# all that start with Z, except ZA-South Africa, ZM-Zambia and ZW-Zimbabwe

# dropping specific values
drop_id <- c("EU","HK","OE")
# checking for filtering
df_pop %>% filter( grepl( paste( drop_id , collapse="|"), df_pop$iso2c ) ) 
# saving the opposite
df_pop <- df_pop %>% filter( !grepl( paste( drop_id , collapse="|"), df_pop$iso2c ) ) 

# dropping values with certain starting characters
# getting the first letter from iso2c
fl_iso2c <- substr(df_pop$iso2c, 1, 1)
retain_id <- c("XK","ZA","ZM","ZW")
# checking before filtering
d1 <- df_pop %>% filter( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                       !grepl( paste( retain_id , collapse="|"), df_pop$iso2c ) ) 
# saving observations which are the opposite
df_pop <- df_pop %>% filter( !( grepl( "X", fl_iso2c ) | grepl( "Z", fl_iso2c ) & 
                          !grepl( paste( retain_id , collapse="|"), df_pop$iso2c ) ) ) 

# dropping unnecessary variables and renaming the remaining ones
df_pop <- df_pop %>% transmute( country = country,
                                population = SP.POP.TOTL )

# Merging COVID and population data ---------------------------------------
# joining the two tables
df <- full_join( df_covid, df_pop, "country" )

# correcting some country names by hand
use_name <- c("Congo, Rep.","Congo, Dem. Rep.","Czech Republic","Korea, Rep.","Kyrgyz Republic",
              "Laos","St. Kitts and Nevis","St. Lucia","St. Vincent and the Grenadines",
              "Slovak Republic","United States","Myanmar")

alter_name <- c("Congo (Brazzaville)","Congo (Kinshasa)","Czechia","Korea, South","Kyrgyzstan",
                "Lao PDR","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines",
                "Slovakia","US","Burma")

# using a for loop to change the name of the countries
for ( i in seq_along( use_name ) ){
  df$country[ df$country == alter_name[ i ] ] <- use_name[ i ]
}

# writing a for-loop to find those which are partial or complete matches

# 1) auxillary table for countries without any population values
aux <- df %>% filter( is.na( population ) )

# 2) getting the names of the countries
countries_nm <- aux$country

# 3) iterating through all potential partial matches
for ( i in seq_along( countries_nm ) ){
  # selecting those observations where there is a partial match
  log_select <- str_detect( df$country , countries_nm[ i ] )
  # getting the population values for partial matches
  c_partial <- df$population[ log_select ]
  # if there is a match: only two countries are selected and one is missing the other has population:
  if ( length( c_partial ) == 2 & sum( is.na( c_partial ) ) == 1 ){
    # replacing the missing value with the match
    df$population[ log_select & is.na(df$population)] = c_partial[ !is.na( c_partial ) ]
    # removing the replaced variable
    df <- df %>% filter( !(log_select & is.na( df$confirmed ) ) )
  }
}

# 4) checking the results:
df %>% filter( is.na( population ) )
# these are:
#   a) cruiser ships which were stuck in national territory (Diamond Princess, MS Zaandam )
#   b) disputed territories which are accepted by covid statistics but not by the World Bank 
#       (Western Sahara, Taiwan or Kosovo)
#   c) countries on which there is no population data available (Ertirea, Holy See (Vatican))

# handling missing values
View( df %>% filter( !complete.cases( df ) ) )
# dropping if population, confirmed cases or death is missing
df <- df %>% filter( !( is.na( population ) | is.na( confirmed ) | is.na( deaths ) ) )

# saving the clean data
write_csv( df , paste0( my_path,'clean/merged_covid_pop.csv'))
