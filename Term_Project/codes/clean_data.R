
# Clean environment -------------------------------------------------------

rm( list=ls() )

# Initialize packages ---------------------------------------------------

library( tidyverse )

# Import data -------------------------------------------------------------

github_url <- 'https://raw.githubusercontent.com/sulekata/Coding_1/master/Term_Project/data/raw/awardees_source.csv'
df <- read_csv( github_url )

# Filter and transform needed variables -------------------------------------------------

df <- df %>% transmute( race = race_ethnicity,
                  award = award, 
                  winner = person,
                  movie = movie,
                  birthplace = birthplace,
                  date_of_birth = date_of_birth,
                  religion = religion,
                  sexual_orientation = sexual_orientation,
                  year_award = year_of_award )

# drop duplicates
df <- df %>% unique()

# create new quantitative variable years_since_award
# which measures the years that have elapsed since the year of the award
df <- df %>% mutate( years_since_award = as.numeric( year( today() ) ) - year_award )

# create new quantitative variable age_at_award
# which measures the awardees' age when they received the award

# split the years from the date_of_birth variable
# and save it into a vector
col <- NULL

for ( i in df$date_of_birth ){
  if ( length( strsplit( i, "" )[[1]] ) == 11 ){
    t <- strsplit( i, '-')[[1]][3]
  } else if ( length( strsplit( i, "" )[[1]] ) == 8 | length( strsplit( i, "" )[[1]] ) == 9 ){
    t <- strsplit( i, '-')[[1]][3]
    t <- paste0( '19', t )
  } else if ( length( strsplit( i, "" )[[1]] ) == 4 ){
    t <- i
  } else if ( length( strsplit( i, "" )[[1]] ) > 11 ){
    t <- substr( i, 8, 11 )
  }
  col <- c( col, t )
}

# add the age_at_award variable to the df
df <- df %>% mutate( age_at_award = year_award - as.numeric( col ) )

# filter unnecessary variables
df <- df %>% select( !c( date_of_birth, year_award ) )

# transform race variable
# if white then white, otherwise not white
df <- df %>% mutate( race = ifelse( race == 'White', 'white', 'not white' ) )

# inspect religion variable
religions <- df %>% group_by( religion ) %>% summarize( n = n() )

# due to the high number of na values I drop this variable
df <- df %>% select( !religion )

# transform birthplace variable
# if USA then usa, otherwise not usa
# US cities have the two letter abbreviation of the states after them
# only exception is New York City which does not have the state code
# I use this to categorize the birthplaces

# check na values, this affects only two observations for the same actor
na_birthplace <- df %>% filter( birthplace == 'Na' )
# since he was not born in the US I assign a 0 to these two na values in the for loop

# then create a for loop
place <- NULL

for ( i in df$birthplace ){
  if ( i == 'New York City' ){
    z <- 'usa'
  } else if ( i == 'Na' ){
    z <- 'not usa'
  } else if ( length( strsplit( strsplit( i, ', ' )[[1]][-1], "" )[[1]] ) == 2 ){
    z <- 'usa'
  } else {
    z <- 'not usa'
  }
  place <- c( place, z )
}

# add the transformed birthplace variable to df
df <- df %>% mutate( birthplace = place )

# transform sexual orientation variable to consist only of lowercase characters
df <- df %>% mutate( sexual_orientation = tolower( sexual_orientation ) )

# inspect the categories of the variable
sexual_orientations <- df %>% group_by( sexual_orientation ) %>% summarize( n = n() )

# merge the gay and lesbian categories into one
df <- df %>% mutate( sexual_orientation = unlist( lapply( sexual_orientation, function( x ){
                                                  ifelse( any( x == c( 'gay', 'lesbian') ),
                                                  'homosexual', x ) } ) ) )

# merge the matter of dispute and the na categories into one
df <- df %>% mutate( sexual_orientation = unlist( lapply( sexual_orientation, function( x ){
  ifelse( any( x == c( 'na', 'matter of dispute') ),
          'na', x ) } ) ) )


# Save clean data ---------------------------------------------------------

# save clean data
my_path <- 'C:/CEU/Fall_Term/Github_Repos/Coding_1/Term_Project/'
write_csv( df, paste0( my_path, 'data/clean/all_awardees.csv' ) )

# filter award variable to keep Best Actor category only
df <- df %>% filter( award == 'Best Actor' )

# drop award variable
df <- df %>% select( !( award ) )

# check duplicates in the years_since_award variable
years <- df %>% group_by( years_since_award ) %>% summarise( n = n() )

# there are 3 duplicates so I correct these by hand to not lose data
# get the years that appear more than once
years <- years %>% filter( n > 1 )
years <- years[ , 1 ]

# get the actors who belong to these years
years_dupl <- df %>% filter( years_since_award == years[[ 1, 1 ]] | years_since_award == years[[ 2, 1 ]] | years_since_award == years[[ 3, 1 ]] )

# modify two values that are errors
df <- df %>% mutate( years_since_award = ifelse( winner == 'Sean Penn' & years_since_award == 18, 17, years_since_award ),
                     years_since_award = ifelse( winner == 'Jack Nicholson' & years_since_award == 24, 23, years_since_award ) )

# save filtered clean data as well
write_csv( df, paste0( my_path, 'data/clean/best_actor_awardees.csv' ) )
