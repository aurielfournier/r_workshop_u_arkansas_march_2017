###Introduction and refreshers for R
# By - Matt Boone (2015) & Auriel Fournier (2015)
# Modified by Auriel Fournier for 2016 NAOC Workshop
# Modified by Auriel Fournier for 2017 R workshop at UA

# all materials available here:
# https://github.com/aurielfournier/r_workshop_u_arkansas_march_2017

####################################
# Necessary packages
####################################

library(gapminder)
library(tidyverse) # this includes a whole series of packages, including, dplyr, tidyr, ggplot2 and many others

###################
## Loading In The Data
####################

data(gapminder)
head(gapminder)   

# Explain What Pipes are %>% 

# Explain the verbs of dplyr

#########################
## Filtering
#########################

gapminder %>%
        filter(continent=='Europe',
               year==1987)

gapminder %>%
  filter(continent=='Europe',
         year==1987) %>% 
        select(country,lifeExp,gdpPercap)

# the "|" means 'or' in R
gapminder %>%
      filter(continent=="Europe"|continent=="Asia") %>% 
      distinct(continent)

# the "&" means "and" in R
gapminder %>%
          filter(year>=1987&year<=2002) %>% distinct(year)

#########################
# Match     %in%   
#########################

sub_countries <- c("Afghanistan","Australia", "Zambia")


gapminder %>%
          filter(country %in% sub_countries) %>% distinct(country)

#########################
## GROUPING
#########################

# if we want to know the mean life expectancy of each continent 
gapminder %>%
  group_by(continent) %>%
  summarize(mean = mean(lifeExp))

# if we want to know the mean life expectancy of each continent in each year
gapminder %>% 
  group_by(continent, year) %>%
  summarize(mean=mean(lifeExp))


#########################################
## CHALLENGE
#########################################

# What is the median life expenctancy 
# and population for each country in Asia in 1987

gapminder %>% 
  filter(continent == "Asia" & year == 1987) %>%
  group_by(country) %>%
  summarise(medianL = median(lifeExp),
            medianP = median(pop))

#########################
## MUTATE
#########################

# create a new column that combines country and continent together into one character string
# select only the year, life expectancy, population, GDP and new country_continent columns

(mgap <- gapminder %>%  
  mutate(country_continent = paste0(country,"_",continent)) %>%
   select(year, lifeExp, pop, gdpPercap, country_continent))

# or

# create a new column called is_USA, where if the country is the United States the column receives a "Yes" and if not a "No"

gapminder %>%
  mutate(is_USA = ifelse(country == "United States","Yes","No")) %>%
  select(is_USA)

########################
## Separate
########################

# seperate our country_continent column into two seperate columsn
# (typically you would not create a column and then later split it, but bear with me for the sake of showing you how this works)

# we can seperate by an item, like an underscore "_"
mgap %>% 
  separate(country_continent, 
           sep="_", 
           into=c("country",
                  "continent")) 

# or
# or we can seperate by the number of characters in a string
mgap %>% 
  separate(year, sep=2, 
           into=c("century","year"))

########################
## Joins
########################

# for no reason other than the awesomeness of star wars 
# we are going to join our data set with another dataset 
# indicating whether or not the original star wars had been released yet in that year

# creating a data frame that contains some of the years from our gapmind data set and incudes a column indicating whether or not the first star wars movie (A New Hope) had yet been released. 

star_wars_dat <- data.frame(year=c(unique(gapminder$year)[2:10],2012), 
                            star_wars_released=c("No","No","No","No","YES","YES","YES",
                                                 "YES","YES","YES"))

unique(gapminder$year)
unique(star_wars_dat$year)
# so gapminder has some years that star wars does not, and vise versa.

# There are a lot of different ways we can join data sets together, first off a full join. 

# This joins together EVERYTHING, 
full_join(gapminder, star_wars_dat, by="year") %>% distinct(year)
full_join(gapminder, star_wars_dat, by="year")
# we have everything, and NAs are inserted for years where things don't exist


# this joins together the two data sets but only keeps the instances of "year" that are in star_wars_dat since it is on the right in the function
right_join(gapminder, star_wars_dat, by="year") %>% distinct(year)
# notice taht 1952, 2002, 2007 are missing, bc they don't exist in star_wars_dat


# this joins together the two data sets but only keeps the instances of "year" that are in gapmminder since it is on the left in the function
left_join(gapminder, star_wars_dat, by="year") %>% distinct(year)
# notice that 2012 is missing, bc it doesn't exist in gapminder

# this joins together the two datasets but only keeps the instances of "year" which are in common among both datasets. 
inner_join(gapminder, star_wars_dat, by="year") %>% distinct(year)
# only things that are in common between both datasets


#####################################
## CHALLENGE
#####################################

# Calculate the average life expectancy in 2002 
# of 2 randomly selected countries for each continent. 
# Then arrange the continent names in reverse order. 
# Hint: Use the dplyr functions arrange() and sample_n(), 
# they have similar syntax to other dplyr functions.
# ?arrange ?sample_n for help

gapminder %>%
  filter(year==2002) %>%
  group_by(continent) %>%
  sample_n(2) %>%
  assign("countries_selected",.) %>%
  group_by(country, continent) %>%
  summarize(mean=mean(lifeExp)) %>%
  arrange(., desc(continent))

part1 <- gapminder %>%
  filter(year==2002) %>%
  group_by(continent) %>%
  sample_n(2)

part2 <- part1 %>% summarize(mean=mean(lifeExp)) %>%
  arrange(., desc(continent))

########################
## Dates and Times
########################

#########dates and times#############################

# We're first going to need to tackle dates. R can handle dates, and it can be quite powerful, but a bit annoying.
# The base functions for this are as.Date, as.POSIXct, as.POSIXlt
# The syntax for these is essentially the same, feed it a date, and tell it the format

Sys.time()

## Good Resource on what letters = what in format
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html

(dt<-as.Date(Sys.time(),format='%Y-%m-%d'))
(ct<-as.POSIXct(Sys.time(),format='%Y-%m-%d %H%M%D'))
(lt<-as.POSIXlt(Sys.time(),format='%Y-%m-%d %H%M%D'))

# whats great is we can now do math on time

dt-10   ##since day is the lowest measurement it counts in days
ct-10   ##however counts in seconds
lt-10   ##does the same thing

# as.POSIXlt is really useful because it allows you to call particular pieces of the time out
lt$yday   ##ordinal date (day of the year)
lt$hour   ##hour
lt$year   ##years since 1900
lt$year+1900  ##converts you to standard time

##these are particularly useful because you can do math on time
earlytime<-as.POSIXct('2015-03-23',format='%Y-%m-%d')

ct - earlytime 

##as well as logical statements
ct > earlytime
ct == earlytime

######################
## GREPL
####################
grepl('Af',gapminder$country) %>% head(n=20) # returns TRUE and FALSEs, which we can feed into filter()

gapminder %>%
    filter(grepl("Af", country)) %>% summary() # we get any country that contains Af at any point

gapminder %>%
  filter(grepl('^Af',country)) %>% summary() # we get any country that BEGINS with Af

#####################################
## CHALLENGE
#####################################

## Median and Mean Life Exp for all Countiries that being with "Ma" for the years 1990-1997 