library(tidyverse) # metapackage with lots of helpful functions
library(lubridate)
library(stringr)
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

raw_df <- read.csv('IMDB Horror movies.csv')
glimpse(raw_df)

# Factorization and some renaming

df <- raw_df %>%
  mutate(title = Title,
         genres = Genres,
         release_date_chr = `Release.Date`,
         release_country = as.factor(`Release.Country`),
         movie_rating = as.factor(`Movie.Rating`),
         review_rating = `Review.Rating`,
         runtime = `Movie.Run.Time`,
         plot = Plot,
         cast = Cast,
         language = Language,
         locations = as.factor(`Filming.Locations`),
         budget = Budget) %>%
  select(title, genres, release_date_chr, release_country, movie_rating, review_rating, runtime, plot, cast, language, locations, budget)

levels(df$movie_rating)
summary(df$movie_rating)
#there are 699 not rated movies and 108 unrated movies, which are same actually.

df_clean <- df %>%
  mutate (movie_rating = fct_recode(movie_rating, 'NOT RATED' = 'UNRATED'))
summary(df_clean$movie_rating)

# collected not rated and unrated together as not rated

df_clean <- df_clean %>%
  mutate(release_date = dmy(release_date_chr)) %>%
  mutate(release_year = ifelse(is.na(release_date), year(as.Date(release_date_chr, format = "%Y")), year(release_date)))


years <- df$title %>%
  str_extract_all("[\\d]+\\)") %>%
  str_replace("\\)", "") %>%
  as.numeric()
#extracted year from movie title

data_frame(year_from_title = years, release_year = df_clean$release_year) %>%
  summarize(match = sum(year_from_title == release_year, na.rm = TRUE),
            mismatch = sum(year_from_title != release_year, na.rm = TRUE))

mismatch_records <- cbind(data_frame(year_from_title = years), df$release_date_chr, df_clean) %>%
  filter(year_from_title != release_year) %>%
  select(title, release_date_chr, release_date, release_year, year_from_title)

head(mismatch_records)

#As you can see some of the years extracted from title not matches release year








