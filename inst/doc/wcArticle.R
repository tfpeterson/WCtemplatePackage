params <-
structure(list(region = "Mountain"), .Names = "region")

## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
knitr::opts_chunk$set(warning = FALSE)
knitr::opts_chunk$set(message = FALSE)

# set any missing NAs in tables to blank
options(knitr.kable.NA = '')

# load R packages
library(fivethirtyeight)
library(tidyverse)
library(knitr)
library(kableExtra)
library(ggthemes)
library(stringr)

# load steak_survey dataset
data("weather_check", package = "fivethirtyeight")

# create subset, no missing data/NAs
# pick a specific region
sdat <- weather_check %>%
  filter(region == params$region)


## ------------------------------------------------------------------------
# create table summary for ages
tb1 <- sdat %>% 
  select(age) %>%
  table() %>%
  prop.table()*100

# create table summary for gender
tb2 <- sdat %>% 
  select(female) %>%
  table() %>%
  prop.table()*100

# create table summary for income
tb3 <- sdat %>% 
  select(hhold_income) %>%
  table() %>%
  prop.table()*100

# convert all tables to data frames
tb1df <- as.data.frame(tb1) 
tb2df <- as.data.frame(tb2)
tb3df <- as.data.frame(tb3)

# merge 1st 2 data frames together
mtb <- merge(data.frame(tb1df, row.names = NULL),
             data.frame(tb2df, row.names = NULL),
             by=0, all=TRUE)[-1]

# merge result with 3rd data frame
mtb2 <- merge(data.frame(mtb, row.names = NULL),
              data.frame(tb3df, row.names = NULL),
              by=0, all=TRUE)[-1]

# use the final data frame
# make into a table with kable
# add styling with kableExtra
# add header with labels spanning 2 columns each
mtb2 %>%
  knitr::kable(format="html",
    col.names=c("Category","%",
                "Category","%",
                "Category","%"),
    digits=2,
    caption="Demographics of Survey Respondents") %>%
  kableExtra::kable_styling(c("striped","bordered"),
                full_width=FALSE) %>%
  add_header_above(c("Ages"=2,"Gender"=2,"Income"=2))

## ------------------------------------------------------------------------

ggplot(sdat, 
       aes(x = weather_source, fill = female)) + 
  geom_bar(position="dodge",colour="black") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  ggtitle(paste0("Weather Source by Gender: ",
                 params$region," Region")) +
  xlab("Weather Source Preference") +
  ylab("Number of Respondents") +
  scale_fill_manual(values=c("skyblue","palevioletred"),
    name="Gender",
    breaks=c(FALSE,TRUE),
    labels=c("Male", "Female")) +
  coord_flip() +
  theme_fivethirtyeight()

