library(tidyverse)
library(magrittr)
library(pdftools)
library(stringr)
library(rvest)
library(odds.converter)

# Scrape pdf
dat = pdftools::pdf_text("data/Picks/") # Needs pre processing to only use last weeks information
Results = dat %>% 
  str_split(., "\n") %>% 
  unlist %>% 
  trimws(.) %>% 
  str_split_fixed(., " {2,}", 2) %>% 
  as_tibble %>% 
  rename("Golfer" = "V1", "Points" = "V2") %>% 
  filter(Points != "", Golfer != "", Golfer != "Team") %>% # change date
  group_by(Golfer, Points) %>% 
  summarise(Count = n()) %>% 
  mutate(Golfer = gsub("[-]", " ", Golfer))

# Odds
url = ""
path = '//*[@id="mP0LzC"]/div/table/tbody' # worked with modification for first 11 weeks
Odds = read_html(url) %>% 
  html_element(x = ., xpath = path) %>% 
  html_text2 %>% 
  str_split(., pattern = "\t") %>% 
  unlist %>% 
  str_remove(., "\n") %>% 
  str_subset(., ".+") %>% 
  matrix(., ncol = 4, byrow = T) %>%  # Some weeks only have odds to win, change ncol = 2
  as_tibble() %>% 
  rename("Golfer" = "V1", "WinOdds" = "V2") %>% 
  select(-V3, -V4) %>% 
  mutate(Golfer = gsub("[-]", " ", Golfer))

# Merge together
groups = read_csv("GolfersGroups.csv") %>%
  mutate(Golfer = gsub("[-]", " ", Golfer))

# May be others with typoed names
Results[which(Results$Golfer == "Brendon Grace"), ]$Golfer = "Branden Grace"
Results[which(Results$Golfer == "Erik van Rooyen"), ]$Golfer = "Erik Van Rooyen"
Results[which(Results$Golfer == "Sungjae Im"), ]$Golfer = "Sung Jae Im"
Results[which(Results$Golfer == "Brandon Hankins"), ]$Golfer = "Brandon Harkins"
Results[which(Results$Golfer == "Alex Noren"), ]$Golfer = "Alexander Noren"
Results[which(Results$Golfer == "Rory McIIroy"), ]$Golfer = "Rory McIlroy"
Results[which(Results$Golfer == "Tyrell Hatton"), ]$Golfer = "Tyrrell Hatton"
Results[which(Results$Golfer == "Kyoung Hoon Lee"), ]$Golfer = "K.H. Lee"
Odds[which(Odds$Golfer == "Alex Noren"), ]$Golfer = "Alexander Noren"
Odds[which(Odds$Golfer == "Cheng Tsung Pan"), ]$Golfer = "C.T. Pan"
Odds[which(Odds$Golfer == "Kyoung Hoon Lee"), ]$Golfer = "K.H. Lee"
Odds[which(Odds$Golfer == "Sungjae Im"), ]$Golfer = "Sung Jae Im"
Odds[which(Odds$Golfer == "Matt Fitzpatrick"), ]$Golfer = "Matthew Fitzpatrick"

comb_res = Results %>% 
  full_join(Odds, by = "Golfer") %>% 
  left_join(groups, by = "Golfer") %>% 
  mutate(across(c(Points, Count), ~replace(., is.na(.), 0)), 
         across(c(Points, WinOdds), ~str_remove_all(., "[,+]")), 
         across(c(WinOdds), 
                ~odds.us2prob(as.numeric(.)),
                .names = "{col}_prob")) %>% 
  filter(!is.na(WinOdds))

# Write csv
write_csv(x = comb_res, file = "data/PicksOddscsv/12Masters.csv")

