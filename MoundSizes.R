## Script to process 2022 mound data

# Load 2022 field data from Google (or local repo)
library(tidyverse)
library(googlesheets4)
m22 <- read_sheet("https://docs.google.com/spreadsheets/d/1Zn-B7ekaMRlAx3QbuQ3RiEGaX_IQlPpfMvTehfRyCy0/edit#gid=330777145")

# if authentication is giving you trouble, switch it off
gs4_deauth()
??deauth

#look at the data
class(m22)

# check for duplicate UUIDs
m22 %>% 
  group_by(uuid) %>% 
  count(sort = T)

which(duplicated(m22$uuid))

# check for duplicate human ids
m22 %>% 
  group_by(identifier) %>% 
  count(sort = T)

# filter out burial mounds
colnames(m22)
m <- m22 %>% 
  select(MoundID, Date, Type, Source, LanduseAroundMound, LanduseOnTopOfMound, Latitude, Longitude, Accuracy, HeightMax, HeightMin, DiameterMax,
         Condition, PrincipalSourceOfImpact)
m %>% 
  group_by(Type) %>% 
  tally()


# well, if you don't want to be filtering the bracketed comments away, start over and pass the dataset through OpenRefine.

# The things to do are: 
# - eliminate comments from Type, Source, Landuse, HeightMax, and Diameter columns 

# mounds can be found here https://docs.google.com/spreadsheets/d/1Zn-B7ekaMRlAx3QbuQ3RiEGaX_IQlPpfMvTehfRyCy0/edit#gid=330777145
# OR script is here https://docs.google.com/document/d/1Xcq5yuQOrQpVOkGF2fp3ZeK4ynyaTVa3sZZ41NI7ZF8/edit

# Loading data again, now with more columns as brackets have been expanded to own columns
getwd()
m <- read_csv("data/2022AllBurialMounds-OR17Octcsv.csv")

colnames(m)
tail(m)
m <- m[-nrow(m),]

summary(m$Accuracy)

# Select columns
m22 <- m %>% 
  select(MoundID, Date, TypeClean, Source, LanduseAroundMound, LanduseOnTopOfMound, Latitude, Longitude, Accuracy, HeightMax, HeightMin, DiameterMax,
         Condition, PrincipalSourceOfImpact, createdBy, createdAtGMT)

m %>% 
  group_by(TypeClean) %>% 
  tally()

# Select only 'mound-like' features, skipping scatters and tells, and other or uncertain features

# convert HeightMax to a number, filter mounds, and converge ?? into ?
mounds22 <- m22 %>% 
  filter(grepl("mound", TypeClean, ignore.case = T)) %>% 
  mutate(HeightMax = as.numeric(HeightMax)) %>% 
  mutate(TypeClean = gsub("\\?\\?","\\?", TypeClean))

# check the types
mounds22 %>% 
  group_by(TypeClean) %>% 
  tally()


# check if HeightMax is a number
mounds22$HeightMax

# calculate height statistics on the basis of TypeClean
mounds22 %>% 
  group_by(TypeClean) %>% 
  summarize(meanH = mean(HeightMax, na.rm = T),
            maxH = max(HeightMax, na.rm = T))

# Plot the differences in mound height on the basis of type certainty  

mounds22 %>% 
  ggplot(aes(x = TypeClean, y = HeightMax)) + 
  geom_violin(alpha = 0)+
  geom_jitter(color = "tomato")+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  labs(x = "Feature type",
       y = "Height of feature",
       title = "Mound height by type")

# Plot the differences in mound height on the basis of source

# What are the unique values in Source column?

unique(mounds22$Source)

# first fix the aberrant values in the Source column by overwriting them or using "case_when". Both annotation and question mark should be converted into "Survey"

mounds22$Source[169] <- "Survey"

mounds22 %>% 
  filter(Source == "Survey?") # id 9740


mounds22 <- mounds22 %>% 
  mutate(Source = case_when(
    Source == "Legacy verification" ~ "Legacy",
    Source == "Survey (The mound is depicted in a top map M 1:5000 and is still visible on the satellite  maps.)" ~ "Survey",
    Source == "Survey?" ~ "Survey",
    Source == "Survey" ~ "Survey",
    Source == "Serendipity" ~ "Serendipity"
  ))

mounds22 %>% 
  count(Source)


mounds22 %>% 
  ggplot(aes(x = Source, y = HeightMax)) + 
  geom_violin(alpha = 0)+
  geom_jitter(color = "tomato")+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  labs(x = "Feature source",
       y = "Height of feature",
       title = "Mound height by source")

# Get progress statistics

# Get tally of visited features by team leader and day
mounds22 %>% 
  group_by(Date, createdBy) %>% 
  tally()

# Plot the number of mounds visited per day and color them by Source 

mounds22 %>% 
 group_by(Date) %>%
  ggplot(aes(x = Date))+
  geom_bar(aes(fill = Source))+
  theme_bw()

# Q2 Are the teams improving or getting tired with time?


# Plot the mounds by damage

unique(mounds22$Condition)

mounds22 %>% 
  #mutate(Condition = gsub("\\D","", Condition)) %>% # filter away non-digit characters
  mutate(Condition = substr(Condition, 1,1)) %>%  # grab only the first number
  filter(Condition != "0") %>% 
  group_by(Condition) %>%
  ggplot(aes(x = Condition))+
  geom_bar(aes(fill = Source))+
  coord_flip()+
  theme_bw()

