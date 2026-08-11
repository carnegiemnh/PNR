###avian point count data from MS accessdb
library(RODBC)
library(dplyr)
library(tidyr)
library(data.table)
library(zoo)
setwd("R:/GIS/Kat-working/BirdPointCounts")
#establish connection--new version with cleaning Nov 2025
channel<-odbcConnectAccess2007("R:/GIS/Kat-working/PNR_BirdCounts_2025_ASHLYN.accdb")
sqlTables(channel)
#import tables
bsp<-sqlFetch(channel,"BirdSurveyPoints") ##too big
species_list<-sqlFetch(channel,"tluBirdSpeciesList")
#joining the survey data with bird data
bpc<-sqlFetch(channel,"BirdPointCounts")
ba<-sqlFetch(channel, "BirdAbundance")

pc_a<-left_join(bpc,ba,by="CountDateTime")

#close connection
close(channel)

##########cleaning
##point_id==plot ids
spd<-read.csv("R:/GIS/Kat-working/VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/survey_plot_data.csv",header=TRUE)

veg_plot_ids<-spd$X %>% unique()
###check valid point and plot id
pc_a_plot<-filter(pc_a,!PointID %in% veg_plot_ids)
#count date time, ensure consistent formatting
library(lubridate)
datetime_str <- "20250929-1430"
pc_a<-pc_a %>% mutate(date_time=ymd_hm(CountDateTime))
##some are in different formats, some irregular, some maybe not a date?
pc_a_cdt<-filter(pc_a,is.na(date_time),TRUE)
head(pc_a_cdt$CountDateTime)

#other strings

datetime_str<-c("06112020-0830",datetime_str)
pc_a<-pc_a %>% mutate(date_time=parse_date_time(CountDateTime,c("ymd-HM","mdy-HM")))
pc_a_cdt<-filter(pc_a,is.na(date_time),TRUE)
head(pc_a_cdt$CountDateTime)
###typos for flag
pc_a_cdt$flag<-"date_typo"
pc_a_obs<-filter(pc_a,is.na(Observer1))
pc_a %>% group_by(Observer1)%>% count() #observer listed by initials--3 or 4 letters
pc_a %>% group_by(Observer2)%>% count() #observer listed by initials--3 or 4 letters
#optional field either observer initials or NA

pc_a_notemp<-filter(pc_a,is.na(Temp_C))
pc_a_notemp$flag<-"no_temp"
pc_a_badtemp<-filter(pc_a,Temp_C>30 | Temp_C<0)
##check for range appropriate

pc_a_nowind<-filter(pc_a,is.na(WindSpeed_Beuf))
pc_a_nowind$flag<-"no_wind"
pc_a_wind<-filter(pc_a,WindSpeed_Beuf>7 | WindSpeed_Beuf<0)
pc_a_wind$flag<-"irregular_wind"
###cloud cover
pc_a_nocc<-filter(pc_a,is.na(CloudCover_Perc))
pc_a_nocc$flag<-"no_cc"
pc_a_cc<-filter(pc_a,CloudCover_Perc>1 | CloudCover_Perc<0)

#count comments--leave as is
pc_a %>% count(is.na(CountComments))
# is.na(CountComments)     n
# 1                FALSE 13708
# 2                 TRUE 36575
###############################################################
#Abundance specific data
#time interval per protocol is 4, 5, or 6
pc_a_int<-filter(pc_a, !Time_Interval %in% c(4,5,6))
###maybe previous survey used 1,2,3 instead?
pc_a_int$flag<-"different_TimeInt"
###repeat per protocol is either 0 (no) or 1 (yes)
pc_a_repeat<-filter(pc_a,!Repeat %in% c(0,1))
pc_a_repeat$flag<-"no_repeat"
#no repeats in time interval 4
pc_a_norepeat<-filter(pc_a, Repeat==1 & Time_Interval==4)
###distance per protocol is either 1,2
pc_a_dist<-filter(pc_a, !Distance %in% c(1,2))
pc_a_dist$flag<-"no_dist"
##species code--just check for entry
pc_a_species<-filter(pc_a,is.na(SpeciesCode))
pc_a_species$flag<-"no_code"
pc_a %>% count(SpeciesCode)
#110 unique codes from controlled vocab

#BirdCount-should be whole number
pc_a_count<-filter(pc_a, is.na(BirdCount))
pc_a_count$flag<-"no_count"
pc_a_badcount<-filter(pc_a,BirdCount==0 | BirdCount>25)
pc_a_badcount$flag<-"high_count"

#species comment
pc_a %>% count(is.na(SpeciesComment))
# is.na(SpeciesComment)     n
# 1                 FALSE   843
# 2                  TRUE 49440
#relatively few comments, leave for now

#back to time for range of collections
pc_a<- pc_a %>% mutate(year=year(date_time)) %>% mutate(month=month(date_time)) %>% mutate(day=day(date_time)) %>% mutate(hour=hour(date_time))
pc_a_year<-filter(pc_a,year>2025 | year<2013) #confine to years in dataset
pc_a_month<-filter(pc_a,month>9 | month<5) #confine to May-sept.
pc_a_day<-filter(pc_a,day>31 | day<1) #standard day
pc_a_hour<-filter(pc_a,hour>12,hour<4) #confine to hours of 4am-12pm on 24 hr clock

#join flags
pc_a_flags<-pc_a_cdt
pc_a_flags<-rbind(pc_a_flags,pc_a_notemp)
pc_a_flags<-rbind(pc_a_flags,pc_a_nowind)
pc_a_flags<-rbind(pc_a_flags,pc_a_wind)
pc_a_flags<-rbind(pc_a_flags,pc_a_nocc)
pc_a_flags<-rbind(pc_a_flags,pc_a_int)
pc_a_flags<-rbind(pc_a_flags,pc_a_repeat)
pc_a_flags<-rbind(pc_a_flags,pc_a_dist)
pc_a_flags<-rbind(pc_a_flags,pc_a_species)
pc_a_flags<-rbind(pc_a_flags,pc_a_count)
pc_a_flags<-rbind(pc_a_flags,pc_a_badcount)
#concatenate flags for each plot
pc_a_flags <- pc_a_flags %>%
  group_by(across(-flag)) %>%  # Group by all columns except Flag_type
  summarise(flag = paste(unique(flag), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(pc_a_flags,"pc_a_flagged_updated.csv") #review various issues- recorded in "flag"
pc_a_comments<-filter(pc_a,!is.na(SpeciesComment))
#identify potential metadata in notes
pc_a_comments %>% group_by(SpeciesComment) %>%count()
###potential parsing later for org data
write.csv(pc_a_comments,"species_comments.csv")
pc_a_countcomments<-filter(pc_a, !is.na(CountComments))
#identify potential metadata in notes
pc_a_countcomments %>% group_by(CountComments) %>%count()
##potential parsing later for survey level data

####import taxonomy
channel<-odbcConnectAccess2007("R:/GIS/Kat-working/PNR_BirdCounts_2025_ASHLYN.accdb")
bsl<-sqlFetch(channel, "tluBirdSpeciesList")
bsl<-bsl %>% rename("SpeciesCode"="FourLetterCode")
pc_a_taxo<-left_join(pc_a,bsl,by="SpeciesCode")

#close connection
close(channel)

#check taxo match
pc_a_taxo_null<-filter(pc_a_taxo, !is.na(SpeciesCode) & is.na(OBJECTID))
#all match

#update flags for dates--still missing weather
pca_flags_fixed<-read.csv("pc_a_flagged_updated.csv")
pca_flags_fixed<-pca_flags_fixed %>% mutate(date_time=parse_date_time(date_time,orders=c("ymd-HM","mdy-HM",'m/d/y H:M:S',"y-m-d H:M:S"),tz="UTC"))

###merge with previous version to update uuids
pc_a_clean<-pc_a_taxo %>%
  left_join(pca_flags_fixed, by="OBJECTID.y",suffix = c("", ".new")) %>%
  mutate(
    date_time = coalesce(date_time.new, date_time)
  ) %>%
  mutate(
    WindSpeed_Beuf = coalesce(WindSpeed_Beuf.new, WindSpeed_Beuf))%>% select(-ends_with(".new"))
###redo mdy parsing
pc_a_clean<- pc_a_clean %>% mutate(year=year(date_time)) %>% mutate(month=month(date_time)) %>% mutate(day=day(date_time)) %>% mutate(hour=hour(date_time))
#####remove broken records
pc_a_clean<-pc_a_clean %>% filter(Delete=="" | is.na(Delete))
write.csv(pc_a_clean,"BirdPointCountsClean.csv")
pc<-pc_a_clean[c(1:9,17:21)] %>% unique()
write.csv(pc, "PointCounts.csv")
abundance<-pc_a_clean[c(1,10:16,23:35,37:38)] %>% unique()
write.csv(abundance,"BirdAbundance.csv")
######fix errors in PointID assignment
event_fixes<-read.csv("data_Adjustments.csv")
##fix preexisting events with incorrect ID
pc_a_clean_fixed<-pc_a_clean %>% mutate(PointID=case_when(OBJECTID.x==1031~"7662-4270",OBJECTID.x==1466~"7936-6801",OBJECTID.x==1384~"7997-5002",TRUE~PointID))
###remove bad dupes
remove_events<-as.integer(event_fixes$remove[2:28])
#filter by objectid.x
pc_a_clean_fixed<-pc_a_clean_fixed %>% filter(!OBJECTID.x %in% remove_events)

####check survey completeness 
survey_comp<-pc_a_clean_fixed %>% group_by(PointID,year) %>% select(Time_Interval,Distance) %>% unique

# Create full grid of combinations for each pointid
years <- unique(pc_a_clean_fixed$year)
time_intervals <- c(4, 5, 6) #for 2016 on
distances <- c(1, 2)

grid <- expand.grid(year = years,
                    Time_Interval = time_intervals,
                    Distance = distances)

# Join with pointid
pca_clean_grid <- survey_comp %>% ungroup() %>%
  select(PointID) %>%
  distinct() %>%
  crossing(grid)
#add flag for survey_comp
survey_comp$has_data<-1
# Join and flag completeness
pca_clean_grid <- pca_clean_grid %>%
  left_join(survey_comp,
            by = c("PointID", "year", "Time_Interval", "Distance")) 
pca_clean_grid<-pca_clean_grid%>%
  mutate(has_data=if_else(is.na(has_data),0,1))
##save
write.csv(pca_clean_grid,"PointCount_grid.csv")
####integration checks
sites_16<-filter(pc_a_clean_fixed,year==2016)
sites_16<- unique(sites_16$PointID)
#95 sites
sites_17<-filter(pc_a_clean_fixed,year==2017)
sites_17<-unique(sites_17$PointID)
#95 sites
sites_18<-filter(pc_a_clean_fixed,year==2018)
sites_18<-unique(sites_18$PointID)
#96 sites
sites_19<-filter(pc_a_clean_fixed,year==2019)
sites_19<-unique(sites_19$PointID)
#95 sites
sites_20<-filter(pc_a_clean_fixed,year==2020)
sites_20<-unique(sites_20$PointID)
#97 sites
sites_21<-filter(pc_a_clean_fixed,year==2021)
sites_21<-unique(sites_21$PointID)
#101 sites
sites_22<-filter(pc_a_clean_fixed,year==2022)
sites_22<-unique(sites_22$PointID)
#101 sites
sites_23<-filter(pc_a_clean_fixed,year==2023)
sites_23<-unique(sites_23$PointID)
#101 sites
sites_24<-filter(pc_a_clean_fixed,year==2024)
sites_24<-unique(sites_24$PointID)
#101 sites
sites_25<-filter(pc_a_clean_fixed,year==2025)
sites_25<-unique(sites_25$PointID)
#101 sites
sites_all<-list(c2016=sites_16,c2017=sites_17,c2018=sites_18,c2019=sites_19,c2020=sites_20,c2021=sites_21,c2022=sites_22,c2023=sites_23,c2024=sites_24,c2025=sites_25)

#create full grid
all_sites <- unique(unlist(sites_all))
years <- names(sites_all)
#full grid
site_grid <- expand.grid(Site = all_sites, Year = years)
#pivot wider
library(purrr)
site_grid <- site_grid %>%
  rowwise() %>%
  mutate(has_data = if_else(Site %in% sites_all[[Year]], 1, 0)) %>%
  ungroup()

grid_wide <- site_grid %>%
  pivot_wider(names_from = Year, values_from = has_data)
grid_wide<-grid_wide %>% mutate(repeats=rowSums(across(starts_with("c")),na.rm=TRUE))
grid_wide %>% count(repeats)
#repeats     n
# <dbl> <int>
#   1       1     1
# 2       5     5
# 3       7     1
# 4      10    95

#95/102 for all 10 years
sites_10yr<-filter(grid_wide, repeats==10)
sites_10yr<-as.character(sites_10yr$Site)
#2013-2015
####integration checks
sites_13<-filter(pc_a_clean_fixed,year==2013)
sites_13<- unique(sites_13$PointID)
#96 sites
sites_14<-filter(pc_a_clean_fixed,year==2014)
sites_14<- unique(sites_14$PointID)
#31 sites
sites_15<-filter(pc_a_clean_fixed,year==2015)
sites_15<- unique(sites_15$PointID)
#23 sites
sites_early<-list(c2013=sites_13,c2014=sites_14,c2015=sites_15)
#create full grid
early_sites <- unique(unlist(sites_early))
years <- names(sites_early)
#full grid
site_grid2 <- expand.grid(Site = early_sites, Year = years)
#pivot wider
library(purrr)
site_grid2 <- site_grid2 %>%
  rowwise() %>%
  mutate(has_data = if_else(Site %in% sites_early[[Year]], 1, 0)) %>%
  ungroup()

grid_wide2 <- site_grid2 %>%
  pivot_wider(names_from = Year, values_from = has_data)
grid_wide2<-grid_wide2 %>% mutate(repeats=rowSums(across(starts_with("c")),na.rm=TRUE))
########quick view
grid_wide2 %>% count(repeats)
#repeats     n
# <dbl> <int>
#   1       1    43
# 2       2    52
# 3       3     1
#most only surveyed twice, or once--in 2013 and 2014/2015
sites_10yr<-filter(pca_clean_grid, has_data==0 & year>2015) %>%select(PointID) %>% distinct()
##weather python script?
#https://github.com/Karlheinzniebuhr/the-weather-scraper
####start with temp
pc_a_clean_notemp<-filter(pc_a_clean_fixed, is.na(Temp_C))
##extract dates needed for python script
library(lubridate)
#remove time
temp_dates<-unique(as_date(pc_a_clean_notemp$date_time))
temp_dates<-as.character(temp_dates)
writeLines(temp_dates,"temp_dates.txt") #pass this to python script --in wsl environment or docker container
###get weather--iffy results
temp_PNR<-read.csv("KPARECTO2_from_file.csv")
colnames(temp_PNR)<-c("date_time","Temp_C")
#convert to C
install.packages("weathermetrics")
library(weathermetrics)
temp_PNR$Temp_C<-fahrenheit.to.celsius(temp_PNR$Temp_C,round=1)
##matching with date
temp_PNR<-temp_PNR %>% mutate(date_time=parse_date_time(date_time,orders="y-m-d H:M:S",tz="UTC"))
###merge with previous version to update temp---NEED TO FIX THIS LATER
notempdates<-pc_a_notemp$date_time %>% unique()
setDT(temp_PNR)
notemp_datatbl<-data.table(notempdates)
colnames(notemp_datatbl)<-"date_time"
setDT(notemp_datatbl)
####event table
pc_clean_fixed<-pc_a_clean_fixed %>% select(c(OBJECTID.x,PointID,CountDateTime,date_time,year,month,day,hour,Observer1,Observer2,Temp_C,WindSpeed_Beuf,CloudCover_Perc,CountComments)) %>% distinct()
# Step 1: Perform the rolling join by date_time to fill only the NA Temp_C values
notemp_datatbl[temp_PNR, Temp_C := i.Temp_C, on = "date_time", roll = TRUE]


library(data.table)

join_temp_and_fill_with_flag <- function(pc_clean, temp_PNR,roll_direction = "nearest",
                                         max_roll = NA) {
  
  # Convert to data.table for efficient operations
  setDT(pc_clean)
  setDT(temp_PNR)
  
  # Ensure both date_time columns are in POSIXct format for accurate matching
 # pc_clean[, date_time := as.POSIXct(date_time)]
#  temp_PNR[, date_time := as.POSIXct(date_time)]
  
  # Key both tables for rolling joins
  setkey(temp_PNR, date_time)
  setkey(pc_clean, date_time)
  
  # Step 1: Perform the rolling join by date_time to fill only the NA Temp_C values
  
  # Rolling join temps onto pc_clean
  if (is.na(max_roll)) {
    pc_clean[
      temp_PNR,
      Temp_C_new := i.Temp_C,
      on = "date_time",
      roll = roll_direction
    ]
  } else {
    pc_clean[
      temp_PNR,
      Temp_C_new := i.Temp_C,
      on = "date_time",
      roll = max_roll
    ]
  }
  
  
  # Step 2: Flag new temperature additions where Temp_C was NA before
  pc_clean[, temp_added_flag := ifelse(is.na(Temp_C) & !is.na(Temp_C_new), 1, 0)]
  
  # Step 3: Fill Temp_C only where it is NA using Temp_C_new from the rolling join
    pc_clean[
    is.na(Temp_C),
    Temp_C := Temp_C_new
  ]
  
  # Step 4: Propagate the flag (temp_added_flag) across the same objectID.x group
  pc_clean[, temp_added_flag := max(temp_added_flag, na.rm = TRUE), by = OBJECTID.x]
  
  # Step 5: Clean up temporary column Temp_C_new
  pc_clean[, Temp_C_new := NULL]
  
  return(pc_clean[])
}

# Assuming pc_a_clean and temp_PNR are your data tables
result <- join_temp_and_fill_with_flag(pc_clean_fixed, temp_PNR)
#validation

summary(result$Temp_C)
table(result$temp_added_flag)
###only NAs left are from actual missing data online
#wind?
pc_clean_nowind<-filter(pc_clean_fixed, is.na(WindSpeed_Beuf))
#easier to manually change to 0 per database
result<-result %>% mutate(wind_added_flag=case_when(is.na(WindSpeed_Beuf) ~1,TRUE~0))
result<-result %>% mutate(WindSpeed_Beuf=case_when(is.na(WindSpeed_Beuf) ~0,TRUE~WindSpeed_Beuf))
##cloud cover--not available from KPAREC
pc_clean_nocc<-filter(pc_clean_fixed, is.na(CloudCover_Perc))
#some dates have some values--interpolate?
result[
  ,
  cc_interpolated_flag := as.integer(
    is.na(CloudCover_Perc) &
      !is.na(na.approx(CloudCover_Perc, x = date_time, na.rm = FALSE))
  )
]
result[
  ,
  CloudCover_Perc := na.approx(
    CloudCover_Perc,
    x = date_time,
    na.rm = FALSE,
    maxgap = 3600
  )
]
pc_clean_weather<-pc_clean_fixed %>% select (-c(Temp_C,WindSpeed_Beuf,CloudCover_Perc)) %>% left_join(result)
write.csv(pca_clean_weather,"PointCountAbundance_clean_weatherfix.csv")
#####add in from non weather station 
# pc_clean_weather <- pc_clean_weather %>%
#   left_join(date_time, by = "date_time", suffix = c("", "_new")) %>%
#   mutate(
#     Temp_C = coalesce(Temp_C, Temp_C_new)
#   ) %>% 
#   select(-Temp_C_new)
pc_clean_weather<-pc_clean_weather %>%
  mutate(CloudCover_Perc=case_when(is.na(CloudCover_Perc)~.5,TRUE~CloudCover_Perc))
#join with occ
pca_clean_weather<-pc_clean_weather %>% left_join(pc_a_clean_fixed,by=("OBJECTID.x")) %>% select(-c("PointID.y","CountDateTime.y","Observer1.y","Observer2.y","date_time.y","year.y","month.y","day.y","hour.y","CountComments.y")) %>%
  rename("PointID"="PointID.x","CountDateTime"="CountDateTime.x","Observer1"="Observer1.x","Observer2"="Observer2.x","date_time"="date_time.x","year"="year.x","month"="month.x","day"="day.x","hour"="hour.x","CountComments"="CountComments.x") %>%
  mutate(Temp_C = coalesce(Temp_C.x, Temp_C.y), CloudCover_Perc=coalesce(CloudCover_Perc.x,CloudCover_Perc.y),WindSpeed_Beuf=coalesce(WindSpeed_Beuf.x,WindSpeed_Beuf.y)) %>%
  select(-c(Temp_C.x,Temp_C.y, CloudCover_Perc.x,CloudCover_Perc.y,WindSpeed_Beuf.x,WindSpeed_Beuf.y))

write.csv(pca_clean_weather,"PointCountAbundance_clean_fixed_weatherfix_PAC.csv")
