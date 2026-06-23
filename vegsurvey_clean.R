####Veg Survey Data Cleaning
#Export 9-9-25
library(dplyr)
library(uuid)
setwd("R:/GIS/Kat-working/VegetationSurvey/PNR_VegetationSurvey_Dupe")
pd<-read.csv("Plot_Data.csv")
#apply unique uid to each table for ease of updates
pd<-pd %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
library(sf)
st_layers("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_2006_2008_v3.gdb")
###this provides lat/long point for each plot id and relation to block id (3x3 grid)
survey_plots<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_2006_2008_v3.gdb",layer="VegetationSurveyPlots") %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
survey_plots %>% group_by(Block_ID) #blockid=647
#647*9=5823 (missing 15 plots to complete block sampling)
center_plots<-filter(survey_plots, Block_ID == Plot_ID) #center is same id/veg_id -C
survey_plots_complete<-filter(survey_plots,Survey_Complete==1)
####Plot_Data table clean
##add in field for flag 
##3 rows with null plot id, 2 were duplicated, 1 blank
#pd<-filter(pd,!Plot_ID=="")

library(lubridate)
pd$Date_clean<-mdy_hms(pd$Date_Surveyed)
pd$year<-year(pd$Date_clean)
pd$month<-month(pd$Date_clean)
pd$day<-day(pd$Date_clean)
pd$time<-hour(pd$Date_clean)
pd$Date_clean<-ifelse(pd$time==0,format(pd$Date_clean, "%Y-%m-%d"),format(pd$Date_clean,"%Y-%m-%d %H:%M:%S"))

###add ground layer cats
pd$GC_CC<-rowSums(pd[,c("BareSoil_GC","Rocks_GC","Ferns_GC","Forbs_GC","Graminoids_GC")])
#qc checks
pd_no_id<-filter(pd,Plot_ID=="") %>% mutate(Flag_type="no_id")

pd_flags<-pd_no_id
pd_nodate<-filter(pd,is.na(Date_clean)) %>% mutate(Flag_type="no_date")#flag
pd_flags<-bind_rows(pd_flags,pd_nodate)

##check dates match 
pd_noyear<-filter(pd,year>2008 & year<2006) %>% mutate(Flag_type="no_year")#flag
pd_flags<-bind_rows(pd_flags,pd_noyear)

pd_month<-filter(pd,month>9 & month<6) %>% mutate(Flag_type="no_month")#flag
pd_flags<-bind_rows(pd_flags,pd_month)

pd_day<-filter(pd,day>31 & day<1) %>% mutate(Flag_type="no_day")#flag
pd_flags<-bind_rows(pd_flags,pd_day)

#check time, strip if listed as 0 (come back)
pd_time<-filter(pd,time>18 | time<5 & time>0) %>% mutate(Flag_type="bad_time") #weird times, flag
pd_flags<-bind_rows(pd_flags,pd_time)


#check disturbance
pd_dist<-filter(pd,Disturbance=="") %>% mutate(Flag_type="no_disturbance")
pd_flags<-bind_rows(pd_flags,pd_dist)

pd %>% count(Disturbance) #check they match
pd %>% count(d_Disturbance)
#flag for filter
pd_dist_unk<-filter(pd, Disturbance=="UNK") %>% mutate(Flag_type="unk_disturbance")
pd_flags<-bind_rows(pd_flags,pd_dist_unk)

#check surveyor; flag for filter 
pd_surv<-filter(pd,Surveyor=="") %>% mutate(Flag_type="no_surveyor")
pd_flags<-bind_rows(pd_flags,pd_surv)

#check match
pd %>% count(Surveyor)
pd %>% count(d_Surveyor)

######canopy cover
pd_canopy<-filter(pd,N_CanCov>96 | E_CanCov>96 | S_CanCov>96 | W_CanCov >96 | is.na(N_CanCov)| is.na(E_CanCov)| is.na(S_CanCov)| is.na(W_CanCov)) %>% mutate(Flag_type="canopycover")
pd_flags<-bind_rows(pd_flags,pd_canopy)

##all na with notes
pd_no_canopy<-filter(pd_canopy,is.na(N_CanCov)& is.na(E_CanCov)& is.na(S_CanCov)& is.na(W_CanCov)) %>% mutate(Flag_type="no_canopycover")
pd_flags<-bind_rows(pd_flags,pd_no_canopy)

#some values, flag for filter later
pd_some_canopy<-filter(pd_canopy,!Plot_ID %in% pd_no_canopy$Plot_ID) %>% mutate(Flag_type="missing_canopy")
pd_flags<-bind_rows(pd_flags,pd_some_canopy)

######veg structure: 0-6
pd_vs<-filter(pd,Mid_VS_CC >6 | Low_VS_CC >6 | Ground_VS_CC >6 |Mid_VS_CC <0 | Low_VS_CC <0 | Ground_VS_CC <0) %>% mutate(Flag_type="nonstandard_vs")
pd_flags<-bind_rows(pd_flags,pd_vs)

pd_no_vs<-filter(pd, is.na(Mid_VS_CC) & is.na(Low_VS_CC) & is.na(Ground_VS_CC))%>% mutate(Flag_type="no_vs")
pd_flags<-bind_rows(pd_flags,pd_no_vs)

#some values, flag for filter later
pd_some_vs<-filter(pd, is.na(Mid_VS_CC) | is.na(Low_VS_CC) | is.na(Ground_VS_CC)) %>%
  filter(!Plot_ID %in% pd_no_vs$Plot_ID)%>% mutate(Flag_type="missing_vs")
pd_flags<-bind_rows(pd_flags,pd_some_vs)

#check that range of values are used
pd %>% count(Mid_VS_CC)
pd %>% count(Low_VS_CC)
pd %>% count(Ground_VS_CC) ######no 0 but high number of NA, investigate

###ground cats: 0-6 but sum should be <20
pd_gc<-filter(pd,BareSoil_GC >6 | Rocks_GC >6 | Ferns_GC >6 |Forbs_GC >6 | Graminoids_GC>6| BareSoil_GC <0 | Rocks_GC <0 | Ferns_GC <0 |Forbs_GC <0 | Graminoids_GC <0) %>% mutate(Flag_type="nonstandard_gc")
pd_flags<-bind_rows(pd_flags,pd_gc)

#flagged with cc
pd_no_gc<-filter(pd, is.na(BareSoil_GC) & is.na(Rocks_GC) & is.na(Ferns_GC) & is.na(Forbs_GC)& is.na(Graminoids_GC))%>% mutate(Flag_type="no_gc")
pd_flags<-bind_rows(pd_flags,pd_no_gc)

#some values, flag for filter later, why are there NA
pd_some_gc<-filter(pd, is.na(BareSoil_GC) | is.na(Rocks_GC) | is.na(Ferns_GC) | is.na(Forbs_GC)| is.na(Graminoids_GC)) %>%
  filter(!Plot_ID %in% pd_no_gc$Plot_ID)%>% mutate(Flag_type="missing_gc")
pd_flags<-bind_rows(pd_flags,pd_some_gc)

#filter for high values, flag
pd_gc_sum<-filter(pd,GC_CC>19) %>% mutate(Flag_type="nonstandard_GC")
pd_flags<-bind_rows(pd_flags,pd_gc_sum)

#check ranges used 
pd %>% count(BareSoil_GC)
pd %>% count(Rocks_GC)
pd %>% count(Ferns_GC)
pd %>% count(Forbs_GC)
pd %>% count(Graminoids_GC)

#leaf litter, separate scale 0:5
#flag, unknown NAs
pd_ll<-filter(pd,Leaf_Litter_Abun >5 | Leaf_Litter_Abun <0 | is.na(Leaf_Litter_Abun==TRUE)) %>% mutate(Flag_type="nonstandard_ll")
pd_flags<-bind_rows(pd_flags,pd_ll)

pd %>% count(Leaf_Litter_Abun)

###Notes leave--assign to table 

#dbase = standard protocol
pd_dbase<-filter(pd,dbase=="") %>% mutate(Flag_type="no_dbase")
pd_flags<-bind_rows(pd_flags,pd_dbase)

pd %>% count(dbase) #check that codes match standard term
pd %>% count(d_dbase)
#concatenate flags for each plot
pd_flags <- pd_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(pd_flags,"plot_data_flagged.csv") #review various issues- recorded in "Flag_type"
##use pd_flags and recombine wih pd
###Trees
trees<-read.csv("Trees_ExportTable.csv")
trees<-trees  %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
trees_no_id<-filter(trees,Plot_ID=="") %>% mutate(Flag_type="no_id")# save for problem records file; ensure all ids present and match survey plots table
trees_wrong_id<-filter(trees,!Plot_ID %in% survey_plots_complete$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#plant codes/taxonomy--just scientific name including non sp level ids
trees_no_code<-filter(trees,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")
trees_unk<-filter(trees,Plant_Code=="UNK") %>% mutate(Flag_type="unk_plantcode")#potentially resolvable with 2025 data

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
trees %>% count(Plant_Code)
trees %>% count(d_Plant_Code)

#check dbh
trees_no_dbh<-filter(trees,DBH_cm=="")%>% mutate(Flag_type="no_dbh")
#irregular dbh
trees_dbh<-filter(trees,DBH_cm<8 | DBH_cm>200) %>% mutate(Flag_type="nonstandard_dbh")
###Notes leave--assign to table 
trees_dbase<-filter(trees,dbase=="") %>% mutate(Flag_type="no_dbase")

unique(pd$dbase)  #check that codes match standard term
unique(trees$dbase)

dbase<-unique(trees$dbase)

unique(pd$d_dbase)  #check that codes match standard term
unique(trees$d_dbase)

#Poss error--auto flag
trees_error<-filter(trees,PossError=="Yes") %>% mutate(Flag_type="poss_error")

##flagged records
trees_flags<-trees_no_id
trees_flags<-bind_rows(trees_flags,trees_wrong_id)
trees_flags<-bind_rows(trees_flags,trees_no_code)
trees_flags<-bind_rows(trees_flags,trees_unk)
trees_flags<-bind_rows(trees_flags,trees_no_dbh)
trees_flags<-bind_rows(trees_flags,trees_dbh)
trees_flags<-bind_rows(trees_flags,trees_dbase)
trees_flags<-bind_rows(trees_flags,trees_error)

#concatenate flags for each plot
trees_flags <- trees_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(trees_flags,"Trees_flagged.csv")

#####Understory
#Note this includes small trees, shrubs, and vines! within 5m of center plot (centroid point is the same but polygon would be different)
us<-read.csv("Understory_ExportTable.csv")
us<-us %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
us_no_id<-filter(us,Plot_ID=="")%>% mutate(Flag_type="no_id")
# save for problem records file; ensure all ids present and match survey plots table
us_wrong_id<-filter(us,!Plot_ID %in% survey_plots_complete$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
us %>% count(Genus)
us %>% count(d_Genus) #use controlled table

#plant codes/taxonomy--just genus
us_no_code<-filter(us,d_Genus=="")%>% mutate(Flag_type="no_plantcode")

######CC: 0-6
us_cc<-filter(us,CC_0_05 >6 | CC_05_2 >6 | CC_2_5 >6 |CC_0_05 <0 | CC_05_2 <0 | CC_2_5 <0)%>% mutate(Flag_type="nonstandard_CC")

us_no_cc<-filter(us, is.na(CC_0_05==TRUE) & is.na(CC_05_2==TRUE) & is.na(CC_2_5==TRUE))%>% mutate(Flag_type="no_cc")

#some values, flag for filter later
us_some_cc<-filter(us, is.na(CC_0_05==TRUE) | is.na(CC_05_2==TRUE) | is.na(CC_2_5==TRUE)) %>%
  filter(!Plot_ID %in% us_no_cc$Plot_ID)%>% mutate(Flag_type="missing_cc")

#check that range of values are used
us %>% count(CC_0_05)
us %>% count(CC_05_2)
us %>% count(CC_2_5)

###Notes leave--assign to table 
us_dbase<-filter(us,dbase=="")%>% mutate(Flag_type="no_dbase")

unique(pd$dbase)  #check that codes match standard term
unique(us$dbase)

unique(pd$d_dbase)  #check that codes match standard term
unique(us$d_dbase)

##flagged records
us_flags<-us_no_id
us_flags<-bind_rows(us_flags,us_wrong_id)
us_flags<-bind_rows(us_flags,us_no_code)
us_flags<-bind_rows(us_flags,us_cc)
us_flags<-bind_rows(us_flags,us_no_cc)
us_flags<-bind_rows(us_flags,us_some_cc)
us_flags<-bind_rows(us_flags,us_dbase)

#concatenate flags for each plot
us_flags <- us_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(us_flags,"Understory_flagged.csv")

########## Dominant Shrubs
#primary >50%
ds<-read.csv("Dominant_Shrubs_ExportTable.csv")
ds<-ds  %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
ds_no_id<-filter(ds,Plot_ID=="")%>% mutate(Flag_type="no_id") # save for problem records file; ensure all ids present and match survey plots table
ds_wrong_id<-filter(ds,!Plot_ID %in% survey_plots_complete$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
ds %>% count(Primary_PlantCode)
ds %>% count(d_Primary_PlantCode) #use controlled table
ds %>% count(Secondary_PlantCode)
ds %>% count(d_Secondary_PlantCode) #use controlled table

#plant codes/taxonomy
ds_no_code<-filter(ds,Primary_PlantCode=="")%>% mutate(Flag_type="no_plantcode")

######CC: 0-6 for primary
ds_cc<-filter(ds,CC_Primary >6 | CC_Primary <0)%>% mutate(Flag_type="nonstandard_cc")

ds_no_cc<-filter(ds, is.na(CC_Primary==TRUE))%>% mutate(Flag_type="no_cc")

######CC: 0-6 for secondary if listed
ds_cc2<-filter(ds,CC_Secondary >6 | CC_Secondary <0)%>% mutate(Flag_type="nonstandard_cc")
##if secondary plant code listed but no cc
ds_no_cc2<-filter(ds, is.na(CC_Secondary==TRUE) & is.na(Secondary_PlantCode==FALSE))%>% mutate(Flag_type="no_cc")
# no plant code but cc
ds_no_pc2<-filter(ds, is.na(CC_Secondary==FALSE) & is.na(Secondary_PlantCode==TRUE))%>% mutate(Flag_type="no_plantcode")

#check that range of values are used
ds %>% count(CC_Primary)
ds %>% count(CC_Secondary)

###Notes leave--assign to table 
ds_dbase<-filter(ds,dbase=="")%>% mutate(Flag_type="no_dbase")

unique(pd$dbase)  #check that codes match standard term
unique(ds$dbase)

unique(pd$d_dbase)  #check that codes match standard term
unique(ds$d_dbase)

##flagged records
ds_flags<-ds_no_id
ds_flags<-bind_rows(ds_flags,ds_wrong_id)
ds_flags<-bind_rows(ds_flags,ds_no_code)
ds_flags<-bind_rows(ds_flags,ds_cc)
ds_flags<-bind_rows(ds_flags,ds_no_cc)
ds_flags<-bind_rows(ds_flags,ds_no_cc2)
ds_flags<-bind_rows(ds_flags,ds_no_pc2)
ds_flags<-bind_rows(ds_flags,ds_dbase)

ds_flags <- ds_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(ds_flags,"Dominant_Shrubs_flagged.csv")
########## Herbaceous
#grasses, forbs, ferns, top 5 species--not listed in a hierarchy like dom shrubs
herb<-read.csv("DataExports/Herbaceous_ExportTable.csv")
herb<-herb  %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
herb_no_id<-filter(herb,Plot_ID=="") %>% mutate(Flag_type="no_id")# save for problem records file; ensure all ids present and match survey plots table
herb_wrong_id<-filter(herb,!Plot_ID %in% survey_plots_complete$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
herb %>% count(Plant_Code)
herb %>% count(d_Plant_Code) #use controlled table

#plant codes/taxonomy
herb_no_code<-filter(herb,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")

######CC: 0-6 for primary
herb_cc<-filter(herb,CC >6 | CC <0)%>% mutate(Flag_type="nonstandard_cc")

herb_no_cc<-filter(herb, is.na(CC==TRUE))%>% mutate(Flag_type="no_cc")

#check that range of values are used
herb %>% count(CC)
###check number of plants per plot
herb_codes_plot<-herb %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Plant_Code))
herb_codes_plot<-filter(herb_codes_plot,unique_count>5) #all left in

###Notes leave--weird notes
herb_notes<-filter(herb,!Notes=="")%>% mutate(Flag_type="check_note")
#dbase
herb_dbase<-filter(herb,dbase=="")%>% mutate(Flag_type="no_dbase")

unique(pd$dbase)  #check that codes match standard term
unique(herb$dbase)

unique(pd$d_dbase)  #check that codes match standard term
unique(herb$d_dbase)

##flagged records
herb_flags<-herb_no_id
herb_flags<-bind_rows(herb_flags,herb_wrong_id)
herb_flags<-bind_rows(herb_flags,herb_no_code)
herb_flags<-bind_rows(herb_flags,herb_cc)
herb_flags<-bind_rows(herb_flags,herb_no_cc)
herb_flags<-bind_rows(herb_flags,herb_notes)
herb_flags<-bind_rows(herb_flags,herb_dbase)

herb_flags <- herb_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(herb_flags,"FlaggedRecords/Herbaceous_flagged_update.csv")
herb_flags<-read.csv("FlaggedRecords/Herbaceous_flagged2008.csv")
##special concern
sc<-read.csv("Special_Concern_ExportTable.csv")
sc<-sc  %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
sc_no_id<-filter(sc,Plot_ID=="") %>% mutate(Flag_type="no_id")# save for problem records file; ensure all ids present and match survey plots table
sc_wrong_id<-filter(sc,!Plot_ID %in% survey_plots_complete$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
sc %>% count(Plant_Code)
sc %>% count(d_Plant_Code) #use controlled table

#plant codes/taxonomy
sc_no_code<-filter(sc,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")

######CC: 0-6 for primary
sc_cc<-filter(sc,CC >6 | CC <0)%>% mutate(Flag_type="nonstandard_cc")

sc_no_cc<-filter(sc, is.na(CC==TRUE))%>% mutate(Flag_type="no_cc")

#check that range of values are used
sc %>% count(CC)
###check number of plants per plot
sc_codes_plot<-sc %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Plant_Code))

###No notes
#dbase
sc_dbase<-filter(sc,dbase=="")%>% mutate(Flag_type="no_dbase")

unique(pd$dbase)  #check that codes match standard term
unique(sc$dbase)

unique(pd$d_dbase)  #check that codes match standard term
unique(sc$d_dbase)

##flagged records
sc_flags<-sc_no_id
sc_flags<-bind_rows(sc_flags,sc_wrong_id)
sc_flags<-bind_rows(sc_flags,sc_no_code)
sc_flags<-bind_rows(sc_flags,sc_cc)
sc_flags<-bind_rows(sc_flags,sc_no_cc)
sc_flags<-bind_rows(sc_flags,sc_dbase)
sc_flags <- sc_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(sc_flags,"Special_Concern_flagged.csv")

##Invasives
inv<-read.csv("Invasives_ExportTable.csv")
inv<-inv  %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
inv_no_id<-filter(inv,Plot_ID=="") %>% mutate(Flag_type="no_id") # save for problem records file; ensure all ids present and match survey plots table
inv_wrong_id<-filter(inv,!Plot_ID %in% survey_plots_complete$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
inv %>% count(Plant_Code)
inv %>% count(d_Plant_Code) #use controlled table

#plant codes/taxonomy
inv_no_code<-filter(inv,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")

######CC: 0-6 for primary
inv_cc<-filter(inv,CC >6 | CC <0)%>% mutate(Flag_type="nonstandard_cc")

inv_no_cc<-filter(inv, is.na(CC==TRUE))%>% mutate(Flag_type="no_cc")

#check that range of values are used
inv %>% count(CC)
###check number of plants per plot
inv_codes_plot<-inv %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Plant_Code))
inv_codes_plot<-filter(inv_codes_plot,unique_count>5)
###Check notes

#dbase
inv_dbase<-filter(inv,dbase=="" | is.na(dbase==TRUE)) %>%mutate(Flag_type="no_dbase")


unique(pd$dbase)  #check that codes match standard term
unique(inv$dbase)

unique(pd$d_dbase)  #check that codes match standard term
unique(inv$d_dbase)

##flagged records
inv_flags<-inv_no_id
inv_flags<-bind_rows(inv_flags,inv_wrong_id)
inv_flags<-bind_rows(inv_flags,inv_no_code)
inv_flags<-bind_rows(inv_flags,inv_cc)
inv_flags<-bind_rows(inv_flags,inv_no_cc)
inv_flags<-bind_rows(inv_flags,inv_no_pc2)
inv_flags<-bind_rows(inv_flags,inv_dbase)
inv_flags<-bind_rows(inv_flags,inv_codes_plot)

inv_flags <- inv_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(inv_flags,"Invasives_flagged.csv")

######################CWD
#related to ground cover--woody debris
cwd<-read.csv("CWD_ExportTable.csv")
cwd<-cwd  %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
cwd_no_id<-filter(cwd,Plot_ID=="") %>% mutate(Flag_type="no_id") # save for problem records file; ensure all ids present and match survey plots table
cwd_wrong_id<-filter(cwd,!Plot_ID %in% survey_plots_complete$Plot_ID)%>% mutate(Flag_type="wrong_pid")

######Decay Class: 1-5
cwd_dc<-filter(cwd,Decay_Class >5 | Decay_Class <1)%>% mutate(Flag_type="nonstandard_dc")
cwd_no_dc<-filter(cwd, is.na(Decay_Class==TRUE))%>% mutate(Flag_type="no_dc")

#size class but decay class >3
cwd_dc_sc<-filter(cwd, Decay_Class>3 & is.na(Size_Class==FALSE))%>% mutate(Flag_type="nonstandard_sc")

#check that range of values are used
cwd %>% count(Decay_Class)

##Size class: 2-6 (should only be entered for decay classes 1-3)
cwd_sc<-filter(cwd,Size_Class>6 | Size_Class<2)%>% mutate(Flag_type="nonstandard_sc")
#check that range of values are used
cwd %>% count(Size_Class)


###check number of entries per plot
cwd_dc_plot<-cwd %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Decay_Class))

###check notes
cwd_notes<-filter(cwd,!Notes=="")%>% mutate(Flag_type="check_notes")
#dbase
cwd_dbase<-filter(cwd,dbase=="")%>% mutate(Flag_type="no_dbase")

unique(pd$dbase)  #check that codes match standard term
unique(cwd$dbase)

unique(pd$d_dbase)  #check that codes match standard term
unique(cwd$d_dbase)

##flagged records
cwd_flags<-cwd_no_id
cwd_flags<-bind_rows(cwd_flags,cwd_wrong_id)
cwd_flags<-bind_rows(cwd_flags,cwd_no_dc)
cwd_flags<-bind_rows(cwd_flags,cwd_dc_sc)
cwd_flags<-bind_rows(cwd_flags,cwd_no_sc)
cwd_flags<-bind_rows(cwd_flags,cwd_dc)
cwd_flags<-bind_rows(cwd_flags,cwd_sc)
cwd_flags<-bind_rows(cwd_flags,cwd_notes)
cwd_flags<-bind_rows(cwd_flags,cwd_dbase)
cwd_flags <- cwd_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(cwd_flags,"CWD_flagged.csv")

##################reconciliation with flagged records
trees_flags_fixed<-read.csv("FlaggedRecords/Flags_2008/Trees_flagged2008.csv")
####merge flag files
trees_flags_fixed<-trees_flags %>%
  left_join(trees_flags_fixed, by="id",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Plant_Code.new, Plant_Code)
  ) %>% mutate(DBH_cm=coalesce(DBH_cm.new,DBH_cm))%>% select(-c(Plant_Code.new,DBH_cm.new,X,Plot_ID.new,Notes.new,dbase.new,PossError.new,d_Plant_Code.new,d_dbase.new))
###merge with previous version to update uuids
trees_clean<-trees %>%
  left_join(trees_flags_fixed, by="id",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Plant_Code.new, Plant_Code)
  ) %>% mutate(DBH_cm=coalesce(DBH_cm.new,DBH_cm))%>% select(-c(Plant_Code.new,DBH_cm.new,Plot_ID.new,Notes.new,dbase.new,PossError.new,d_Plant_Code.new,d_dbase.new))
#separate flagged records
trees_flags_remove<-filter(trees_flags_fixed,Keep.=="N")
#match remove data with main data
trees_clean<-filter(trees_clean,!id %in% trees_flags_remove$id)
write.csv(trees_clean,"TreesClean.csv")
#understory has issues
us_flags_fixed<-read.csv("FlaggedRecords/Flags_2008/Understory_flagged2008.csv")
us_flags_fixed<-us_flags %>%
  left_join(us_flags_fixed, by="id",suffix = c("", ".new")) %>%
  mutate(
    d_Genus = coalesce(d_Genus.new, d_Genus)
  ) %>%  select(-c(Genus.new,CC_0_05.new,CC_05_2.new,CC_2_5.new,d_CC_0_05.new,d_CC_05_2.new,d_CC_2_5.new,X,Plot_ID.new,Notes.new,dbase.new,d_Genus.new,d_dbase.new))

us_flags_remove<-filter(us_flags_fixed,Keep.=="N")
###merge with previous version to update uuids
us_clean<-us %>%
  left_join(us_flags_fixed, by="id",suffix = c("", ".new")) %>%
  mutate(
    d_Genus = coalesce(d_Genus.new, d_Genus)
  ) %>%  select(-c(Genus.new,CC_0_05.new,CC_05_2.new,CC_2_5.new,d_CC_0_05.new,d_CC_05_2.new,d_CC_2_5.new,Plot_ID.new,Notes.new,dbase.new,d_Genus.new,d_dbase.new))
us_clean<-filter(us_clean,!id %in% us_flags_remove$id)
write.csv(us_clean,"UnderstoryClean.csv")
#dominant shrubs keep original

ds_flags_fixed<-read.csv("FlaggedRecords/Flags_2008/Dominant_Shrubs_flagged2008.csv")
#no updates necessary
ds_flags_remove<-filter(ds_flags_fixed,Keep.=="N")
###merge with previous version to update uuids
ds_clean<-ds %>%
  left_join(ds_flags_fixed, by="id",suffix = c("", ".new")) %>%
   select(-c(Plot_ID.new,Primary_PlantCode.new,CC_Primary.new,Secondary_PlantCode.new,CC_Secondary.new,Notes.new,dbase.new,d_Primary_PlantCode.new,d_CC_Primary.new,d_Secondary_PlantCode.new,d_CC_Secondary.new,d_dbase.new,X))
ds_clean<-filter(ds_clean,!id %in% ds_flags_remove$id)
write.csv(ds_clean,"DominantShrubs_Clean.csv")

#Herbaceous
herb_flags_fixed<-read.csv("FlaggedRecords/Flags_2008/Herbaceous_flagged2008.csv")
####merge flag files
herb_flags_remove<-filter(herb_flags_fixed,Keep.=="N")
#id add step if needed
herb<-herb %>% left_join(herb_clean,by=c("Plot_ID","Plant_Code","CC","Notes","dbase","d_Plant_Code","d_CC","d_dbase")) %>% select(-c(acceptedPlant_Code,Flag_type,Keep.,Kat.notes))
herb<-distinct(herb)
###merge with previous version to update uuids
herb_clean<-herb %>%
  left_join(herb_flags_fixed, by="id",suffix = c("", ".new")) %>%
  mutate(
    d_Plant_Code = coalesce(d_Plant_Code.new, d_Plant_Code)
  ) %>%mutate(
    Plant_Code = coalesce(Plant_Code.new, Plant_Code)
  )
#cleanup--alt function
herb_clean<-herb_clean %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new"))

#match remove data with main data
herb_clean<-filter(herb_clean,!id %in% herb_flags_remove$id)
herb_clean<-filter(herb_clean, !is.na(id))
write.csv(herb_clean,"Herbaceous_clean_2008.csv")
#special concern no flags
sc_clean<-sc
write.csv(sc_clean,"SpecialConcern.csv")
#invasives
inv_flags_fixed<-read.csv("FlaggedRecords/Flags_2008/Invasives_flagged.csv")

###merge with previous version to update uuids
inv_clean<-inv %>%
  left_join(inv_flags_fixed, by=c("id","Plot_ID","Plant_Code","CC","Notes","dbase","d_Plant_Code","d_CC","d_dbase")) %>% select(-c(X,unique_count))

write.csv(inv_clean,"Invasives.csv")
#cwd
cwd_flags_fixed<-read.csv("FlaggedRecords/Flags_2008/CWD_flagged2008.csv")
cwd_flags_fixed<-cwd_flags %>%
  left_join(cwd_flags_fixed, by=c("Plot_ID","Decay_Class","Size_Class","Notes","dbase","d_Decay_Class","d_Size_Class","d_dbase","id"))

#match remove data with main data
cwd_clean<-cwd %>%
  left_join(cwd_flags_fixed, by=c("Plot_ID","Decay_Class","Size_Class","Notes","dbase","d_Decay_Class","d_Size_Class","d_dbase","id"))
#cleanup
cwd_clean<-cwd_clean %>%
  select(-"X")

cwd_flags_remove<-filter(cwd_clean,Keep.=="N")

#match remove data with main data
cwd_clean<-filter(cwd_clean,!id %in% cwd_flags_remove$id)
check_plots<-cwd_clean %>% filter(Flag_type=="check_notes") %>% group_by(Plot_ID) %>% summarise(n=n())
##confirm plots are correct
pd_clean %>% filter(Plot_ID %in% check_plots$Plot_ID)
cwd_clean_ag<-cwd_clean %>% select(-c(Flag_type,Keep.))
#ARcGIS format
write.csv(cwd_clean_ag,"CWD_2008_ag")
###########synthesize plot level data==Plot Data, survey_plots into raster layers
###occurrence for trees, understory, dominant shrubs, herbaceous species, special concern, and invasives associated with plot level data
pd_flags_fixed<-read.csv("FlaggedRecords/Flags_2008/plot_data_flagged2008.csv")
pd_flags_fixed<-pd_flags %>%
  left_join(pd_flags_fixed, by="id",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new")) 
###merge
pd_clean<-pd %>%
  left_join(pd_flags_fixed, by="id",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new")) %>% select(-X)

pd_flags_remove<-filter(pd_flags_fixed,Keep.=="N")

#match remove data with main data
pd_clean<-filter(pd,!(Plot_ID %in% pd_flags_remove$Plot_ID))
pd_clean_ag<-pd_clean[c(1:37)]
write.csv(pd_clean_ag,"plot_data_ag.csv")
pd_sp<-full_join(pd_clean,survey_plots_complete,by="Plot_ID")
##trim columns
pd_sp<-pd_sp[c(1:21,26:44,46:54)]
pd_sp<-pd_sp %>% rename("sp_id"="id.y")
pd_sp<-pd_sp %>% rename("pd_id"="id.x")
pd_sp$SHAPE<-st_as_text(pd_sp$SHAPE)
write.csv(pd_sp,"survey_plot_data.csv")

######################plot data 
pd_sp<-read.csv("CleanRecords/survey_plot_data_2008.csv") #full level data
pd_sp <-pd_sp %>% rename("pd_Notes"="Notes")
sp_ag<-survey_plots_complete
write.csv(sp_ag,"survey_plot_ag.csv")
###match relevant columns to occurrence tables
#cwd
pd_sp_cwd<-cwd_clean %>% full_join(pd_sp, by="Plot_ID")
##compare dbase values--is this consistent across all tables?
pd_sp_cwd <- pd_sp_cwd %>%
  mutate(dbases = case_when(
    is.na(dbase.x) | is.na(dbase.y) ~ NA,         # NA if either is NA
    dbase.x == dbase.y ~ TRUE,                    # TRUE if equal
    dbase.x != dbase.y ~ FALSE                    # FALSE otherwise
  ))
mismatch_db<-filter(pd_sp_cwd,dbases==FALSE) %>% mutate(Flag_type="mismatch_dbase")
#rejoin
pd_sp_cwd<-pd_sp_cwd%>%
  left_join(mismatch_db, by="id",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new"))

###occasionally databases are different, denote in data with flag
pd_sp_cwd <- pd_sp_cwd %>% rename("CWD_Notes"="Notes") %>% rename("CWD_dbase"="dbase.x") %>% rename("pd_dbase"="dbase.y") %>% rename("d_CWD_dbase"="d_dbase.x") %>% rename("d_pd_dbase"="d_dbase.y") %>% rename("CWD_id"="id")
pd_sp_cwd<-pd_sp_cwd %>% select(-dbases)
write.csv(pd_sp_cwd,"Plot_Veg_Data.csv")
########################################
#2025 data; exported 10-2-2025
pd_2<-read.csv("DataExports/10mVegetationPlotData_ExportTable.csv")
#already has global id
head(pd_2)
library(lubridate)
pd_2$Date_clean<-mdy_hms(pd_2$Date_Surveyed)
pd_2$year<-year(pd_2$Date_clean)
pd_2$month<-month(pd_2$Date_clean)
pd_2$day<-day(pd_2$Date_clean)
pd_2$time<-hour(pd_2$Date_clean)
pd_2$Date_clean<-ifelse(pd_2$time==0,format(pd_2$Date_clean, "%Y-%m-%d"),format(pd_2$Date_clean,"%Y-%m-%d %H:%M:%S"))

###add ground layer cats
pd_2$GC_CC<-rowSums(pd_2[,c("BareSoil_GC","Rocks_GC","Ferns_GC","Forbs_GC","Graminoids_GC")])
#qc checks
pd_2_no_id<-filter(pd_2,Plot_ID=="") %>% mutate(Flag_type="no_id")
pd_2_nodate<-filter(pd_2,is.na(Date_clean)) %>% mutate(Flag_type="no_date")#flag

##check dates match --updated to 2025
pd_2_noyear<-filter(pd_2,!year==2025) %>% mutate(Flag_type="no_year")#flag
pd_2_month<-filter(pd_2,month>9 & month<5) %>% mutate(Flag_type="no_month")#flag
pd_2_day<-filter(pd_2,day>31 & day<1) %>% mutate(Flag_type="no_day")#flag

#check time, strip if listed as 0 (come back)
pd_2_time<-filter(pd_2,time>18 | time<5 & time>0) %>% mutate(Flag_type="bad_time") #weird times, flag

#check disturbance
pd_2_dist<-filter(pd_2,Disturbance=="") %>% mutate(Flag_type="no_disturbance")

pd_2 %>% count(Disturbance) #check they match
pd_2 %>% count(d_Disturbance)

#check surveyor; flag for filter 
pd_2_surv<-filter(pd_2,Surveyor=="") %>% mutate(Flag_type="no_surveyor")

#surveyor_2
pd_2_surv2<-filter(pd_2,surveyor_2=="") %>% mutate(Flag_type="no_surveyor2")

#check match
pd_2 %>% count(Surveyor)
pd_2 %>% count(d_Surveyor)
pd_2 %>% count(surveyor_2)
pd_2 %>% count(d_surveyor_2)

library(stringi)
library(fuzzyjoin)
#reciprocity of surveyors
surveyors<-stringdist_left_join(
  pd_2 %>% select(Surveyor),
  pd_2 %>% select(surveyor_2),
  by=c("Surveyor"="surveyor_2"),
  method="jw",
  max_dist=0.2) %>%
  distinct()
dsurveyors<-stringdist_left_join(
  pd_2 %>% select(d_Surveyor),
  pd_2 %>% select(d_surveyor_2),
  by=c("d_Surveyor"="d_surveyor_2"),
  method="jw",
  max_dist=0.2) %>%
  distinct()
#replace col 2 with standard spelling
pd_2<-pd_2 %>%
  left_join(dsurveyors, by="d_surveyor_2") %>%
  mutate(d_surveyor_2_std=coalesce(d_Surveyor.y,d_surveyor_2)) %>%
  select(-d_Surveyor.y,-d_surveyor_2) %>%
  rename(d_Surveyor=d_Surveyor.x,d_surveyor_2=d_surveyor_2_std)
###confirm changes
dsurveyors<-stringdist_left_join(
  pd_2 %>% select(d_Surveyor),
  pd_2%>% select(d_surveyor_2),
  by=c("d_Surveyor"="d_surveyor_2"),
  method="jw",
  max_dist=0.2) %>%
  distinct()
pd_2_surv2_clean<-filter(pd_2,d_surveyor_2=="Grace Bartley") %>% mutate(Flag_type="name_std")

###rerun filters
#qc checks
pd_2_no_id<-filter(pd_2,Plot_ID=="") %>% mutate(Flag_type="no_id")
pd_2_nodate<-filter(pd_2,is.na(Date_clean)) %>% mutate(Flag_type="no_date")#flag

##check dates match --updated to 2025
pd_2_noyear<-filter(pd_2,!year==2025) %>% mutate(Flag_type="no_year")#flag
pd_2_month<-filter(pd_2,month>9 & month<5) %>% mutate(Flag_type="no_month")#flag
pd_2_day<-filter(pd_2,day>31 & day<1) %>% mutate(Flag_type="no_day")#flag

#check time, strip if listed as 0 (come back)
pd_2_time<-filter(pd_2,time>18 | time<5 & time>0) %>% mutate(Flag_type="bad_time") #weird times, flag

#check disturbance
pd_2_dist<-filter(pd_2,Disturbance=="") %>% mutate(Flag_type="no_disturbance")
#check surveyor; flag for filter 
pd_2_surv<-filter(pd_2,Surveyor=="") %>% mutate(Flag_type="no_surveyor")

#surveyor_2
pd_2_surv2<-filter(pd_2,surveyor_2=="") %>% mutate(Flag_type="no_surveyor2")

######canopy cover--only north
pd_2_canopy<-filter(pd_2,N_CanCov>96 | is.na(N_CanCov)) %>% mutate(Flag_type="canopycover")

###ground cats: 0-6 but sum should be <20
pd_2_gc<-filter(pd_2,BareSoil_GC >6 | Rocks_GC >6 | Ferns_GC >6 |Forbs_GC >6 | Graminoids_GC>6| BareSoil_GC <0 | Rocks_GC <0 | Ferns_GC <0 |Forbs_GC <0 | Graminoids_GC <0) %>% mutate(Flag_type="nonstandard_gc")

#flagged with cc
pd_2_no_gc<-filter(pd_2, is.na(BareSoil_GC) & is.na(Rocks_GC) & is.na(Ferns_GC) & is.na(Forbs_GC)& is.na(Graminoids_GC))%>% mutate(Flag_type="no_gc")

#some values, flag for filter later, why are there NA
pd_2_some_gc<-filter(pd_2, is.na(BareSoil_GC) | is.na(Rocks_GC) | is.na(Ferns_GC) | is.na(Forbs_GC)| is.na(Graminoids_GC)) %>%
  filter(!Plot_ID %in% pd_2_no_gc$Plot_ID)%>% mutate(Flag_type="missing_gc")

#filter for high values, flag
pd_2_gc_sum<-filter(pd_2,GC_CC>19) %>% mutate(Flag_type="nonstandard_GC")

#check ranges used 
pd_2 %>% count(BareSoil_GC)
pd_2 %>% count(Rocks_GC)
pd_2 %>% count(Ferns_GC)
pd_2 %>% count(Forbs_GC)
pd_2 %>% count(Graminoids_GC)

#leaf litter, separate scale 0:5
#flag, unknown NAs
pd_2_ll<-filter(pd_2,Leaf_Litter_Abun >5 | Leaf_Litter_Abun <0 | is.na(Leaf_Litter_Abun==TRUE)) %>% mutate(Flag_type="nonstandard_ll")
pd_2 %>% count(Leaf_Litter_Abun)

###Notes examine
pd_2_notes<-filter(pd_2,!Notes=="") %>% mutate(Flag_type="check_note")

pd_2_flags<-pd_2_no_id
pd_2_flags<-bind_rows(pd_2_flags,pd_2_nodate)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_noyear)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_month)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_day)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_time)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_dist)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_surv)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_surv2)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_surv2_clean)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_canopy)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_gc)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_no_gc)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_some_gc)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_gc_sum)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_ll)
pd_2_flags<-bind_rows(pd_2_flags,pd_2_notes)


#concatenate flags for each plot
pd_2_flags <- pd_2_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(pd_2_flags,"FlaggedRecords/plot_data_flagged_2025.csv") #review various issues- recorded in "Flag_type"
###Trees
trees_2<-read.csv("DataExports/10mTrees_ExportTable.csv")
trees_2_no_id<-filter(trees_2,Plot_ID=="") %>% mutate(Flag_type="no_id")# save for problem records file; ensure all ids present and match survey plots table
trees_2_wrong_id<-filter(trees_2,!Plot_ID %in% pd_2$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#plant codes/taxonomy--just scientific name including non sp level ids
trees_2_no_code<-filter(trees_2,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")
trees_2 %>% count(Plant_Code)

#check codes and def
trees_2 %>% count(d_Plant_Code)

#check dbh
trees_2_no_dbh<-filter(trees_2,DBH_cm=="")%>% mutate(Flag_type="no_dbh")
#irregular dbh
trees_2_dbh<-filter(trees_2,DBH_cm<8 | DBH_cm>200) %>% mutate(Flag_type="nonstandard_dbh")
###Notes examine
trees_2_notes<-filter(trees_2,!Notes=="") %>% mutate(Flag_type="check_note")
#basal area
trees_2_no_ba<-filter(trees_2,BasalArea=="")%>% mutate(Flag_type="no_ba")
#irregular ba?
trees_2_ba<-filter(trees_2,BasalArea==0) %>% mutate(Flag_type="nonstandard_ba")

##flagged records
trees_2_flags<-trees_2_no_id
trees_2_flags<-bind_rows(trees_2_flags,trees_2_wrong_id)
trees_2_flags<-bind_rows(trees_2_flags,trees_2_no_code)
trees_2_flags<-bind_rows(trees_2_flags,trees_2_no_ba)
trees_2_flags<-bind_rows(trees_2_flags,trees_2_ba)
trees_2_flags<-bind_rows(trees_2_flags,trees_2_no_dbh)
trees_2_flags<-bind_rows(trees_2_flags,trees_2_dbh)
trees_2_flags<-bind_rows(trees_2_flags,trees_2_notes)

#concatenate flags for each plot
trees_2_flags <- trees_2_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
##tree notes cleaning--how to check that notes that indicate 1 of 2, 1 of 3, etc. are represented in the data
library(dplyr)
library(stringr)

# Step 1: Extract total indicated number from Tree_Notes (flexible format)
trees_checked <- trees_2 %>% 
  mutate(
    # Extract both numerator and denominator
    note_match = str_match(Notes, "(\\d+)\\s*(?:of|/|OF)\\s*(\\d+)"),
    tree_seq = as.integer(note_match[, 2]),         # the '1' in 1 of 2
    total_indicated = as.integer(note_match[, 3])   # the '2' in 1 of 2
  )

# Step 2: Compare actual counts per group with indicated total
validation_results <- trees_checked %>%
  group_by(Plot_ID, Plant_Code) %>%
  summarise(
    actual_count = n(),
    indicated_total = max(total_indicated, na.rm = TRUE),
    any_notes = any(!is.na(total_indicated)),
    .groups = "drop"
  ) %>%
  filter(any_notes & actual_count != indicated_total)
#reapply notes check
trees_2_flags<-left_join(trees_2_flags,validation_results,by=c("Plot_ID","Plant_Code"))
write.csv(trees_2_flags,"FlaggedRecords/trees_2_flagged.csv")

# validation of notes for 1 of 2, etc. validated--inconsistent recording
#####Understory
#Note this includes small trees, shrubs, and vines! within 5m of center plot (centroid point is the same but polygon would be different)
us_2<-read.csv("DataExports/5mUnderstory_ExportTable.csv")
#us_2<-us_2 %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
us_2_no_id<-filter(us_2,Plot_ID=="")%>% mutate(Flag_type="no_id")
# save for problem records file; ensure all ids present and match survey plots table
us_2_wrong_id<-filter(us_2,!Plot_ID %in% pd_2$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs--this column is Species
us_2 %>% count(Species) #these are messy
us_2 %>% count(d_Species) #use controlled table
###irregular names
us_2_code_o<-filter(us_2,Species=="OTHE")%>% mutate(Flag_type="other_plantcode")
#plant codes/taxonomy--just genus
us_2_no_code<-filter(us_2,d_Species=="")%>% mutate(Flag_type="no_plantcode")
us_2_no_sp<-filter(us_2,Species=="")%>% mutate(Flag_type="no_species")


# cover class
us_2_cc<-filter(us_2,Cover_Class >6 |Cover_Class <0)%>% mutate(Flag_type="nonstandard_CC")
us_2_no_cc<-filter(us_2, is.na(Cover_Class==TRUE) | Cover_Class=="") %>% mutate(Flag_type="no_cc")
##flagged records
us_2_flags<-us_2_no_id
us_2_flags<-bind_rows(us_2_flags,us_2_wrong_id)
us_2_flags<-bind_rows(us_2_flags,us_2_no_code)
us_2_flags<-bind_rows(us_2_flags,us_2_cc)
us_2_flags<-bind_rows(us_2_flags,us_2_no_cc)
us_2_flags<-bind_rows(us_2_flags,us_2_code_o)
us_2_flags<-bind_rows(us_2_flags,us_2_no_sp)

#concatenate flags for each plot
us_2_flags <- us_2_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(us_2_flags,"FlaggedRecords/Understory_flagged_2.csv")
##special concern
sc_2<-read.csv("DataExports/10mSpeciesofSpecialConcern_ExportTable.csv")
sc_2_no_id<-filter(sc_2,Plot_ID=="") %>% mutate(Flag_type="no_id")# save for problem records file; ensure all ids present and match survey plots table
sc_2_wrong_id<-filter(sc_2,!Plot_ID %in% pd_2$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
sc_2 %>% count(Plant_Code)
sc_2 %>% count(d_Plant_Code) #use controlled table

#plant codes/taxonomy
sc_2_no_code<-filter(sc_2,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")

######CC: 0-6 
sc_2_cc<-filter(sc_2,CC >6 | CC <0)%>% mutate(Flag_type="nonstandard_cc")

sc_2_no_cc<-filter(sc_2, CC=="" |is.na(CC==TRUE))%>% mutate(Flag_type="no_cc")

#check that range of values are used
sc_2 %>% count(CC)
###check number of plants per plot
sc_2_codes_plot<-sc_2 %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Plant_Code))

##flagged records
sc_2_flags<-sc_2_no_id
sc_2_flags<-bind_rows(sc_2_flags,sc_2_wrong_id)
sc_2_flags<-bind_rows(sc_2_flags,sc_2_no_code)
sc_2_flags<-bind_rows(sc_2_flags,sc_2_cc)
sc_2_flags<-bind_rows(sc_2_flags,sc_2_no_cc)
sc_2_flags <- sc_2_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(sc_2_flags,"FlaggedRecords/Special_Concern_flagged_2.csv")

inv_2<-read.csv("DataExports/10mInvasives_ExportTable.csv")
inv_2_no_id<-filter(inv_2,Plot_ID=="") %>% mutate(Flag_type="no_id") # save for problem records file; ensure all ids present and match survey plots table
inv_2_wrong_id<-filter(inv_2,!Plot_ID %in% pd_2$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs, could be combined with 2025 data as P/A even though domain checklist changed with additions/subtractions
inv_2 %>% count(Plant_Code)
inv_2 %>% count(d_Plant_Code) #use controlled table

#plant codes/taxonomy
inv_2_no_code<-filter(inv_2,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")

######CC: 0-6 for primary
inv_2_cc<-filter(inv_2,CC >6 | CC <0)%>% mutate(Flag_type="nonstandard_cc")

inv_2_no_cc<-filter(inv_2, CC=="" | is.na(CC==TRUE))%>% mutate(Flag_type="no_cc")

#check that range of values are used
inv_2 %>% count(CC)
###check number of plants per plot
inv_2_codes_plot<-inv_2 %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Plant_Code))
inv_2_codes_plot<-filter(inv_2_codes_plot,unique_count>5)
###Check notes
inv2_notes<-filter(inv_2,!Notes=="") %>% mutate(Flag_type="check_note")
#filter out no_invasives for occ table?
inv_2_no<-filter(inv_2, No_Invasives=="No")
##flagged records
inv_2_flags<-inv_2_no_id
inv_2_flags<-bind_rows(inv_2_flags,inv_2_wrong_id)
inv_2_flags<-bind_rows(inv_2_flags,inv_2_no_code)
inv_2_flags<-bind_rows(inv_2_flags,inv_2_cc)
inv_2_flags<-bind_rows(inv_2_flags,inv_2_no_cc)
inv_2_flags<-bind_rows(inv_2_flags,inv_2_no)

inv_2_flags <- inv_2_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(inv_2_flags,"FlaggedRecords/invasives_flagged_2.csv")
####contains absence records--is this complete?
inv_plots<-filter(pd_2, !Plot_ID %in% inv_2$Plot_ID)
##missing 13 plot records--add to issues
################regen trees 2025
regen<-read.csv("DataExports/5mRegenTrees_ExportTable.csv")
#compare to other tree data
regen_overlap<-filter(regen,Plot_ID %in% trees_2$Plot_ID)
#ok almost everything is in the same plots
##check for no plot or plot that isn't in pd_2
regen_no_id<-filter(regen,Plot_ID=="")%>% mutate(Flag_type="no_id")
# save for problem records file; ensure all ids present and match survey plots table
regen_wrong_id<-filter(regen,!Plot_ID %in% pd_2$Plot_ID)%>% mutate(Flag_type="wrong_pid")

#check codes and defs--this column is Species
regen %>% count(Species) #these are messy
regen %>% count(d_Species) #use controlled table
###irregular names
regen_code_unk<-filter(regen,Species=="UNK")%>% mutate(Flag_type="unk_plantcode")
#plant codes/taxonomy-
regen_no_code<-filter(regen,d_Species=="")%>% mutate(Flag_type="no_plantcode")
regen_no_sp<-filter(regen,Species=="")%>% mutate(Flag_type="no_species")
##flagged records
regen_flags<-regen_no_id
regen_flags<-bind_rows(regen_flags,regen_wrong_id)
regen_flags<-bind_rows(regen_flags,regen_no_code)
regen_flags<-bind_rows(regen_flags,regen_code_unk)
regen_flags<-bind_rows(regen_flags,regen_no_sp)

#concatenate flags for each plot
regen_flags <- regen_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(regen_flags,"FlaggedRecords/regen_trees_flagged2.csv")
###2023 regen
regen_23<-read.csv("DataExports/PNRVegetationSurvey2023RegenTrees_ExportTable.csv")
#compare to other tree data
regen_23_overlap<-filter(regen_23,Plot_ID %in% regen$Plot_ID)
##about 50% overlap with the 2025 data
regen_23_overlap<-filter(regen_23, Plot_ID %in% trees_2$Plot_ID)
##check for no plot or plot that isn't in pd_2
regen_23_no_id<-filter(regen_23,Plot_ID=="")%>% mutate(Flag_type="no_id")
# save for problem records file; ensure all ids present and match survey plots table (both 2008 and 2025)
regen_23_wrong_id<-filter(regen_23,!Plot_ID %in% c(pd_2$Plot_ID,pd$Plot_ID))%>% mutate(Flag_type="wrong_pid")

#check codes and defs--this column is Species
regen_23 %>% count(Species) #these are messy
regen_23 %>% count(d_Species) #use controlled table
###irregular names
regen_23_code_unk<-filter(regen_23,Species=="UNK")%>% mutate(Flag_type="unk_plantcode")
#plant codes/taxonomy-
regen_23_no_code<-filter(regen_23,d_Species=="")%>% mutate(Flag_type="no_plantcode")
regen_23_no_sp<-filter(regen_23,Species=="")%>% mutate(Flag_type="no_species")
# ######CC: 0-6 for primary
regen_23_cc<-filter(regen_23,CC >6 | CC <0)%>% mutate(Flag_type="nonstandard_cc")

regen_23_no_cc<-filter(regen_23, CC=="" | is.na(CC==TRUE))%>% mutate(Flag_type="no_cc")

#check that range of values are used
regen_23 %>% count(CC) #all NA not used

#check notes
regen_23_notes<-filter(regen_23, !Notes=="") %>% mutate(Flag_type="check_note")
##flagged records
regen_23_flags<-regen_23_no_id
regen_23_flags<-bind_rows(regen_23_flags,regen_23_wrong_id)
regen_23_flags<-bind_rows(regen_23_flags,regen_23_no_code)
regen_23_flags<-bind_rows(regen_23_flags,regen_23_code_unk)
regen_23_flags<-bind_rows(regen_23_flags,regen_23_no_sp)
regen_23_flags<-bind_rows(regen_23_flags,regen_23_notes)

#concatenate flags for each plot
regen_23_flags <- regen_23_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(regen_23_flags,"FlaggedRecords/regen_trees_2023_flagged2.csv")

#related to ground cover--woody debris
cwd_2<-read.csv("DataExports/10mCoarseWoodyDebris.csv")
cwd_2_no_id<-filter(cwd_2,Plot_ID=="") %>% mutate(Flag_type="no_id") # save for problem records file; ensure all ids present and match survey plots table
cwd_2_wrong_id<-filter(cwd_2,!Plot_ID %in% c(pd_2$Plot_ID,pd$Plot_ID))%>% mutate(Flag_type="wrong_pid")

cwd_2_new_id<-filter(cwd_2,!Plot_ID %in% survey_plots$Plot_ID)%>% mutate(Flag_type="new_pid")

######Decay Class: 1-5
cwd_2_dc<-filter(cwd_2,Decay_Class >5 | Decay_Class <1)%>% mutate(Flag_type="nonstandard_dc")
cwd_2_no_dc<-filter(cwd_2, Decay_Class==""|is.na(Decay_Class==TRUE))%>% mutate(Flag_type="no_dc")

#size class but decay class >3
cwd_2_dc_sc<-filter(cwd_2, Decay_Class>3 & is.na(Size_Class==FALSE))%>% mutate(Flag_type="nonstandard_sc")
cwd_2_no_sc<-filter(cwd_2,Decay_Class<4 &is.na(Size_Class==TRUE)) %>% mutate(Flag_type="no_sc")
#check that range of values are used
cwd_2 %>% count(Decay_Class)

##Size class: 2-6 (should only be entered for decay classes 1-3)
cwd_2_sc<-filter(cwd_2,Size_Class>6 | Size_Class<2)%>% mutate(Flag_type="nonstandard_sc")
#check that range of values are used
cwd_2 %>% count(Size_Class)

###check number of entries per plot
cwd_2_dc_plot<-cwd_2 %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Decay_Class))

###check notes
cwd_2_notes<-filter(cwd_2,!Notes=="")%>% mutate(Flag_type="check_notes")
##flagged records
cwd_2_flags<-cwd_2_no_id
cwd_2_flags<-bind_rows(cwd_2_flags,cwd_2_wrong_id)
cwd_2_flags<-bind_rows(cwd_2_flags,cwd_2_no_dc)
cwd_2_flags<-bind_rows(cwd_2_flags,cwd_2_dc_sc)
cwd_2_flags<-bind_rows(cwd_2_flags,cwd_2_no_sc)
cwd_2_flags<-bind_rows(cwd_2_flags,cwd_2_dc)
cwd_2_flags<-bind_rows(cwd_2_flags,cwd_2_sc)
cwd_2_flags<-bind_rows(cwd_2_flags,cwd_2_notes)
cwd_2_flags <- cwd_2_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(cwd_2_flags,"FlaggedRecords/cwd_2_flagged.csv")


#####2023 veg survey plots
pd_23<-read.csv("DataExports/PNRVegetationSurvey2023VegetationPlotData_ExportTable.csv")
#already has global id

library(lubridate)
pd_23$Date_clean<-mdy_hms(pd_23$Date_Surveyed)
pd_23$year<-year(pd_23$Date_clean)
pd_23$month<-month(pd_23$Date_clean)
pd_23$day<-day(pd_23$Date_clean)
pd_23$time<-hour(pd_23$Date_clean)
pd_23$Date_clean<-ifelse(pd_23$time==0,format(pd_23$Date_clean, "%Y-%m-%d"),format(pd_23$Date_clean,"%Y-%m-%d %H:%M:%S"))

###add ground layer cats
pd_23$GC_CC<-rowSums(pd_23[,c("BareSoil_GC","Rocks_GC","Ferns_GC","Forbs_GC","Graminoids_GC")])
#qc checks
pd_23_no_id<-filter(pd_23,Plot_ID=="") %>% mutate(Flag_type="no_id")
pd_23_nodate<-filter(pd_23,is.na(Date_clean)) %>% mutate(Flag_type="no_date")#flag

#check disturbance
pd_23_dist<-filter(pd_23,Disturbance=="") %>% mutate(Flag_type="no_disturbance")

pd_23 %>% count(Disturbance) #check they match
pd_23 %>% count(d_Disturbance) #all na
#check surveyor; flag for filter 
pd_23_surv<-filter(pd_23,Surveyor=="") %>% mutate(Flag_type="no_surveyor")

#check match-all na
pd_23 %>% count(Surveyor)
pd_23 %>% count(d_Surveyor)

######canopy cover--only north
pd_23_canopy<-filter(pd_23,N_CanCov>96 | is.na(N_CanCov)) %>% mutate(Flag_type="canopycover")

###ground cats: 0-6 but sum should be <20
pd_23_gc<-filter(pd_23,BareSoil_GC >6 | Rocks_GC >6 | Ferns_GC >6 |Forbs_GC >6 | Graminoids_GC>6| BareSoil_GC <0 | Rocks_GC <0 | Ferns_GC <0 |Forbs_GC <0 | Graminoids_GC <0) %>% mutate(Flag_type="nonstandard_gc")

#flagged with cc
pd_23_no_gc<-filter(pd_23, is.na(BareSoil_GC) & is.na(Rocks_GC) & is.na(Ferns_GC) & is.na(Forbs_GC)& is.na(Graminoids_GC))%>% mutate(Flag_type="no_gc")

#some values, flag for filter later, why are there NA
pd_23_some_gc<-filter(pd_23, is.na(BareSoil_GC) | is.na(Rocks_GC) | is.na(Ferns_GC) | is.na(Forbs_GC)| is.na(Graminoids_GC)) %>%
  filter(!Plot_ID %in% pd_23_no_gc$Plot_ID)%>% mutate(Flag_type="missing_gc")

#filter for high values, flag
pd_23_gc_sum<-filter(pd_23,GC_CC>19) %>% mutate(Flag_type="nonstandard_GC")

#check ranges used 
pd_23 %>% count(BareSoil_GC)
pd_23 %>% count(Rocks_GC)
pd_23 %>% count(Ferns_GC)
pd_23 %>% count(Forbs_GC)
pd_23 %>% count(Graminoids_GC)

#leaf litter, separate scale 0:5
#flag, unknown NAs
pd_23_ll<-filter(pd_23,Leaf_Litter_Abun >5 | Leaf_Litter_Abun <0 | is.na(Leaf_Litter_Abun==TRUE)) %>% mutate(Flag_type="nonstandard_ll")
pd_23 %>% count(Leaf_Litter_Abun)

###Notes examine
pd_23_notes<-filter(pd_23,!Notes=="") %>% mutate(Flag_type="check_note")

pd_23_flags<-pd_23_no_id
pd_23_flags<-bind_rows(pd_23_flags,pd_23_nodate)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_dist)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_surv)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_gc)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_no_gc)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_some_gc)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_gc_sum)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_ll)
pd_23_flags<-bind_rows(pd_23_flags,pd_23_notes)


#concatenate flags for each plot
pd_23_flags <- pd_23_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(pd_23_flags,"FlaggedRecords/plot_data_flagged_23.csv") #review various issues- recorded in "Flag_type"
###Trees
trees_23<-read.csv("DataExports/PNRVegetationSurvey2023Trees_ExportTable.csv")
trees_23_no_id<-filter(trees_23,Plot_ID=="") %>% mutate(Flag_type="no_id")# save for problem records file; ensure all ids present and match survey plots table
trees_23_wrong_id<-filter(trees_23,!Plot_ID %in% c(pd_23$Plot_ID,pd_2$Plot_ID,pd$Plot_ID))%>% mutate(Flag_type="wrong_pid") #checks all plot id datasets

#plant codes/taxonomy--just scientific name including non sp level ids
trees_23_no_code<-filter(trees_23,Plant_Code=="")%>% mutate(Flag_type="no_plantcode")
trees_23 %>% count(Plant_Code)

#check codes and def
trees_23 %>% count(d_Plant_Code)

#check dbh
trees_23_no_dbh<-filter(trees_23,DBH_cm=="")%>% mutate(Flag_type="no_dbh")
#irregular dbh
trees_23_dbh<-filter(trees_23,DBH_cm<8 | DBH_cm>200) %>% mutate(Flag_type="nonstandard_dbh")
###Notes examine
trees_23_notes<-filter(trees_23,!Notes=="") %>% mutate(Flag_type="check_note")

##flagged records
trees_23_flags<-trees_23_no_id
trees_23_flags<-bind_rows(trees_23_flags,trees_23_wrong_id)
trees_23_flags<-bind_rows(trees_23_flags,trees_23_no_code)
trees_23_flags<-bind_rows(trees_23_flags,trees_23_no_dbh)
trees_23_flags<-bind_rows(trees_23_flags,trees_23_dbh)
trees_23_flags<-bind_rows(trees_23_flags,trees_23_notes)

#concatenate flags for each plot
trees_23_flags <- trees_23_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(trees_23_flags,"FlaggedRecords/trees_23_flagged.csv")
us_23<-read.csv("DataExports/PNRVegetationSurvey2023UnderstorySpeciesModifiedCollection_ExportTable.csv")
us_23_no_id<-filter(us_23,Plot_ID=="")%>% mutate(Flag_type="no_id")
# save for problem records file; ensure all ids present and match survey plots table
us_23_wrong_id<-filter(us_23,!Plot_ID %in% c(pd_23$Plot_ID,pd_2$Plot_ID,pd$Plot_ID))%>% mutate(Flag_type="wrong_pid") #checks all plot id datasets

#check codes and defs--this column is Species
us_23 %>% count(Genus) #these are messy
us_23 %>% count(d_Genus) #use controlled table
###irregular names
us_23_code_o<-filter(us_23,Genus=="Other")%>% mutate(Flag_type="other_plantcode")
#plant codes/taxonomy--just genus
us_23_no_code<-filter(us_23,d_Genus=="")%>% mutate(Flag_type="no_plantcode")
us_23_no_sp<-filter(us_23,Genus=="")%>% mutate(Flag_type="no_species")

# cover class
us_23_cc<-filter(us_23,CC >6 |CC <0)%>% mutate(Flag_type="nonstandard_CC")
us_23_no_cc<-filter(us_23, is.na(CC==TRUE) | CC=="") %>% mutate(Flag_type="no_cc")

us_23_notes<-filter(us_23, !Notes=="") %>% mutate(Flag_type="check_note")
##flagged records
us_23_flags<-us_23_no_id
us_23_flags<-bind_rows(us_23_flags,us_23_wrong_id)
us_23_flags<-bind_rows(us_23_flags,us_23_no_code)
us_23_flags<-bind_rows(us_23_flags,us_23_cc)
us_23_flags<-bind_rows(us_23_flags,us_23_no_cc)
us_23_flags<-bind_rows(us_23_flags,us_23_code_o)
us_23_flags<-bind_rows(us_23_flags,us_23_no_sp)
us_23_flags<-bind_rows(us_23_flags,us_23_notes)

#concatenate flags for each plot
us_23_flags <- us_23_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(us_23_flags,"FlaggedRecords/Understory_flagged_23.csv")
#related to ground cover--woody debris
cwd_23<-read.csv("DataExports/PNRVegetationSurvey2023CoarseWoodyDebris_ExportTable.csv")
#cwd_23<-cwd_23  %>% mutate(id = UUIDgenerate(n = n(), output = "string"))
cwd_23_no_id<-filter(cwd_23,Plot_ID=="") %>% mutate(Flag_type="no_id") # save for problem records file; ensure all ids present and match survey plots table
cwd_23_wrong_id<-filter(cwd_23,!Plot_ID %in% c(pd_23$Plot_ID,pd_2$Plot_ID,pd$Plot_ID))%>% mutate(Flag_type="wrong_pid") #checks all plot id datasets

######Decay Class: 1-5
cwd_23_dc<-filter(cwd_23,Decay_Class >5 | Decay_Class <1)%>% mutate(Flag_type="nonstandard_dc")
cwd_23_no_dc<-filter(cwd_23, Decay_Class==""|is.na(Decay_Class==TRUE))%>% mutate(Flag_type="no_dc")

#size class but decay class >3
cwd_23_dc_sc<-filter(cwd_23, Decay_Class>3 & is.na(Size_Class==FALSE))%>% mutate(Flag_type="nonstandard_sc")
cwd_23_no_sc<-filter(cwd_23,Decay_Class<4 &is.na(Size_Class==TRUE)) %>% mutate(Flag_type="no_sc")
#check that range of values are used
cwd_23 %>% count(Decay_Class)

##Size class: 2-6 (should only be entered for decay classes 1-3)
cwd_23_sc<-filter(cwd_23,Size_Class>6 | Size_Class<2)%>% mutate(Flag_type="nonstandard_sc")
#check that range of values are used
cwd_23 %>% count(Size_Class)

###check number of entries per plot
cwd_23_dc_plot<-cwd_23 %>% group_by(Plot_ID) %>% summarize(unique_count=n_distinct(Decay_Class))

###check notes
cwd_23_notes<-filter(cwd_23,!Notes=="")%>% mutate(Flag_type="check_notes")
##flagged records
cwd_23_flags<-cwd_23_no_id
cwd_23_flags<-bind_rows(cwd_23_flags,cwd_23_wrong_id)
cwd_23_flags<-bind_rows(cwd_23_flags,cwd_23_no_dc)
cwd_23_flags<-bind_rows(cwd_23_flags,cwd_23_dc_sc)
cwd_23_flags<-bind_rows(cwd_23_flags,cwd_23_no_sc)
cwd_23_flags<-bind_rows(cwd_23_flags,cwd_23_dc)
cwd_23_flags<-bind_rows(cwd_23_flags,cwd_23_sc)
cwd_23_flags<-bind_rows(cwd_23_flags,cwd_23_notes)
cwd_23_flags <- cwd_23_flags %>%
  group_by(across(-Flag_type)) %>%  # Group by all columns except Flag_type
  summarise(Flag_type = paste(unique(Flag_type), collapse = ", "), .groups = "drop") %>%
  distinct()
write.csv(cwd_23_flags,"FlaggedRecords/cwd_23_flagged.csv")
##ephemeral regen trees
######pilot ephemerals--comp to survey plots
eph<-read.csv("DataExports/pilotEphemeralplots_ExportTable.csv")
eph %>% group_by(Block_ID) #blockid=10
eph_plots<-filter(eph, Plot_ID %in% pd_2$Plot_ID)
##2 are in pd_2
eph_plots<-filter(eph, Plot_ID %in% pd$Plot_ID)

eph_trees_23<-read.csv("DataExports/Pilot_EphemeralPlots_Regen_Trees_ExportTable.csv")
eph_trees_23_no_id<-filter(eph_trees_23,Plot_ID=="") %>% mutate(Flag_type="no_id")# save for problem records file; ensure all ids present and match survey plots table
eph_trees_23_wrong_id<-filter(eph_trees_23,!Plot_ID %in% c(eph$Plot_ID,pd_2$Plot_ID,pd$Plot_ID))%>% mutate(Flag_type="wrong_pid") #checks all plot id datasets

#plant codes/taxonomy--just scientific name including non sp level ids
eph_trees_23_no_code<-filter(eph_trees_23,Species=="")%>% mutate(Flag_type="no_plantcode")
eph_trees_23 %>% count(Species)

#check codes and def
eph_trees_23 %>% count(d_Species)

#check count
eph_trees_23_no_count<-filter(eph_trees_23,Count_=="")%>% mutate(Flag_type="no_count")
#irregular count
eph_trees_23_count<-filter(eph_trees_23,Count_==0 | Count_>200) %>% mutate(Flag_type="nonstandard_count")

##flagged records
eph_trees_23_flags<-eph_trees_23_no_id
eph_trees_23_flags<-bind_rows(eph_trees_23_flags,eph_trees_23_wrong_id)
eph_trees_23_flags<-bind_rows(eph_trees_23_flags,eph_trees_23_no_code)
eph_trees_23_flags<-bind_rows(eph_trees_23_flags,eph_trees_23_no_count)
eph_trees_23_flags<-bind_rows(eph_trees_23_flags,eph_trees_23_count)
###no flags, but adjust the species names later
#concatenate flags for each plot
##################reconciliation with flagged records
trees_2_flags_fixed<-read.csv("FlaggedRecords/Flags_2025/Trees_2_flagged.csv")
####merge flag files
trees_2_flags_fixed<-trees_2_flags %>%
  left_join(trees_2_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Plant_Code.new, Plant_Code)
  ) %>% mutate(DBH_cm=coalesce(DBH_cm.new,DBH_cm))%>% select(-c(Plant_Code.new,DBH_cm.new,Column1,Plot_ID.new,Notes.new,dbase.new,PossError.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,BasalArea.new,d_Plant_Code.new,d_dbase.new,Flag_type.new,actual_count.new,indicated_total.new,any_notes.new))
###merge with previous version to update uuids
trees_2_clean<-trees_2 %>%
  left_join(trees_2_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Plant_Code.new, Plant_Code)
  ) %>% mutate(DBH_cm=coalesce(DBH_cm.new,DBH_cm))%>% select(-c(Plant_Code.new,DBH_cm.new,Plot_ID.new,Notes.new,dbase.new,PossError.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,BasalArea.new,d_Plant_Code.new,d_dbase.new))
#separate flagged records
trees_2_flags_remove<-filter(trees_2_flags_fixed,Keep.=="N")
trees_2_flags_q<-filter(trees_2_flags_fixed,Keep.=="?")
#match remove data with main data
trees_2_clean<-filter(trees_2_clean,!GlobalID %in% trees_2_flags_remove$GlobalID)
write.csv(trees_2_clean,"CleanRecords/Trees2Clean.csv")
###trees 2023
trees_23_flags_fixed<-read.csv("FlaggedRecords/Flags_2023/trees_23_flagged.csv")
####merge flag files
trees_23_flags_fixed<-trees_23_flags %>%
  left_join(trees_23_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Species = coalesce(Species.new, Species)
  ) %>% mutate(DBH_cm=coalesce(DBH_cm.new,DBH_cm))%>% select(-c(X,Plant_Code.new,DBH_cm.new,Plot_ID.new,Notes.new,dbase.new,PossError.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,d_Plant_Code.new,d_dbase.new,Flag_type.new,Species.new))
###merge with previous version to update uuids
trees_23_clean<-trees_23 %>%
  left_join(trees_23_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Species = coalesce(Species.new, Species)
  ) %>% mutate(DBH_cm=coalesce(DBH_cm.new,DBH_cm))%>% select(-c(Plant_Code.new,DBH_cm.new,Plot_ID.new,Notes.new,dbase.new,PossError.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,d_Plant_Code.new,d_dbase.new,Species.new))
#separate flagged records
trees_23_flags_remove<-filter(trees_23_flags_fixed,Keep.=="N")
trees_23_flags_q<-filter(trees_23_flags_fixed,Keep.=="?")
#match remove data with main data
trees_23_clean<-filter(trees_23_clean,!GlobalID %in% trees_23_flags_remove$GlobalID)
write.csv(trees_23_clean,"CleanRecords/Trees23Clean.csv")

#understory has issues
us_2_flags_fixed<-read.csv("FlaggedRecords/Flags_2025/Understory_flagged_2.csv")
us_2_flags_fixed<-us_2_flags %>%
  left_join(us_2_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    d_Species = coalesce(d_Species.new, d_Species)
  ) %>%  select(-c(Species.new,Cover_Class.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,d_Cover_Class.new,X,Plot_ID.new,d_Species.new,Flag_type.new))

us_2_flags_remove<-filter(us_2_flags_fixed,Keep.=="N")
us_2_flags_q<-filter(us_2_flags_fixed,Keep.=="?")

###merge with previous version to update uuids
us_2_clean<-us_2 %>%
  left_join(us_2_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    d_Species = coalesce(d_Species.new, d_Species)
  ) %>%  select(-c(Species.new,Cover_Class.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,d_Cover_Class.new,Plot_ID.new,d_Species.new))
us_2_clean<-filter(us_2_clean,!GlobalID %in% us_2_flags_remove$GlobalID)
##remove weird ? records for now
us_2_clean<-filter(us_2_clean,!GlobalID %in% us_2_flags_q$GlobalID)

write.csv(us_2_clean,"CleanRecords/Understory2Clean.csv")
#2023
#understory has issues
us_23_flags_fixed<-read.csv("FlaggedRecords/Flags_2023/Understory_flagged_23.csv")
us_23_flags_fixed<-us_23_flags %>%
  left_join(us_23_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    d_Genus = coalesce(d_Genus.new, d_Genus)
  ) %>% mutate(
    Genus = coalesce(Genus.new, Genus)
  ) %>%  select(-c(Genus.new,CC.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,d_CC.new,X,Plot_ID.new,d_Genus.new,Flag_type.new))

us_23_flags_remove<-filter(us_23_flags_fixed,Keep.=="N")
us_23_flags_q<-filter(us_23_flags_fixed,Keep.=="?")

###merge with previous version to update uuids
us_23_clean<-us_23 %>%
  left_join(us_23_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    d_Genus = coalesce(d_Genus.new, d_Genus)
  ) %>%  mutate(
    Genus = coalesce(Genus.new, Genus)
  ) %>%select(-c(Genus.new,CC.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,d_CC.new,Plot_ID.new,d_Genus.new,Notes.new))
us_23_clean<-filter(us_23_clean,!GlobalID %in% us_23_flags_remove$GlobalID)
write.csv(us_23_clean,"CleanRecords/Understory23Clean.csv")

#special concern no flags
sc_2_clean<-sc_2
write.csv(sc_2_clean,"CleanRecords/SpecialConcern2.csv")
#invasives
inv_2_flags_fixed<-read.csv("FlaggedRecords/Flags_2025/invasives_flagged_2.csv")

###merge with previous version to update uuids
inv_2_clean<-inv_2  %>%
  left_join(inv_2_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new"))

inv_2_flags_remove<-filter(inv_2_flags_fixed,Keep.=="N")
inv_2_flags_q<-filter(inv_2_flags_fixed,Keep.=="?")
inv_2_clean<-filter(inv_2_clean,!GlobalID %in% inv_2_flags_remove$GlobalID)
write.csv(inv_2_clean,"CleanRecords/Invasives2_clean.csv")
#cwd
cwd_2_flags_fixed<-read.csv("FlaggedRecords/Flags_2025/cwd_2_flagged.csv")

#match remove data with main data
cwd_2_clean<-cwd_2 %>%
  left_join(cwd_2_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new"))
#cleanup
cwd_2_clean<-cwd_2_clean %>%
  select(-"X")

cwd_2_flags_remove<-filter(cwd_2_flags_fixed,Keep.=="N")
cwd_2_flags_q<-filter(cwd_2_flags_fixed,Keep.=="?")
cwd_2_clean<-filter(cwd_2_clean,!GlobalID %in% cwd_2_flags_remove$GlobalID)
write.csv(cwd_2_clean,"CleanRecords/cwd_2clean.csv")
cwd_2025_ag<-cwd_2_clean[c(1:13)]
write.csv(cwd_2025_ag,"cwd_2025_ag.csv")
##cwd 2023
cwd_23_flags_fixed<-read.csv("FlaggedRecords/Flags_2023/cwd_23_flagged.csv")

#match remove data with main data
cwd_23_clean<-cwd_23 %>%
  left_join(cwd_23_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new"))
#cleanup
cwd_23_clean<-cwd_23_clean %>%
  select(-"X")

cwd_23_flags_remove<-filter(cwd_23_flags_fixed,Keep.=="N")
cwd_23_clean<-filter(cwd_23_clean,!GlobalID %in% cwd_23_flags_remove$GlobalID)
write.csv(cwd_23_clean,"CleanRecords/cwd_23_clean.csv")
cwd_2023_ag<-cwd_23_clean[c(1:13)]
write.csv(cwd_2023_ag,"cwd_2023_ag.csv")

##regen trees 2025
regen_flags_fixed<-read.csv("FlaggedRecords/Flags_2025/regen_trees_flagged2.csv")
####merge flag files
regen_flags_fixed<-regen_flags %>%
  left_join(regen_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Species.new, Species)) %>% mutate(d_Species=coalesce(d_Species.new,d_Species)
  ) %>% select(-c(Species.new,Count_.new,X,Plot_ID.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,Count_under1m.new,d_Species.new,Flag_type.new))
###merge with previous version to update uuids
regen_clean<-regen %>%
  left_join(regen_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Species.new, Species)) %>% mutate(d_Species=coalesce(d_Species.new,d_Species)
    ) %>% select(-c(Species.new,Count_.new,Plot_ID.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,Count_under1m.new,d_Species.new,Flag_type))#separate flagged records
regen_flags_remove<-filter(regen_flags_fixed,Keep.=="N")
regen_flags_q<-filter(regen_flags_fixed,Keep.=="?")
#match remove data with main data
regen_clean<-filter(regen_clean,!GlobalID %in% regen_flags_remove$GlobalID)
write.csv(regen_clean,"CleanRecords/RegenClean.csv")
##regen trees 2023--lots of unknowns about plots 
regen_flags_2023_fixed<-read.csv("FlaggedRecords/Flags_2023/regen_trees_2023_flagged2.csv")
####merge flag files
regen_flags_2023_fixed<-regen_23_flags %>%
  left_join(regen_flags_2023_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Species.new, Species)) %>% mutate(d_Species=coalesce(d_Species.new,d_Species)
    ) %>% select(-c(Species.new,Count_Under_1m.new,X,Plot_ID.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,Count_Over_1m.new,d_Species.new,Flag_type.new,CC,CC.new))
###merge with previous version to update uuids
regen_23_clean<-regen_23 %>%
  left_join(regen_flags_2023_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(
    Plant_Code = coalesce(Species.new, Species)) %>% mutate(d_Species=coalesce(d_Species.new,d_Species)
    ) %>% select(-c(Species.new,Count_Under_1m.new,Plot_ID.new,CreationDate.new,Creator.new,EditDate.new,Editor.new,Count_Over_1m.new,d_Species.new,Flag_type,CC,d_CC,d_CC.new.new,Notes.new.new,d_CC.new,Notes.new))#separate flagged records
regen_flags_23_remove<-filter(regen_flags_2023_fixed,Keep.=="N")
regen_flags_23_q<-filter(regen_flags_2023_fixed,Keep.=="?")
#match remove data with main data
regen_23_clean<-filter(regen_23_clean,!GlobalID %in% regen_flags_23_remove$GlobalID)
#remove the ?
regen_23_clean<-filter(regen_23_clean,!GlobalID %in% regen_flags_23_q$GlobalID)

write.csv(regen_23_clean,"CleanRecords/RegenClean_2023.csv")
###ephemerals regen trees
eph_trees_23_clean<-eph_trees_23
write.csv(eph_trees_23_clean,"CleanRecords/EphemeralRegenTrees.csv")
#all 10 in pd og
####more plot tables
library(sf)
st_layers("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb")
###this provides lat/long point for each plot id and relation to block id (3x3 grid)
buffnut_plots_fishnet<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="BuffaloNutFishnetFilter") 
buffnut_plots<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="BuffaloNutPlots") 
buffnut_plots_fishnet_label<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="BuffaloNutFishnet_label") 

rea_plots_fishnet<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="ReaFishnetFilter") 
rea_plots<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="ReaPlots") 
fr_plots_fishnet<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="FurnaceRunFishnetFilter") 
fr_plots<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="FurnaceRunPlots_ExportFeatures")
vp_2022<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="Vegetation_Survey_Plots2022selection_ExportFeatures")
parcel<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Analysis.gdb",layer="parcel_Project")

st_layers("R:/GIS/Projects/PNR_VegetationSurvey/PermanentPlots/PNR_VegSurvey_PermanentPlots.gdb")
perm_plots<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PermanentPlots/PNR_VegSurvey_PermanentPlots.gdb",layer="PermanentPlotBlockCenters")

st_layers("R:/GIS/Projects/PNR_BuffaloNutResearch/PNR_BuffaloNutResearch.gdb")
kuebbing_plots<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/PNR_VegetationSurvey_Kuebbing_2019.gdb",layer="Vegetation_Blocks_120m") 
tree2022_plots<-st_read("R:/GIS/Projects/PNR_VegetationSurvey/Trees_June162022.gdb",layer="Vegetation_Survey_Plots") 

####consolidate all the plot map data--2008, 2025, other misc
###########synthesize plot level data==Plot Data, survey_plots into raster layers
###occurrence for trees, understory, dominant shrubs, herbaceous species, special concern, and invasives associated with plot level data
pd_2_flags_fixed<-read.csv("FlaggedRecords/Flags_2025/plot_data_flagged_2025.csv")
###merge
pd_2_clean<-pd_2 %>%
  left_join(pd_2_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new")) %>% select(-X)

pd_2_flags_remove<-filter(pd_2_flags_fixed,Keep.=="N")
pd_2_flags_q<-filter(pd_2_flags_fixed,Keep.=="?")

#match remove data with main data
pd_2_clean<-filter(pd_2,!(GlobalID %in% pd_2_flags_remove$GlobalID))
pd_2_ag<-pd_2_clean[c(1:42,48)]
write.csv(pd_2_ag,"plot_data_2025_ag.csv")
pd2_sp2<-left_join(pd_2_clean,survey_plots,by="Plot_ID") #uses original survey plot database, need to add in new plots
#buffalo nut
bfplot<-buffnut_plots_fishnet %>% mutate(Plot_ID=Block_ID) #add column for plot id (same as block id here)
bfplot<-bfplot %>% mutate(SHAPE=st_centroid(Shape))
bfplot<-bfplot %>% mutate(Block_ID="Buffalo Nut")
pd2_sp2<-left_join(pd2_sp2,bfplot,by="Plot_ID")%>% mutate(Block_ID=coalesce(Block_ID.x,Block_ID.y)) %>% mutate(SHAPE = if_else(!st_is_empty(SHAPE.x), SHAPE.x, SHAPE.y))%>% select(-c(Block_ID.x,Block_ID.y,SHAPE.x,SHAPE.y,Shape,Shape_Area,Shape_Length))
#furnace run
frplot<-fr_plots %>% rename("Plot_ID"="PlotID","Block_ID"="BlockID","Veg_ID"="vegid","SHAPE"="Shape","Survey_Complete"="PlotSetCompleted","id"="GlobalID") %>% select(-c(CreationDate,Creator,EditDate,Editor,Id,Notes))
frplot$Survey_Complete<-1
frplot<-frplot %>% mutate(Block_ID="Furnace Run")
pd2_sp2<-left_join(pd2_sp2,frplot,by="Plot_ID")%>% mutate(Block_ID=coalesce(Block_ID.x,Block_ID.y)) %>% mutate(SHAPE = if_else(!st_is_empty(SHAPE.x), SHAPE.x, SHAPE.y))%>% mutate(Veg_ID=coalesce(Veg_ID.x,Veg_ID.y))%>% mutate(Survey_Complete=coalesce(Survey_Complete.x,Survey_Complete.y))%>% mutate(id=coalesce(id.x,id.y)) %>% select(-c(Block_ID.x,Block_ID.y,Veg_ID.x,Veg_ID.y,SHAPE.x,SHAPE.y,Survey_Complete.x,Survey_Complete.y,id.x,id.y,Section))
#RS
reaplot<-rea_plots_fishnet %>% mutate(Plot_ID=Block_ID) #add column for plot id (same as block id here)
reaplot<-reaplot %>% mutate(SHAPE=st_centroid(Shape)) %>% select(-c(Shape_Length,Shape_Area)) 
reaplot$Shape<-NULL
reaplot<-reaplot %>% mutate(Block_ID="Rea")

pd2_sp2<-left_join(pd2_sp2,reaplot,by="Plot_ID")%>% mutate(Block_ID=coalesce(Block_ID.x,Block_ID.y)) %>% mutate(SHAPE = if_else(!st_is_empty(SHAPE.x), SHAPE.x, SHAPE.y))%>% select(-c(Block_ID.x,Block_ID.y,SHAPE.x,SHAPE.y))

##trim columns
pd2_sp2<-pd2_sp2[c(1:27,32:60)]
pd2_sp2<-pd2_sp2 %>% rename("sp_id"="id")
pd2_sp2 <-pd2_sp2 %>% rename("pd_Notes"="Notes")
pd2_sp2$SHAPE<-st_as_text(pd2_sp2$SHAPE)
write.csv(pd2_sp2,"CleanRecords/survey_plot_data_2025.csv")
#cwd merge
cwd_2_clean<-read.csv("CleanRecords/cwd_2clean.csv")
pd2_sp2_cwd2<-cwd_2_clean %>% full_join(pd2_sp2, by="Plot_ID")
##compare dbase values--is this consistent across all tables?
pd2_sp2_cwd2 <- pd2_sp2_cwd2 %>%
  mutate(CreationDate = case_when(
    is.na(CreationDate.x) | is.na(CreationDate.y) ~ NA,         # NA if either is NA
    CreationDate.x == CreationDate.y ~ TRUE,                    # TRUE if equal
    CreationDate.x != CreationDate.y ~ FALSE                    # FALSE otherwise
  ))
mismatch_create<-filter(pd2_sp2_cwd2,CreationDate==FALSE) %>% mutate(Flag_type="mismatch_create") # these are like seconds off. will keep separate for now.
###cleanup column names
names(pd2_sp2_cwd2)
pd2_sp2_cwd2 <- pd2_sp2_cwd2 %>% rename("CWD_Notes"="Notes") %>% rename("CWD_dbase"="dbase.x") %>% rename("cwd_id"="GlobalID.x") %>%rename("cwd_CreationDate"="CreationDate.x")%>%rename("cwd_Creator"="Creator.x")%>%rename("cwd_EditDate"="EditDate.x")%>%rename("cwd_Editor"="Editor.x")%>%rename("pd_CreationDate"="CreationDate.y")%>%rename("pd_Creator"="Creator.y")%>%rename("pd_EditDate"="EditDate.y")%>%rename("pd_Editor"="Editor.y")%>%rename("pd_dbase"="dbase.y") %>% rename("d_CWD_dbase"="d_dbase.x") %>% rename("d_pd_dbase"="d_dbase.y") %>% rename("pd_id"="GlobalID.y")
pd2_sp2_cwd2<-pd2_sp2_cwd2 %>% select(-CreationDate)
write.csv(pd2_sp2_cwd2,"CleanRecords/Plot_Veg_Data_2025.csv")
########################################
#plot data 2023/2024
pd_23_flags_fixed<-read.csv("FlaggedRecords/Flags_2023/plot_data_flagged_23.csv")
###merge
pd_23_clean<-pd_23 %>%
  left_join(pd_23_flags_fixed, by="GlobalID",suffix = c("", ".new")) %>%
  mutate(across(ends_with(".new"), ~ coalesce(., get(sub(".new", "", cur_column()))),
                .names = "{sub('.new', '', .col)}")) %>%
  select(-ends_with(".new")) %>% select(-X)

pd_23_flags_remove<-filter(pd_23_flags_fixed,Keep.=="N")

#match remove data with main data
pd_23_clean<-filter(pd_23_clean,!(GlobalID %in% pd_23_flags_remove$GlobalID))
pd_23_ag<-pd_23_clean[c(1:42,49)]
write.csv(pd_23_ag,"plot_data_2023_ag.csv")

##End plot level clean up--event records
