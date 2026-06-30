library(dplyr)
library(stringr)
library(tidyr)
setwd("R:/GIS/Kat-working/VegetationSurvey/PNR_VegetationSurvey_Dupe")
###taxonomy
trees_clean<-read.csv("CleanRecords/TreesClean.csv")
##############plant taxonomy 
###########################trees
tree_names<-data.frame(trees_clean$Plant_Code,trees_clean$d_Plant_Code)
tree_names<-distinct(tree_names)
#add key for invalid names
colnames(tree_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code")
#special edits based on the flagged names
tree_names<-tree_names %>% mutate(acceptedPlant_Code=case_when(verbatimPlant_Code=="SNAG"~NA,verbatimPlant_Code=="UNK"~NA,verbatimPlant_Code=="CRATUK"~"CRATA",verbatimPlant_Code=="RHUUK"~"RHUS",verbatimPlant_Code=="MAPUN2"~"MAPU",verbatimPlant_Code=="VITUK"~"VITIS",verbatimPlant_Code=="CRSP"~NA,TRUE~verbatimPlant_Code))
### add key to tree dataset
trees_clean_taxo<-trees_clean
trees_clean_taxo<-trees_clean_taxo %>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
trees_clean_taxo<-trees_clean_taxo %>%
  left_join(tree_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code"))
trees_clean_taxo<-trees_clean_taxo %>%
  filter(!is.na(Plot_ID),!is.na(verbatimPlant_Code),!is.na(DBH_cm))
trees_clean_taxo<-trees_clean_taxo %>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")
#arcgis copy
trees_clean_ag<-trees_clean_taxo[c(2:10,13,14)]
write.csv(trees_clean_ag, "trees_2008_ag.csv")

################################shrubs
ds_clean<-read.csv("CleanRecords/DominantShrubs_Clean.csv")
shrubs_names_p<-data.frame(ds_clean$Primary_PlantCode,ds_clean$d_Primary_PlantCode)
shrubs_names_s<-data.frame(ds_clean$Secondary_PlantCode,ds_clean$d_Secondary_PlantCode)
col_names<-c("Plant_Code","d_Plant_Code") #match trees
colnames(shrubs_names_p)<-col_names
colnames(shrubs_names_s)<-col_names
shrubs_names<-bind_rows(shrubs_names_p,shrubs_names_s)
shrubs_names<-distinct(shrubs_names)
colnames(shrubs_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code")
#special edits based on the flagged names
shrubs_names<-shrubs_names %>% mutate(acceptedPlant_Code=case_when(verbatimPlant_Code=="XXXX"~NA,verbatimPlant_Code=="UNK"~NA,verbatimPlant_Code=="ZZZZ"~"CRATA",verbatimPlant_Code=="SACA12" ~ "SANIC4",verbatimPlant_Code==""~NA,TRUE~verbatimPlant_Code))
### add key to tree dataset
shrubs_clean_taxo<-ds_clean%>%
  rename(
    PlantCode_Primary = Primary_PlantCode,
    PlantCode_Secondary = Secondary_PlantCode,
    d_PlantCode_Primary = d_Primary_PlantCode,
    d_PlantCode_Secondary = d_Secondary_PlantCode,
    CC_Primary = CC_Primary,
    CC_Secondary = CC_Secondary,
    d_CC_Primary = d_CC_Primary,
    d_CC_Secondary = d_CC_Secondary
  )
#pivot with new column for primary or secondary
library(tidyr)
shrubs_clean_taxo<- shrubs_clean_taxo %>% pivot_longer(cols=c(PlantCode_Primary,PlantCode_Secondary,d_PlantCode_Primary,d_PlantCode_Secondary,CC_Primary,CC_Secondary,d_CC_Primary,d_CC_Secondary),names_to=c(".value","type"),names_pattern = "^(.*)_(Primary|Secondary)$"
) %>%
  mutate(type = tolower(type)) 
#now preserve as the verbatim names
shrubs_clean_taxo<-shrubs_clean_taxo %>% rename("verbatimPlant_Code"="PlantCode","verbatimd_Plant_Code"="d_PlantCode")
shrubs_clean_taxo<-shrubs_clean_taxo %>%
  left_join(shrubs_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code"))
#check for NA rows
shrubs_clean_taxo<-shrubs_clean_taxo %>%
  filter(!is.na(Plot_ID)) %>% filter(!is.na(verbatimPlant_Code)) %>% filter(!verbatimPlant_Code=="")
shrubs_clean_taxo<-shrubs_clean_taxo %>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")
#arcgis
shrubs_clean_ag<-shrubs_clean_taxo %>% pivot_wider(
  names_from  = "type",
  values_from = c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code","verbatimd_PlantID","CC","d_CC")
)
shrubs_clean_ag <- shrubs_clean_ag %>%
  group_by(across("id")) %>%
  summarise(
    across(where(is.character), ~ first(na.omit(.x))),
    .groups = "drop"
  )
shrubs_clean_ag<-shrubs_clean_ag[c(1:4,7:15)]
write.csv(shrubs_clean_ag, "shrubs_2008_ag.csv")

###################################herbs
herb_clean<-read.csv("CleanRecords/Herbaceous_clean_2008.csv")
herb_names<-data.frame(herb_clean$Plant_Code,herb_clean$d_Plant_Code,herb_clean$acceptedPlant_Code,herb_clean$Notes)
herb_names<-distinct(herb_names)
#add key for invalid names
colnames(herb_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code","acceptedPlant_Code","Notes")

#special edits based on the flagged names
excluded_codes <- c("GRASS", "UNK", "RUSH", "SEDGE", "O", "")  # "" to catch explicit blanks
herb_names <- herb_names %>%
  mutate(
    acceptedPlant_Code   = na_if(str_trim(acceptedPlant_Code), ""),
    is_excluded          = !is.na(verbatimPlant_Code) & verbatimPlant_Code %in% excluded_codes,
    # Fill acceptedPlant_Code only when it's missing AND verbatim is usable & not excluded
    acceptedPlant_Code = case_when(
      # accepted is missing, verbatim present, and not excluded
      is.na(acceptedPlant_Code) & !is.na(verbatimPlant_Code) & !is_excluded ~ verbatimPlant_Code,
      # otherwise keep accepted as is (including NA)
      TRUE ~ acceptedPlant_Code
    )
  ) %>%
  # remove helper columns
  select(-is_excluded)
herb_names<-herb_names %>% distinct()
###changes
herb_names<-herb_names %>% mutate(acceptedPlant_Code=ifelse(acceptedPlant_Code=="DILA5","PRLA9",acceptedPlant_Code))
herb_names<-herb_names %>% mutate(acceptedPlant_Code=ifelse(acceptedPlant_Code=="POLYUK","POLYG2",acceptedPlant_Code))
herb_names<-herb_names %>% mutate(acceptedPlant_Code=ifelse(acceptedPlant_Code=="O",NA_character_,acceptedPlant_Code))
herb_names<-herb_names %>% mutate(acceptedPlant_Code=ifelse(verbatimd_Plant_Code=="	
Aralia nudicaulis","ARNU2",acceptedPlant_Code)) 
herb_names<-herb_names%>% mutate(acceptedPlant_Code=ifelse(acceptedPlant_Code=="CAULO","CATH2",acceptedPlant_Code))

herb_names<-distinct(herb_names)

### add key to herb dataset
herb_clean_taxo<-herb_clean
herb_clean_taxo<-herb_clean_taxo %>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
herb_clean_taxo<-herb_clean_taxo %>%
  left_join(herb_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code","Notes"="Notes"))
herb_clean_taxo$acceptedPlant_Code.x<-NULL
herb_clean_taxo<-herb_clean_taxo %>% rename("acceptedPlant_Code"="acceptedPlant_Code.y")
#check for NA rows
herb_clean_taxo<-herb_clean_taxo %>%
  filter(!is.na(Plot_ID)) %>% filter(!is.na(verbatimPlant_Code)) %>% filter(!verbatimPlant_Code=="")
herb_clean_taxo<-herb_clean_taxo%>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")
#arcgis
herb_clean_ag<-herb_clean_taxo[c(2:10,13,14)]
write.csv(herb_clean_ag,"herb_2008_ag.csv")

#species of special concern
sc_clean<-read.csv("CleanRecords/SpecialConcern.csv")
sc_names<-data.frame(sc_clean$Plant_Code,sc_clean$d_Plant_Code)
sc_names<-distinct(sc_names)
###add verbatim and applied names 
#add key for invalid names
colnames(sc_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code")
#no flags
sc_names<-sc_names %>% mutate(acceptedPlant_Code=verbatimPlant_Code)
### add key to herb dataset
sc_clean_taxo<-sc_clean
sc_clean_taxo<-sc_clean_taxo %>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
sc_clean_taxo<-sc_clean_taxo %>%
  left_join(sc_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code"))
#check for NA rows
sc_clean_taxo<-sc_clean_taxo %>%
  filter(!is.na(Plot_ID)) %>% filter(!is.na(verbatimPlant_Code)) %>% filter(!verbatimPlant_Code=="")
sc_clean_taxo<-sc_clean_taxo%>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")
sc_clean_ag<-sc_clean_taxo[c(2:10)]
write.csv(sc_clean_ag,"sc_2008_ag.csv")

##########################invasive
inv_clean<-read.csv("CleanRecords/Invasives.csv")
invasive_names<-data.frame(inv_clean$Plant_Code,inv_clean$d_Plant_Code)
invasive_names<-distinct(invasive_names)
#add key for invalid names
colnames(invasive_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code")
#no flags
invasive_names<-invasive_names %>% mutate(acceptedPlant_Code=verbatimPlant_Code)

### add key to herb dataset
inv_clean_taxo<-inv_clean
inv_clean_taxo<-inv_clean_taxo %>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
inv_clean_taxo<-inv_clean_taxo %>%
  left_join(invasive_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code"))
#check for NA rows
inv_clean_taxo<-inv_clean_taxo %>%
  filter(!is.na(Plot_ID)) %>% filter(!is.na(verbatimPlant_Code)) %>% filter(!verbatimPlant_Code=="")
inv_clean_taxo<-inv_clean_taxo %>%rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")
inv_clean_ag<-inv_clean_taxo[c(2:10,13,14)]
write.csv(inv_clean_ag,"inv_2008_ag.csv")

###understory 
us_clean<-read.csv("CleanRecords/UnderstoryClean.csv")
understory_names<-data.frame(us_clean$Genus,us_clean$d_Genus)
understory_names<-distinct(understory_names)

#special edits based on the flagged names
understory_names<-understory_names %>% mutate(acceptedd_Genus=case_when(us_clean.Genus=="Lirodendron"~"Liriodendron",us_clean.Genus=="robinia"~"Robinia",us_clean.Genus=="smilax"~"Smilax",us_clean.Genus=="Mitchella"~"Mitchella",us_clean.Genus=="Euonymus"~"Euonymus",us_clean.Genus=="Acer pensylvanicum" ~"Acer",us_clean.Genus=="Other"~NA,TRUE~us_clean.d_Genus)) %>% distinct()
us_clean_taxo<-us_clean 

us_clean_taxo<-us_clean_taxo %>% rename("verbatimGenus"="Genus","verbatimd_Genus"="d_Genus")
us_clean_taxo<-us_clean_taxo %>%
  left_join(understory_names,by=c("verbatimGenus"="us_clean.Genus","verbatimd_Genus"="us_clean.d_Genus"))
#check for NA rows
us_clean_taxo<-us_clean_taxo %>%
  filter(!is.na(Plot_ID),!is.na(acceptedd_Genus))
us_clean_taxo<-us_clean_taxo %>% rename("verbatimPlantID"="verbatimGenus","verbatimd_PlantID"="verbatimd_Genus","Genus"="acceptedd_Genus")
#extra check that genus is valid
us_clean_taxo$Genus %>% unique()
us_clean_ag<-us_clean_taxo[c(2:14,17:18)]
write.csv(us_clean_ag,"us_2008_ag.csv")

#####2025
###taxonomy
trees_2_clean<-read.csv("CleanRecords/Trees2Clean.csv")
##############plant taxonomy 
###########################trees
tree_2_names<-data.frame(trees_2_clean$Plant_Code,trees_2_clean$d_Plant_Code)
tree_2_names<-distinct(tree_2_names)
#add key for invalid names
colnames(tree_2_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code")
#special edits based on the flagged names
tree_2_names<-tree_2_names %>% mutate(acceptedPlant_Code=case_when(verbatimPlant_Code=="SNAG"~NA,verbatimPlant_Code=="UNK"~NA,verbatimPlant_Code=="CRATUK"~"CRATA",verbatimPlant_Code=="RHUUK"~"RHUS",verbatimPlant_Code=="MAPUN2"~"MAPU",verbatimPlant_Code=="VITUK"~"VITIS",verbatimPlant_Code=="CRSP"~NA,TRUE~verbatimPlant_Code))
### add key to tree dataset
trees_2_clean_taxo<-trees_2_clean
trees_2_clean_taxo<-trees_2_clean_taxo%>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
trees_2_clean_taxo<-trees_2_clean_taxo %>%
  left_join(tree_2_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code"))
trees_2_clean_taxo<-trees_2_clean_taxo %>%
  filter(!is.na(Plot_ID),!is.na(verbatimPlant_Code),!is.na(DBH_cm))
trees_2_clean_taxo<-trees_2_clean_taxo %>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")
#arcgis copy
trees_2_clean_ag<-trees_2_clean_taxo[c(2:15,21:22)]
write.csv(trees_2_clean_ag, "trees_2025_ag.csv")

##regen trees
regen_clean<-read.csv("CleanRecords/RegenClean.csv")
regen_names<-data.frame(regen_clean$Species,regen_clean$d_Species)
colnames(regen_names)<-c("verbatimSpecies","verbatimd_Species")

regen_names<-distinct(regen_names)
#add key for invalid names
#special edits needed for non code names
regen_names<-regen_names %>% mutate(acceptedPlant_Code=tree_2_names$acceptedPlant_Code[match(verbatimd_Species,tree_2_names$verbatimd_Plant_Code)])
### add in additionals
regen_names<-regen_names %>% mutate(acceptedPlant_Code=case_when(verbatimSpecies=="MAPUN2"~"MAPU",is.na(acceptedPlant_Code)& verbatimSpecies=="Acer rubrum "~"ACRU",is.na(acceptedPlant_Code)& verbatimSpecies=="Acer saccharum "~"ACSA3",
  is.na(acceptedPlant_Code)& verbatimSpecies=="SNAG"~NA,is.na(acceptedPlant_Code)& verbatimSpecies=="UNK"~NA,is.na(acceptedPlant_Code)& verbatimSpecies=="Amelanchier spp."~"AMELA",is.na(acceptedPlant_Code)~verbatimSpecies,TRUE~acceptedPlant_Code))
### add key to tree dataset
regen_clean_taxo<-regen_clean
regen_clean_taxo<-regen_clean_taxo %>% rename("verbatimSpecies"="Species","verbatimd_Species"="d_Species")
regen_clean_taxo<-regen_clean_taxo %>%
  left_join(regen_names,by=c("verbatimSpecies"="verbatimSpecies","verbatimd_Species"="verbatimd_Species"))
regen_clean_taxo<-regen_clean_taxo %>%
  filter(!is.na(Plot_ID),!is.na(verbatimSpecies)) %>%select(-Plant_Code)
regen_clean_taxo<-regen_clean_taxo  %>% rename("verbatimPlantID"="verbatimSpecies","verbatimd_PlantID"="verbatimd_Species")
#arcgis copy
regen_clean_ag<-regen_clean_taxo[c(2:11,13:14)]
write.csv(regen_clean_ag, "regen_2025_ag.csv")

#species of special concern
sc_2_clean<-read.csv("CleanRecords/SpecialConcern2.csv")
sc_2_names<-data.frame(sc_2_clean$Plant_Code,sc_2_clean$d_Plant_Code)
sc_2_names<-distinct(sc_2_names)
###add verbatim and applied names 
#add key for invalid names
colnames(sc_2_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code")
#no flags
sc_2_names<-sc_2_names %>% mutate(acceptedPlant_Code=verbatimPlant_Code)
### add key to sc dataset
sc_2_clean_taxo<-sc_2_clean
sc_2_clean_taxo<-sc_2_clean_taxo %>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
sc_2_clean_taxo<-sc_2_clean_taxo %>%
  left_join(sc_2_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code"))
#check for NA rows
sc_2_clean_taxo<-sc_2_clean_taxo %>%
  filter(!is.na(Plot_ID)) %>% filter(!is.na(verbatimPlant_Code)) %>% filter(!verbatimPlant_Code=="")
sc_2_clean_taxo<-sc_2_clean_taxo %>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")
#arcgis copy
sc_2_clean_ag<-sc_2_clean_taxo[c(2:14)]
write.csv(sc_2_clean_ag,"sc_2025_ag.csv")

##########################invasive
inv_2_clean<-read.csv("CleanRecords/Invasives2_clean.csv")
invasive_2_names<-data.frame(inv_2_clean$Plant_Code,inv_2_clean$d_Plant_Code)
invasive_2_names<-distinct(invasive_2_names)
#add key for invalid names
colnames(invasive_2_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code")
#no flags --leave blanks
invasive_2_names<-invasive_2_names %>% mutate(acceptedPlant_Code=case_when(verbatimPlant_Code=="EUAL"~"EUAL13",TRUE~verbatimPlant_Code))

### add key to herb dataset
inv_2_clean_taxo<-inv_2_clean
inv_2_clean_taxo<-inv_2_clean_taxo %>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
inv_2_clean_taxo<-inv_2_clean_taxo %>%
  left_join(invasive_2_names,by=c("verbatimPlant_Code"="verbatimPlant_Code","verbatimd_Plant_Code"="verbatimd_Plant_Code"))
#check for NA rows
inv_2_clean_taxo<-inv_2_clean_taxo %>%
  filter(!is.na(Plot_ID)) 
inv_2_clean_taxo<-inv_2_clean_taxo %>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code")

####remove absences
inv_2_clean_taxo_presence<-filter(inv_2_clean_taxo,d_No_Invasives=="Yes")
#arcgis copy
inv_2_clean_ag<-inv_2_clean_taxo[c(2:16,20,21)]
write.csv(inv_2_clean_ag,"inv_2025_ag.csv")

###understory --like regen trees
us_2_clean<-read.csv("CleanRecords/Understory2Clean.csv")
understory_2_names<-data.frame(us_2_clean$Species,us_2_clean$d_Species)
understory_2_names<-distinct(understory_2_names)
colnames(understory_2_names)<-c("verbatimSpecies","verbatimd_Species")

#add key for invalid names
#special edits needed for non code names
understory_2_names<-understory_2_names %>% mutate(d_Species=case_when(verbatimSpecies=="Lindera"~"Lindera spp.",verbatimSpecies=="Euonymous" ~ "Euonymus spp.",verbatimd_Species==""~verbatimSpecies,TRUE~verbatimd_Species))
####divide data into two columns
understory_2_names<-understory_2_names %>%
  mutate(
    Plant_Code = case_when(
      str_detect(verbatimSpecies, "^[A-Z]{4,5}\\d?$") ~ verbatimSpecies,
      TRUE ~ NA_character_
    )
  )

###fix non matches
understory_2_names<-understory_2_names %>%mutate(acceptedPlant_Code=case_when(Plant_Code=="BERB"~"BERBE",Plant_Code=="CEOR"~"CEOR7",Plant_Code=="GAUL"~"GAULT",Plant_Code=="GAYL"~"GAYLU",Plant_Code=="HAMA"~"HAMAM",Plant_Code=="HAVI"~"HAVI4",Plant_Code=="KALM"~"KALMI",Plant_Code=="LIBE"~"LIBE3",Plant_Code=="LIND"~"LINDE2", Plant_Code=="LONI"~"LONIC",d_Species=="Other"~ NA,d_Species=="Pyularia pubera"~"PYPU", Plant_Code=="RHOD"~"RHODO",Plant_Code=="ROSA"~"ROSA5",Plant_Code=="RUBU"~"RUBUS",Plant_Code=="SAMB"~"SAMBU",Plant_Code=="SMIL"~"SMILA2",Plant_Code=="TORA"~"TORA2",Plant_Code=="TOXI"~"TOXIC",Plant_Code=="VACC"~"VACCI",Plant_Code=="VIBU"~"VIBUR",Plant_Code=="VITI"~"VITIS",d_Species=="Euonymus spp."~"EUONY2",d_Species=="Lindera spp."~"LINDE2",TRUE~Plant_Code))
#fix typos
### add key to tree dataset
us_2_clean_taxo<-us_2_clean 
us_2_clean_taxo<-us_2_clean_taxo %>% rename("verbatimSpecies"="Species","verbatimd_Species"="d_Species")
us_2_clean_taxo<-us_2_clean_taxo %>%
  left_join(understory_2_names,by=c("verbatimSpecies","verbatimd_Species"))
us_2_clean_taxo<-us_2_clean_taxo %>%select(-c(d_Species,Plant_Code))
#arcgis copy
us_2_clean_ag<-us_2_clean_taxo[c(2:11,14:17)]
write.csv(us_2_clean_ag,"us_2025_ag.csv")

####regen trees 2023
regen_23_clean<-read.csv("CleanRecords/RegenClean_2023.csv")
regen_23_names<-data.frame(regen_23_clean$Species,regen_23_clean$d_Species)
colnames(regen_23_names)<-c("verbatimSpecies","verbatimd_Species")

regen_23_names<-regen_23_names %>% mutate(verbatimd_Species=str_trim(verbatimd_Species))
regen_23_names<-distinct(regen_23_names)
#add key for invalid names
#special edits needed for non code names
regen_23_names<-regen_23_names %>% mutate(acceptedPlant_Code=regen_names$acceptedPlant_Code[match(verbatimd_Species,regen_names$verbatimd_Species)])
### add in additionals for this dataset
regen_23_names<-regen_23_names %>% mutate(acceptedPlant_Code=case_when(verbatimSpecies=="CORNU"~"COSE16",is.na(acceptedPlant_Code)& verbatimd_Species=="Viburnum spp."~"VIBUR",is.na(acceptedPlant_Code)& verbatimSpecies=="RHUUK"~"RHUS",is.na(acceptedPlant_Code)& verbatimd_Species=="Unknown"~NA,is.na(acceptedPlant_Code)~verbatimSpecies,TRUE~acceptedPlant_Code))
### add key to tree dataset
regen_23_clean_taxo<-regen_23_clean
regen_23_clean_taxo<-regen_23_clean_taxo %>% rename("verbatimSpecies"="Species","verbatimd_Species"="d_Species")
regen_23_clean_taxo<-regen_23_clean_taxo %>%
  left_join(regen_23_names,by=c("verbatimSpecies"="verbatimSpecies","verbatimd_Species"="verbatimd_Species"))
regen_23_clean_taxo<-regen_23_clean_taxo  %>% rename("verbatimPlantID"="verbatimSpecies","verbatimd_PlantID"="verbatimd_Species") %>% select(-Plant_Code,-X)
#arcgis copy
regen_23_clean_ag<-regen_23_clean_taxo[c(1:12,14)]
write.csv(regen_23_clean_ag, "regen_2023_ag.csv")



trees_23_clean<-read.csv("CleanRecords/Trees23Clean.csv")
##############plant taxonomy 
###########################trees
tree_23_names<-data.frame(trees_23_clean$Plant_Code,trees_23_clean$d_Plant_Code,trees_23_clean$Species,trees_23_clean$Kat.notes)
tree_23_names<-distinct(tree_23_names)
#add key for invalid names
colnames(tree_23_names)<-c("verbatimPlant_Code","verbatimd_Plant_Code","acceptedPlant_Code","Kat.notes")
#special edits based on the flagged names
tree_23_names<-tree_23_names %>% mutate(acceptedPlant_Code=case_when(!is.na(acceptedPlant_Code)~acceptedPlant_Code,verbatimPlant_Code=="SNAG"~NA,verbatimPlant_Code=="UNK"~NA,verbatimPlant_Code=="CRATUK"~"CRATA",verbatimPlant_Code=="TSUGA"~"TSCA",verbatimPlant_Code=="RHUUK"~"RHUS",verbatimPlant_Code=="MAPUN2"~"MAPU",is.na(acceptedPlant_Code) & verbatimPlant_Code=="VITUK"~"VITIS",is.na(acceptedPlant_Code) & verbatimPlant_Code=="VIBUK"~"VIBUR",verbatimPlant_Code=="CRSP"~NA,TRUE~verbatimPlant_Code))
tree_23_names<-distinct(tree_23_names)

### add key to tree dataset
trees_23_clean_taxo<-trees_23_clean
trees_23_clean_taxo<-trees_23_clean_taxo%>% rename("verbatimPlant_Code"="Plant_Code","verbatimd_Plant_Code"="d_Plant_Code")
trees_23_clean_taxo <- trees_23_clean_taxo %>%
  left_join(
    tree_23_names,
    by = c
     ("verbatimPlant_Code"   = "verbatimPlant_Code", "verbatimd_Plant_Code"="verbatimd_Plant_Code","Kat.notes"="Kat.notes"),
    suffix = c("", ".right")
  ) %>%
  mutate(
    acceptedPlant_Code = coalesce(acceptedPlant_Code, Species)
  ) %>%
  select(-Species)
trees_23_clean_taxo<-trees_23_clean_taxo  %>% rename("verbatimPlantID"="verbatimPlant_Code","verbatimd_PlantID"="verbatimd_Plant_Code") %>% select(-X)
#arcgis copy
trees_23_clean_ag<-trees_23_clean_taxo[c(1:13,16:17)]
write.csv(trees_23_clean_ag, "trees_2023_ag.csv")

##2023 understory
###understory --like regen trees
us_23_clean<-read.csv("CleanRecords/Understory23Clean.csv")
understory_23_names<-data.frame(us_23_clean$Genus,us_23_clean$d_Genus)
understory_23_names<-distinct(understory_23_names)
colnames(understory_23_names)<-c("verbatimGenus","verbatimd_Genus")
###rename verbatim d_Genus to accepted_genus
understory_23_names<-understory_23_names %>% rename("accepted_Genus" ="verbatimd_Genus")
### add key to tree dataset
us_23_clean_taxo<-us_23_clean 
us_23_clean_taxo<-us_23_clean_taxo %>% rename("verbatimGenus"="Genus","accepted_Genus"="d_Genus")
understory_23_names<-understory_23_names %>% mutate(accepted_Genus=case_when(accepted_Genus==""~verbatimGenus,TRUE~accepted_Genus))
us_23_clean_taxo<-us_23_clean_taxo %>%
  left_join(understory_23_names,by="verbatimGenus") %>%
  mutate(
    accepted_Genus = accepted_Genus.y) %>%
  select(-c(X,Notes.new.new,accepted_Genus.x,accepted_Genus.y))
us_23_clean_taxo<-us_23_clean_taxo  %>% rename("verbatimPlantID"="verbatimGenus","acceptedd_Genus"="accepted_Genus") 
#arcgis copy
us_23_clean_ag<-us_23_clean_taxo[1:10,13:14]
write.csv(us_23_clean_ag,"us_2023_ag.csv")

## ephemeral regen tres
eph_trees_23_clean<-read.csv("CleanRecords/EphemeralRegenTrees.csv")
eph_trees_23_names<-data.frame(eph_trees_23_clean$Species,eph_trees_23_clean$d_Species)
colnames(eph_trees_23_names)<-c("verbatimSpecies","verbatimd_Species")
eph_trees_23_names<-distinct(eph_trees_23_names)
### add in additionals for this dataset
eph_trees_23_names<-eph_trees_23_names %>% mutate(acceptedPlant_Code=case_when(verbatimSpecies=="Acer pensylvanicum"~"ACPE",verbatimSpecies=="Acer rubrum "~"ACRU",verbatimSpecies=="Acer saccharum "~"ACSA3",verbatimSpecies=="Amelanchier spp."~"AMELA", verbatimSpecies=="Betula lenta"~"BELE",                                                       verbatimSpecies=="SNAG"~NA,verbatimSpecies=="Carpinus caroliniana"~"CACA18",verbatimSpecies=="Carya spp."~"CARYA",verbatimSpecies=="Cornus florida"~"COFL2",verbatimSpecies=="Cratagus spp."~"CRATA",verbatimSpecies=="Fagus grandifolia"~"FAGR",verbatimSpecies=="Ostrya virginiana"~"OSVI",verbatimSpecies=="Quercus alba"~"QUAL",verbatimSpecies=="Quercus velutina"~"QUVE",verbatimSpecies=="Robinia pseudoacacia"~"ROPS",TRUE~NA))


### add key to tree dataset
eph_trees_23_clean_taxo<-eph_trees_23_clean
eph_trees_23_clean_taxo<-eph_trees_23_clean_taxo %>% rename("verbatimSpecies"="Species","verbatimd_Species"="d_Species")
eph_trees_23_clean_taxo<-eph_trees_23_clean_taxo %>%
  left_join(eph_trees_23_names,by=c("verbatimSpecies"="verbatimSpecies","verbatimd_Species"="verbatimd_Species"))
eph_trees_23_clean_taxo<-eph_trees_23_clean_taxo  %>% rename("verbatimPlantID"="verbatimSpecies","verbatimd_PlantID"="verbatimd_Species") %>% select(-X)
#arcgis copy
eph_trees_23_clean_ag<-eph_trees_23_clean_taxo[c(1:10)]
write.csv(eph_trees_23_clean_ag, "ephemeral_regentrees_2023_ag.csv")


#compile names--2008
plants_names<-bind_rows(tree_names,shrubs_names,herb_names,sc_names,invasive_names,understory_names)
plants_names<-distinct(plants_names)
plants_names<-plants_names%>% mutate(verbatimPlantID=coalesce(verbatimPlant_Code,us_clean.Genus)) %>% mutate(verbatimd_PlantID=coalesce(us_clean.d_Genus,verbatimd_Plant_Code)) %>% select(c(verbatimPlantID,verbatimd_PlantID,acceptedPlant_Code,acceptedd_Genus))
plants_names<-distinct(plants_names)

usda_names<-read.csv("PlantTaxonomy/Pennsylvania_NRCS_csv.txt")

usda_names<-usda_names %>% rename("acceptedPlant_Code"="Symbol")

#no synonyms
plant_taxo_accepted<-filter(usda_names, Synonym.Symbol=="")
#checked for duplicates or misspellings
#############revised plant taxonomy for all names to 2025
plants_names_2025<-bind_rows(tree_2_names,regen_names,sc_2_names,invasive_2_names,understory_2_names)
plants_names_2025<-distinct(plants_names_2025)
plants_names_2025<-plants_names_2025 %>% mutate(verbatimPlantID=coalesce(verbatimPlant_Code,verbatimSpecies)) %>% mutate(verbatimd_PlantID=coalesce(verbatimd_Plant_Code,verbatimd_Species)) %>% select(c(verbatimPlantID,verbatimd_PlantID,acceptedPlant_Code))
plants_names_2025<-distinct(plants_names_2025)

#####2023 names
plants_names_2023<-bind_rows(eph_trees_23_names,tree_23_names,regen_23_names,understory_23_names)
plants_names_2023<-distinct(plants_names_2023)
plants_names_2023<-plants_names_2023 %>% mutate(verbatimPlantID=coalesce(verbatimPlant_Code,verbatimGenus,verbatimSpecies)) %>% mutate(verbatimd_PlantID=coalesce(verbatimd_Plant_Code,verbatimd_Species)) %>% select(c(verbatimPlantID,verbatimd_PlantID,acceptedPlant_Code,accepted_Genus)) %>% rename("acceptedd_Genus"="accepted_Genus")
plants_names_2023<-distinct(plants_names_2023)


##joining
plants_names_all<-bind_rows(plants_names,plants_names_2025,plants_names_2023) %>% distinct()
#apply backbone--get genus only codes to fill in 
genus_backbone <- plant_taxo_accepted %>%
  filter(str_detect(Scientific.Name.with.Author, "^[A-Z][a-z]+\\s[\\(A-Z]"))
genus_backbone <- genus_backbone %>% mutate(Genus=str_extract(Scientific.Name.with.Author, "^[A-Za-z]+"))
plant_taxo_accepted_all<-left_join(plants_names_all,plant_taxo_accepted,by="acceptedPlant_Code")
##second join on genus name
plant_taxo_accepted_all <- left_join(
  plant_taxo_accepted_all,
  genus_backbone,
  by = c("acceptedd_Genus" = "Genus"),
  suffix = c(".plant", ".genus")
)

plant_taxo_accepted_all <- plant_taxo_accepted_all %>%
 mutate(acceptedPlant_Code=coalesce(acceptedPlant_Code.plant,acceptedPlant_Code.genus)) %>% mutate(Scientific.Name.with.Author=coalesce(Scientific.Name.with.Author.plant,Scientific.Name.with.Author.genus)) %>% mutate(State.Common.Name=coalesce(State.Common.Name.plant,State.Common.Name.genus)) %>% mutate(Family=coalesce(Family.plant,Family.genus)) %>%
  select(-c(acceptedPlant_Code.plant,acceptedPlant_Code.genus,Scientific.Name.with.Author.plant,Scientific.Name.with.Author.genus, State.Common.Name.plant,State.Common.Name.genus,Family.plant,Family.genus,Synonym.Symbol.plant,Synonym.Symbol.genus))

plant_taxo_accepted_all<-plant_taxo_accepted_all %>% distinct()
#checked for duplicates or misspellings
plant_taxo_accepted_all<-plant_taxo_accepted_all%>%
  mutate(Genus = word(Scientific.Name.with.Author, 1))
# use standard taxo backbone for each table
#### trees
trees_clean_taxo<-trees_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
#### shrubs
shrubs_clean_taxo<-shrubs_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))

####herbs
herb_clean_taxo<-herb_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
####sc
sc_clean_taxo<-sc_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
####invasive
inv_clean_taxo<-inv_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
#genus backbone
us_clean_taxo<-us_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID"))
us_clean_taxo<-us_clean_taxo %>% mutate(Genus=coalesce(Genus.x,Genus.y)) %>% select(-c(Genus.x,Genus.y))
#extra cleanup
us_clean_taxo<-us_clean_taxo %>%
  group_by(id) %>%
  summarise(across(everything(),
                   ~ .[!is.na(.)][1]),
            .groups = "drop")

####plant record aggregation--trees + understory + dom shrubs + herbaceous + sc + invasive
occ_dfs <- list(trees_clean_taxo=trees_clean_taxo,us_clean_taxo=us_clean_taxo,shrubs_clean_taxo=shrubs_clean_taxo,herb_clean_taxo=herb_clean_taxo,sc_clean_taxo=sc_clean_taxo,inv_clean_taxo=inv_clean_taxo)

# Add source column to each dataframe
tagged_dfs <- lapply(names(occ_dfs), function(name) {
  df <- occ_dfs[[name]]
  df$source <- name
  df
})

#merge and coalesce
merge_coalesce <- function(x, y) {
  merged_df <- merge(x, y, by = "id", all = TRUE, suffixes = c(".x", ".y"))
  
  # Identify overlapping columns excluding "id"
  common_cols <- intersect(names(x), names(y))
  common_cols <- setdiff(common_cols, "id")
  
  for (col in common_cols) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")
    
    # Coalesce the two columns into one
    merged_df[[col]] <- coalesce(merged_df[[col_x]], merged_df[[col_y]])
    
    # Drop the old suffix columns
    merged_df[[col_x]] <- NULL
    merged_df[[col_y]] <- NULL
  }
  
  merged_df
}

# Use with Reduce
merged_coalesce_df <- Reduce(merge_coalesce, tagged_dfs)

#inspect
head(merged_coalesce_df)
merged_coalesce_df<-merged_coalesce_df %>% select(-X)
######notes about other columns
#id is a uuid based on original table row--reassigned as occ_id
#DBH_cm is tree specific measure
#PossError was a column in trees--merge with notes?
merged_coalesce_df<- merged_coalesce_df %>% select(-PossError)
#CC_0_05, CC_05_2, CC_2_5 (d)is specific to understory, maybe collapse with CC and add a height column
# Coalesce the two columns into one
#type is specific to the dominant shrubs table
#Notes should work as a standard coalesced column--could have an additional "Kat notes" column as metadata
#Plot_Id will link to the pd_sp_cwd table via this id
#dbase is a standard column but still unsure what the ref is and if it is standard across all tables
write.csv(merged_coalesce_df,"VegetationOcc_2008.csv")

###########################################################
#2025
#### trees
trees_2_clean_taxo<-trees_2_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))

#### regen trees
regen_clean_taxo<-regen_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))

####sc
sc_2_clean_taxo<-sc_2_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
####invasive--use presence data
inv_2_clean_taxo_presence<-inv_2_clean_taxo_presence %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
#now add full taxonomy
#genus backbone
us_2_clean_taxo <- us_2_clean_taxo %>%
  left_join(plant_taxo_accepted_all, by = c("verbatimSpecies"="verbatimPlantID","verbatimd_Species"="verbatimd_PlantID","acceptedPlant_Code"="acceptedPlant_Code"))
#coalesce step to remove dupes

us_2_clean_taxo<-us_2_clean_taxo %>%
  group_by(GlobalID) %>%
  summarise(across(everything(), ~ Reduce(coalesce, .) ))
us_2_clean_taxo<-us_2_clean_taxo %>% rename("verbatimPlantID"="verbatimSpecies","verbatimd_PlantID"="verbatimd_Species")
# List of named dataframes
occ_dfs <- list(
  trees_2_clean_taxo = trees_2_clean_taxo,
  regen_clean_taxo = regen_clean_taxo,
  us_2_clean_taxo = us_2_clean_taxo,
  sc_2_clean_taxo = sc_2_clean_taxo,
  inv_2_clean_taxo_presence = inv_2_clean_taxo_presence,
  us_2_clean_taxo= us_2_clean_taxo
)

# Add source column to each dataframe
tagged_dfs <- lapply(names(occ_dfs), function(name) {
  df <- occ_dfs[[name]]
  df$source <- name
  df
})

#merge and coalesce--update id 
merge_coalesce <- function(x, y) {
  merged_df <- merge(x, y, by = "GlobalID", all = TRUE, suffixes = c(".x", ".y"))
  
  # Identify overlapping columns excluding "id"
  common_cols <- intersect(names(x), names(y))
  common_cols <- setdiff(common_cols, "GlobalID")
  
  for (col in common_cols) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")
    
    # Coalesce the two columns into one
    merged_df[[col]] <- coalesce(merged_df[[col_x]], merged_df[[col_y]])
    
    # Drop the old suffix columns
    merged_df[[col_x]] <- NULL
    merged_df[[col_y]] <- NULL
  }
    merged_df
}

# Use with Reduce
merged_coalesce_df_2025 <- Reduce(merge_coalesce, tagged_dfs)

#inspect
head(merged_coalesce_df_2025)
######notes about other columns
#GlobalID is a uuid based on original table row--assign as occ_id?
#DBH_cm is tree specific measure
#PossError was a column in trees--merge with notes?
merged_coalesce_df_2025<- merged_coalesce_df_2025 %>% select(-c(X,X.1,PossError,actual_count,indicated_total,any_notes))
#CC and Cover Class merge
merged_coalesce_df_2025<-merged_coalesce_df_2025 %>% mutate(CC=coalesce(CC,Cover_Class)) %>% mutate(d_CC=coalesce(d_CC,d_Cover_Class)) %>% select(-c(d_Cover_Class,Cover_Class))

#type is specific to the dominant shrubs table
#Notes should work as a standard coalesced column--could have an additional "Kat notes" column as metadata
#Plot_Id will link to the pd_sp_cwd table via this id
#dbase is a standard column but still unsure what the ref is and if it is standard across all tables
write.csv(merged_coalesce_df_2025,"VegetationOcc_2025.csv")

#####proceed to open refine
#trimmed whitespace, filled in NA, spot checked records, redundant checks in darwin core mapping script

#other 2023
#### regen trees
regen_23_clean_taxo<-regen_23_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
###trees 2023
trees_23_clean_taxo<-trees_23_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
###trees 2023--remove absence
trees_23_clean_taxo_presence<-trees_23_clean_taxo %>%
  filter(Keep.=="" | is.na(Keep.))
#understory
us_23_clean_taxo<-us_23_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID"="verbatimPlantID","acceptedd_Genus"="acceptedd_Genus"))
us_23_clean_taxo<-us_23_clean_taxo %>%
  group_by(GlobalID) %>%
  summarise(across(everything(), ~ Reduce(coalesce, .) ))
###--remove absence
us_23_clean_taxo_presence<-us_23_clean_taxo %>%
  filter(Keep.=="" | is.na(Keep.))
#### eph regen trees
eph_trees_23_clean_taxo<-eph_trees_23_clean_taxo %>%
  left_join(plant_taxo_accepted_all,by=c("verbatimPlantID","verbatimd_PlantID","acceptedPlant_Code"))
# List of named dataframes
occ_dfs <- list(
  trees_23_clean_taxo_presence = trees_23_clean_taxo_presence,
  regen_23_clean_taxo = regen_23_clean_taxo,
  us_23_clean_taxo_presence = us_23_clean_taxo_presence,
  eph_trees_23_clean_taxo = eph_trees_23_clean_taxo
)

# Add source column to each dataframe
tagged_dfs <- lapply(names(occ_dfs), function(name) {
  df <- occ_dfs[[name]]
  df$source <- name
  df
})

#merge and coalesce--update id 
merge_coalesce <- function(x, y) {
  merged_df <- merge(x, y, by = "GlobalID", all = TRUE, suffixes = c(".x", ".y"))
  
  # Identify overlapping columns excluding "id"
  common_cols <- intersect(names(x), names(y))
  common_cols <- setdiff(common_cols, "GlobalID")
  
  for (col in common_cols) {
    col_x <- paste0(col, ".x")
    col_y <- paste0(col, ".y")
    
    # Coalesce the two columns into one
    merged_df[[col]] <- coalesce(merged_df[[col_x]], merged_df[[col_y]])
    
    # Drop the old suffix columns
    merged_df[[col_x]] <- NULL
    merged_df[[col_y]] <- NULL
  }
  merged_df
}

# Use with Reduce
merged_coalesce_df_2023 <- Reduce(merge_coalesce, tagged_dfs)

#inspect
head(merged_coalesce_df_2023)
######notes about other columns
#id is a uuid based on original table row--assign as occ_id?
#DBH_cm is tree specific measure
#PossError was a column in trees--merge with notes?
#Count_ is for all heights in ephemeral; count_under_1m and count_over_1m is for regen trees--will add as measurement
merged_coalesce_df_2023<- merged_coalesce_df_2023 %>% select(-PossError)
write.csv(merged_coalesce_df_2023,"VegetationOcc_2023.csv")
