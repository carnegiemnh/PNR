###bird point count darwin core mapping
###bird mapping

library(readxl)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(lubridate)
library(taxize)
library(RODBC)

#setwd("R:/GIS/Kat-working")
plotpc_map<-read_xlsx("C:/Users/sullivank/OneDrive - Carnegie Institute d.b.a Carnegie Museums of Pittsburgh/Documentation/PointCount_cleaning.xlsx",sheet="PointCount_map")
map_cols<-c("original_col","field","class")
colnames(plotpc_map)<-map_cols


abundance_pc_map<-read_xlsx("C:/Users/sullivank/OneDrive - Carnegie Institute d.b.a Carnegie Museums of Pittsburgh/Documentation/PointCount_cleaning.xlsx",sheet="Abundance_map")
colnames(abundance_pc_map)<-map_cols
abundance_pc_map<-abundance_pc_map[,(1:3)]

###input clean data sources
birdpc<-read.csv("R:/GIS/Kat-working/BirdPointCounts/PointCountAbundance_clean_fixed_weatherfix_PAC.csv") #all clean data
survey_plot_veg<-read.csv("R:/GIS/Kat-working/VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/survey_plot_data_2008.csv") #just the plot level data for veg in 2008
survey_plot<-survey_plot_veg %>% select(Plot_ID,UTM_Corrd_N,UTM_Coord_E,Latitude,Longitude,Elevation_m,SHAPE) %>%
  rename("PointID"="Plot_ID")
#join plot details for event table
birdpc<-birdpc %>% left_join(survey_plot, by="PointID")


####get subset of data that matches col map scheme and rename 
###create mapping key for event
plotpc_mapped<-birdpc %>% select(any_of(plotpc_map$original_col))
#Build a mapping named vector: field = new, original_col = old
old<-colnames(plotpc_mapped)
key<-filter(plotpc_map,original_col %in% old & class=="event")
key<-tibble(old=key$original_col,new=key$field)
###function for renaming and merging columns in dwc format

# -- Custom handlers registry: add more named handlers here --
.custom_handlers <- list(
  # Example: Bird point count specific adjustments
  eco = function(df) {
    df <- df %>%
      dplyr::mutate(
        reportedWeather = paste(
          paste0("Temp=", Temp_C, "C"),
          paste0("WindSpeed_Beaufort=", WindSpeed_Beuf),
          paste0("CloudCover_Percent=", CloudCover_Perc),
          sep = "; "
        )
      )
  },
  emof_occ=function(df) {
    df<-df %>%
      pivot_longer(cols=c(Time_Interval,Repeat,Distance),names_to="measurementType",values_to="measurementValue")
  }
)
#dwc_map function same as for veg
dwc_map <- function(
    df,
    key,                        # data frame with columns: old, new
    delimiter = "|",            # delimiter to use when merging duplicates
    custom=NULL,
    drop_na_named_cols = TRUE,  # remove columns named NA or literal "NA"
    remove_value_na = TRUE,     # drop NA values during concatenation
    remove_value_blank = TRUE,  # drop "" during concatenation
    trim_values = TRUE          # trim whitespace before concatenating
) {
  # --- Validate key ---
  if (!all(c("old", "new") %in% names(key))) {
    stop("`key` must have columns named 'old' and 'new'.")
  }
  
  # Coerce to base data.frame to keep duplicate names as-is
  df <- as.data.frame(df, check.names = FALSE)
  
  # Optional: trim character columns before processing
  if (trim_values) {
    df[] <- lapply(df, function(x) if (is.character(x)) trimws(x) else x)
  }
  # (optional): apply custom handler if provided
  if (!is.null(custom)) {
    if (custom %in% names(.custom_handlers)) {
      df <- .custom_handlers[[custom]](df)
      # After custom, still keep base df structure
      df <- as.data.frame(df, check.names = FALSE)
    } else {
      warning(sprintf(
        "No custom handler found for '%s'. Skipping custom adjustments.",
        custom
      ))
    }
  }
  
  # --- Step 1: Rename first using the key (old -> new) ---
  recode_map <- with(key, setNames(new, old))  # named vector: names = old, values = new
  nm <- names(df)
  # Replace names that appear in the map
  match_idx <- match(nm, names(recode_map))
  nm_new <- ifelse(is.na(match_idx), nm, recode_map[match_idx])
  names(df) <- nm_new
  
  
  # --- Step 2: Remove columns titled NA (actual NA) or literal "NA" ---
  if (drop_na_named_cols) {
    nm <- names(df)
    keep <- !(is.na(nm) | nm == "NA")
    df <- df[keep]
  }
  
  # --- Step 3: Merge duplicate columns by concatenating row-wise ---
  nm <- names(df)
  uniq <- unique(nm)
  
  # Helper to combine row values across duplicate columns
  combine_rows <- function(cols_list) {
    n <- nrow(df)
    cols_chr <- lapply(cols_list, function(col) as.character(col))
    vapply(seq_len(n), function(i) {
      vals <- vapply(cols_chr, function(col) col[i], character(1))
      if (remove_value_na)    vals <- vals[!is.na(vals)]
      if (remove_value_blank) vals <- vals[vals != ""]
      if (trim_values)        vals <- trimws(vals)
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = delimiter)
    }, character(1))
  }
  
  out_cols <- lapply(uniq, function(n) {
    idx <- which(nm == n)
    if (length(idx) == 1) {
      # Preserve original type for non-duplicates
      df[[idx]]
    } else {
      # Concatenate duplicates row-wise
      combine_rows(df[idx])
    }
  })
  
  out <- as.data.frame(out_cols, stringsAsFactors = FALSE)
  names(out) <- uniq
  
  # Return tibble for tidyverse workflows
  tibble::as_tibble(out)
}
plotpc_mapped_dwc_event<-dwc_map(plotpc_mapped,key,delimiter = "|",custom=NULL)
###reconcile duplicated columns--cleanup
#concatenate notes, split columns
plotpc_mapped_dwc_event<-plotpc_mapped_dwc_event  %>%
  rename("verbatimLatitude"= "verbatimCoordinateSystem | verbatimLatitude")%>% rename("verbatimLongitude"= "verbatimCoordinateSystem | verbatimLongitude") %>% 
  mutate(minimumElevationInMeters = verbatimElevation,
    verbatimElevation = case_when(!is.na(verbatimElevation)~paste0(verbatimElevation, " m"), TRUE~NA)) %>%
    select(-c(X,flag,Observer1,Temp_C,WindSpeed_Beuf,CloudCover_Perc,Observer2))
plotpc_mapped_dwc_event<-plotpc_mapped_dwc_event %>% distinct()
#####make humboldt core extension table
key<-filter(plotpc_map,original_col %in% old & class=="HumboldtCore")
key<-tibble(old=key$original_col,new=key$field)

##add in eventID
eid<-c("OBJECTID.x","eventID")
key<-rbind(key,eid)
#use custom handler for weather data
plotpc_mapped_dwc_eco<-dwc_map(plotpc_mapped,key,delimiter = "|",custom="eco")
#concatenate observers
plotpc_mapped_dwc_eco<-plotpc_mapped_dwc_eco %>% mutate(samplingPerformedBy=case_when(is.na(`samplingPerformedBy`)& is.na(`samplingPerformedBy.1`)~NA_character_,is.na(`samplingPerformedBy`)~`samplingPerformedBy.1`,is.na(`samplingPerformedBy.1`)~`samplingPerformedBy`,TRUE~paste(`samplingPerformedBy`,`samplingPerformedBy.1`,sep=",")))
plotpc_mapped_dwc_eco<-plotpc_mapped_dwc_eco %>% select(-c(reportedWeather,reportedWeather.1,reportedWeather.2)) %>% rename("reportedWeather"="reportedWeather.3")
# --- Step 1.5: Drop columns that were not mapped ---
plotpc_mapped_dwc_eco <- plotpc_mapped_dwc_eco %>% select(c(eventID,samplingPerformedBy,reportedWeather))
plotpc_mapped_dwc_eco<-plotpc_mapped_dwc_eco %>% distinct()
####occurrence table
####get subset of data that matches col map scheme and rename 
###create mapping key for event
abundance_pc_mapped<-birdpc %>% select(c(OBJECTID.x,any_of(abundance_pc_map$original_col)))
#Build a mapping named vector: field = new, original_col = old
old<-colnames(abundance_pc_mapped)
key<-filter(abundance_pc_map,original_col %in% old & class=="occurrence")
key<-tibble(old=key$original_col,new=key$field)

##add in eventID
eid<-c("OBJECTID.x","eventID")
key<-rbind(key,eid)
#use custom handler for weather data
abundance_pc_mapped_dwc_occ<-dwc_map(abundance_pc_mapped,key,delimiter = "|",custom=NULL)
#combine occ remarks
abundance_pc_mapped_dwc_occ <- abundance_pc_mapped_dwc_occ %>% mutate(occurrenceRemarks = case_when(
      (is.na(occurrenceRemarks) | occurrenceRemarks == "") &
        (is.na(occurrenceRemarks.1) | occurrenceRemarks.1 == "") ~ NA_character_,
      
      (is.na(occurrenceRemarks) | occurrenceRemarks == "") ~ occurrenceRemarks.1,
      
      (is.na(occurrenceRemarks.1) | occurrenceRemarks.1 == "") ~ occurrenceRemarks,
      
      TRUE ~ paste(occurrenceRemarks, occurrenceRemarks.1, sep = "|")
    )
  )
# --- Step 1.5: Drop columns that were not mapped ---
abundance_pc_mapped_dwc_occ <- abundance_pc_mapped_dwc_occ %>% select(c(eventID,occurrenceID, verbatimIdentification,organismQuantity,occurrenceRemarks,taxonID,taxonRemarks, scientificName,vernacularName,taxonRank,infraspecificEpithet,specificEpithet,genus,family))
####occurrence table-emof ext
old<-colnames(abundance_pc_mapped)
key<-filter(abundance_pc_map,original_col %in% old & class=="emof-occ")
key<-tibble(old=key$original_col,new=key$field)

##add in eventID and occID
eid<-c("OBJECTID.x","eventID")
oid<-c("OBJECTID.y","occurrenceID")
key<-rbind(key,eid,oid)
#use custom handler for weather data
abundance_pc_mapped_dwc_occemof<-dwc_map(abundance_pc_mapped,key,delimiter = "|",custom="emof_occ")
# --- Step 1.5: Drop columns that were not mapped ---
abundance_pc_mapped_dwc_occemof <- abundance_pc_mapped_dwc_occemof %>% select(c(eventID,occurrenceID, measurementType,measurementValue))
# Write output
write.csv(plotpc_mapped_dwc_event,"Database/DWC_PCA/PointCount_event.csv")
write.csv(plotpc_mapped_dwc_eco,"Database/DWC_PCA/PointCount_eco.csv")
write.csv(abundance_pc_mapped_dwc_occ,"Database/DWC_PCA/PointCount_occ.csv")
write.csv(abundance_pc_mapped_dwc_occemof,"Database/DWC_PCA/PointCount_occ_emof.csv")
####edits for GBIF
GBIF_export<-list(event=plotpc_mapped_dwc_event, eco=plotpc_mapped_dwc_eco, occ=abundance_pc_mapped_dwc_occ, emof=abundance_pc_mapped_dwc_occemof)
#metadata and gbif fixes
GBIF_export<-lapply(GBIF_export,
                    function(df) {
                      df[df == "NA"] <- NA ##remove NA
                      df<-df %>% ##remove brackets
                        mutate(across(where(is.character), ~ gsub("[{}]", "", .)))
                      df
                    })

#gbif metadata additions
GBIF_export$occ$basisofRecord<-"HumanObservation" #basisofRecord
GBIF_export$occ <- GBIF_export$occ %>%
  mutate(occurrenceStatus=case_when(is.na(organismQuantity) ~"Absent",TRUE~"Present")) #presence/absence defined by orgQuantity and ID
GBIF_export$occ <- GBIF_export$occ %>%
  mutate(organismQuantity=case_when(is.na(organismQuantity) ~0,TRUE~organismQuantity)) #change NA/absence to 0

GBIF_export$occ$organismQuantityType<- "individuals" #ind count
#taxonomic backbone--reapply a standard
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(
    taxonRank = case_when(
      !is.na(infraspecificEpithet) & specificEpithet != "" ~ "subspecies",
      !is.na(specificEpithet) & specificEpithet != "" ~ "species",
      !is.na(genus) & genus != "" ~ "genus",
      !is.na(family) & family != "" ~ "family",
      TRUE ~ taxonRank   # or keep as is and investigate
    )
  )
#add kingdom
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(kingdom=case_when(!is.na(scientificName) ~"Animalia"))
#add phylum except to absence
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(phylum=case_when(!is.na(scientificName) ~"Chordata"))
#add class except to absence
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(class=case_when(!is.na(scientificName) ~"Aves"))
library(taxize)
library(purrr)
bird_names<-unique(GBIF_export$occ$scientificName) 
#use gbif backbone
bird_backbone<-classification(bird_names,db="gbif")
##remove Collaptes and add later 
bird_backbone<-bird_backbone[-107]
taxo_lookup <- map_dfr(names(bird_backbone), function(f) {
  
  x <- bird_backbone[[f]]
  
  tibble(
    scientificName=names(bird_backbone[f]),
    specificEpithet=word(x$name[x$rank == "species"][1],2),
    genus=x$name[x$rank == "genus"][1],
    order = x$name[x$rank == "order"][1],
    family = x$name[x$rank == "family"][1]
  )
  
})
#remove the Piciformes
taxo_lookup<-filter(taxo_lookup,!is.na(family))
##add in NA row
null<-c(NA,NA,NA,NA,NA)
taxo_lookup<-rbind(taxo_lookup,null) %>% distinct()
#add order and family using gbif backbone--remove existing family, genus, specificEpithet
GBIF_export$occ$family<-NULL
GBIF_export$occ$genus<-NULL
GBIF_export$occ$specificEpithet<-NULL

GBIF_export$occ<-GBIF_export$occ %>%
  left_join(taxo_lookup,by="scientificName") %>%
  distinct()

#add special cases 
GBIF_export$occ <- GBIF_export$occ %>%
  mutate(specificEpithet= case_when(scientificName == "Colaptes auratus auratus x cafer" ~"auratus",scientificName == "Piciformes sp." ~ NA_character_,
                                          TRUE ~ specificEpithet),
    genus= case_when(scientificName == "Colaptes auratus auratus x cafer" ~"Colaptes",scientificName == "Piciformes sp." ~ NA_character_,
                     TRUE ~ genus),
    family = case_when(
      scientificName == "Colaptes auratus auratus x cafer" ~ "Picidae",
      scientificName == "Piciformes sp." ~ NA_character_,
      TRUE ~ family
    ),
    order = case_when(
      scientificName %in% c("Colaptes auratus auratus x cafer", "Piciformes sp.") ~ "Piciformes",
      TRUE ~ order
    )
  )
#adjust ranks????
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(taxonRank=case_when(!is.na(family) & is.na(genus) ~"family",!order=="" & is.na(family)~"order", !phylum=="" & order=="" & is.na(family) ~"phylum",phylum=="" ~"kingdom",TRUE~taxonRank))
###list the hybrid as hybrid
GBIF_export$occ <-GBIF_export$occ %>%
  mutate(taxonRank=case_when(
    scientificName %in% c("Colaptes auratus auratus x cafer") ~"hybrid",TRUE~taxonRank))

###event adjustments
GBIF_export$event<-GBIF_export$event %>% mutate(geodeticDatum=case_when(is.na(decimalLatitude)~NA,TRUE~"WGS84"))
#time
has_time <- grepl("\\d{2}:\\d{2}:\\d{2}", GBIF_export$event$eventDate)

parsed <- ymd_hms(GBIF_export$event$eventDate, tz = "UTC", quiet = TRUE)

# fallback for date-only rows
parsed[!has_time] <- ymd(GBIF_export$event$eventDate[!has_time])

GBIF_export$event$eventDate_iso <- ifelse(
  has_time,
  format(parsed, "%Y-%m-%dT%H:%M:%SZ"),
  format(parsed, "%Y-%m-%d")
)

GBIF_export$event<-GBIF_export$event %>% select(-eventDate)%>% rename("eventDate"="eventDate_iso") 
#placeholder for protocolNames-->samplingProtocol
#locality data
#sample size
GBIF_export$event$sampleSizeUnit<-"m_radius"
GBIF_export$event$sampleSizeValue<-"150"
GBIF_export$event$countryCode<-"US"
GBIF_export$event$locality<-"Powdermill Nature Reserve and Field Station"
GBIF_export$event$stateProvince<-"Pennsylvania"
GBIF_export$event$verbatimCoordinateSystem<-"UTM"
##HC columns
GBIF_export$eco$targetTaxonomicScope<-"Aves"
GBIF_export$eco$isTaxonomicScopeFullyReported<-"TRUE"
GBIF_export$eco$isAbsenceReported<-"FALSE"
GBIF_export$eco$protocolNames<-"PointCount"
GBIF_export$eco$isAbundanceReported<-"TRUE"
GBIF_export$eco$samplingEffortValue<-10
GBIF_export$eco$samplingEffortUnit<-"personMinutes"
GBIF_export$eco$totalAreaSampledValue<-150
GBIF_export$eco$totalAreaSampledUnit<-"m_radius"
GBIF_export$eco <-GBIF_export$eco %>% mutate(samplingEffortProtocol=case_when(eventID>204~"10 minute counts of birds observed from a central point and recorded into time interval bins at 3 min, 5 min, and 10 min.",TRUE~"10 minute counts of birds observed from a central point and recorded into time interval bins at 3:20 min, 6:40 min, and 10 min."))
#adjust id columns to first position
cols <- "eventID"
GBIF_export$event <- GBIF_export$event[, c(cols, setdiff(names(GBIF_export$event), cols))]
GBIF_export$eco <- GBIF_export$eco[, c(cols, setdiff(names(GBIF_export$eco), cols))]

cols <- c("eventID","occurrenceID")
GBIF_export$occ<-GBIF_export$occ[, c(cols, setdiff(names(GBIF_export$occ), cols))]
GBIF_export$emof<-GBIF_export$emof[, c(cols, setdiff(names(GBIF_export$emof), cols))]
# ##proposed workflows for creating IPT archives
# --direct creation of file for internal use, someone else find home for files
#final clean 
GBIF_export<-lapply(GBIF_export,
                    function(df) {
                      df[df == "NA"] <- NA ##remove NA
                      df<-df %>% ##remove brackets
                        mutate(across(where(is.character), ~ gsub("[{}]", "", .)))
                      df<-df %>% 
                        mutate(across(where(is.character), trimws))
                    })
#check for unique records
unique(GBIF_export$event$eventID[duplicated(GBIF_export$event$eventID)])
unique(GBIF_export$eco$eventID[duplicated(GBIF_export$eco$eventID)])
unique(GBIF_export$occ$occurrenceID[duplicated(GBIF_export$occ$occurrenceID)])
##check eventID alignment
missing_occ <- setdiff(GBIF_export$occ$eventID,
                       GBIF_export$event$eventID)

missing_emof <- setdiff(GBIF_export$emof$eventID,
                        GBIF_export$event$eventID)

missing_eco <- setdiff(GBIF_export$eco$eventID,
                       GBIF_export$event$eventID)
##check on emof in occ
missing_emof_occ <- setdiff(GBIF_export$emof$occurrenceID,
                            GBIF_export$occ$occurrenceID)


file_names <- paste0(names(GBIF_export), ".txt")

GBIF_export<-lapply(seq_along(GBIF_export),function(i) {
  write.table(GBIF_export[[i]],
              file = file_names[i],
              sep = "\t",
              row.names = FALSE,
              na="",
              quote = FALSE,
              fileEncoding = "UTF-8")
})

zip("dwca.zip", files = file_names)

###########################################

