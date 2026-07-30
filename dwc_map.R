####DWC mapping script
#####example with veg and bird point count

library(readxl)
library(dplyr)
library(tibble)
library(tidyr)
library(stringr)
library(lubridate)
library(taxize)
setwd("R:/GIS/Kat-working")
#load veg mapping schema

plotveg_map<-read_xlsx("C:/Users/sullivank/OneDrive - Carnegie Institute d.b.a Carnegie Museums of Pittsburgh/Documentation/VegSurvey_cleaning.xlsx",sheet="Plot_Veg_Data_map")
occveg_map<-read_xlsx("C:/Users/sullivank/OneDrive - Carnegie Institute d.b.a Carnegie Museums of Pittsburgh/Documentation/VegSurvey_cleaning.xlsx",sheet="Veg_Occ_map")
map_cols<-c("original_col","field","dwca_class","dwcdp_class","notes")
colnames(plotveg_map)<-map_cols
colnames(occveg_map)<-map_cols

###input clean data sources
survey_plot_veg<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/survey_plot_data_2008.csv") #just the plot level data for veg in 2008
plot_veg_data<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/Plot_Veg_Data.csv") #plot level data and cwd for veg in 2008
occ_veg_data<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/VegetationOcc_2008.csv")
#add eventID based on common plot id to occ dataset
occ_veg_data<-occ_veg_data %>% left_join(plot_veg_data %>% select(Plot_ID,pd_id),by="Plot_ID") %>% unique()


####get subset of data that matches col map scheme and rename 
##plot_veg to plotveg_map
###create mapping key
plot_veg_mapped<-survey_plot_veg %>% select(any_of(plotveg_map$original_col[
  !is.na(plotveg_map$original_col) & plotveg_map$original_col != ""
]))

#Build a mapping named vector: field = new, original_col = old--event core only
old<-colnames(plot_veg_mapped)
key<-filter(plotveg_map,original_col %in% old & dwca_class=="event")
key<-tibble(old=key$original_col,new=key$field)
###function for renaming and merging columns in dwc format

# -- Custom handlers registry: add more named handlers here --
.custom_handlers <- list(
  # Example: Vegetation plot specific adjustments
  vegplot = function(df) {
    num_cols <- c(
      "N_CanCov","E_CanCov","W_CanCov","S_CanCov")
    
    desc_cols <- c(
      "d_Mid_VS_CC","d_Low_VS_CC","d_Ground_VS_CC",
      "d_BareSoil_GC","d_Rocks_GC","d_Ferns_GC","d_Forbs_GC",
      "d_Graminoids_GC","d_Leaf_Litter_Abun"
    )
    
    
    # ✅ Only rename numeric cols if present
    present_num <- intersect(names(df), num_cols)
    if (length(present_num) > 0) {
      df <- df %>%
        rename_with(~ paste0("assertionNumericValue_", .x), all_of(present_num))
    }
    
    # ✅ Only rename descriptive cols if present
    present_desc <- intersect(names(df), desc_cols)
    if (length(present_desc) > 0) {
      df <- df %>%
        rename_with(~ paste0("assertionValue_", sub("^d_", "", .x)), all_of(present_desc))
    }
    
      
      # ✅ Find matching columns AFTER rename
      pivot_cols <- grep("^(assertionNumericValue|assertionValue)_", names(df), value = TRUE)
    
    # ✅ Only run mutate/across if columns exist
    if (length(pivot_cols) > 0) {
      df <- df %>%
        mutate(across(all_of(pivot_cols), as.character)) %>%
        pivot_longer(
          cols = all_of(pivot_cols),
          names_to = "measurementType",
          values_to = "measurementValue",
          values_drop_na = TRUE
        ) %>%
        mutate(
          measurementType = str_remove(
            measurementType,
            "^assertion(NumericValue|Value)_"
          )
        )
    }
   df
    },
  vegplot25 = function(df) {
    num_cols <- c(
      "N_CanCov","E_CanCov","W_CanCov","S_CanCov")
    df<-df %>% mutate(N_CanCov=(96-N_CanCov))
    desc_cols <- c(
      "d_Mid_VS_CC","d_Low_VS_CC","d_Ground_VS_CC",
      "d_BareSoil_GC","d_Rocks_GC","d_Ferns_GC","d_Forbs_GC",
      "d_Graminoids_GC","d_Leaf_Litter_Abun"
    )
    
    
    # ✅ Only rename numeric cols if present
    present_num <- intersect(names(df), num_cols)
    if (length(present_num) > 0) {
      df <- df %>%
        rename_with(~ paste0("assertionNumericValue_", .x), all_of(present_num))
    }
    
    # ✅ Only rename descriptive cols if present
    present_desc <- intersect(names(df), desc_cols)
    if (length(present_desc) > 0) {
      df <- df %>%
        rename_with(~ paste0("assertionValue_", sub("^d_", "", .x)), all_of(present_desc))
    }
    
    
    # ✅ Find matching columns AFTER rename
    pivot_cols <- grep("^(assertionNumericValue|assertionValue)_", names(df), value = TRUE)
    
    # ✅ Only run mutate/across if columns exist
    if (length(pivot_cols) > 0) {
      df <- df %>%
        mutate(across(all_of(pivot_cols), as.character)) %>%
        pivot_longer(
          cols = all_of(pivot_cols),
          names_to = "measurementType",
          values_to = "measurementValue",
          values_drop_na = TRUE
        ) %>%
        mutate(
          measurementType = str_remove(
            measurementType,
            "^assertion(NumericValue|Value)_"
          )
        )
    }
    df
  },vegocc = function(df) {
    df<-df %>%
      mutate(
        # Create new columns for Primary type
        Primary_CC_0_05 = if_else(tolower(type) == "primary", d_CC_0_05, NA_character_),
        Primary_CC_05_2 = if_else(tolower(type) == "primary", d_CC_05_2, NA_character_),
        Primary_CC_2_5  = if_else(tolower(type) == "primary", d_CC_2_5,  NA_character_),
        
        # Create new columns for Secondary type
        Secondary_CC_0_05 = if_else(tolower(type) == "secondary", d_CC_0_05, NA_character_),
        Secondary_CC_05_2 = if_else(tolower(type) == "secondary", d_CC_05_2, NA_character_),
        Secondary_CC_2_5  = if_else(tolower(type) == "secondary", d_CC_2_5,  NA_character_)
      ) %>%
      # Null out original columns for rows where type is primary or secondary
      mutate(
        d_CC_0_05 = if_else(tolower(type) %in% c("primary", "secondary"), NA_character_, d_CC_0_05),
        d_CC_05_2 = if_else(tolower(type) %in% c("primary", "secondary"), NA_character_, d_CC_05_2),
        d_CC_2_5  = if_else(tolower(type) %in% c("primary", "secondary"), NA_character_, d_CC_2_5)
      ) %>% pivot_longer(cols=c("d_CC","d_CC_0_05","d_CC_05_2","d_CC_2_5","Primary_CC_0_05","Primary_CC_05_2","Primary_CC_2_5","Secondary_CC_0_05","Secondary_CC_05_2","Secondary_CC_2_5"),names_to="organismQuantityType",values_to="organismQuantity") %>%  select(-c(type,CC,CC_0_05,CC_05_2,CC_2_5,Kat.notes))
  },
  vegocc2025 = function(df) {
    df<-df %>% mutate(across(c(`Count_`, `Count_under1m`), as.character)) %>% pivot_longer(cols=c("d_CC","Count_","Count_under1m"),names_to="organismQuantityType",values_to="organismQuantity")%>%  pivot_longer(cols=c("No_Invasives"),names_to="degreeOfEstablishment",values_to="degreeOfEstablishmentTF")%>% 
      mutate(degreeOfEstablishment=case_when(degreeOfEstablishmentTF=="Yes"~"Invasive",TRUE~NA)) %>%
      select(-c(CC,Kat.notes,degreeOfEstablishmentTF))
  },vegocc2023 = function(df) {
    df<-df %>% mutate(across(c(`Count_`, `Count_Under_1m`, `Count_Over_1m`), as.character)) %>% pivot_longer(cols=c("d_CC","Count_","Count_Under_1m","Count_Over_1m"),names_to="organismQuantityType",values_to="organismQuantity")%>%
      select(-c(CC))
  },
  tree = function(df) {
    measurement_cols <- c("DBH_cm","BasalArea")
    
    df %>%
      # Rename numeric columns
      rename_with(~ paste0("assertionNumericValue_", .x), any_of(measurement_cols)) %>%
      pivot_longer(
        cols = matches("assertionNumericValue_"),
        names_to = "measurementType",
        names_pattern = "assertionNumericValue_(.*)$",
        values_to="measurementValue",
        values_drop_na = TRUE
      )
  }
)
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
  
  # --- Step 1.5: Drop columns that were not mapped ---
  mapped_old <- key$old
  keep_cols <- nm %in% mapped_old
  df <- df[keep_cols]
  
  
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
plot_veg_mapped_dwcevent<-dwc_map(plot_veg_mapped,key,delimiter = "|",custom=NULL)
###reconcile duplicated columns--cleanup
plot_veg_mapped_dwcevent<-plot_veg_mapped_dwcevent %>% select(-c(habitat,locationID.1,locationID.2)) %>% 
  #rename("samplingPerformedBy"=`samplingPerformedBy.1`)%>% 
  rename("verbatimLatitude"= "verbatimCoordinateSystem | verbatimLatitude")%>% rename("verbatimLongitude"= "verbatimCoordinateSystem | verbatimLongitude") %>% 
  #rename("protocolNames"= "protocolNames.1") %>% 
  rename("habitat"= "habitat.1") %>% 
  mutate(minimumElevationInMeters = verbatimElevation,
    verbatimElevation = case_when(!is.na(verbatimElevation)~paste0(verbatimElevation, " m"), TRUE~NA)
    )
#####make humboldt core extension table
key<-filter(plotveg_map,original_col %in% old & dwca_class=="HumboldtCore")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)
plot_veg_mapped_eco<-dwc_map(plot_veg_mapped,key,delimiter = "|",custom=NULL)
plot_veg_mapped_eco<-plot_veg_mapped_eco %>% select(-c(samplingPerformedBy,protocolNames)) %>% 
  rename("samplingPerformedBy"=`samplingPerformedBy.1`)%>% 
  rename("protocolNames"= "protocolNames.1")
###make measurement table
key<-filter(plotveg_map,original_col %in% old & dwca_class=="event-emof")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)

dwc_emof_map <- function(
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
#map
plot_veg_mapped_emof<-dwc_emof_map(plot_veg_mapped,key,delimiter = "|",custom="vegplot")
# --- Step 1.5: Drop columns that were not mapped ---
plot_veg_mapped_emof <- plot_veg_mapped_emof %>% select(eventID,measurementValue,measurementType)
######occurrences + emof
###create mapping key
occ_veg_mapped<-occ_veg_data  %>% select(any_of(occveg_map$original_col[
  !is.na(occveg_map$original_col) & occveg_map$original_col != ""
]))

#Build a mapping named vector: field = new, original_col = old
old<-colnames(occ_veg_mapped)
key<-filter(occveg_map,original_col %in% old & dwca_class=="occurrence")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)

###mapping script
occ_veg_mapped_dwcocc<-dwc_emof_map(occ_veg_mapped,key,delimiter = "|",custom="vegocc")
#cleanup columns
occ_veg_mapped_dwcocc <- occ_veg_mapped_dwcocc %>%
  mutate(
    verbatimIdentification =
      case_when(
        # If the two columns are exactly equal (including both NA), keep one
        verbatimIdentification == verbatimIdentification.1 ~ verbatimIdentification,
        
        # If one is missing, prefer the non-missing one
        is.na(verbatimIdentification) & !is.na(verbatimIdentification.1) ~ verbatimIdentification.1,
        !is.na(verbatimIdentification) & is.na(verbatimIdentification.1) ~ verbatimIdentification,
        
        # Otherwise, concatenate
        TRUE ~ paste(verbatimIdentification, verbatimIdentification.1, sep = ",")
      )
  )  %>%
  # Keep rows where at least one of the two numeric columns is not NA or blank
  filter(
    !( (is.na(organismQuantity) | organismQuantity == "") ) | source=="trees_clean_taxo"
  ) %>%
  # Set type columns to NA when their value columns are NA
  mutate(
    organismQuantityType = if ("organismQuantityType" %in% names(.)) {
      if_else(is.na(organismQuantity), NA_character_, as.character(organismQuantityType))
    } else {
      NULL
    }
  ) %>% unique()%>% ###append suffix for understory records to give unique OID 
  group_by(occurrenceID) %>%
  mutate(occurrenceID = if (n() == 1) {
    occurrenceID
  } else {
    paste0(occurrenceID, "-", sprintf("%02d", row_number()))
  }) %>%
  ungroup() %>% select(-verbatimIdentification.1) %>% unique()
occ_veg_mapped_dwcocc <- occ_veg_mapped_dwcocc %>% select(-c(DBH_cm,Flag_type,Keep.,dbase,d_dbase,source))
####occurrence emof field
###make measurement table
key<-filter(occveg_map,original_col %in% old & dwca_class=="occurrence-emof")
key<-tibble(old=key$original_col,new=key$field)
##add in occID
oid<-c("id","occurrenceID")
eid<-c("pd_id","eventID")
key<-rbind(key,oid,eid)

occ_veg_mapped_emof<-dwc_emof_map(occ_veg_mapped,key,delimiter = "|",custom="tree")
# --- Step 1.5: Drop columns that were not mapped ---
occ_veg_mapped_emof <- occ_veg_mapped_emof %>% select(eventID,occurrenceID,measurementValue,measurementType)
####emof can be combined!
occ_veg_mapped_emof$measurementValue<-as.character(occ_veg_mapped_emof$measurementValue)
veg_mapped_emof<-full_join(plot_veg_mapped_emof,occ_veg_mapped_emof)


####other years for veg survey--integrate with 2008
#2025
###input clean data sources
survey_plot_veg25<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/survey_plot_data_2025.csv") #just the plot level data for veg in 2008
#####change column names to match key
survey_plot_veg25<-survey_plot_veg25 %>% rename("pd_id"="GlobalID")
#random dupes --select later one
survey_plot_veg25 <- survey_plot_veg25 %>%
  group_by(Plot_ID) %>%
  slice_max(order_by = Date_clean, n = 1, with_ties = FALSE) %>%
  ungroup()

plot_veg_data25<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/Plot_Veg_Data_2025.csv") #plot level data and cwd for veg in 2008
#match the survey_plot ids
plot_veg_data25<-filter(plot_veg_data25, pd_id %in% survey_plot_veg25$pd_id)
occ_veg_data25<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/VegetationOcc_2025.csv")
occ_veg_data25<-occ_veg_data25 %>% left_join(plot_veg_data25 %>% select(Plot_ID,pd_id),by="Plot_ID") %>% unique()
#no dupes
###create mapping key
plot_veg_mapped_25<-survey_plot_veg25 %>% select(any_of(plotveg_map$original_col[
  !is.na(plotveg_map$original_col) & plotveg_map$original_col != ""
]))

#Build a mapping named vector: field = new, original_col = old--event core only
old<-colnames(plot_veg_mapped_25)
key<-filter(plotveg_map,original_col %in% old & dwca_class=="event")
key<-tibble(old=key$original_col,new=key$field)
###function for renaming and merging columns in dwc format-null custom
plot_veg_mapped25_dwcevent<-dwc_map(plot_veg_mapped_25,key,delimiter = "|",custom=NULL)
#same adjustments as 2008 with extra surveyor addin
###reconcile duplicated columns--cleanup
plot_veg_mapped25_dwcevent<-plot_veg_mapped25_dwcevent %>% select(-c(habitat,locationID.1,locationID.2))%>% #mutate(
  # samplingPerformedBy =
  #   case_when(
  #     # If surveyor2 is missing, just include one
  #     !is.na(samplingPerformedBy.2) & is.na(samplingPerformedBy.3) ~ samplingPerformedBy.2,
  #     # Otherwise, concatenate
  #     TRUE ~ paste(samplingPerformedBy.2,samplingPerformedBy.3, sep = ",")
  #  )) %>% 
  rename("verbatimLatitude"= "verbatimCoordinateSystem | verbatimLatitude") %>% 
  rename("verbatimLongitude"= "verbatimCoordinateSystem | verbatimLongitude") %>% 
  rename("habitat"= "habitat.1") %>% 
  mutate(
    verbatimElevation = case_when(!is.na(verbatimElevation)~paste0(verbatimElevation, " m"), TRUE~NA),
    minimumElevationInMeters = verbatimElevation)
#####make humboldt core extension table
key<-filter(plotveg_map,original_col %in% old & dwca_class=="HumboldtCore")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)
plot_veg_mapped25_eco<-dwc_map(plot_veg_mapped_25,key,delimiter = "|",custom=NULL)
plot_veg_mapped25_eco<-plot_veg_mapped25_eco %>% select(-c(samplingPerformedBy,samplingPerformedBy.1,protocolNames)) %>% 
  mutate(samplingPerformedBy =
           case_when(
             # If surveyor2 is missing, just include one
             !is.na(samplingPerformedBy.2) & is.na(samplingPerformedBy.3) ~ samplingPerformedBy.2,
             # Otherwise, concatenate
             TRUE ~ paste(samplingPerformedBy.2,samplingPerformedBy.3, sep = ",")
           )) %>% 
  rename("protocolNames"= "protocolNames.1") %>% select(-c(samplingPerformedBy.2,samplingPerformedBy.3))
###make measurement table
key<-filter(plotveg_map,original_col %in% old & dwca_class=="event-emof")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)
#map
plot_veg_mapped25_emof<-dwc_emof_map(plot_veg_mapped_25,key,delimiter = "|",custom="vegplot25")
# --- Step 1.5: Drop columns that were not mapped ---
plot_veg_mapped25_emof <- plot_veg_mapped25_emof %>% select(eventID,measurementValue,measurementType)
#occurrences
###create mapping key
occ_veg_mapped25<-occ_veg_data25  %>% select(any_of(occveg_map$original_col[
  !is.na(occveg_map$original_col) & occveg_map$original_col != ""
]))

#Build a mapping named vector: field = new, original_col = old
old<-colnames(occ_veg_mapped25)
key<-filter(occveg_map,original_col %in% old & dwca_class=="occurrence")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)
###mapping script
occ_veg_mapped25_dwcocc<-dwc_emof_map(occ_veg_mapped25,key,delimiter = "|",custom="vegocc2025")
# Clean up columns
occ_veg_mapped25_dwcocc <- occ_veg_mapped25_dwcocc %>%
  mutate(
    verbatimIdentification = case_when(
      # If the two columns are exactly equal (including both NA), keep one
      verbatimIdentification == verbatimIdentification.1 ~ verbatimIdentification,
      
      # If one is missing, prefer the non-missing one
      is.na(verbatimIdentification) & !is.na(verbatimIdentification.1) ~ verbatimIdentification.1,
      !is.na(verbatimIdentification) & is.na(verbatimIdentification.1) ~ verbatimIdentification,
      
      # Otherwise, concatenate both
      TRUE ~ paste(verbatimIdentification, verbatimIdentification.1, sep = ",")
    )
  ) %>%
 # Keep rows where at least one of the two numeric columns is not NA or blank
  filter(
    !( (is.na(organismQuantity) | organismQuantity == "") ) | source=="trees_2_clean_taxo"
  ) %>%
  # Set type columns to NA when their value columns are NA
  mutate(
    organismQuantityType = if ("organismQuantityType" %in% names(.)) {
      if_else(is.na(organismQuantity), NA_character_, as.character(organismQuantityType))
    } else {
      NULL
    }
  ) %>% unique()%>% ###append suffix for understory records to give unique OID 
  group_by(occurrenceID) %>%
  mutate(occurrenceID = if (n() == 1) {
    occurrenceID
  } else {
    paste0(occurrenceID, "-", sprintf("%02d", row_number()))
  }) %>%
  ungroup() %>% select(-verbatimIdentification.1) %>% unique()
occ_veg_mapped25_dwcocc <- occ_veg_mapped25_dwcocc %>% select(-c(BasalArea,DBH_cm,Flag_type,Keep.,dbase,d_dbase,source))
####occurrence emof field
###make measurement table
key<-filter(occveg_map,original_col %in% old & dwca_class=="occurrence-emof")
key<-tibble(old=key$original_col,new=key$field)
##add in occID
oid<-c("GlobalID","occurrenceID")
eid<-c("pd_id","eventID")
key<-rbind(key,oid,eid)

occ_veg_mapped25_emof<-dwc_emof_map(occ_veg_mapped25,key,delimiter = "|",custom="tree")
# --- Step 1.5: Drop columns that were not mapped ---
occ_veg_mapped25_emof <- occ_veg_mapped25_emof %>% select(eventID,occurrenceID,measurementValue,measurementType)
####emof can be combined!
occ_veg_mapped25_emof$measurementValue<-as.character(occ_veg_mapped25_emof$measurementValue)
veg_mapped25_emof<-full_join(plot_veg_mapped25_emof,occ_veg_mapped25_emof)

###2023

###input clean data sources
survey_plot_veg23<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/survey_plot_data_2023.csv") #just the plot level data for veg in 2008
#####change column names to match key
survey_plot_veg23<-survey_plot_veg23 %>% rename("pd_id"="GlobalID")
plot_veg_data23<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/Plot_Veg_Data_2023.csv") #plot level data and cwd for veg in 2008
#match the survey_plot ids
plot_veg_data23<-filter(plot_veg_data23, pd_id %in% survey_plot_veg23$pd_id)
occ_veg_data23<-read.csv("VegetationSurvey/PNR_VegetationSurvey_Dupe/CleanRecords/VegetationOcc_2023.csv")
occ_veg_data23<-occ_veg_data23 %>% left_join(plot_veg_data23 %>% select(Plot_ID,pd_id),by="Plot_ID") %>% unique()
#remove records with no event info
occ_veg_data23_plots<-filter(occ_veg_data23,!is.na(pd_id))
###create mapping key
plot_veg_mapped_23<-survey_plot_veg23 %>% select(any_of(plotveg_map$original_col[
  !is.na(plotveg_map$original_col) & plotveg_map$original_col != ""
]))

#Build a mapping named vector: field = new, original_col = old--event core only
old<-colnames(plot_veg_mapped_23)
key<-filter(plotveg_map,original_col %in% old & dwca_class=="event")
key<-tibble(old=key$original_col,new=key$field)
###function for renaming and merging columns in dwc format-null custom
plot_veg_mapped23_dwcevent<-dwc_map(plot_veg_mapped_23,key,delimiter = "|",custom=NULL)
#same adjustments as 2008 with extra surveyor addin
###reconcile duplicated columns--cleanup
plot_veg_mapped23_dwcevent<-plot_veg_mapped23_dwcevent %>% select(-c(locationID.1,locationID.2))%>% mutate(
  eventRemarks =
    case_when((
      # If one column if missing, just include one
      !eventRemarks=="" | is.na(eventRemarks)) & eventRemarks.1=="" ~ eventRemarks,
      # Otherwise, concatenate
      TRUE ~ paste(eventRemarks,eventRemarks.1, sep = " | ")
   )) %>%
  rename("verbatimLatitude"= "verbatimCoordinateSystem | verbatimLatitude") %>% 
  rename("verbatimLongitude"= "verbatimCoordinateSystem | verbatimLongitude") %>% 
  mutate(
    verbatimElevation = case_when(!is.na(verbatimElevation)~paste0(verbatimElevation, " m"), TRUE~NA),
    minimumElevationInMeters = verbatimElevation) %>% select(-eventRemarks.1)
###make measurement table
key<-filter(plotveg_map,original_col %in% old & dwca_class=="event-emof")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)
#map
plot_veg_mapped23_emof<-dwc_emof_map(plot_veg_mapped_23,key,delimiter = "|",custom="vegplot")
# --- Step 1.5: Drop columns that were not mapped ---
plot_veg_mapped23_emof <- plot_veg_mapped23_emof %>% select(eventID,measurementValue,measurementType)
#occurrences
###create mapping key
occ_veg_mapped23<-occ_veg_data23_plots  %>% select(any_of(occveg_map$original_col[
  !is.na(occveg_map$original_col) & occveg_map$original_col != ""
]))

#Build a mapping named vector: field = new, original_col = old
old<-colnames(occ_veg_mapped23)
key<-filter(occveg_map,original_col %in% old & dwca_class=="occurrence")
key<-tibble(old=key$original_col,new=key$field)
##add in eventID
eid<-c("pd_id","eventID")
key<-rbind(key,eid)
###mapping script
occ_veg_mapped23_dwcocc<-dwc_emof_map(occ_veg_mapped23,key,delimiter = "|",custom="vegocc2023")
# Clean up columns
occ_veg_mapped23_dwcocc <- occ_veg_mapped23_dwcocc %>%
  mutate(
    occurrenceRemarks =
      case_when(
        # If one column if missing, just include one
        !(occurrenceRemarks=="" | is.na(occurrenceRemarks)) & occurrenceRemarks.1=="" ~ occurrenceRemarks,
        !(occurrenceRemarks.1=="" | is.na(occurrenceRemarks.1)) & occurrenceRemarks=="" ~ occurrenceRemarks.1,
        #both columns are missing
        (occurrenceRemarks=="" | is.na(occurrenceRemarks)) & (occurrenceRemarks.1=="" | is.na(occurrenceRemarks.1))~NA_character_,
        # Otherwise, concatenate
        TRUE ~ paste(occurrenceRemarks,occurrenceRemarks.1, sep = " | ")
      )) %>%
  mutate(
    verbatimIdentification = case_when(
      # If the two columns are exactly equal (including both NA), keep one
      verbatimIdentification == verbatimIdentification.1 ~ verbatimIdentification,
      
      # If one is missing, prefer the non-missing one
      is.na(verbatimIdentification) & !is.na(verbatimIdentification.1) ~ verbatimIdentification.1,
      !is.na(verbatimIdentification) & is.na(verbatimIdentification.1) ~ verbatimIdentification,
      
      # Otherwise, concatenate both
      TRUE ~ paste(verbatimIdentification, verbatimIdentification.1, sep = ", ")
    )
  ) %>%
  # Keep rows where orgQ is not NA or blank and replace orgQ with NA if all four rows are NA
  filter(!is.na(organismQuantity) | source == "trees_23_clean_taxo_presence") %>%
  # Set type columns to NA when their value columns are NA
  mutate(
    organismQuantityType = if ("organismQuantityType" %in% names(.)) {
      if_else(is.na(organismQuantity), NA_character_, as.character(organismQuantityType))
    } else {
      NULL
    }
  ) %>% distinct() %>%###append suffix for regen trees records to give unique OID 
  group_by(occurrenceID) %>%
  mutate(occurrenceID = if (n() == 1) {
    occurrenceID
  } else {
    paste0(occurrenceID, "-", sprintf("%02d", row_number()))
  }) %>%
  ungroup() %>%
  # Remove the duplicate column after merging
  select(-verbatimIdentification.1) %>%
  distinct()

occ_veg_mapped23_dwcocc <- occ_veg_mapped23_dwcocc %>% select(-c(DBH_cm,Flag_type,Keep.,dbase,d_dbase,source,occurrenceRemarks.1))
####occurrence emof field
###make measurement table
key<-filter(occveg_map,original_col %in% old & dwca_class=="occurrence-emof")
key<-tibble(old=key$original_col,new=key$field)
##add in occID
oid<-c("GlobalID","occurrenceID")
eid<-c("pd_id","eventID")
key<-rbind(key,oid,eid)

occ_veg_mapped23_emof<-dwc_emof_map(occ_veg_mapped23,key,delimiter = "|",custom="tree")
# --- Step 1.5: Drop columns that were not mapped ---
occ_veg_mapped23_emof <- occ_veg_mapped23_emof %>% select(eventID,occurrenceID,measurementValue,measurementType)
####emof can be combined!
occ_veg_mapped23_emof$measurementValue<-as.character(occ_veg_mapped23_emof$measurementValue)
veg_mapped23_emof<-full_join(plot_veg_mapped23_emof,occ_veg_mapped23_emof)


###dwca format (flat file) FINAL EXPORT WITH HUMBODLT CORE
##optional joins
###big fat outer join
library(purrr)
#merge 2008 and 2025 separately
setwd("Database/DWC_Veg")
write.csv(plot_veg_mapped_dwcevent,"VegPlot2008_dwcEvent.csv")
write.csv(plot_veg_mapped25_dwcevent,"VegPlot2025_dwcEvent.csv")
write.csv(plot_veg_mapped23_dwcevent,"VegPlot2023_dwcEvent.csv")

write.csv(plot_veg_mapped_eco,"VegPlot2008_dwcEco.csv")
write.csv(plot_veg_mapped25_eco,"VegPlot2025_dwcEco.csv")

#occ tables
write.csv(occ_veg_mapped_dwcocc,"VegOcc2008_dwcOcc.csv")
write.csv(occ_veg_mapped25_dwcocc,"VegOcc2025_dwcOcc.csv")
write.csv(occ_veg_mapped23_dwcocc,"VegOcc2023_dwcOcc.csv")

#emof tables
write.csv(veg_mapped_emof,"Veg2008_dwcEmof.csv")
write.csv(veg_mapped25_emof,"Veg2025_dwcEmof.csv")
write.csv(veg_mapped23_emof,"Veg2023_dwcEmof.csv")

## Combine 3 datasets: 2008, 2023, 2025
# Put data frames in a list
df_list <- list(
  df2008 = plot_veg_mapped_dwcevent,
  df2023 = plot_veg_mapped23_dwcevent,   
  df2025 = plot_veg_mapped25_dwcevent
)

# Check column consistency across all datasets
colnames_list <- lapply(df_list, names)

# Compare all to the first dataframe
ref_names <- colnames_list[[1]]

for (i in seq_along(colnames_list)) {
  if (identical(ref_names, colnames_list[[i]])) {
    message(names(df_list)[i], ": Columns match reference")
  } else {
    message(names(df_list)[i], ": Columns differ")
    
    cat("Only in reference:\n")
    print(setdiff(ref_names, colnames_list[[i]]))
    
    cat("Only in ", names(df_list)[i], ":\n", sep = "")
    print(setdiff(colnames_list[[i]], ref_names))
  }
}

# Align all dataframes to the same column order (and fill missing columns if needed)
all_cols <- Reduce(union, colnames_list)

df_list_aligned <- lapply(df_list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  
  # Add missing columns as NA
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  
  # Reorder columns
  df <- df[, all_cols]
  return(df)
})

# Combine all rows
plot_veg_mapped_all_dwcevent <- dplyr::bind_rows(df_list_aligned)

# Write output
write.csv(plot_veg_mapped_all_dwcevent, "VegPlotAll_dwcEvent.csv", row.names = FALSE)
#humboldt core
#check colnames
if (identical(names(plot_veg_mapped_eco), names(plot_veg_mapped25_eco))) {
  message("Columns match exactly")
} else {
  message("Columns differ")
  
  cat("Only in df1:\n")
  print(setdiff(names(plot_veg_mapped_eco), names(plot_veg_mapped25_eco)))
  
  cat("Only in df2:\n")
  print(setdiff(names(plot_veg_mapped25_eco), names(plot_veg_mapped_eco)))
}
#align column order
plot_veg_mapped25_eco <- plot_veg_mapped25_eco[, names(plot_veg_mapped_eco)]
plot_veg_mapped_all_eco<-bind_rows(plot_veg_mapped_eco,plot_veg_mapped25_eco)
write.csv(plot_veg_mapped_all_eco,"VegPlotAll_eco.csv")

##occ
## Combine 3 datasets: 2008, 2023, 2025
# Put data frames in a list
df_list <- list(
  df2008 = occ_veg_mapped_dwcocc,
  df2023 = occ_veg_mapped23_dwcocc,   
  df2025 = occ_veg_mapped25_dwcocc
)

# Check column consistency across all datasets
colnames_list <- lapply(df_list, names)

# Compare all to the first dataframe
ref_names <- colnames_list[[1]]

for (i in seq_along(colnames_list)) {
  if (identical(ref_names, colnames_list[[i]])) {
    message(names(df_list)[i], ": Columns match reference")
  } else {
    message(names(df_list)[i], ": Columns differ")
    
    cat("Only in reference:\n")
    print(setdiff(ref_names, colnames_list[[i]]))
    
    cat("Only in ", names(df_list)[i], ":\n", sep = "")
    print(setdiff(colnames_list[[i]], ref_names))
  }
}

# Align all dataframes to the same column order (and fill missing columns if needed)
all_cols <- Reduce(union, colnames_list)

df_list_aligned <- lapply(df_list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  
  # Add missing columns as NA
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  
  # Reorder columns
  df <- df[, all_cols]
  return(df)
})

# Combine all rows
occ_veg_mapped_all_dwcocc <- dplyr::bind_rows(df_list_aligned)

# Write output
write.csv(occ_veg_mapped_all_dwcocc,"VegoccAll_dwcocc.csv")
#extended measurement or fact table
## Combine 3 datasets: 2008, 2023, 2025
# Put data frames in a list
df_list <- list(
  df2008 = veg_mapped_emof,
  df2023 = veg_mapped23_emof,   
  df2025 = veg_mapped25_emof
)

# Check column consistency across all datasets
colnames_list <- lapply(df_list, names)

# Compare all to the first dataframe
ref_names <- colnames_list[[1]]

for (i in seq_along(colnames_list)) {
  if (identical(ref_names, colnames_list[[i]])) {
    message(names(df_list)[i], ": Columns match reference")
  } else {
    message(names(df_list)[i], ": Columns differ")
    
    cat("Only in reference:\n")
    print(setdiff(ref_names, colnames_list[[i]]))
    
    cat("Only in ", names(df_list)[i], ":\n", sep = "")
    print(setdiff(colnames_list[[i]], ref_names))
  }
}

# Align all dataframes to the same column order (and fill missing columns if needed)
all_cols <- Reduce(union, colnames_list)

df_list_aligned <- lapply(df_list, function(df) {
  missing_cols <- setdiff(all_cols, names(df))
  
  # Add missing columns as NA
  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA
  }
  
  # Reorder columns
  df <- df[, all_cols]
  return(df)
})

# Combine all rows
veg_mapped_all_emof <- dplyr::bind_rows(df_list_aligned)

# Write output
write.csv(veg_mapped_all_emof,"VegAll_emof.csv")
GBIF_export<-list(event=plot_veg_mapped_all_dwcevent, eco=plot_veg_mapped_all_eco, occ=occ_veg_mapped_all_dwcocc, emof=veg_mapped_all_emof)
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
  mutate(occurrenceStatus=case_when(organismQuantity=="Absent" ~"Absent",TRUE~"Present")) #presence/absence defined by orgQuantity
GBIF_export$occ$organismQuantityType<- gsub("d_CC","CoverClass", GBIF_export$occ$organismQuantityType)#remove d_ from columns
###taxonomic backbone additions using gbif
library(rgbif)
plants <- name_backbone(name = "Plantae", kingdom = "Plantae")
plants$usageKey
# Define region (PA)
occ_data <- occ_search(
  kingdomKey = 6,   # Plantae
  country = "US",
  stateProvince= "Pennsylvania",
  limit = 100000
)
#taxonomic backbone
taxonomy <- occ_data$data %>%
  dplyr::select(
    kingdom, phylum, class, order, family,
  ) %>%
  distinct()
##remove genus, species, na fmaily
taxonomy<-filter(taxonomy,!is.na(family))
GBIF_export$occ<-GBIF_export$occ %>% mutate(specificEpithet=
 if_else(is.na(scientificName) | scientificName == "",
"",gbif_parse(scientificName)[,"specificepithet"]))
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(
    taxonRank = case_when(
      !is.na(specificEpithet) & specificEpithet != "" ~ "species",
      !is.na(genus) & genus != "" ~ "genus",
      !is.na(family) & family != "" ~ "family",
      TRUE ~ ""   # or "unranked"
    )
  )
#add kingdom
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(kingdom="Plantae")
#add phylum except to unknown/other
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(phylum=case_when(!verbatimIdentification %in% c("UNK,Unknown","OTHE,Other")~"Tracheophyta",TRUE~""))
#add order just for rushes
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(order=case_when(verbatimIdentification ==("RUSH,Rush")~"Poales",TRUE~""))
#add families for sedge and grass
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(family=case_when(verbatimIdentification =="SEDGE,Sedge"~"Cyperaceae",verbatimIdentification=="GRASS,Grass"~"Poaceae",family=="Aceraceae"~"Sapindaceae",family=="Tiliaceae"~"Malvaceae",family=="Monotropaceae"~"Ericaceae",family=="Santalaceae"~"Cervantesiaceae",family=="Clusiaceae"~"Hypericaceae",TRUE~family))
#change Acer, Tilia

#adjust ranks
#TREE=Tracheophyta; phylum--plantae?
#SNAG=Tracheophyta; phylum
#RUSH=Poales; order
#SEDGE=Cyperaceae; family
#GRASS=Poaceae; family
GBIF_export$occ<-GBIF_export$occ %>%
  mutate(taxonRank=case_when(!is.na(family) & is.na(genus) ~"family",!order=="" & is.na(family)~"order", !phylum=="" & order=="" & is.na(family) ~"phylum",phylum=="" ~"kingdom",TRUE~taxonRank))
#fill in class, order
GBIF_export$occ<- GBIF_export$occ %>% left_join(taxonomy %>% select(class,order,family),by=c("family"))
GBIF_export$occ<-GBIF_export$occ %>% mutate(order=coalesce(order.y,order.x)) %>% select(-c(order.x,order.y))
#class
GBIF_export$occ<-GBIF_export$occ %>% mutate(class=case_when(order=="Poales"~"Liliopsida",TRUE~class))
#buffalo nut special add in
GBIF_export$occ<-GBIF_export$occ %>% mutate(class=case_when(genus=="Pyrularia"~"Magnoliopsida",TRUE~class))
GBIF_export$occ<-GBIF_export$occ %>% mutate(order=case_when(genus=="Pyrularia"~"Santalales",TRUE~order))
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

GBIF_export$event<-GBIF_export$event %>% select(-eventTime,-eventDate)%>% rename("eventDate"="eventDate_iso") 
#placeholder for protocolNames-->samplingProtocol
#locality data
#sample size
GBIF_export$event$sampleSizeUnit<-"radius_m"
GBIF_export$event$sampleSizeValue<-"10"
GBIF_export$event$countryCode<-"US"
GBIF_export$event$locality<-"Powdermill Nature Reserve and Field Station"
GBIF_export$event$stateProvince<-"Pennsylvania"
GBIF_export$event$verbatimCoordinateSystem<-"UTM"
GBIF_export$event$parentEventID<-NULL
##HC columns
GBIF_export$eco$protocolReferences<-NA
GBIF_export$eco$isVegetationCoverReported<-"Yes"
#adjust id columns to first position
cols <- "eventID"
GBIF_export$event <- GBIF_export$event[, c(cols, setdiff(names(GBIF_export$event), cols))]
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




