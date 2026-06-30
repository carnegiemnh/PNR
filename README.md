# Vegetation Survey Data Cleaning and GBIF Preparation Scripts

## Overview

This repository contains scripts and workflows used to clean, reconcile, standardize, and integrate vegetation survey datasets from Powdermill Nature Reserve. The processed outputs are formatted to meet biodiversity data publication requirements and are intended for submission to the **Global Biodiversity Information Facility (GBIF)**.

The primary objectives of these scripts are to:

- Consolidate vegetation survey data from multiple source files collected over different time periods in ArcGIS.
- Standardize species names and taxonomic information.
- Resolve data quality issues and inconsistencies.
- Harmonize plot, site, and sampling metadata.
- Generate Darwin Core-compliant outputs for biodiversity data publication.
- Perform quality assurance checks prior to GBIF submission.

#How to Use the scripts
vegsurvey_clean: Ingests raw files from ArcGIS, creates data quality checks and outputs flagged records for review. Cleaned files are reimported with notes for changes made and updated ArcGIS tables are exported. 

vegsurvey_taxo: Ingests the plot and occurrence cleaned files from veg_clean. Taxonomy was mapped using USDA plant names and alphanumeric codes were mapped to scientific names with common names and family information.
Event/plot level data and occurrence level data were exported for each year of surveys.

dwc_map: The plot and occurrence files with standardized taxonomy are mapped to darwin core survey-event format. Each year contains unique attributes that were separately mapped to a parent event file, HumboldtCore extension, occurrence file, and extended measurement or fact extension file. The parent event files, HumboldtCore (eco), occurrence, and extended measurement (emof) tables were merged across years for the final dataset. For GBIF dataset creation additional metadata columns were added before generating the dwca zip file that contains four text files. This file may be published to GBIF through the Carnegie Museum of Natural History IPT server.
