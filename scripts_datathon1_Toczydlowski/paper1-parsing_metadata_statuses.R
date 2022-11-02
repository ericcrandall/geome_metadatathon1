#idea: make figures for datathon perspectives piece

# NOTE - planning to state that we counted written out instances of "not applicable" etc. as missing
# NOTE - for material sample ID, planning to state there was a value in x, x, and/or x NCBI metadata columns and these may or may not be informative/match IDs in associated published papers
# NOTE - should NA type values be coded as no or maybe in a paper?

#METADATA CATEGORIES:
#paper available
#material sample id
#localilty
#coordinates
#country
#habitat
#environ medium
#yr collected
#permit info
#preservative
#derived genetic data



#load libraries ----
library(googledrive)    #get data
library(googlesheets4)  #get data
library(dplyr)          #data wrangling
library(tidyr)          #data wrangling
library(ggplot2)        #graphing
library(moonBook)       #pie chart
library(webr)           #pie chart
library(ggpattern)      #remotes::install_github("coolbutuseless/ggpattern")
library(stringr)        #filtering species names



# *******************************************************************************
# *******************************************************************************
# NEW SECTION - PRE DATATHON STATUS ----------------------------------------------

rm(list = ls())
gc()


# READ IN DATA

# all records from NCBI (minus our filters like not human)
allrecords.raw <- read.csv("/Users/rachel/Desktop/DivDiv/divdiv_collecting_genetic_data/final_files_NCBI_records/11-NCBI_bioprojects__not-human_bacteria_viral_metagenome__BioProjectsANDSRAANDBioSamplesANDTaxonomy-WITHseqdataofinterest.csv") %>% dplyr::select(-X)

#get columns with metadata we care about
df.all <- allrecords.raw %>% dplyr::select(link,project_acc_bioprj,organism_biosamp,biosample_acc_sra,run_acc_sra,
                                           collection_date_biosamp,lat_long_biosamp,publication_ID_bioprj,DOI_pubmed,
                                           library_name_expxml_sra,samplename_sampledata_biosamp,
                                           identifiers_biosamp,samplename_identifiers_biosamp,infraspecies_biosamp,
                                           expxml_sra,sampledata_biosamp,
                                           date_this_sequence_first_added_to_our_records,publicationdate_biosamp,
                                           division_taxonomy, contains("via.taxize"),total_number_base_pairs_sra)



#check which packages were used to upload data to NCBI
#figure out how many times "<Package display_name=" occurs in sampledata col. (should be once) - so our code to extract it will work
ncolumns <- df.all %>% dplyr::select(sampledata_biosamp) %>% 
  mutate(n_occurs = stringr::str_count(df.all$sampledata_biosamp, "<Package display_name=")) %>% 
  filter(is.na(sampledata_biosamp)==F)
ncolumns %>% ggplot(aes(x=n_occurs)) + geom_histogram(binwidth = 1) + theme_bw() + theme_classic()
ncolumns %>% filter(n_occurs > 1) %>% nrow() #should be 0
rm(ncolumns)
#extract submission package into new clean col.
df.all <- df.all %>% separate(., sampledata_biosamp, into = c("garbage","pkg.working"), sep = "<Package display_name=", remove = FALSE) %>% 
  select(-garbage) %>% separate(.,pkg.working, into = c("pkg_sampledata_biosamp","garbage"), sep = "<", remove = TRUE) %>% 
  dplyr::select(-garbage)
#look at packages we have data from (just for datathon, pre 2019)
df.all %>% filter(date_this_sequence_first_added_to_our_records == "10-28-2019 - 11-7-2019") %>% 
  group_by(project_acc_bioprj,pkg_sampledata_biosamp) %>% 
  summarise(n=n()) %>% group_by(pkg_sampledata_biosamp) %>% summarise(n=n()) %>% as.data.frame() %>% arrange(desc(n))



# ***************************************************************************************
# GET STATUS OF EACH TYPE OF METADATA
#add on col that says if diff types of metadata are present or not

#lat/long -----
#lat/long extracted from column" sampledata" that is returned by entrez_summary(), db = "biosample" 
#term we pulled out data for: display_name=\"latitude and longitude\
df.all %>% group_by(lat_long_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))
df.all <- df.all %>% mutate(haslatlong.temp = ifelse(lat_long_biosamp == "unknown", "no", "yes")) %>% 
  mutate(haslatlong = ifelse(is.na(haslatlong.temp)==T, "maybe in pub. paper", haslatlong.temp)) %>% dplyr::select(-haslatlong.temp)
df.all %>% group_by(haslatlong) %>% summarise(n=n())


#publication -----
#publication_ID_bioprj is a column returned by entrez_fetch(), db = "bioproject" 
#term we pulled out data for: <Publication\ id=
#DOI_pubmed is "doi" extracted from column "articleids" retured by entrez_summary(), db = "pubmed" 
#saying publication present if info present in publication_ID_bioprj and/or DOI_pubmed columns
df.all %>% group_by(publication_ID_bioprj,DOI_pubmed) %>% summarise(n=n()) %>% arrange(desc(n))
df.all <- df.all %>% mutate(haspub = ifelse(is.na(publication_ID_bioprj)==T & is.na(DOI_pubmed)==T, "maybe in pub. paper", "yes"))
df.all %>% group_by(haspub) %>% summarise(n=n())


#locality -----
#locality_sampledata_biosamp extracted from column" sampledata" that is returned by entrez_summary(), db = "biosample" 
#term we pulled out data for: display_name=\"geographic location\">
#figure out how many times "display_name=\"geographic location\">" occurs in sampledata col. (should be once) - so our code to extract it will work
ncolumns <- df.all %>% dplyr::select(sampledata_biosamp) %>% 
  mutate(n_occurs = stringr::str_count(df.all$sampledata_biosamp, "display_name=\"geographic location\">")) %>% 
  filter(is.na(sampledata_biosamp)==F)
ncolumns %>% ggplot(aes(x=n_occurs)) + geom_histogram(binwidth = 1) + theme_bw() + theme_classic()
ncolumns %>% filter(n_occurs > 1) %>% nrow() #should be 0
rm(ncolumns)
#extract locality into new clean col.
df.all <- df.all %>% separate(., sampledata_biosamp, into = c("garbage","locality.working"), sep = "display_name=\"geographic location\">", remove = FALSE) %>% 
  select(-garbage) %>% separate(.,locality.working, into = c("locality_sampledata_biosamp","garbage"), sep = "<", remove = TRUE) %>% 
  dplyr::select(-garbage)
#replace a bunch of localities that aren't actually localities with NAs
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "missing"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Missing"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "MISSING"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "na"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "n/a"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "?"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "-"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "NA"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "N/A"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "not collected"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Not collected"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Not Collected"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "NULL"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "not available"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Not available"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "not applicable"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Not applicable"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Not Applicable"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "NOT APPLICABLE"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "unknown"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Unknown"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "UNKNOWN"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "unspecified"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Unk"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "not determined"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "None"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "unk"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "not_collected"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "not collcted"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "not collect"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "No application"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "Not applicabe"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "none"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "n/ap"] = "no"
df.all$locality_sampledata_biosamp[df.all$locality_sampledata_biosamp == "None"] = "no"

#extract out more specific locality than country
df.all <- df.all %>% separate(., locality_sampledata_biosamp, into = c("garbage","specific_locality_sampledata_biosamp"), sep = ":", remove = FALSE) %>% 
  mutate(specific_locality_sampledata_biosamp = ifelse(locality_sampledata_biosamp == "no", "no", specific_locality_sampledata_biosamp))

df.all %>% group_by(specific_locality_sampledata_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))
df.all$haslocality[is.na(df.all$specific_locality_sampledata_biosamp)==T] = "maybe in pub. paper"
df.all$haslocality[is.na(df.all$specific_locality_sampledata_biosamp)==F] = "yes"
df.all$haslocality[df.all$specific_locality_sampledata_biosamp=="no"] = "no"
df.all %>% group_by(haslocality) %>% summarise(n=n())


#country -----
#countryish_locality_sampledata_biosamp is extracted from "locality_sampledata_biosamp" which was extracted from
#column" sampledata" that is returned by entrez_summary(), db = "biosample" 
#term we pulled out data for: display_name=\"geographic location\"> to get locality_sampledata_biosamp
#and then built countryish_locality_sampledata_biosamp with what was to left of : in locality_sampledata_biosamp

#extract value before ":" in locality into country ish column (from skimming this seems to always be country)
df.all <- df.all %>% separate(., locality_sampledata_biosamp, into = c("countryish_locality_sampledata_biosamp"), sep = ":", remove = FALSE)
#note not all of resulting entries are countries (e.g. Antarctica, cities within the US)

df.all %>% group_by(countryish_locality_sampledata_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))
df.all$hascountry[is.na(df.all$countryish_locality_sampledata_biosamp)==T] = "maybe in pub. paper"
df.all$hascountry[is.na(df.all$countryish_locality_sampledata_biosamp)==F] = "yes"
df.all$hascountry[df.all$countryish_locality_sampledata_biosamp=="no"] = "no"
df.all %>% group_by(hascountry) %>% summarise(n=n())


#year collected -----
#collection_date_biosamp extracted from column" sampledata" that is returned by entrez_summary(), db = "biosample" 
#term we pulled out data for: display_name="collection date">
#re-extracting collection date bc original code seems to have had an issue and not grabbed all dates
ncolumns <- df.all %>% dplyr::select(sampledata_biosamp) %>% 
  mutate(n_occurs = stringr::str_count(df.all$sampledata_biosamp, "display_name=\"collection date\">")) %>% 
  filter(is.na(sampledata_biosamp)==F)
ncolumns %>% ggplot(aes(x=n_occurs)) + geom_histogram(binwidth = 1) + theme_bw() + theme_classic()
ncolumns %>% filter(n_occurs > 1) %>% nrow() #should be 0
rm(ncolumns)
#re-extract collection date into new clean col.
df.all <- df.all %>% mutate(collection_date_biosamp.orig = collection_date_biosamp) 
df.all <- df.all %>% separate(., sampledata_biosamp, into = c("garbage","collection_date_biosamp.working"), sep = "display_name=\"collection date\">", remove = FALSE) %>% 
  select(-garbage) %>% separate(.,collection_date_biosamp.working, into = c("collection_date_biosamp","garbage"), sep = "<", remove = TRUE) %>% 
  dplyr::select(-garbage)
#replace a bunch of collection dates that aren't actually collection dates with NAs
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Unknown Date"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "missing"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Missing"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "MISSING"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "na"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "n/a"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "?"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "-"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "NA"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "N/A"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "not collected"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Not collected"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Not Collected"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "NULL"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "not available"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Not available"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "not applicable"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Not applicable"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Not Applicable"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "NOT APPLICABLE"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "unknown"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Unknown"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "UNKNOWN"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "unspecified"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Unk"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "not determined"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "None"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "unk"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "not_collected"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "not collcted"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "not collect"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "No application"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "Not applicabe"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "none"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "n/ap"] = "no"
df.all$collection_date_biosamp[df.all$collection_date_biosamp == "None"] = "no"
#what entires are left in collection date for cells with no numbers present?
df.all %>% filter(grepl("^[^0-9]+$",collection_date_biosamp)) %>% group_by(collection_date_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))

df.all %>% group_by(collection_date_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))
df.all$hascollectiondate[is.na(df.all$collection_date_biosamp)==T] = "maybe in pub. paper"
df.all$hascollectiondate[is.na(df.all$collection_date_biosamp)==F] = "yes"
df.all$hascollectiondate[df.all$collection_date_biosamp=="no"] = "no"
df.all %>% group_by(hascollectiondate) %>% summarise(n=n())


#material sample id -----
#library_name_expxml_sra extracted from column "expxml" that is returned by entrez_summary, db = "sra"
#term we pulled out data for: LIBRARY_NAME
#samplename_sampledata_biosamp extracted from column "sampledata" that is returned by entrez_summary, db = "biosample"
#term we pulled out data for:display_name="sample name"
#samplename_identifiers_biosamp extracted from column "identifiers" that is returned by entrez_summary, db = "biosample"
#term we pulled out data for: display_name=Sample name:
df.all %>% group_by(library_name_expxml_sra,samplename_sampledata_biosamp,samplename_identifiers_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))
df.all <- df.all %>% mutate(hassampID = ifelse(is.na(library_name_expxml_sra) == T & 
                                                 is.na(samplename_sampledata_biosamp)==T &
                                                 is.na(samplename_identifiers_biosamp)==T, "no", "yes"))
df.all %>% group_by(hassampID) %>% summarise(n=n())


#derived genetic data -----
#not an attribute in NCBI
df.all <- df.all %>% mutate(hasderivedgeneticdata = "maybe in pub. paper")


#preservative -----
#not an attribute in NCBI
df.all <- df.all %>% mutate(haspreservative = "maybe in pub. paper")


#permit -----
#not an attribute in NCBI
df.all <- df.all %>% mutate(haspermit = "maybe in pub. paper")


#habitat -----
#envirobroad_sampledata_biosamp extracted from column "sampledata" that is returned by entrez_summary, db = "biosample"
#term we pulled out data for:display_name="broad-scale environmental context">
#figure out how many times "display_name=\"broad-scale environmental context\">" occurs in sampledata col. (should be once) - so our code to extract it will work
ncolumns <- df.all %>% dplyr::select(sampledata_biosamp) %>% 
  mutate(n_occurs = stringr::str_count(df.all$sampledata_biosamp, "display_name=\"broad-scale environmental context\">")) %>% 
  filter(is.na(sampledata_biosamp)==F)
ncolumns %>% ggplot(aes(x=n_occurs)) + geom_histogram(binwidth = 1) + theme_bw() + theme_classic()
ncolumns %>% filter(n_occurs > 1) %>% nrow() #should be 0
rm(ncolumns)
#extract environment broad scale into new clean col.
df.all <- df.all %>% separate(., sampledata_biosamp, into = c("garbage","envirobroad.working"), sep = "display_name=\"broad-scale environmental context\">", remove = FALSE) %>% 
  select(-garbage) %>% separate(.,envirobroad.working, into = c("envirobroad_sampledata_biosamp","garbage"), sep = "<", remove = TRUE) %>% 
  dplyr::select(-garbage)
#replace a bunch of collection dates that aren't actually collection dates with NAs
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "missing"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Missing"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "MISSING"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "na"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "n/a"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "?"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "-"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "NA"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "N/A"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "not collected"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Not collected"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Not Collected"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "NULL"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "not available"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Not available"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "not applicable"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Not applicable"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Not Applicable"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "NOT APPLICABLE"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "unknown"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Unknown"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "UNKNOWN"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "unspecified"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Unk"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "not determined"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "None"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "unk"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "not_collected"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "not collcted"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "not collect"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "No application"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "Not applicabe"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "none"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "n/ap"] = "no"
df.all$envirobroad_sampledata_biosamp[df.all$envirobroad_sampledata_biosamp == "None"] = "no"

df.all %>% group_by(envirobroad_sampledata_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))
df.all$hasenvirobroad[is.na(df.all$envirobroad_sampledata_biosamp)==T] = "maybe in pub. paper"
df.all$hasenvirobroad[is.na(df.all$envirobroad_sampledata_biosamp)==F] = "yes"
df.all$hasenvirobroad[df.all$envirobroad_sampledata_biosamp=="no"] = "no"
df.all %>% group_by(hasenvirobroad) %>% summarise(n=n())


#enviro medium -----
#enviromedium_sampledata_biosamp extracted from column "sampledata" that is returned by entrez_summary, db = "biosample"
#term we pulled out data for:display_name="environmental medium"
#figure out how many times "display_name=\"environmental medium\">" occurs in sampledata col. (should be once) - so our code to extract it will work
ncolumns <- df.all %>% dplyr::select(sampledata_biosamp) %>% 
  mutate(n_occurs = stringr::str_count(df.all$sampledata_biosamp, "display_name=\"environmental medium\">")) %>% 
  filter(is.na(sampledata_biosamp)==F)
ncolumns %>% ggplot(aes(x=n_occurs)) + geom_histogram(binwidth = 1) + theme_bw() + theme_classic()
ncolumns %>% filter(n_occurs > 1) %>% nrow() #should be 0
rm(ncolumns)
#extract environment medium into new clean col.
df.all <- df.all %>% separate(., sampledata_biosamp, into = c("garbage","enviromedium.working"), sep = "display_name=\"environmental medium\">", remove = FALSE) %>% 
  select(-garbage) %>% separate(.,enviromedium.working, into = c("enviromedium_sampledata_biosamp","garbage"), sep = "<", remove = TRUE) %>% 
  dplyr::select(-garbage)
#replace a bunch of collection dates that aren't actually collection dates with NAs
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "missing"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Missing"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "MISSING"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "na"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "n/a"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "?"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "-"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "NA"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "N/A"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "not collected"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Not collected"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Not Collected"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "NULL"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "not available"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Not available"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "not applicable"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Not applicable"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Not Applicable"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "NOT APPLICABLE"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "unknown"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Unknown"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "UNKNOWN"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "unspecified"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Unk"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "not determined"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "None"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "unk"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "not_collected"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "not collcted"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "not collect"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "No application"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "Not applicabe"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "none"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "n/ap"] = "no"
df.all$enviromedium_sampledata_biosamp[df.all$enviromedium_sampledata_biosamp == "None"] = "no"

df.all %>% group_by(enviromedium_sampledata_biosamp) %>% summarise(n=n()) %>% arrange(desc(n))
df.all$hasenviromedium[is.na(df.all$enviromedium_sampledata_biosamp)==T] = "maybe in pub. paper"
df.all$hasenviromedium[is.na(df.all$enviromedium_sampledata_biosamp)==F] = "yes"
df.all$hasenviromedium[df.all$enviromedium_sampledata_biosamp=="no"] = "no"
df.all %>% group_by(hasenviromedium) %>% summarise(n=n())


#add column to denote which samples have time AND space data
#counting collection date as time and lat/long as space
df.all$hasspatiotemp[df.all$haslatlong=="yes" & df.all$hascollectiondate=="yes"] = "yes"
df.all$hasspatiotemp[df.all$haslatlong=="yes" & df.all$hascollectiondate=="no"] = "no"
df.all$hasspatiotemp[df.all$haslatlong=="yes" & df.all$hascollectiondate=="maybe in pub. paper"] = "maybe in pub. paper"
df.all$hasspatiotemp[df.all$haslatlong=="no" & df.all$hascollectiondate=="yes"] = "no"
df.all$hasspatiotemp[df.all$haslatlong=="no" & df.all$hascollectiondate=="no"] = "no"
df.all$hasspatiotemp[df.all$haslatlong=="no" & df.all$hascollectiondate=="maybe in pub. paper"] = "no"
df.all$hasspatiotemp[df.all$haslatlong=="maybe in pub. paper" & df.all$hascollectiondate=="yes"] = "maybe in pub. paper"
df.all$hasspatiotemp[df.all$haslatlong=="maybe in pub. paper" & df.all$hascollectiondate=="no"] = "no"
df.all$hasspatiotemp[df.all$haslatlong=="maybe in pub. paper" & df.all$hascollectiondate=="maybe in pub. paper"] = "maybe in pub. paper"

df.all %>% group_by(haslatlong,hascollectiondate,hasspatiotemp) %>% summarise(n=n())

#save this file that takes awhile to generate
write.csv(df.all, "working_files/pre-datathon_full_list_from_NCBI.csv")



# FILTERING PRE DATATHON LIST ----------------------------------------------------------

#NOTE - for datathon we only had records that we grabbed in 2019 to work from

#read in dataframe with metadata status for each sample
#df.all <- read.csv("working_files/pre-datathon_full_list_from_NCBI.csv", header = T) %>% dplyr::select(-X)

#filter out more bacteria, enviro samples, and viruses that still snuck through
df.all <- df.all %>% filter(division_taxonomy != "Environmental samples") %>% 
  filter(division_taxonomy != "Bacteria") %>% 
  filter(division_taxonomy != "Viruses")

df.all %>% distinct(run_acc_sra) %>% nrow()
#505,609 sequences 
df.all %>% distinct(biosample_acc_sra) %>% nrow()
#432,079 biosamples
df.all %>% distinct(project_acc_bioprj) %>% nrow()
#6,741 bioprjs

#remove weird characters from species names
df.all <- df.all %>% mutate(organism_biosamp_clean = gsub("\\[","",organism_biosamp)) %>% 
  mutate(organism_biosamp_clean = gsub("\\]","",organism_biosamp_clean)) %>% 
  mutate(organism_biosamp_clean = gsub("\\(","",organism_biosamp_clean)) %>% 
  mutate(organism_biosamp_clean = gsub("\\)","",organism_biosamp_clean)) %>% 
  mutate(organism_biosamp_clean = gsub("\\'","",organism_biosamp_clean))

#make genus column that we can use for filtering
df.all <- df.all %>% separate(., organism_biosamp_clean, into = c("genus_organism_biosamp"), sep = " ", remove = F, extra = "warn")

#get list of "species" to filter out (e.g. human pathogens and model orgs.)
species_to_drop <- read.delim("NonWildSpecies/nonWildSpecies_final_sources.tsv", header = T)
species_to_drop.models <- species_to_drop %>% filter(Category == "Model Species")
species_to_drop.hpaths <- species_to_drop %>% filter(Category == "Human Pathogen")

#test of code
#temp <- data.frame(species = c("Citrus something","Citrus","Something citrus","Something citrusea","Citrus citrus citrus","Hippo","Hippo poto","Hippo  poto", "Hippo poto ","citrus","Something Citrus"), anotherthing = c(2,3,4,1,6,8,8,2,3,3,1))
#toremove <- data.frame(speciestokick = c("Citrus","somethingelse","another","Hippo poto"), anothercol = c("domestic1","domestic2","domestic3","domestic4"))
#temp %>% filter(!str_detect(species, str_c(toremove$speciestokick, collapse="|")))
#temp %>% filter(!str_detect(species, regex(str_c("^", toremove$speciestokick, collapse="|"), ignore_case = FALSE)))
#temp %>% mutate(is_domestic = ifelse(str_detect(species, str_c(toremove$speciestokick, collapse="|")) == TRUE, "yes", "no")) %>% 
#  mutate(domestic_matched = str_extract(species, str_c(toremove$speciestokick, collapse="|")))

#filter out model orgs. and human pathogens
df.nomodels <- df.all %>% 
  filter(!str_detect(organism_biosamp_clean, str_c(species_to_drop.models$Species, collapse="|")))
nrow(df.all) - nrow(df.nomodels) #number of entries that were model orgs
#100,499 rows were model org. with exact name matching before
#107,238 rows were model org. with grepl name matching now

df <- df.nomodels %>% 
  filter(!str_detect(organism_biosamp_clean, str_c(species_to_drop.hpaths$Species, collapse="|")))
nrow(df.nomodels) - nrow(df) #number of entries that were model orgs
#16,767 rows were human pathogen with exact name matching before
#18,142 rows were human pathogen with grepl name matching now


#assign a column that IDs species as domestic
species_domestic <- species_to_drop %>% filter(grepl("Domestic",Category)) %>% dplyr::select(Species,Category) %>% 
  rename("domestic_type" = "Category") %>% mutate(is_domestic = "yes")

df <- df %>% mutate(is_domestic = ifelse(str_detect(organism_biosamp_clean, str_c(species_domestic$Species, collapse="|")) == TRUE, 
                                         "yes", 
                                         "no")) %>% 
  mutate(domestic_matched = str_extract(organism_biosamp_clean, str_c(species_domestic$Species, collapse="|")))

df <- merge(df, 
            species_domestic %>% dplyr::select(Species,domestic_type),
            by.x = "domestic_matched", by.y = "Species",
            all.x = TRUE)

#get rid of 23 samples that are somehow 2021 even tho we ran code in Nov 2020
df <- df %>% filter(publicationdate_biosamp != "2021/05/07") %>% filter(publicationdate_biosamp != "2021/07/01")

#11 samples don't have a date when added to our records, so filtering them out
df <- df %>% filter(is.na(date_this_sequence_first_added_to_our_records)==F)

#get rid of some samples (N = 91) that have same biosample accession but refer to a bunch of diff samples
#when we calc things later at biosample level these samples mess up calcs
df <- df %>% filter(!grepl("P5-2",biosample_acc_sra)) %>% filter(!grepl("SC23",biosample_acc_sra)) %>% 
  filter(!grepl("M35-1",biosample_acc_sra)) %>% filter(!grepl("QL12",biosample_acc_sra))

# fix some inconsistent metadata status coding -------
# aka where a biosample was used for multiple things and one record has metadata but the other doesn't

#if checkspatiotemp > 1 then metadata status is not the same for that metadata type and biosamp
df %>% group_by(biosample_acc_sra) %>% 
  mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(biosample_acc_sra,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj,biosample_acc_sra,metadata_status_all) %>%
  arrange(biosample_acc_sra) %>% 
  View()

df$haslatlong[df$biosample_acc_sra == "B963676"] = "no"
df$haslocality[df$biosample_acc_sra == "B963676"] = "no"
df$hascountry[df$biosample_acc_sra == "B963676"] = "no"
df$hascollectiondate[df$biosample_acc_sra == "B963676"] = "no"
df$haslatlong[df$biosample_acc_sra == "SAMD00074876"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMD00093090"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMD00093091"] = "yes"
df$haslocality[df$biosample_acc_sra == "SAMN00139642"] = "no"
df$hascountry[df$biosample_acc_sra == "SAMN00139642"] = "no"
df$haslocality[df$biosample_acc_sra == "Macia"] = "no"
df$hascountry[df$biosample_acc_sra == "Macia"] = "no"
df$hascountry[df$biosample_acc_sra == "SAMN00254051"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN00254051"] = "yes"
df$haslocality[df$biosample_acc_sra == "SAMN01991230"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN01991230"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN01991230"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN01932302"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMN02228567"] = "yes"
df$haslocality[df$biosample_acc_sra == "SAMN02228567"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN02228567"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN02228567"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMN02228568"] = "yes"
df$haslocality[df$biosample_acc_sra == "SAMN02228568"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN02228568"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN02228568"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN02378606"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN02378606"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN02378617"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN02378617"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN02378619"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN02378619"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN02378632"] = "yes"
df$hascollectiondate[df$biosample_acc_sra == "SAMN02378632"] = "yes"
df$haslocality[df$biosample_acc_sra == "SAMN02867734"] = "yes"
df$hascountry[df$biosample_acc_sra == "SAMN02867734"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMN03893650"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMN03893669"] = "yes"
df$haslocality[df$biosample_acc_sra == "SAMN04320769"] = "no"
df$hascountry[df$biosample_acc_sra == "SAMN04320769"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMN09667862"] = "yes"
df$haslocality[df$biosample_acc_sra == "SAMN04320862"] = "no"
df$hascountry[df$biosample_acc_sra == "SAMN04320862"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMN11479977"] = "yes"
df$haslatlong[df$biosample_acc_sra == "SAMN11479978"] = "yes"

#all from a bioprj where samples were used as samples and then in genome work later
recordsthatneedcorrecting1 <- data.frame(biosample_acc_sra = c("SAMN12702089", "SAMN12702090", "SAMN12702091", "SAMN12702092",
                                                               "SAMN12702093", "SAMN12702094", "SAMN12702095", "SAMN12702096",
                                                               "SAMN12702097", "SAMN12702098", "SAMN12702099", "SAMN12702100",
                                                               "SAMN12702101", "SAMN12702102", "SAMN12702103", "SAMN12702104",
                                                               "SAMN12702105", "SAMN12702106", "SAMN12702107", "SAMN12702108",
                                                               "SAMN12702109", "SAMN12702110", "SAMN12702111", "SAMN12702112",
                                                               "SAMN12702113", "SAMN12702114", "SAMN12702115", "SAMN12702116",
                                                               "SAMN12702117", "SAMN12702118", "SAMN12702119", "SAMN12702120",
                                                               "SAMN12702121", "SAMN12702122", "SAMN12702123", "SAMN12702124",
                                                               "SAMN12702125", "SAMN12702126", "SAMN12702127", "SAMN12702128",
                                                               "SAMN12702129", "SAMN12702130", "SAMN12702131", "SAMN12702132",
                                                               "SAMN12702133", "SAMN12702134", "SAMN12702135", "SAMN12702136",
                                                               "SAMN12702137", "SAMN12702138", "SAMN12702139", "SAMN12702140",
                                                               "SAMN12702141", "SAMN12702142", "SAMN12702143", "SAMN12702144",
                                                               "SAMN12702145", "SAMN12702146", "SAMN12702147", "SAMN12702148",
                                                               "SAMN12702149", "SAMN12702150", "SAMN12702151", "SAMN12702152",
                                                               "SAMN12702153", "SAMN12702154", "SAMN12702155", "SAMN12702156",
                                                               "SAMN12702157", "SAMN12702158","SAMN12702159"))
df <- df %>% mutate(haslocality = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting1$biosample_acc_sra,
                                         "yes",haslocality)) %>% 
  mutate(hascountry = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting1$biosample_acc_sra,
                             "yes",hascountry)) %>% 
  mutate(hascollectiondate = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting1$biosample_acc_sra,
                                    "yes",hascollectiondate))

recordsthatneedcorrecting2 <- data.frame(biosample_acc_sra = c("SAMN14933025", "SAMN14933024", "SAMN14933023", "SAMN14933028", 
                                                               "SAMN14933027", "SAMN14933026", "SAMN14933016", "SAMN03274639", 
                                                               "SAMN03274760", "SAMN03274761", "SAMN10993452", "SAMN10993456", 
                                                               "SAMN10993454", "SAMN10993453", "SAMN10993450", "SAMN10993451"))
df <- df %>% mutate(hascollectiondate = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting2$biosample_acc_sra,
                                               "yes",hascollectiondate))

recordsthatneedcorrecting3 <- data.frame(biosample_acc_sra = c("SAMN07269197", "SAMN07269195", "SAMN07269200", "SAMN07269198", 
                                                               "SAMN07269199", "SAMN07269196"))
df <- df %>% mutate(haslatlong = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting3$biosample_acc_sra,
                                        "yes",haslatlong))

recordsthatneedcorrecting4 <- data.frame(biosample_acc_sra = c("SAMN01999152", "SAMN01999153", "SAMN01999154", "SAMN01999155", 
                                                               "SAMN01999148", "SAMN01999156", "SAMN01999157", "SAMN01999150", 
                                                               "SAMN01999151", "SAMN01999149"))
df <- df %>% mutate(haslocality = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting4$biosample_acc_sra,
                                         "no",haslocality)) %>% 
  mutate(hascountry = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting4$biosample_acc_sra,
                             "no",hascountry)) %>% 
  mutate(hascollectiondate = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting4$biosample_acc_sra,
                                    "yes",hascollectiondate))

recordsthatneedcorrecting5 <- data.frame(biosample_acc_sra = c("PI525695", "PI563516", "PI585749", "PI586430"))
df <- df %>% mutate(haslatlong = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting5$biosample_acc_sra,
                                        "no",haslatlong)) %>% 
  mutate(haslocality = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting5$biosample_acc_sra,
                              "yes",haslocality)) %>% 
  mutate(hascountry = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting5$biosample_acc_sra,
                             "yes",hascountry)) %>% 
  mutate(hascollectiondate = ifelse(biosample_acc_sra %in% recordsthatneedcorrecting5$biosample_acc_sra,
                                    "no",hascollectiondate))

#should be 0 if all biosample IDs only have one consistent metadata status value for:
#haslatlong,haslocality,hascountry,hascollectiondate
df %>% group_by(biosample_acc_sra) %>% 
  mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(biosample_acc_sra,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj,biosample_acc_sra,metadata_status_all) %>% nrow()

#remake column to denote which samples have time AND space data after we made above corrections -----
#counting collection date as time and lat/long as space
df$hasspatiotemp[df$haslatlong=="yes" & df$hascollectiondate=="yes"] = "yes"
df$hasspatiotemp[df$haslatlong=="yes" & df$hascollectiondate=="no"] = "no"
df$hasspatiotemp[df$haslatlong=="yes" & df$hascollectiondate=="maybe in pub. paper"] = "maybe in pub. paper"
df$hasspatiotemp[df$haslatlong=="no" & df$hascollectiondate=="yes"] = "no"
df$hasspatiotemp[df$haslatlong=="no" & df$hascollectiondate=="no"] = "no"
df$hasspatiotemp[df$haslatlong=="no" & df$hascollectiondate=="maybe in pub. paper"] = "no"
df$hasspatiotemp[df$haslatlong=="maybe in pub. paper" & df$hascollectiondate=="yes"] = "maybe in pub. paper"
df$hasspatiotemp[df$haslatlong=="maybe in pub. paper" & df$hascollectiondate=="no"] = "no"
df$hasspatiotemp[df$haslatlong=="maybe in pub. paper" & df$hascollectiondate=="maybe in pub. paper"] = "maybe in pub. paper"


#write out filtered list for plotting
write.csv(df, "working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-11-3-2021-greplnamematching.csv")




# *******************************************************************************
# *******************************************************************************
# NEW SECTION - MID DATATHON STATUS ----------------------------------------------

rm(list = ls())
gc()


# READ IN DATA
# get list of records that went into datathon

#get path to spreadsheet in GDrive
nonmarineSRA <- googledrive::drive_ls(path = "BioProject_Tables", pattern = "nonMarine_SRA_BioProjects")
marineSRA <- googledrive::drive_ls(path = "BioProject_Tables", pattern = "Marine_SRA_BioProjects") %>% .[2,]

#read spreadsheet into df
df.nonmarine <- googlesheets4::range_read(nonmarineSRA, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(Species_all,project_index,project_acc_bioprj,BioSamples,Runs,Relevant,
                Paper_Available,Authors_Contacted,Author_Contact_Comments,
                Author_Response_Comments) %>% mutate(datagroup = "nonmarine")

df.marine <- googlesheets4::range_read(marineSRA, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(Species_all,project_index,project_acc_bioprj,BioSamples,Runs,Relevant,
                Paper_Available,Authors_Contacted,Author_Contact_Comments,
                Author_Response_Comments) %>% mutate(datagroup = "marine")

#bind cols from all datasheets together
df <- rbind(df.nonmarine,df.marine)
head(df)

#remove double spaces that mess up comment parsing
df <- df %>% mutate(Author_Contact_Comments = gsub("  "," ",Author_Contact_Comments))
df <- df %>% mutate(Author_Response_Comments = gsub("  "," ",Author_Response_Comments))

#unlist columns that were drop-down menus in excel
df$Paper_Available[df$Paper_Available == "NULL"] <- NA
df <- df %>% mutate(Relevant = unlist(Relevant)) %>% mutate(Authors_Contacted = unlist(Authors_Contacted)) %>% 
  mutate(Paper_Available = unlist(Paper_Available)) %>% 
  mutate(Species_all = unlist(Species_all))
str(df)

#filter out domestic etc. species that shouldn't have been in datathon in first place
species_to_drop <- read.delim("NonWildSpecies/nonWildSpecies_final_sources.tsv", header = T)
df <- df %>% filter(!str_detect(Species_all, str_c(species_to_drop$Species, collapse = "|"))) %>% 
  dplyr::select(-Species_all)


# ***************************************************************************************
# GET STATUS OF EACH TYPE OF METADATA

# publication ---
df <- df %>% rename("haspub" = "Paper_Available")

#parse the author contact columns to get rest of metadata statuses
#loop through each category of metadata and pull out status from Author_Contact_Comments ----

for (metadata in c("materialSampleID","locality",
                   "coordinates","country","habitat",
                   "environmentalMedium", "yearCollected", 
                   "permitInformation", "preservative",
                   "derivedGeneticDataX")) {
  
  df <- df %>% 
    separate(., Author_Contact_Comments, into = c("garbage","working"), sep = paste(metadata), remove = F, extra = "drop") %>% 
    dplyr::select(-garbage) %>% 
    mutate(working = gsub("^ ","",working)) %>%
    mutate(working = gsub("[“”]","\"", working)) %>% 
    mutate(working = gsub("^=","",working)) %>% 
    mutate(working = gsub("^ ","",working)) %>% 
    mutate(working = gsub("^[[:punct:]]","",working)) %>% 
    separate(., working, into = c("working.1"), sep = "\"", remove = T, extra = "drop") %>% 
    separate(., working.1, into = c("working"), sep = " \\|", remove = T, extra = "drop") %>% 
    separate(., working, into = c("working.1"), sep = ",", remove = T, extra = "drop") %>%
    separate(., working.1, into = c("working"), sep = " ", remove = T, extra = "drop") %>%
    separate(., working, into = c("working.1"), sep = "\\|", remove = T, extra = "drop") %>%
    separate(., working.1, into = c("final"), sep = "-", remove = T, extra = "drop") %>%
    mutate(final = gsub("True","TRUE",final)) %>%
    mutate(final = gsub("true","TRUE",final)) %>%
    mutate(final = gsub("most","MOST",final)) %>%
    mutate(final = gsub("Most","MOST",final)) %>%
    mutate(final = gsub("MOST;","MOST",final)) %>%
    mutate(final = gsub("MOSTsee","MOST",final)) %>%
    mutate(final = gsub("some","SOME",final)) %>%
    mutate(final = gsub("Some","SOME",final)) %>%
    mutate(final = gsub("false","FALSE",final)) %>%
    mutate(final = gsub("False","FALSE",final)) %>% 
    mutate(final = gsub("FALSEsee","FALSE",final)) %>%
    mutate(final = gsub("FALSE&FALSE","FALSE",final)) %>%
    mutate(final = gsub("MOST&MOST","MOST",final)) %>%
    mutate(final = gsub("SOME&SOME","SOME",final)) %>%
    mutate(final = gsub("TRUE&TRUE","TRUE",final)) %>%
    mutate(final = gsub("\\|permitInformation=","",final)) %>% 
    mutate(final = gsub(";","",final)) %>%
    mutate(final = gsub("\\.","",final)) %>%
    mutate(final = gsub(":","",final)) %>% 
    mutate(final = gsub("'","",final))
  
  names(df)[names(df)=="final"] <- paste("hasMIDdatathon", metadata, sep = "")
  
}


#note - the relevance of these projects changed after QC in datathon, but we want them to be same as when we published PNAS paper
#so manually changing to match relevance for when PNAS paper was published
df$Relevant[df$project_acc_bioprj == "PRJNA184708"] <- "TRUE"
df$Relevant[df$project_acc_bioprj == "PRJNA204958"] <- "TRUE"
df$Relevant[df$project_acc_bioprj == "PRJNA223532"] <- "TRUE"
df$Relevant[df$project_acc_bioprj == "PRJNA227359"] <- "TRUE"
df$Relevant[df$project_acc_bioprj == "PRJNA343301"] <- "FALSE"
df$Relevant[df$project_acc_bioprj == "PRJNA521848"] <- "FALSE"

#and drop marine projects that are also in full datathon 
#(Eric made sure both structured author comments and paper_avail cols matched btwn these "duplicate" records)
df <- df %>% filter(!project_index %in% c("M0012","M0022","M0037","M0050","M0062","M0063"))

#and drop 5 prjs that are missing from 2020 master NCBI list (bc they have e.g. "exome" or "phenotype" in bioprj data type field)
#and we can't easily look up metadata for them in NCBI now
df <- df %>% filter(!project_acc_bioprj %in% c("PRJNA315895", "PRJNA324830", "PRJNA386149", "PRJNA429104", "PRJNA453553", "PRJNA377812"))

#save
write.csv(df, "working_files/alldatathonbioprjs_authorcommentcols_parsed-11-8-2021.csv", row.names = F)



#***************************************************************************************************
#***************************************************************************************************
#***************************************************************************************************

# ! GRAVEYARD ! ------- 

# below is original code that was used for paper 1, but we have since organized this better
# in the git repo just for paper 1 and we have not kept the below section up to date
# this script should only be used for parsing - all downstream scripts to generate
# the stats and numbers for paper 1 are in paper 1 repo:
# https://bitbucket.org/toczydlowski/status_of_insdc_genomic_metadata




# GET NUMBERS FOR MAIN TEXT ---------

#get df
rm(list = ls())
gc()

df <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-11-3-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X)

#add has date and ANY spatio temp category
df$hasANYspatiotemp[df$hasspatiotemp=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="no"] = "no"
df$hasANYspatiotemp[df$hascollectiondate=="maybe in pub. paper"] = "no"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslatlong=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslocality=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$hascountry=="yes"] = "yes"
df$hasANYspatiotemp[is.na(df$hasANYspatiotemp)==TRUE] = "no"

#how many of these bioprojects have mixed metadata statuses (aka some samples with lat/long and some samples without?)
df %>% mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  group_by(project_acc_bioprj) %>%
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(project_acc_bioprj,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj) %>% nrow()
df %>% distinct(project_acc_bioprj) %>% nrow()
#678/5043 = 13% of all bioprjs have at least one type of mixed metdata (aka some biosamps with date and some without)
df %>% filter(is_domestic == "no") %>% mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  group_by(project_acc_bioprj) %>%
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(project_acc_bioprj,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj) %>% nrow()
df %>% filter(is_domestic == "no") %>% distinct(project_acc_bioprj) %>% nrow()
#567/3903 = 15% of wild bioprjs have at least one type of mixed metdata (aka some biosamps with date and some without)
df %>% filter(is_domestic == "yes") %>% mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  group_by(project_acc_bioprj) %>%
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(project_acc_bioprj,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj) %>% nrow()
df %>% filter(is_domestic == "yes") %>% distinct(project_acc_bioprj) %>% nrow()
#131/1396 = 9% of domesticated bioprjs have at least one type of mixed metdata (aka some biosamps with date and some without)

#what percent of indivs within mixed bioprojects are actually missing data?
#this focuses on haslatlong, haslocality, hascountry, and hascollectiondate
#aka - can we calc rates of metadata at bioprj level?
df %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  dplyr::select(project_acc_bioprj,haslatlong,haslocality,hascountry,hascollectiondate) %>% 
  mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  group_by(project_acc_bioprj) %>%
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  ungroup() %>% as.data.frame() %>% 
  filter(checkmetadata > 1) %>% group_by(project_acc_bioprj,metadata_status_all) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = "metadata_status_all", values_from = "n") %>% replace(is.na(.), 0) %>% 
  mutate(total = sum(c_across(no_no_no_no:yes_no_no_no))) %>%
  pivot_longer(., names_to = "metadata_status_all", values_to = "n", cols = 2:13) %>%
  filter(n > 0) %>% dplyr::select(project_acc_bioprj,metadata_status_all,n,total) %>% 
  mutate(perc = (n/total)*100) %>% 
  mutate(number_yeses = str_count(metadata_status_all, "yes")) %>%
  group_by(project_acc_bioprj) %>% mutate(max_yeses = max(number_yeses)) %>%
  mutate(is_most_complete = ifelse(number_yeses == max_yeses, "yes", "no")) %>% ungroup() %>% 
  filter(is_most_complete == "no") %>%
  ggplot() + geom_histogram(aes(x = perc), binwidth = 1, fill = "gray70", colour = "black") + theme_bw()

#seems reasonable to count a bioprj as yes having data if at least 50% of samples in it have data



#total
options(scipen = 999)
sum(df$total_number_base_pairs_sra, na.rm = T)
#1628422238648653 bps
#1 byte ~ 2.75 bps
#1628422238648653/2.75 = 592153541326783 bytes = 592 terabytes
df %>% distinct(run_acc_sra) %>% nrow()
#380,410 sequences
df %>% distinct(biosample_acc_sra) %>% nrow()
#327,577 biosamples
df %>% distinct(project_acc_bioprj) %>% nrow()
#5,043 bioprjs
df %>% distinct(species_ncbitaxonomy.via.taxize) %>% nrow()
#16,776 species (looked up organism_biosamp in NCBI taxonomy db)
df %>% distinct(biosample_acc_sra,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n())
#43809/(265019+18749+43809) = 0.13 - calced at biosample level

df %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp) %>% 
  group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#574/5043 = 11% of all bioprjs have spatiotemp

#estimated sequence cost
#number of biosamps with maybe spatiotemp + number of biosamps without spatiotemp - number of samples we resurrected in datathon
265256+18197-24652
#258801 individuals * $50 per sample = $12,940,050

#"wild"
df %>% filter(is_domestic == "no") %>% distinct(biosample_acc_sra) %>% nrow()
#233,639 biosamples
df %>% filter(is_domestic == "no") %>% distinct(project_acc_bioprj) %>% nrow()
#3,903 bioprjs potentially wild (4,295 bioprjs)
df %>% distinct(project_acc_bioprj) %>% nrow()
#5,043 bioprjs from domestic + "wild"

#stats for "wild" samples at BIOPROJECT level ---------
#if >= of samples in bioprj have metadata present than coding bioprj as metadata present and vice versa
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,haslatlong) %>% 
  group_by(project_acc_bioprj,haslatlong) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "haslatlong", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(haslatlong_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>%
  group_by(haslatlong_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#17% of "wild" bioprjs have latlong
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,haslocality) %>% 
  group_by(project_acc_bioprj,haslocality) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "haslocality", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(haslocality_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(haslocality_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#51% of "wild" bioprjs have locality   
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hascountry) %>% 
  group_by(project_acc_bioprj,hascountry) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hascountry", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hascountry_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(hascountry_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#66% of "wild" bioprjs have country 
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hascollectiondate) %>% 
  group_by(project_acc_bioprj,hascollectiondate) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hascollectiondate", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hascollectiondate_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(hascollectiondate_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#40% of "wild" bioprjs have collection date
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp) %>% 
  group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>%
  group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#14% of "wild" bioprjs have lat/long and collection date
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasANYspatiotemp) %>% 
  group_by(project_acc_bioprj,hasANYspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasANYspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasANYspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>%
  group_by(hasANYspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#38% of "wild" bioprjs have collection date and ANY spatial data


#how many individuals are in datasets with and without spatiotemp - wild?
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp) %>% 
  group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  mutate(total_samples = yes + no) %>% group_by(hasspatiotemp_bioprjlevel) %>% 
  summarise(meansampspergroup = mean(total_samples), se = plotrix::std.error(total_samples))
#80 +/- 8 samples in bioprjs with spatiotemp and 57 +/- 3 samples in those without spatiotemp

#how many individuals are in datasets with and without spatiotemp - domesticated?
df %>% filter(is_domestic == "yes") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp) %>% 
  group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  mutate(total_samples = yes + no) %>% group_by(hasspatiotemp_bioprjlevel) %>% 
  summarise(meansampspergroup = mean(total_samples), se = plotrix::std.error(total_samples))
#43 +/- 15 samples in bioprjs with spatiotemp and 68 +/- 6 samples in those without spatiotemp


#stats for "wild" samples at BIOSAMPLE level
df %>% filter(is_domestic == "no") %>% distinct(biosample_acc_sra,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n())
#58717/(167031+7891+58717) =  0.25 has latlong    (old 60272/(187785+8686+60272) = 0.23 has latlong)
df %>% filter(is_domestic == "no") %>% distinct(biosample_acc_sra,haslocality) %>% group_by(haslocality) %>% summarise(n=n())
#113652/(105765+14222+113652) = 0.49 has locality   (old 122623/(117918+16272+122623) = 0.48 has locality)
df %>% filter(is_domestic == "no") %>% distinct(biosample_acc_sra,hascountry) %>% group_by(hascountry) %>% summarise(n=n())
#145857/(73560+14222+145857) = 0.62 has country    (old 159485/(81060+16272+159485) = 0.62 has country)
df %>% filter(is_domestic == "no") %>% distinct(biosample_acc_sra,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n())
#96048/(127228+10363+96048) = 0.41 has collection date    (old 99206/(145520+12108+99206) = 0.39 has collection date)
df %>% filter(is_domestic == "no") %>% distinct(biosample_acc_sra,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n())
#42082/(176917+14640+42082) = 0.18 have lat/long and collection date

#mini datathon
#how many of presumably wild datasets are actually wild?
#get mini datathon - 200 random bioprj level
bps200nodomestic <- googledrive::drive_get(path = "GEOME_Datathon/Datathon_Working_Directory/BioProject_Tables/MiniDatathon_SRA_BioProjects")
df.datathon <- googlesheets4::range_read(bps200nodomestic, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(project_index,project_acc_bioprj,Relevant) %>% mutate(Relevant = unlist(Relevant))
#get metadata status
metadatastatus <- df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp) %>% 
  group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no"))
#merge
df.datathon <- merge(df.datathon, metadatastatus, by = "project_acc_bioprj", all.x = T)
#check we will have 200 bioprjs
df.datathon %>% distinct(project_acc_bioprj) %>% nrow()
#how many bioprjs are actually wild?
df.datathon %>% distinct(project_acc_bioprj,Relevant) %>% 
  group_by(Relevant) %>% summarise(n=n()) %>% pivot_wider(., names_from = 1, values_from = 2) %>% 
  mutate(trulywild = `TRUE`/(`TRUE`+`FALSE`))
#70% of bioprjs estimated to be from wild populations

#how do rates of spatiotemp metadata compare between wild and fake wild
df.datathon %>% filter(Relevant != "NA") %>% group_by(Relevant, hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 2, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#spatiotemp present for 12% of relevant bioprjs and 15% of not truly wild bioprjs - from 200 rando bioprjs

#get just presumably wild records
df.mayberelevant <- df %>%  
  dplyr::select(project_acc_bioprj,organism_biosamp,biosample_acc_sra,run_acc_sra,contains("has"),is_domestic) %>% 
  filter(is_domestic == "no")

#how many of these presumably wild bioprojects have mixed metadata statuses (aka some samples with lat/long and some samples without?)
df.mayberelevant %>% mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  group_by(project_acc_bioprj) %>%
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(project_acc_bioprj,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj) %>% nrow()
df.mayberelevant %>% distinct(project_acc_bioprj) %>% nrow()
#558/3903 = 14% of bioprjs







# *******************************************************************************
# *******************************************************************************
# NEW SECTION - RELEVANCE RATE from MINI DATATHON  ------------------------------

#determining relevance rate for random grab of 200 bioprojects
#with all of our initial/original filters applied including domestic

rm(list = ls())
gc()

#get spreadsheet from mini datathon of 200 randos
bps200nodomestic <- googledrive::drive_ls(path = "BioProject_Tables", pattern = "MiniDatathon_SRA_BioProjects")
#read spreadsheet into df
df.datathon <- googlesheets4::range_read(bps200nodomestic, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(project_index,project_acc_bioprj,Relevant) %>% mutate(Relevant = unlist(Relevant))

#read in dataframe with metadata status for each sample
df.all <- read.csv("working_files/pre-datathon_full_list_from_NCBI.csv", header = T) %>% 
  dplyr::select(project_acc_bioprj,organism_biosamp,biosample_acc_sra,run_acc_sra,contains("has"))

#get metadata status
df <- merge(df.datathon, df.all, by = "project_acc_bioprj", all.x = T)

#add additional metadata status col
df$hasANYspatiotemp[df$hasspatiotemp=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="no"] = "no"
df$hasANYspatiotemp[df$hascollectiondate=="maybe in pub. paper"] = "no"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslatlong=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslocality=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$hascountry=="yes"] = "yes"
df$hasANYspatiotemp[is.na(df$hasANYspatiotemp)==TRUE] = "no"

#check if any bioprjs are coded as relevant and nonrelevant (should be 0 if not)
df %>% group_by(project_acc_bioprj,Relevant) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% filter(n > 1) %>% nrow()


#calc rates ----
#what percent of studies are truly wild indivs?
df.datathon %>% group_by(Relevant) %>% summarise(n=n()) %>% pivot_wider(., names_from = 1, values_from = "n") %>% 
  mutate(percrelevant = (`TRUE`/(`FALSE`+`TRUE`))*100)
#70% are relevant

#how many of these bioprojects have mixed metadata statuses (aka some samples with lat/long and some samples without?)
df %>% mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  group_by(project_acc_bioprj) %>%
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(project_acc_bioprj,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj) %>% nrow()
df %>% distinct(project_acc_bioprj) %>% nrow()
#31/200 = 16% of all bioprjs have at least one type of mixed metdata (aka some biosamps with date and some without)


#BIOPROJECT LEVEL
#do rates of missing spatiotemp vary btw relevant and non-relevant?
df.bioprj <- df %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp,Relevant) %>% 
  group_by(project_acc_bioprj,hasspatiotemp,Relevant) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,Relevant) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no"))
df.bioprj %>% filter(Relevant == "FALSE") %>% 
  group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#14% of not relevant have spatiotemp
df.bioprj %>% filter(Relevant == "TRUE") %>% 
  group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#13% of relevant have spatiotemp
#no

#what about if we look at any type of spatiotemp data?
df.bioprj <- df %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasANYspatiotemp,Relevant) %>% 
  group_by(project_acc_bioprj,hasANYspatiotemp,Relevant) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,Relevant) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasANYspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasANYspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no"))
df.bioprj %>% filter(Relevant == "FALSE") %>% 
  group_by(hasANYspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#25% of not relevant have any spatiotemp
df.bioprj %>% filter(Relevant == "TRUE") %>% 
  group_by(hasANYspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#35% of relevant have any spatiotemp
#a bit

#consensus was to bootstrap rate of spatiotemp for truly wild
df.bioprj <- df %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp,Relevant) %>% 
  group_by(project_acc_bioprj,hasspatiotemp,Relevant) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,Relevant) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  ungroup() %>% 
  filter(Relevant == "TRUE") %>% dplyr::select(project_acc_bioprj,hasspatiotemp_bioprjlevel)
all_bootsamp <- data.frame(total = NA, percpresent = NA, rep = NA)
for(i in 1:100){
  df.samp <- df.bioprj %>% sample_n(., size = 70, replace = TRUE) %>%
    mutate(fake_project_id = 1:n()) %>%
    group_by(fake_project_id,hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
    group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) 
  bootsamp <- df.samp %>% pivot_wider(., names_from = 1, values_from = "n") %>% 
    mutate(total = rowSums(.)) %>% 
    mutate(percpresent = (yes/total)*100) %>% mutate(rep = i) %>% 
    dplyr::select(total,percpresent,rep)
  all_bootsamp <- rbind(all_bootsamp,bootsamp)
}
all_bootsamp <- all_bootsamp %>% filter(is.na(rep)==F)
quantile(all_bootsamp$percpresent, c(.025, .975))
#6% - 20%


#bootstrap spatiotemp for mis-IDed as wild too
df.bioprj <- df %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp,Relevant) %>% 
  group_by(project_acc_bioprj,hasspatiotemp,Relevant) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,Relevant) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  ungroup() %>% 
  filter(Relevant == "FALSE") %>% dplyr::select(project_acc_bioprj,hasspatiotemp_bioprjlevel)
all_bootsamp <- data.frame(total = NA, percpresent = NA, rep = NA)
for(i in 1:100){
  df.samp <- df.bioprj %>% sample_n(., size = 30, replace = TRUE) %>%
    mutate(fake_project_id = 1:n()) %>%
    group_by(fake_project_id,hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
    group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) 
  bootsamp <- df.samp %>% pivot_wider(., names_from = 1, values_from = "n") %>% 
    mutate(total = rowSums(.)) %>% 
    mutate(percpresent = (yes/total)*100) %>% mutate(rep = i) %>% 
    dplyr::select(total,percpresent,rep)
  all_bootsamp <- rbind(all_bootsamp,bootsamp)
}
all_bootsamp <- all_bootsamp %>% filter(is.na(rep)==F)
quantile(all_bootsamp$percpresent, c(.025, .975))
#3% - 23%


#BIOSAMPLE LEVEL
#do rates of missing spatiotemp vary btw relevant and non-relevant?
df %>% filter(Relevant == "FALSE") %>% group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(hasspatiotemp) %>% summarise(n=n()) %>% pivot_wider(., names_from = 1, values_from = "n") %>% 
  mutate(percpresent = (yes/(`maybe in pub. paper`+no+yes))*100)
#15% of not relevant have spatiotemp
df %>% filter(Relevant == "TRUE") %>% group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(hasspatiotemp) %>% summarise(n=n()) %>% pivot_wider(., names_from = 1, values_from = "n") %>% 
  mutate(percpresent = (yes/(`maybe in pub. paper`+no+yes))*100)
#12% of relevant have spatiotemp
#no

#what about if we look at any type of spatiotemp data?
df %>% filter(Relevant == "FALSE") %>% group_by(project_acc_bioprj,hasANYspatiotemp) %>% summarise(n=n()) %>% 
  group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% pivot_wider(., names_from = 1, values_from = "n") %>% 
  mutate(percpresent = (yes/(no+yes))*100)
#27% of not relevant have spatiotemp
df %>% filter(Relevant == "TRUE") %>% group_by(project_acc_bioprj,hasANYspatiotemp) %>% summarise(n=n()) %>% 
  group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% pivot_wider(., names_from = 1, values_from = "n") %>% 
  mutate(percpresent = (yes/(no+yes))*100)
#35% of relevant have spatiotemp
#a bit




# *******************************************************************************
# *******************************************************************************
# NEW MINI SECTION - METADATA LEVELS FOR "wild" <5 indivs BioPrjs ----------------------------------

#reviewer asked for this

rm(list = ls())
gc()

df <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-6-10-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X) %>% 
  mutate_all(funs(str_replace(., "maybe in pub. paper", "no")))

df$hasANYspatiotemp[df$hasspatiotemp=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="no"] = "no"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslatlong=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslocality=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$hascountry=="yes"] = "yes"
df$hasANYspatiotemp[is.na(df$hasANYspatiotemp)==TRUE] = "no"
#sanity check
df %>% group_by(hasANYspatiotemp,haslatlong,haslocality,hascountry,hascollectiondate) %>% summarise(n=n())

#wild with < 5 indivs
# set minimum number of indivs per bioprj/species combo
n.indiv = 5
#group at level of biorpj/species and get at least X indivs per combo
filterlist.Xindivs <- df %>% filter(is_domestic == "no") %>% 
  group_by(biosample_acc_sra,project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  filter(n < n.indiv) %>% mutate(link = paste(project_acc_bioprj, organism_biosamp, sep = "_")) %>% 
  mutate(link = gsub(pattern = " ", replacement = "-", link))
filterlist.Xindivs %>% distinct(project_acc_bioprj) %>% nrow()
#2,674 bioprjs
#get only above list from full records
df <- df %>% filter(link %in% filterlist.Xindivs$link)

df %>% distinct(project_acc_bioprj,biosample_acc_sra,haslatlong) %>% 
  group_by(project_acc_bioprj,haslatlong) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "haslatlong", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(haslatlong_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>%
  group_by(haslatlong_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#15% of "wild" bioprjs have latlong
df %>% distinct(project_acc_bioprj,biosample_acc_sra,haslocality) %>% 
  group_by(project_acc_bioprj,haslocality) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "haslocality", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(haslocality_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(haslocality_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#51% of "wild" bioprjs have locality   
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hascountry) %>% 
  group_by(project_acc_bioprj,hascountry) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hascountry", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hascountry_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(hascountry_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#67% of "wild" bioprjs have country 
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hascollectiondate) %>% 
  group_by(project_acc_bioprj,hascollectiondate) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hascollectiondate", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hascollectiondate_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(hascollectiondate_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#39% of "wild" bioprjs have collection date
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp) %>% 
  group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>%
  group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#13% of "wild" bioprjs have lat/long and collection date



# just datasets with 5 indivs or more -----

rm(list = ls())
gc()

df <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-6-10-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X) %>% 
  mutate_all(funs(str_replace(., "maybe in pub. paper", "no")))

df$hasANYspatiotemp[df$hasspatiotemp=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="no"] = "no"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslatlong=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$haslocality=="yes"] = "yes"
df$hasANYspatiotemp[df$hascollectiondate=="yes" & df$hascountry=="yes"] = "yes"
df$hasANYspatiotemp[is.na(df$hasANYspatiotemp)==TRUE] = "no"
#sanity check
df %>% group_by(hasANYspatiotemp,haslatlong,haslocality,hascountry,hascollectiondate) %>% summarise(n=n())

#wild with >+ 5 indivs
# set minimum number of indivs per bioprj/species combo
n.indiv = 5
#group at level of biorpj/species and get at least X indivs per combo
filterlist.Xindivs <- df %>% filter(is_domestic == "no") %>% 
  group_by(biosample_acc_sra,project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  filter(n >= n.indiv) %>% mutate(link = paste(project_acc_bioprj, organism_biosamp, sep = "_")) %>% 
  mutate(link = gsub(pattern = " ", replacement = "-", link))
filterlist.Xindivs %>% distinct(project_acc_bioprj) %>% nrow()
#1,727 bioprjs
#get only above list from full records
df <- df %>% filter(link %in% filterlist.Xindivs$link)

df %>% distinct(project_acc_bioprj,biosample_acc_sra,haslatlong) %>% 
  group_by(project_acc_bioprj,haslatlong) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "haslatlong", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(haslatlong_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>%
  group_by(haslatlong_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#20% of "wild" bioprjs have latlong
df %>% distinct(project_acc_bioprj,biosample_acc_sra,haslocality) %>% 
  group_by(project_acc_bioprj,haslocality) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "haslocality", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(haslocality_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(haslocality_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#50% of "wild" bioprjs have locality   
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hascountry) %>% 
  group_by(project_acc_bioprj,hascountry) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hascountry", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hascountry_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(hascountry_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#64% of "wild" bioprjs have country 
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hascollectiondate) %>% 
  group_by(project_acc_bioprj,hascollectiondate) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hascollectiondate", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hascollectiondate_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>% 
  group_by(hascollectiondate_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#41% of "wild" bioprjs have collection date
df %>% filter(is_domestic == "no") %>% mutate_all(funs(str_replace(., "maybe in pub. paper", "no"))) %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hasspatiotemp) %>% 
  group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% mutate(n_bioprj_records = n()) %>%
  pivot_wider(., names_from = "hasspatiotemp", values_from = "n") %>% 
  replace(is.na(.), 0) %>% mutate(prop_present = yes/(no+yes)) %>% 
  mutate(hasspatiotemp_bioprjlevel = ifelse(prop_present >= 0.5, "yes","no")) %>%
  group_by(hasspatiotemp_bioprjlevel) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(total = yes + no, perc_present = yes/total)
#15% of "wild" bioprjs have lat/long and collection date









# PLOTTING ----------------------------------------------------------

# plot - taxonomic donut plot --------------

#class
#Mammals - Mammalia
#Birds - Aves
#Fish - Actinopteri, Chondrichthyes, Coelacanthimorpha
#Amphibians and reptiles - Amphibia, Lepidosauria

#get df
rm(list = ls())
gc()

df <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-6-10-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X)

#get unique biosamples
df.taxdonut <- df %>% dplyr::select(biosample_acc_sra,division_taxonomy,is_domestic,domestic_type,organism_biosamp,contains("taxize")) %>% distinct()
#assign tax categories
df.taxdonut <- df.taxdonut %>% mutate(tax_category = ifelse(kingdom_ncbitaxonomy.via.taxize == "Fungi", "Fungi", ".")) %>% 
  mutate(tax_category = ifelse(kingdom_ncbitaxonomy.via.taxize == "Viridiplantae", "Other Plants", tax_category)) %>%
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Cycadopsida", "Gymnosperms", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Gnetopsida", "Gymnosperms", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Pinopsida", "Gymnosperms", tax_category)) %>%
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Magnoliopsida", "Angiosperms", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Aves", "Birds", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize == "Mammalia", "Mammals", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize %in% c("Actinopteri","Chondrichthyes","Coelacanthimorpha"), "Fish", tax_category)) %>% 
  mutate(tax_category = ifelse(class_ncbitaxonomy.via.taxize %in% c("Lepidosauria","Amphibia"), "Amphibians and Reptiles", tax_category)) %>%
  mutate(tax_category = ifelse(phylum_ncbitaxonomy.via.taxize == "Arthropoda", "Arthropods", tax_category)) %>% 
  mutate(tax_category = ifelse(phylum_ncbitaxonomy.via.taxize == "Mollusca", "Mollusks", tax_category)) %>%
  mutate(tax_category = ifelse(division_taxonomy == "Mammals" & is.na(tax_category)==T, "Mammals", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Vertebrates" & tax_category==".", "Other Vertebrates", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Invertebrates" & tax_category==".", "Invertebrates", tax_category)) %>% mutate(tax_category = ifelse(kingdom_ncbitaxonomy.via.taxize == "Fungi" & is.na(tax_category)==T, "Fungi", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Vertebrates" & is.na(tax_category)==T, "Other Vertebrates", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Invertebrates" & is.na(tax_category)==T, "Invertebrates", tax_category)) %>% 
  mutate(tax_category = ifelse(division_taxonomy == "Plants and Fungi" & is.na(tax_category)==T, "Other", tax_category)) %>% 
  mutate(tax_category = ifelse(tax_category == "Invertebrates", "Other Invertebrates", tax_category))

df.taxdonut <- df.taxdonut %>% mutate(tax_category = ifelse(is.na(domestic_type)==F,domestic_type,tax_category))
#make broader tax categories
key <- data.frame(tax_category = c("Other Plants", "Angiosperms", "Other", 
                                   "Arthropods", "Gymnosperms", "Fish", "Mammals", "Amphibians and Reptiles", 
                                   "Birds", "Other Invertebrates", "Fungi", "Mollusks", "Other Vertebrates", 
                                   "Domesticated Plant", "Domesticated Animal", "Domesticated Aquaculture", 
                                   "Domesticated Fungus"),
                  broad_tax_category = c("Plants", "Plants", "Other", 
                    "Invertebrates", "Plants", "Vertebrates", "Vertebrates", "Vertebrates", 
                    "Vertebrates", "Invertebrates", "Fungi", "Invertebrates", "Vertebrates", 
                    "Domesticated", "Domesticated", "Domesticated","Domesticated"))
                    
#tack broader categories on
df.taxdonut <- merge(df.taxdonut,key,by="tax_category",all.x=T)

df.taxdonut %>% group_by(broad_tax_category,tax_category) %>% summarise(n=n())

#build nested piechart
PieDonut(data = df.taxdonut, aes(broad_tax_category,tax_category),
         labelposition=1, r0=0.3, showPieName = FALSE, color = "white",
         pieLabelSize = 4, donutLabelSize = 3)

ggsave("figures/predatathon_taxonomicbreakdown-perbiosample-nested-R1.pdf", width = 180, height = 180, dpi = 600, units = c("mm"))
#have to save manually using Export - otherwise inner ring is lost :(
#numbers that go in pie chart
df.taxdonut %>% group_by(broad_tax_category) %>% summarise(n=n()) %>% mutate(total = sum(n)) %>% 
  mutate(prop = round((n/total)*100,1)) %>% mutate(totalprop = sum(prop)) %>% dplyr::select(broad_tax_category,prop,everything())
df.taxdonut %>% group_by(broad_tax_category,tax_category) %>% summarise(n=n()) %>% mutate(total = sum(n)) %>% 
  mutate(prop = round((n/total)*100,1)) %>% mutate(totalprop = sum(prop)) %>% as.data.frame() %>% ungroup() %>% 
  dplyr::select(tax_category,prop,n,total,totalprop)


# pre datathon metadata status ---

#get df
rm(list = ls())
gc()

df <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-6-10-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X)

# pare down from 1 row = sequence data to 1 row = biosample
dim(df)
df.biosamplevel <- df %>% dplyr::select(biosample_acc_sra,contains("has"),contains("domestic")) %>% distinct()
dim(df.biosamplevel)

#check if we only have unique biosamp IDS now (if not, means metadata status is diff for same biosamp ID)
#should return 0 if no biosamp IDs duplicates present
#nrow(df.biosamplevel) - df.biosamplevel %>% dplyr::select(biosample_acc_sra) %>% distinct() %>% nrow()
#find the duplicates so we can investigate
#dup_biosamps_ids <- df.biosamplevel[duplicated(df.biosamplevel$biosample_acc_sra),]
#df.biosamplevel %>% filter(biosample_acc_sra %in% dup_biosamps_ids$biosample_acc_sra) %>% View()
#looks like most are non-unique biosamps being assigned, not going to mess with this further, leave them in

#convert wide to long for plotting
head(df.biosamplevel)
df.predatathon.long <- df.biosamplevel %>% 
  pivot_longer(haslatlong:hasenviromedium, names_to = "metadata", values_to = "status") %>% as.data.frame()

#get rid of material sample ID
df.predatathon.long <- df.predatathon.long %>% filter(metadata != "hassampID")

#reorder metadata categories
df.predatathon.long <- df.predatathon.long %>%
  mutate(metadata_ordered = factor(metadata, levels=c("haspreservative","haspermit","hasenviromedium","hasenvirobroad","hasderivedgeneticdata","haspub","hascollectiondate","hascountry","haslocality","haslatlong"))) %>% 
  mutate(is_domestic_ordered = factor(is_domestic, levels = c("no","yes")))

#rename some factors for graphing
df.predatathon.long <- df.predatathon.long %>% 
  mutate(status = factor(status, labels=c("Maybe outside of INSDC","Missing","Present"))) %>% 
  mutate(metadata_ordered = factor(metadata_ordered, labels = c("Preservative","Permit ID","Enviro. medium","Habitat","Derived genetic data","Publication DOI","Collection date","Country","Place name","Lat. + long."))) %>% 
  mutate(is_domestic_ordered = factor(is_domestic_ordered, labels = c("Wild","Domestic")))



#plot - metadata pre datathon, domestic vs natural ---------------

#filter out metadata types we decided not to plot
df.g <- df.predatathon.long %>% filter(!metadata_ordered %in% c("Preservative","Derived genetic data","Enviro. medium","Permit ID"))

#get n relevant biosamples missing lat/long before datathon
#get list of all BioSamp IDs in relevant datathon projects
#get middatathon status
relevantbiprjs <- read.csv("working_files/alldatathonbioprjs_authorcommentcols_parsed-11-8-2021.csv", header = T) %>% filter(Relevant == "TRUE")
relevantbiosamps <- read.delim("working_files/relevant_BioSamples_20210222.tsv", header = T, sep = "\t") %>% 
  filter(biosample_acc_sra != "") %>% 
  filter(project_acc_bioprj %in% relevantbiprjs$project_acc_bioprj) %>% 
  dplyr::select(biosample_acc_sra) %>% mutate(indatathon = "yes") %>% filter(biosample_acc_sra != "") %>% 
  distinct()
#get metadata status for each sample pre-datathon (all samples from NCBI)
predatathonbiosamps <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-6-10-2021-greplnamematching.csv", header = T) %>% 
  dplyr::select(biosample_acc_sra,haslatlong) %>% distinct()
#merge and calc proportion of biosamps that went into datathon
merge(relevantbiosamps,predatathonbiosamps,by="biosample_acc_sra",all = T) %>% 
  group_by(indatathon,haslatlong) %>% summarise(n=n())
#(57968+519)/(57968+83+519+195783+10835+62908) = 0.178 (old values - (61462+2329)/(61462+83+2329+198581+10999+63197) = 0.189)

datathondatasets <- data.frame(xmin = 5.55, xmax = 6.45, ymin = 0.75, ymax = 0.75+0.178, is_domestic_ordered = "Wild",
                               xmin = NA, xmax = NA, ymin = NA, ymax = NA, is_domestic_ordered = "Domestic") %>% 
  mutate(is_domestic_ordered = factor(is_domestic_ordered, levels = c("Wild","Domestic")))

ggplot() + 
  geom_bar(data = df.g, aes(x=metadata_ordered, fill=status, y=(..count..)/sum(..count..)), 
           stat = "count", position = "fill", colour = "black", size = 0.2) +
  geom_rect_pattern(data = datathondatasets, aes(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), inherit.aes = F,
                    colour = "black", pattern_density = 0.06, pattern_fill = "black", pattern_colour = NA, fill = NA,
                    pattern_spacing = 0.01, pattern_angle = 45, size = 0.2) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("#ccece6","gray40","#2A9134")) +
  labs(x = "Type of metadata", y = "Percent of BioSamples", fill = "Status:") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0),
        strip.background =element_blank(),
        strip.text = element_text(face="bold", size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.key.size = unit(1,"line"),
        legend.position = "bottom") +
  facet_wrap(~is_domestic_ordered) +
  coord_flip()

ggsave("figures/predatathon_allmetadata-domesticVwild-2222021-R1.pdf", width = 105, height = 120, dpi = 600, units = c("mm"))



#plot - samples through time, colored by spatiotemp metadata status ------------
df %>% dplyr::select(publicationdate_biosamp,hasspatiotemp,biosample_acc_sra) %>% distinct() %>%  
  mutate(pubyear_biosamp = as.Date(publicationdate_biosamp, format = "%Y")) %>% 
  mutate(hasspatiotemp = factor(hasspatiotemp, labels=c("Maybe outside of INSDC","Definitively missing","Present"))) %>% 
  ggplot() + 
  geom_bar(aes(x=pubyear_biosamp, fill=hasspatiotemp), stat = "count", position = "stack", colour = "black", size = 0.2) +
  scale_fill_manual(values = c("#ccece6","gray40","forestgreen")) +
  scale_x_date(date_minor_breaks = "1 year") +
  labs(x = "Year", y = "Number of BioSamples added", fill = "Spatiotemporal metadata:") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.key.size = unit(1,"line"),
        legend.position = c(0.19, 0.18)
        #legend.position="bottom"
        )

ggsave("figures/predatathon_spatiotemp_by_year-2172021-R1.pdf", width = 120, height = 120, dpi = 600, units = c("mm"))

# for talk ----
df %>% filter(is_domestic == "no") %>% dplyr::select(publicationdate_biosamp,hasspatiotemp,biosample_acc_sra) %>% distinct() %>%  
  mutate(pubyear_biosamp = as.Date(publicationdate_biosamp, format = "%Y")) %>% 
  mutate(hasspatiotemp = factor(hasspatiotemp, labels=c("Maybe outside of INSDC","Missing","Present"))) %>% 
  ggplot() + 
  geom_bar(aes(x=pubyear_biosamp), stat = "count", colour = "black", size = 0.25, fill = "gray70") +
  scale_x_date(date_minor_breaks = "1 year") +
  labs(x = "Year", y = "Number of BioSamples added") +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12)
        )

ggsave("figures/predatathon_by_year-3-23-2021.pdf", width = 90, height = 130, dpi = 600, units = c("mm"))








# ***************************************************************************************
# PLOT -----------------------------------------------------

#*****************
#get middatathon status
df <- read.csv("working_files/alldatathonbioprjs_authorcommentcols_parsed-11-8-2021.csv", header = T) %>% 
  mutate(haspub = as.character(haspub))

#convert wide to long, keeping just relevant studies
df.middatathon.long <- df %>% dplyr::select(-Runs,-contains("Comments"),-contains("hasPOST"),
                                            -datagroup,-Authors_Contacted,-hasMIDdatathonmaterialSampleID) %>% 
  filter(Relevant == "TRUE") %>% 
  pivot_longer(., names_to = "metadata", values_to = "status", haspub:hasMIDdatathonlocality)

df.middatathon.long %>% group_by(status) %>% summarise(n=n())

#reorder some factors
df.middatathon.long <- df.middatathon.long %>% 
  mutate(status_ordered = factor(status, levels=c("FALSE","SOME","MOST","TRUE"))) %>% 
  mutate(metadata_ordered = factor(metadata, levels = c("hasMIDdatathonpreservative","hasMIDdatathonpermitInformation","hasMIDdatathonenvironmentalMedium","hasMIDdatathonhabitat","hasMIDdatathonderivedGeneticDataX","haspub","hasMIDdatathonyearCollected","hasMIDdatathoncountry","hasMIDdatathonlocality","hasMIDdatathoncoordinates")))

#rename some factors for graphing
df.middatathon.long <- df.middatathon.long %>%
  mutate(status_ordered = factor(status_ordered, labels=c("Missing","Some present","Most present","Present"))) %>% 
  mutate(metadata_ordered = factor(metadata_ordered, labels=c("Preservative","Permit ID","Enviro. medium","Habitat","Derived genetic data","Publication DOI","Collection date","Country","Place name","Lat. + long.")))

df.middatathon.long <- df.middatathon.long %>% 
  filter(!metadata_ordered %in% c("Preservative","Derived genetic data","Enviro. medium","Permit ID"))

#*****************
#need to figure out how many bioprjs had complete metadata in each metadata category before we looked at papers

#get list of all BioSamp IDs in relevant datathon projects
relevantbiosamps <- read.delim("working_files/relevant_BioSamples_20210222.tsv", header = T, sep = "\t") %>% 
  filter(biosample_acc_sra != "")
dim(relevantbiosamps)
#filter out domestic etc. species that shouldn't have been in datathon in first place
relevantbiosamps <- relevantbiosamps %>% filter(project_acc_bioprj %in% df.middatathon.long$project_acc_bioprj)
dim(relevantbiosamps)

#get metadata status for each sample pre-datathon (all samples from NCBI)
predatathonbiosamps <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-6-10-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X)
dim(predatathonbiosamps)

#pull relevant biosamps that were in datathon out of the predatathon metadata status list
relevant.predatathon <- merge(predatathonbiosamps,
                              relevantbiosamps %>% dplyr::select(biosample_acc_sra) %>% mutate(indatathon = "yes"),
                              by = "biosample_acc_sra",
                              all.y = T)
dim(relevant.predatathon)

#convert our list of just relevant biosamps with predatathon metadata status to bioprj level
relevant.predatathon.bioprj <- relevant.predatathon %>% 
  group_by(project_acc_bioprj,haslatlong,haslocality,hascountry,hascollectiondate,
           haspub,hasenvirobroad) %>% summarise(n=n()) %>% 
  dplyr::select(-n) %>% ungroup() %>% as.data.frame() %>% 
  pivot_longer(., names_to = "metadata", values_to = "status", haslatlong:hasenvirobroad) %>% 
  mutate(status = gsub("maybe in pub. paper","no",status)) %>% filter(is.na(status)==F) %>% 
  distinct() %>% group_by(project_acc_bioprj,metadata) %>% add_tally(name = "check") %>% ungroup()

#figure out how many bioprjs have metadata that is somewhat there (aka not all present or all missing)  
relevant.predatathon.bioprj %>% filter(check > 1) %>% group_by(project_acc_bioprj) %>% summarise(n=n()) %>% dim()
#71 bioprjs have partially present metadata in at least 1 category (89 old value)
relevant.predatathon.bioprj %>% group_by(project_acc_bioprj) %>% summarise(n=n()) %>% dim()
#497 projects total (563 old value)
#71/497 = 14% of projects have partial data (old values - 89/563 = 16%)

#recode bioprojects as metadata complete there or not (no some/partial allowed)
relevant.predatathon.bioprj <- relevant.predatathon.bioprj %>% mutate(new_status = ifelse(check > 1, "no",status)) %>% 
  dplyr::select(project_acc_bioprj, metadata, new_status) %>% distinct()

#check that each bioprj just has yes or no for each type of metadata
#should return 0 if so (n inside filter command needs to equal number of metdata types included)
relevant.predatathon.bioprj %>% group_by(project_acc_bioprj) %>% summarise(n=n()) %>% filter(n != 6) %>% nrow()

#calculate proportions of metadata present for relevant bioprojects before the datathon
relevant.predatathon.bioprj %>% group_by(metadata,new_status) %>% summarise(n=n()) %>% 
  pivot_wider(names_from = new_status, values_from = n) %>% 
  mutate(yes = ifelse(is.na(yes)==T,0,yes)) %>%
  mutate(proppresent = yes/(no+yes))

#set points and lines for predatathon level of present data (proppresent = value below)
relevant.predatathon <- data.frame(
  metadata_ordered = c("Habitat", "Publication DOI", "Collection date", 
                       "Country", "Place name", "Lat. + long."),
  value = c(0.02,0.07,0.28,0.51,0.33,0),
  x = c(0.55,1.55,2.55,3.55,4.55,5.55),
  xend = c(1.45,2.45,3.45,4.45,5.45,6.45))

#calculate proportions of metadata present for relevant bioprjs mid-datathon (after looking in papers)
df.middatathon.long %>% mutate_all(funs(str_replace(., "Some present", "Missing"))) %>%
  mutate_all(funs(str_replace(., "Most present", "Present"))) %>% 
  dplyr::select(project_index,project_acc_bioprj,status_ordered,metadata_ordered) %>% 
  pivot_wider(., names_from="metadata_ordered",values_from="status_ordered") %>% 
  mutate(hasspatiotemp = ifelse(`Lat. + long.`=="Present"&`Collection date`=="Present", "Present","Missing")) %>% 
  mutate(hasANYspatial = ifelse(`Lat. + long.`=="Present"|`Country`=="Present"|`Place name`=="Present","Present","Missing")) %>% 
  mutate(hasANYspatiotemp = ifelse(hasANYspatial=="Present"&`Collection date`=="Present", "Present","Missing")) %>% 
  pivot_longer(., names_to="metadata_ordered", values_to="status_ordered", cols=3:11) %>% 
  group_by(status_ordered,metadata_ordered) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from="status_ordered", values_from="n") %>% 
  mutate(total = Missing+Present) %>% 
  mutate(proppresent = Present/(Missing+Present)) %>% mutate(propmissing = 1-proppresent)
#


#plot - metadata mid datathon - FIGURE 2B ---------------

#position = "stack" for absolute numbers
  ggplot() + 
  geom_bar(data = df.middatathon.long, aes(x=metadata_ordered, fill=status_ordered, y=(..count..)/sum(..count..)), stat = "count", position = "fill", colour = "black", size = 0.15) + 
  scale_fill_manual(values = c("gray40","#ADEBAD","#5BBA6F","#2A9134")) +
  scale_y_continuous(labels=scales::percent) +
  geom_point(data = relevant.predatathon, aes(x=metadata_ordered, y=value), size = 1.25, shape = 18) +
  geom_segment(data = relevant.predatathon, aes(x=x, y=value, xend=xend, yend=value), colour = "black", linetype = "dashed", size = 0.15) +
  labs(x = "Type of metadata", y = "Percent of BioSamples", fill = "Metadata\nstatus:") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.key.size = unit(1,"line")) +
  coord_flip()

ggsave("figures/middatathon_allmetadata-relevantstudies-R1.pdf", width = 92, height = 100, dpi = 600, units = c("mm"))




## START NEW SECTION GIDEON ------
rm(list = ls())
gc()

allrecords.raw <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/final_files_NCBI_records/11-NCBI_bioprojects__not-human_bacteria_viral_metagenome__BioProjectsANDSRAANDBioSamplesANDTaxonomy-WITHseqdataofinterest.csv") %>% dplyr::select(-X)

#get all records
df.mayberelevant <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-6-10-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X) %>% 
  dplyr::select(project_acc_bioprj,organism_biosamp,biosample_acc_sra,run_acc_sra,contains("has"),is_domestic) %>% 
  filter(is_domestic == "no")
df.mayberelevant$hasspatiotemp[df.mayberelevant$hasspatiotemp == "maybe in pub. paper"] = "no"

#get mini datathon - 200 random bioprj level
bps200nodomestic <- googledrive::drive_get(path = "GEOME_Datathon/Datathon_Working_Directory/BioProject_Tables/MiniDatathon_SRA_BioProjects")
df.datathon <- googlesheets4::range_read(bps200nodomestic, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(project_index,project_acc_bioprj,Relevant) %>% mutate(Relevant = unlist(Relevant))
#read in dataframe with metadata status for each sample
df.all <- read.csv("working_files/pre-datathon_full_list_from_NCBI.csv", header = T) 
#get metadata status
df.datathon <- merge(df.datathon, df.all, by = "project_acc_bioprj", all.x = T)
df.datathon$hasspatiotemp[df.datathon$hasspatiotemp == "maybe in pub. paper"] = "no"

library(plotrix)





# all potentially relevant records 
#find projects with mixed spatiotemp presence
mixedbps <- df.mayberelevant %>% group_by(project_acc_bioprj,hasspatiotemp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% filter(n > 1)
#bioprjs with all present spatiotemp
df.mayberelevant %>% filter(hasspatiotemp == "yes") %>% filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.mayberelevant %>% filter(hasspatiotemp == "yes") %>% filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with all missing spatiotemp
df.mayberelevant %>% filter(hasspatiotemp == "no") %>% filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.mayberelevant %>% filter(hasspatiotemp == "no") %>% filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with mixed missing spatiotemp
df.mayberelevant %>% filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.mayberelevant %>% filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()

df.mayberelevant %>% distinct(project_acc_bioprj) %>% nrow()

# mini datathon records - relevant
#find projects with mixed spatiotemp presence
mixedbps <- df.datathon %>% filter(Relevant == "TRUE") %>% group_by(project_acc_bioprj,hasspatiotemp) %>% 
  summarise(n=n()) %>% group_by(project_acc_bioprj) %>% summarise(n=n()) %>% filter(n > 1)
#bioprjs with all present spatiotemp
df.datathon %>% filter(Relevant == "TRUE") %>% filter(hasspatiotemp == "yes") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(Relevant == "TRUE") %>% filter(hasspatiotemp == "yes") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with all missing spatiotemp
df.datathon %>% filter(Relevant == "TRUE") %>% filter(hasspatiotemp == "no") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(Relevant == "TRUE") %>% filter(hasspatiotemp == "no") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with mixed missing spatiotemp
df.datathon %>% filter(Relevant == "TRUE") %>% 
  filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()

df.datathon %>% filter(Relevant == "TRUE") %>% distinct(project_acc_bioprj) %>% nrow()

# mini datathon records - NOT relevant
#find projects with mixed spatiotemp presence
mixedbps <- df.datathon %>% filter(Relevant == "FALSE") %>% group_by(project_acc_bioprj,hasspatiotemp) %>% 
  summarise(n=n()) %>% group_by(project_acc_bioprj) %>% summarise(n=n()) %>% filter(n > 1)
#bioprjs with all present spatiotemp
df.datathon %>% filter(Relevant == "FALSE") %>% filter(hasspatiotemp == "yes") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(Relevant == "FALSE") %>% filter(hasspatiotemp == "yes") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with all missing spatiotemp
df.datathon %>% filter(Relevant == "FALSE") %>% filter(hasspatiotemp == "no") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(Relevant == "FALSE") %>% filter(hasspatiotemp == "no") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with mixed missing spatiotemp
df.datathon %>% filter(Relevant == "FALSE") %>% 
  filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()

df.datathon %>% filter(Relevant == "FALSE") %>% distinct(project_acc_bioprj) %>% nrow()
df.datathon %>% filter(Relevant == "TRUE") %>% distinct(project_acc_bioprj) %>% nrow()
df.datathon %>% filter(Relevant == "NA") %>% distinct(project_acc_bioprj) %>% nrow()



# mini datathon records - all (regardless of relevance)
#find projects with mixed spatiotemp presence
mixedbps <- df.datathon %>% group_by(project_acc_bioprj,hasspatiotemp) %>% 
  summarise(n=n()) %>% group_by(project_acc_bioprj) %>% summarise(n=n()) %>% filter(n > 1)
#bioprjs with all present spatiotemp
df.datathon %>% filter(hasspatiotemp == "yes") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(hasspatiotemp == "yes") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with all missing spatiotemp
df.datathon %>% filter(hasspatiotemp == "no") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(hasspatiotemp == "no") %>% 
  filter(!project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()
#bioprjs with mixed missing spatiotemp
df.datathon %>% 
  filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj,biosample_acc_sra) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% summarise(mean = mean(n))
df.datathon %>% filter(project_acc_bioprj %in% mixedbps$project_acc_bioprj) %>% 
  group_by(project_acc_bioprj) %>% summarise(n=n()) %>% nrow()

## END NEW SECTION GIDEON ------









# ************************************
#get numbers for SuppTable ----------
# ************************************

#add column to denote which samples have time AND space data ****
#counting collection date as time and any locality as space (not just lat/long)
df.stats <- df

df.stats$hasANYspatiotemp[df.stats$hasspatiotemp=="yes"] = "yes"
df.stats$hasANYspatiotemp[df.stats$hascollectiondate=="no"] = "no"
df.stats$hasANYspatiotemp[df.stats$hascollectiondate=="maybe in pub. paper"] = "no"
df.stats$hasANYspatiotemp[df.stats$hascollectiondate=="yes" & df.stats$haslatlong=="yes"] = "yes"
df.stats$hasANYspatiotemp[df.stats$hascollectiondate=="yes" & df.stats$haslocality=="yes"] = "yes"
df.stats$hasANYspatiotemp[df.stats$hascollectiondate=="yes" & df.stats$hascountry=="yes"] = "yes"
df.stats$hasANYspatiotemp[is.na(df.stats$hasANYspatiotemp)==TRUE] = "no"
#sanity check
#df.stats %>% group_by(hasANYspatiotemp,haslatlong,haslocality,hascountry,hascollectiondate) %>% summarise(n=n()) %>% View()

#add column to denote which samples have any space data ****
df.stats$hasANYspatio[df.stats$haslatlong=="yes"] = "yes"
df.stats$hasANYspatio[df.stats$haslocality=="yes"] = "yes"
df.stats$hasANYspatio[df.stats$hascountry=="yes"] = "yes"
df.stats$hasANYspatio[is.na(df.stats$hasANYspatio)==TRUE] = "no"
#sanity check
#df.stats %>% group_by(hasANYspatio,haslatlong,haslocality,hascountry) %>% summarise(n=n()) %>% View()


#our filters ****
df.stats %>% distinct(biosample_acc_sra) %>% nrow()
#327,582 biosamples
df.stats %>% distinct(biosample_acc_sra,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#14% have latlong and collection date
df.stats %>% distinct(biosample_acc_sra,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#31% have spatiotemp of any kind
df.stats %>% distinct(biosample_acc_sra,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#67% have spatio of any kind
df.stats %>% distinct(biosample_acc_sra,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#34% have collection date
df.stats %>% distinct(biosample_acc_sra,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#19% have lat/long
df.stats %>% distinct(biosample_acc_sra,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#47% have locality
df.stats %>% distinct(biosample_acc_sra,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#64% have country


#domestic ****
df.stats.dom <- df.stats %>% filter(is_domestic == "yes") 
df.stats.dom %>% distinct(biosample_acc_sra) %>% nrow()
#93,965 biosamples
df.stats.dom %>% distinct(biosample_acc_sra,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#2% have latlong and collection date
df.stats.dom %>% distinct(biosample_acc_sra,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#15% have spatiotemp of any kind
df.stats.dom %>% distinct(biosample_acc_sra,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#69% have spatio of any kind
df.stats.dom %>% distinct(biosample_acc_sra,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#17% have collection date
df.stats.dom %>% distinct(biosample_acc_sra,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#5% have lat/long
df.stats.dom %>% distinct(biosample_acc_sra,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#43% have locality
df.stats.dom %>% distinct(biosample_acc_sra,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#69% have country


#wild
#finding biosamps that are domestic and not domestic at the same time
#mysteries <- df %>% distinct(biosample_acc_sra,is_domestic) %>% group_by(is_domestic,biosample_acc_sra) %>% 
#  summarise(n=n()) %>% group_by(biosample_acc_sra) %>% summarise(n=n()) %>% filter(n > 1)
#df %>% filter(biosample_acc_sra %in% mysteries$biosample_acc_sra) %>% dplyr::select(biosample_acc_sra,run_acc_sra,organism_biosamp,is_domestic) %>% 
#  View()
df.stats.wild <- df.stats %>% filter(is_domestic == "no") 
df.stats.wild %>% distinct(biosample_acc_sra) %>% nrow()
#233,644 biosamples
df.stats.wild %>% distinct(biosample_acc_sra,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#18% have latlong and collection date
df.stats.wild %>% distinct(biosample_acc_sra,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#38% have spatiotemp of any kind
df.stats.wild %>% distinct(biosample_acc_sra,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#66% have spatio of any kind
df.stats.wild %>% distinct(biosample_acc_sra,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#41% have collection date
df.stats.wild %>% distinct(biosample_acc_sra,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#25% have lat/long
df.stats.wild %>% distinct(biosample_acc_sra,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#49% have locality
df.stats.wild %>% distinct(biosample_acc_sra,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#62% have country


#wild with < 5 indivs
# set minimum number of indivs per bioprj/species combo
n.indiv = 5
#group at level of biorpj/species and get at least X indivs per combo
filterlist.Xindivs <- df.stats %>% filter(is_domestic == "no") %>% 
  group_by(biosample_acc_sra,project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  filter(n < n.indiv) %>% mutate(link = paste(project_acc_bioprj, organism_biosamp, sep = "_")) %>% 
  mutate(link = gsub(pattern = " ", replacement = "-", link))
filterlist.Xindivs %>% distinct(project_acc_bioprj) %>% nrow()
#2,674 bioprjs
#get only above list from full records
dim(df.stats)
dim(filterlist.Xindivs)
wildlessthan5 <- df.stats %>% filter(link %in% filterlist.Xindivs$link)
dim(wildlessthan5)
wildlessthan5 %>% distinct(project_acc_bioprj) %>% nrow()
wildlessthan5 %>% distinct(biosample_acc_sra) %>% nrow()
#22,253 biosamples
wildlessthan5 %>% distinct(biosample_acc_sra,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#6% have latlong and collection date
wildlessthan5 %>% distinct(biosample_acc_sra,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#30% have spatiotemp of any kind
wildlessthan5 %>% distinct(biosample_acc_sra,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#68% have spatio of any kind
wildlessthan5 %>% distinct(biosample_acc_sra,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#32% have collection date
wildlessthan5 %>% distinct(biosample_acc_sra,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#11% have lat/long
wildlessthan5 %>% distinct(biosample_acc_sra,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#48% have locality
wildlessthan5 %>% distinct(biosample_acc_sra,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#67% have country


#wild with 5+ indivs
# set minimum number of indivs per bioprj/species combo
n.indiv = 5
#group at level of biorpj/species and get at least X indivs per combo
filterlist.Xindivs <- df.stats %>% filter(is_domestic == "no") %>% 
  group_by(biosample_acc_sra,project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  filter(n >= n.indiv) %>% mutate(link = paste(project_acc_bioprj, organism_biosamp, sep = "_")) %>% 
  mutate(link = gsub(pattern = " ", replacement = "-", link))
filterlist.Xindivs %>% distinct(project_acc_bioprj) %>% nrow()
#1,727 bioprjs
#get only above list from full records
dim(df.stats)
dim(filterlist.Xindivs)
wildmorethan5 <- df.stats %>% filter(link %in% filterlist.Xindivs$link)
dim(wildmorethan5)
wildmorethan5 %>% distinct(project_acc_bioprj) %>% nrow()
wildmorethan5 %>% distinct(biosample_acc_sra) %>% nrow()
#211,411 biosamples
wildmorethan5 %>% distinct(biosample_acc_sra,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#19% have latlong and collection date
wildmorethan5 %>% distinct(biosample_acc_sra,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#39% have spatiotemp of any kind
wildmorethan5 %>% distinct(biosample_acc_sra,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#66% have spatio of any kind
wildmorethan5 %>% distinct(biosample_acc_sra,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#42% have collection date
wildmorethan5 %>% distinct(biosample_acc_sra,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#27% have lat/long
wildmorethan5 %>% distinct(biosample_acc_sra,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#49% have locality
wildmorethan5 %>% distinct(biosample_acc_sra,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#62% have country


# AT BIOPROJECT LEVEL *******
#our filters ****
df.stats %>% distinct(project_acc_bioprj) %>% nrow()
#5,043 bioprjs
df.stats %>% distinct(project_acc_bioprj,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#12% have latlong and collection date
df.stats %>% distinct(project_acc_bioprj,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#32% have spatiotemp of any kind
df.stats %>% distinct(project_acc_bioprj,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#63% have spatio of any kind
df.stats %>% distinct(project_acc_bioprj,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#34% have collection date
df.stats %>% distinct(project_acc_bioprj,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#15% have lat/long
df.stats %>% distinct(project_acc_bioprj,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#45% have locality
df.stats %>% distinct(project_acc_bioprj,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#62% have country


#domestic ****
df.stats.dom <- df.stats %>% filter(is_domestic == "yes") 
df.stats.dom %>% distinct(project_acc_bioprj) %>% nrow()
#1,397 bioprjs
df.stats.dom %>% distinct(project_acc_bioprj,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#4% have latlong and collection date
df.stats.dom %>% distinct(project_acc_bioprj,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#13% have spatiotemp of any kind
df.stats.dom %>% distinct(project_acc_bioprj,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100) 
#57% have spatio of any kind
df.stats.dom %>% distinct(project_acc_bioprj,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#15% have collection date
df.stats.dom %>% distinct(project_acc_bioprj,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#6% have lat/long
df.stats.dom %>% distinct(project_acc_bioprj,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#35% have locality
df.stats.dom %>% distinct(project_acc_bioprj,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#57% have country


#wild
df.stats.wild <- df.stats %>% filter(is_domestic == "no") 
df.stats.wild %>% distinct(project_acc_bioprj) %>% nrow()
#3,903 bioprojs
df.stats.wild %>% distinct(project_acc_bioprj,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#14% have latlong and collection date
df.stats.wild %>% distinct(project_acc_bioprj,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#37% have spatiotemp of any kind
df.stats.wild %>% distinct(project_acc_bioprj,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#65% have spatio of any kind
df.stats.wild %>% distinct(project_acc_bioprj,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#39% have collection date
df.stats.wild %>% distinct(project_acc_bioprj,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#18% have lat/long
df.stats.wild %>% distinct(project_acc_bioprj,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#48% have locality
df.stats.wild %>% distinct(project_acc_bioprj,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#64% have country


#wild with < 5 indivs
# set minimum number of indivs per bioprj/species combo
n.indiv = 5
#group at level of biorpj/species and get at least X indivs per combo
filterlist.Xindivs <- df.stats %>% filter(is_domestic == "no") %>% 
  group_by(biosample_acc_sra,project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  filter(n < n.indiv) %>% mutate(link = paste(project_acc_bioprj, organism_biosamp, sep = "_")) %>% 
  mutate(link = gsub(pattern = " ", replacement = "-", link))
filterlist.Xindivs %>% distinct(project_acc_bioprj) %>% nrow()
#2,674 bioprjs
#get only above list from full records
dim(df.stats)
dim(filterlist.Xindivs)
wildlessthan5 <- df.stats %>% filter(link %in% filterlist.Xindivs$link)
dim(wildlessthan5)
wildlessthan5 %>% distinct(project_acc_bioprj) %>% nrow()
wildlessthan5 %>% distinct(project_acc_bioprj,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#13% have latlong and collection date
wildlessthan5 %>% distinct(project_acc_bioprj,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#36% have spatiotemp of any kind
wildlessthan5 %>% distinct(project_acc_bioprj,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#66% have spatio of any kind
wildlessthan5 %>% distinct(project_acc_bioprj,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#38% have collection date
wildlessthan5 %>% distinct(project_acc_bioprj,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#16% have lat/long
wildlessthan5 %>% distinct(project_acc_bioprj,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#48% have locality
wildlessthan5 %>% distinct(project_acc_bioprj,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#65% have country


#wild with 5+ indivs
# set minimum number of indivs per bioprj/species combo
n.indiv = 5
#group at level of biorpj/species and get at least X indivs per combo
filterlist.Xindivs <- df.stats %>% filter(is_domestic == "no") %>% 
  group_by(biosample_acc_sra,project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  filter(n >= n.indiv) %>% mutate(link = paste(project_acc_bioprj, organism_biosamp, sep = "_")) %>% 
  mutate(link = gsub(pattern = " ", replacement = "-", link))
filterlist.Xindivs %>% distinct(project_acc_bioprj) %>% nrow()
#1,727 bioprjs
#get only above list from full records
dim(df.stats)
dim(filterlist.Xindivs)
wildmorethan5 <- df.stats %>% filter(link %in% filterlist.Xindivs$link)
dim(wildmorethan5)
wildmorethan5 %>% distinct(project_acc_bioprj) %>% nrow()
wildmorethan5 %>% distinct(project_acc_bioprj,hasspatiotemp) %>% group_by(hasspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100) 
#15% have latlong and collection date
wildmorethan5 %>% distinct(project_acc_bioprj,hasANYspatiotemp) %>% group_by(hasANYspatiotemp) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#37% have spatiotemp of any kind
wildmorethan5 %>% distinct(project_acc_bioprj,hasANYspatio) %>% group_by(hasANYspatio) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no))*100)
#64% have spatio of any kind
wildmorethan5 %>% distinct(project_acc_bioprj,hascollectiondate) %>% group_by(hascollectiondate) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#40% have collection date
wildmorethan5 %>% distinct(project_acc_bioprj,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#20% have lat/long
wildmorethan5 %>% distinct(project_acc_bioprj,haslocality) %>% group_by(haslocality) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#47% have locality
wildmorethan5 %>% distinct(project_acc_bioprj,hascountry) %>% group_by(hascountry) %>% summarise(n=n()) %>% 
  pivot_wider(., names_from = 1, values_from = "n") %>% mutate(percpresent = (yes/(yes+no+`maybe in pub. paper`))*100)
#62% have country










#plot - metadata pre datathon, domestic vs natural LONG ---------------
#position = "stack" for absolute numbers
df.predatathon.long %>%
  filter(!metadata_ordered %in% c("Preservative","Derived genetic data","Enviro. medium")) %>% 
  ggplot() + 
  geom_bar(aes(x=metadata_ordered, fill=status, y=(..count..)/sum(..count..)), 
           stat = "count", position = "fill", colour = "black", size = 0.2) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values = c("#ccece6","gray40","#2A9134")) +
  labs(x = "Type of metadata", y = "Percent of BioSamples", fill = "Status:") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0),
        strip.background =element_blank(),
        strip.text = element_text(face="bold", size = 8),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        legend.key.size = unit(1,"line"),
        legend.position = "right") +
  facet_wrap(~is_domestic, ncol = 1) +
  coord_flip()

ggsave("figures/predatathon_allmetadata-domesticVwild-2172021-LONG.pdf", width = 100, height = 140, dpi = 600, units = c("mm"))



#plot - metadata pre datathon, "all" records ------------
df.predatathon.long %>% ggplot() + 
  geom_bar(aes(x=metadata, fill=status), stat = "count", position = "stack") + 
  scale_fill_manual(values = c("#ccece6","gray40","forestgreen")) +
  labs(x = "Metadata", y = "Count", fill = "Status") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(~date_added)




#calc proportions per broad tax categories
props.broad <- df.taxdonut %>% add_count(broad_tax_category, name = "n") %>% 
  mutate(broad_tax_category_props = n/nrow(.)) %>% distinct(broad_tax_category,n,broad_tax_category_props)

#calc positions of donut
props.broad <- props.broad %>% mutate(ymax = cumsum(broad_tax_category_props)) %>% mutate(ymin = c(0, head(ymax, n=-1)))

props.broad %>% ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=broad_tax_category)) +
  geom_rect() +
  scale_fill_manual(values = c("#FEA634","#EB5E55","#70F8BA","#524948","#CBFE48","#7CB4B8")) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())
c("#EFCA08","#F49F0A","#B86800","#BBDEF0","#00A6A6","#007A7A")

#filter out model organisms
dim(df.all)
df.all <- df.all %>% 
  filter(organism_biosamp != "Saccharomyces cerevisiae") %>% filter(organism_biosamp != "Drosophila melanogaster") %>% 
  filter(organism_biosamp != "Caenorhabditis elegans") %>% filter(organism_biosamp != "Xenopus tropicalis") %>% 
  filter(organism_biosamp != "Mus musculus") %>% filter(organism_biosamp != "Danio rerio") %>% 
  filter(organism_biosamp != "Arabidopsis thaliana")
dim(df.all)






mockup <- tibble(
  project_index = 1:1000,
  BioSamples =rnorm(1000,mean = mean(counts$BioSamples), sd = sd(counts$BioSamples)),
  N_Species  =rnorm(1000,mean = mean(counts$N_Species), sd = sd(counts$N_Species)),
  Paper_Available = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.7,0.3)),
  Authors_Contacted = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.5,0.5)),
  materialSampleID = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.95,0.05)),
  locality = sample(c("TRUE","MOST", "SOME","FALSE"), size = 1000, replace = T, 
                    prob = c(0.9,0.05,0.00,0.00)),
  Coordinates = sample(c("TRUE","MOST", "SOME","FALSE"), size = 1000, replace = T,  
                       prob = c(0.8,0.1,0.05,0.05)),
  country = sample(c("TRUE","MOST", "SOME","FALSE"), size = 1000, replace = T,  
                   prob = c(0.98,0.02,0.0,0.0)),
  habitat = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.9,0.1)),
  environmentalMedium = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.9,0.1)),
  yearCollected = sample(c("TRUE","MOST", "SOME","FALSE"), size = 1000, replace = T,  
                         prob = c(0.9,0.05,0.05,0.0)),
  permitInformation = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.5,0.5)),
  preservative = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.8,0.2)),
  derivedGeneticDataX = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.6,0.4)),
  metadataEntered = sample(c("TRUE","FALSE"), size = 1000, replace = T, prob = c(0.2,0.8))
)

mockup$BioSamples[which(mockup$BioSamples < 5)] <- 5
mockup$N_Species[which(mockup$N_Species < 1)] <- 1

melted_mockup <- melt(mockup, id.vars = c("project_index", "BioSamples", "N_Species") )


ggplot(melted_mockup, aes(x = variable)) + geom_bar(aes(fill = value)) + theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 1))



l1 <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/dataset_lists_for_DIPnet_datathon/list1-divdiv_with_latlong_located_outsideofNCBI-6-22-2020.csv") %>% dplyr::select(-X_bioprj,-Comments_about_spatial_data_location,-X,-link_to_published_paper) %>% mutate(list = "list1")
l2 <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/dataset_lists_for_DIPnet_datathon/list2-divdiv_without_latlong_located_yet_outsideofNCBI-6-22-2020.csv") %>% dplyr::select(-X,-X_bioprj) %>% mutate(list = "list2")
l3 <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/dataset_lists_for_DIPnet_datathon/list3-divdiv_couldntfind_latlong_outsideofNCBI-6-22-2020.csv") %>% dplyr::select(-X,-X_bioprj) %>% mutate(list = "list3")
l4 <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/dataset_lists_for_DIPnet_datathon/list4-eukaryotes-min5indivs-_without_latlong_located_yet_outsideofNCBI-6-22-2020.csv") %>% dplyr::select(-X,-X_bioprj,-haslatlong) %>% mutate(list = "list4")

df.pre.datathon <- rbind(l1,l2,l3,l4) 

df.pre.datathon <- df.pre.datathon %>% mutate(haslatlong.temp = ifelse(lat_long_biosamp == "unknown", "no", "in NCBI")) %>% 
  mutate(haslatlong = ifelse(is.na(haslatlong.temp)==T, "maybe in pub. paper", haslatlong.temp)) %>% dplyr::select(-haslatlong.temp)
df.pre.datathon %>% group_by(haslatlong) %>% summarise(n=n())

rm(l1,l2,l3,l4)

df <- read.csv(nonmarineSRA.pre)


