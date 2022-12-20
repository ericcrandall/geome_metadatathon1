#written by R.H. Toczydlowski (rhtoczydlowski@gmail.com, ORCID: 0000-0002-8141-2036)

#idea: parse metadata statuses for pre, mid, and post datathon

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
library(stringr)        #parsing
library(ggplot2)        #graphing





# *******************************************************************************
# *******************************************************************************
# NEW SECTION - WRANGLE DATATHON BIOPRJ LISTS -----------------------------------

rm(list = ls())
gc()

# get list of records that went into datathon

#get path to spreadsheet in GDrive
nonmarineSRA <- googledrive::drive_ls(path = "BioProject_Tables", pattern = "nonMarine_SRA_BioProjects")
marineSRA <- googledrive::drive_ls(path = "BioProject_Tables", pattern = "^Marine_SRA_BioProjects")

#read spreadsheet into df
df.nonmarine <- googlesheets4::range_read(nonmarineSRA, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(Species_all,project_index,project_acc_bioprj,BioSamples,Runs,Relevant,
                Paper_Available,Authors_Contacted,Author_Contact_Comments,
                Author_Response_Comments,Metadata_Curator,Quality_Controller) %>% mutate(datagroup = "nonmarine")

df.marine <- googlesheets4::range_read(marineSRA, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(Species_all,project_index,project_acc_bioprj,BioSamples,Runs,Relevant,
                Paper_Available,Authors_Contacted,Author_Contact_Comments,
                Author_Response_Comments,Metadata_Curator,Quality_Controller) %>% mutate(datagroup = "marine")

#bind cols from all datasheets together
df <- rbind(df.nonmarine,df.marine)
head(df)

#remove double spaces that mess up comment parsing
df <- df %>% mutate(Author_Contact_Comments = gsub("  "," ",Author_Contact_Comments))
df <- df %>% mutate(Author_Response_Comments = gsub("  "," ",Author_Response_Comments))

#unlist columns that were drop-down menus in excel
df$Paper_Available[df$Paper_Available == "NULL"] <- NA
df <- df %>% mutate(Relevant = unlist(as.character(Relevant))) %>% 
  mutate(Authors_Contacted = unlist(Authors_Contacted)) %>% 
  mutate(Paper_Available = unlist(Paper_Available)) 

#filter out domestic etc. species that shouldn't have been in datathon in first place
species_to_drop <- read.delim("NonWildSpecies/nonWildSpecies_final_sources.tsv", header = T)
df <- df %>% filter(!str_detect(Species_all, str_c(species_to_drop$Species, collapse = "|"))) %>% 
  dplyr::select(-Species_all)

#keep only relevant projects
df <- df %>% filter(Relevant == "TRUE")

str(df)



# *******************************************************************************
# *******************************************************************************
# NEW SECTION - MID DATATHON STATUS ---------------------------------------------

# publication ---
df <- df %>% rename("hasMIDdatathonpublication" = "Paper_Available") %>% mutate(hasPOSTdatathonpublication = hasMIDdatathonpublication)

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

#check values
df %>% dplyr::select(project_index,project_acc_bioprj,contains("hasMID")) %>% 
  pivot_longer(., names_to = "category", values_to = "status", cols = 3:ncol(.)) %>%
  group_by(status) %>% summarise(n=n())
#should only be FALSE, MOST, SOME, or TRUE values




# *******************************************************************************
# ***************************************************************************************
# NEW SECTION - POST DATATHON STATUS ----------------------------------------------
#loop through each category of metadata and pull out status from Author_Response_Comments ----

for (metadata in c("materialSampleID","locality",
                   "coordinates","country","habitat",
                   "environmentalMedium", "yearCollected", 
                   "permitInformation", "preservative",
                   "derivedGeneticDataX")) {
  
  df <- df %>% 
    separate(., Author_Response_Comments, into = c("garbage","working"), sep = paste(metadata), remove = F, extra = "drop") %>% 
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
  
  names(df)[names(df)=="final"] <- paste("hasPOSTdatathon", metadata, sep = "")
  
}


#check values
df %>% dplyr::select(Authors_Contacted, project_index,project_acc_bioprj,contains("hasPOST")) %>% 
  pivot_longer(., names_to = "category", values_to = "status", cols = 4:ncol(.)) %>%
  group_by(Authors_Contacted,status) %>% summarise(n=n())
#should only be FALSE, MOST, SOME, or TRUE values

#find any projects with NAs for MID and/or POST statuses so they can be fixed
projsmissingmetadata <- df %>% dplyr::select(Authors_Contacted,Metadata_Curator,Quality_Controller,project_index,project_acc_bioprj,contains("hasPOST")) %>%
  pivot_longer(., names_to = "category", values_to = "status", cols = 6:ncol(.)) %>%
  filter(is.na(status)==T) %>%
  group_by(Authors_Contacted,Metadata_Curator,Quality_Controller,project_index) %>% summarise(n=n()) %>% dplyr::select(-n)
#write.csv(projsmissingmetadata, "working_files/remaining_issues_in_authorresponsecomments.csv")




# *******************************************************************************
# *******************************************************************************
# NEW SECTION - PRE DATATHON STATUS ----------------------------------------------

# READ IN DATA

#start with this file - 
# all records from NCBI (minus our filters like not human)
#allrecords.raw <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/final_files_NCBI_records/11-NCBI_bioprojects__not-human_bacteria_viral_metagenome__BioProjectsANDSRAANDBioSamplesANDTaxonomy-WITHseqdataofinterest.csv") %>% dplyr::select(-X)
#ran code to extract metadata categories we care about and see if data were there or not
#saved as this file 
#write.csv(df.all, "working_files/pre-datathon_full_list_from_NCBI.csv")

#read in metadata status for all records from NCBI (minus our filters like not human)
df.all <- read.csv("working_files/pre-datathon_full_list_from_NCBI-JUSTdomesticANDwild-11-3-2021-greplnamematching.csv", header = T) %>% dplyr::select(-X)

# keep just the bioprj IDs in mid and post datathon list
df.pre <- df.all %>% filter(project_acc_bioprj %in% df$project_acc_bioprj) %>% dplyr::select(-contains("taxize"))

#and keep just the relevant biosamples included in the datathon (aka those missing lat/lon) 
#get list of all BioSamp IDs in relevant datathon projects
relevantbiosamps <- read.delim("working_files/paper2_all_biosample_list.tsv", header = T) %>% 
  dplyr::select(project_index, project_acc_bioprj, biosample_acc_sra, run_acc_sra)
#filter
dim(df.pre)
df.pre <- df.pre %>% filter(biosample_acc_sra %in% relevantbiosamps$biosample_acc_sra) 
dim(df.pre)
  
#should be 0 if all biosample IDs only have one consistent metadata status value for:
#haslatlong,haslocality,hascountry,hascollectiondate
df.pre %>% group_by(biosample_acc_sra) %>% 
  mutate(metadata_status_all = paste(haslatlong,haslocality,hascountry,hascollectiondate, sep = "_")) %>% 
  mutate(checkmetadata = length(unique(metadata_status_all))) %>%
  dplyr::select(biosample_acc_sra,checkmetadata,metadata_status_all,everything()) %>%
  filter(checkmetadata > 1) %>% distinct(project_acc_bioprj,biosample_acc_sra,metadata_status_all) %>% nrow()

#consolidate to bioprj level so we can tack onto mid- and post-datathon status df ------
#if >= of samples in bioprj have metadata present then coding bioprj as metadata present and vice versa

df.pre %>% dplyr::select(hascollectiondate,haslatlong,hasenvirobroad,hascountry,haslocality,haspub) %>% 
  pivot_longer(., names_to = "metadata", values_to = "status", cols = 1:ncol(.)) %>%
  group_by(status) %>% summarise(n=n())

#derived data
df.pre1 <- df.pre %>% distinct(project_acc_bioprj) %>% mutate(hasPREdatathonderivedGeneticDataX = "FALSE")
#permit info
df.pre2 <- df.pre %>% distinct(project_acc_bioprj) %>% mutate(hasPREdatathonpermitInformation = "FALSE")
#preservative
df.pre3 <- df.pre %>% distinct(project_acc_bioprj) %>% mutate(hasPREdatathonpreservative = "FALSE")
#year
df.pre4 <- df.pre %>%
  distinct(project_acc_bioprj,biosample_acc_sra,hascollectiondate) %>% 
  group_by(project_acc_bioprj,hascollectiondate) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>%
  pivot_wider(., names_from = "hascollectiondate", values_from = "n") %>% 
  replace(is.na(.), 0) %>% ungroup() %>% 
  mutate(total = rowSums(dplyr::select(.,contains("yes"),contains("no"),contains("maybe")))) %>% 
  mutate(prop_present = yes/(total)) %>%
  mutate(hasPREdatathonyearCollected = ifelse(prop_present < 0.5, "SOME",NA)) %>%
  mutate(hasPREdatathonyearCollected = ifelse(prop_present >= 0.5, "MOST",hasPREdatathonyearCollected)) %>%
  mutate(hasPREdatathonyearCollected = ifelse(prop_present == 1, "TRUE",hasPREdatathonyearCollected)) %>% 
  mutate(hasPREdatathonyearCollected = ifelse(prop_present == 0, "FALSE",hasPREdatathonyearCollected)) %>% 
  dplyr::select(project_acc_bioprj,hasPREdatathonyearCollected)
#latlong
df.pre5 <- df.pre %>%
  distinct(project_acc_bioprj,biosample_acc_sra,haslatlong) %>% 
  group_by(project_acc_bioprj,haslatlong) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>%
  pivot_wider(., names_from = "haslatlong", values_from = "n") %>% 
  replace(is.na(.), 0) %>% ungroup() %>% 
  mutate(total = rowSums(dplyr::select(.,contains("yes"),contains("no"),contains("maybe")))) %>% 
  mutate(prop_present = yes/(total)) %>%
  mutate(hasPREdatathoncoordinates = ifelse(prop_present < 0.5, "SOME",NA)) %>%
  mutate(hasPREdatathoncoordinates = ifelse(prop_present >= 0.5, "MOST",hasPREdatathoncoordinates)) %>%
  mutate(hasPREdatathoncoordinates = ifelse(prop_present == 1, "TRUE",hasPREdatathoncoordinates)) %>% 
  mutate(hasPREdatathoncoordinates = ifelse(prop_present == 0, "FALSE",hasPREdatathoncoordinates)) %>% 
  dplyr::select(project_acc_bioprj,hasPREdatathoncoordinates)
#samp ID (all have sampID - check that this is true if dataset changes)
df.pre %>% group_by(hassampID) %>% summarise(n=n())
df.pre6 <- df.pre %>% distinct(project_acc_bioprj) %>% mutate(hasPREdatathonmaterialSampleID = "TRUE")
#enviro medium
df.pre %>% group_by(hasenviromedium) %>% summarise(n=n())
df.pre7 <- df.pre %>% distinct(project_acc_bioprj) %>% mutate(hasPREdatathonenvironmentalMedium = "FALSE")
#habitat
df.pre8 <- df.pre %>% 
  distinct(project_acc_bioprj,biosample_acc_sra,hasenvirobroad) %>% 
  group_by(project_acc_bioprj,hasenvirobroad) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>%
  pivot_wider(., names_from = "hasenvirobroad", values_from = "n") %>% 
  replace(is.na(.), 0) %>% ungroup() %>% 
  mutate(total = rowSums(dplyr::select(.,contains("yes"),contains("no"),contains("maybe")))) %>% 
  mutate(prop_present = yes/(total)) %>%
  mutate(hasPREdatathonhabitat = ifelse(prop_present < 0.5, "SOME",NA)) %>%
  mutate(hasPREdatathonhabitat = ifelse(prop_present >= 0.5, "MOST",hasPREdatathonhabitat)) %>%
  mutate(hasPREdatathonhabitat = ifelse(prop_present == 1, "TRUE",hasPREdatathonhabitat)) %>% 
  mutate(hasPREdatathonhabitat = ifelse(prop_present == 0, "FALSE",hasPREdatathonhabitat)) %>% 
  dplyr::select(project_acc_bioprj,hasPREdatathonhabitat)
#country
df.pre9 <- df.pre %>% 
  distinct(project_acc_bioprj,biosample_acc_sra,hascountry) %>% 
  group_by(project_acc_bioprj,hascountry) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>%
  pivot_wider(., names_from = "hascountry", values_from = "n") %>% 
  replace(is.na(.), 0) %>% ungroup() %>% 
  mutate(total = rowSums(dplyr::select(.,contains("yes"),contains("no"),contains("maybe")))) %>% 
  mutate(prop_present = yes/(total)) %>%
  mutate(hasPREdatathoncountry = ifelse(prop_present < 0.5, "SOME",NA)) %>%
  mutate(hasPREdatathoncountry = ifelse(prop_present >= 0.5, "MOST",hasPREdatathoncountry)) %>%
  mutate(hasPREdatathoncountry = ifelse(prop_present == 1, "TRUE",hasPREdatathoncountry)) %>% 
  mutate(hasPREdatathoncountry = ifelse(prop_present == 0, "FALSE",hasPREdatathoncountry)) %>% 
  dplyr::select(project_acc_bioprj,hasPREdatathoncountry)
#locality
df.pre10 <- df.pre %>% 
  distinct(project_acc_bioprj,biosample_acc_sra,haslocality) %>% 
  group_by(project_acc_bioprj,haslocality) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>%
  pivot_wider(., names_from = "haslocality", values_from = "n") %>% 
  replace(is.na(.), 0) %>% ungroup() %>% 
  mutate(total = rowSums(dplyr::select(.,contains("yes"),contains("no"),contains("maybe")))) %>% 
  mutate(prop_present = yes/(total)) %>%
  mutate(hasPREdatathonlocality = ifelse(prop_present < 0.5, "SOME",NA)) %>%
  mutate(hasPREdatathonlocality = ifelse(prop_present >= 0.5, "MOST",hasPREdatathonlocality)) %>%
  mutate(hasPREdatathonlocality = ifelse(prop_present == 1, "TRUE",hasPREdatathonlocality)) %>% 
  mutate(hasPREdatathonlocality = ifelse(prop_present == 0, "FALSE",hasPREdatathonlocality)) %>% 
  dplyr::select(project_acc_bioprj,hasPREdatathonlocality)
#publication
df.pre11 <- df.pre %>% 
  distinct(project_acc_bioprj,biosample_acc_sra,haspub) %>% 
  group_by(project_acc_bioprj,haspub) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj) %>%
  pivot_wider(., names_from = "haspub", values_from = "n") %>% 
  replace(is.na(.), 0) %>% ungroup() %>% 
  mutate(total = rowSums(dplyr::select(.,contains("yes"),contains("no"),contains("maybe")))) %>% 
  mutate(prop_present = yes/(total)) %>%
  mutate(hasPREdatathonpublication = ifelse(prop_present < 0.5, "SOME",NA)) %>%
  mutate(hasPREdatathonpublication = ifelse(prop_present >= 0.5, "MOST",hasPREdatathonpublication)) %>%
  mutate(hasPREdatathonpublication = ifelse(prop_present == 1, "TRUE",hasPREdatathonpublication)) %>% 
  mutate(hasPREdatathonpublication = ifelse(prop_present == 0, "FALSE",hasPREdatathonpublication)) %>% 
  dplyr::select(project_acc_bioprj,hasPREdatathonpublication)

#join all pre-datathon statuses together
df.pre <- Reduce(merge, list(df.pre1,df.pre2,df.pre3,df.pre4,df.pre5,df.pre6,df.pre7,df.pre8,df.pre9,df.pre10,df.pre11))

#and drop marine projects that are also in full datathon 
#(Eric made sure both structured author comments and paper_avail cols matched btwn these "duplicate" records)
df <- df %>% filter(!project_index %in% c("M0012","M0022","M0037","M0050","M0062","M0063"))

#and drop 5 prjs that are missing from 2020 master NCBI list (bc they have e.g. "exome" or "phenotype" in bioprj data type field)
df <- df %>% filter(!project_acc_bioprj %in% c("PRJNA315895", "PRJNA324830", "PRJNA386149", "PRJNA429104", "PRJNA453553", "PRJNA377812"))

#and join onto mid- and post-datathon status
df <- merge(df, df.pre, by = "project_acc_bioprj", all = T)

rm(df.pre,df.pre1,df.pre10,df.pre11,df.pre2,df.pre3,df.pre4,df.pre5,df.pre6,df.pre7,df.pre8,df.pre9)


# ***********************************************
#check that metadata always increases ----------

#find entries where metadata is lost (e.g. TRUE_FALSE_FALSE)
#(including NA here just in case there are some that R doesn't see as NAs)
nums <- data.frame(PRE = c("NA","FALSE","SOME","MOST","TRUE"), PRE.num = c(0,1,2,3,4),
                   MID = c("NA","FALSE","SOME","MOST","TRUE"), MID.num = c(0,1,2,3,4),
                   POST = c("NA","FALSE","SOME","MOST","TRUE"), POST.num = c(0,1,2,3,4))

#tack on numerical entries for metadata status and then code if they always increase or not
#if numbers don't increase from PRE to MID to POST, then we need to investigate, bc we lost metadata somehow during datathon (as opposed to stayed the same or gained)
issues <- df %>% dplyr::select(project_index,project_acc_bioprj,Metadata_Curator,Quality_Controller,contains("has")) %>% mutate_all(as.character) %>%
  pivot_longer(., 5:ncol(.), names_to = "metadatadetailed", values_to = "status") %>% as.data.frame() %>% 
  mutate(stage = gsub("has|datathoncoordinates|datathoncountry|datathonderivedGeneticDataX|datathonenvironmentalMedium|datathonhabitat|datathonlocality|datathonmaterialSampleID|datathonpermitInformation|datathonpreservative|datathonpublication|datathonmaterialSampleID|datathonyearCollected","",metadatadetailed)) %>% 
  mutate(metadata = gsub("has|MID|POST|PRE","",metadatadetailed)) %>% 
  dplyr::select(project_index,project_acc_bioprj,status,Metadata_Curator,Quality_Controller,stage,metadata) %>% 
  pivot_wider(., names_from = "stage",values_from = "status") %>% 
  merge(., nums %>% dplyr::select(PRE,PRE.num), by = "PRE", all.x = T) %>%
  merge(., nums %>% dplyr::select(MID,MID.num), by = "MID", all.x = T) %>%
  merge(., nums %>% dplyr::select(POST,POST.num), by = "POST", all.x = T) %>%
  dplyr::select(project_index,project_acc_bioprj,Metadata_Curator,Quality_Controller,metadata,PRE,MID,POST,PRE.num,MID.num,POST.num) %>%
  mutate(data_check = ifelse(POST.num >= MID.num & MID.num >= PRE.num, "okay", "losing_metadata")) %>% 
  mutate(data_check = ifelse(is.na(data_check)==T, "has_NAs",data_check)) %>% 
  filter(data_check == "losing_metadata" | data_check == "has_NAs") %>% 
  dplyr::select(-contains(".num"))

#materialsampIDs have been checked and confirmed to be true losses
#E1476 aka PRJNA544260 has a range of years for all samples, so we left coded as lost metadata for MID and POST (and present for PRE)
issues %>% filter(metadata != "datathonmaterialSampleID")

#write.csv(issues, paste0("working_files/remaining_issues-",Sys.Date(),".csv"))



# *******************************************************************************
# *******************************************************************************
# NEW SECTION - GET ADDITIONAL DATA FOR TEMOPORAL DECAY MODELS ------------------


#***************************************
# get date bioprj was published

# get all records from NCBI
allrecords.raw <- read.csv("/Users/rachel/Desktop/DivDiv/divdiv_collecting_genetic_data/final_files_NCBI_records/11-NCBI_bioprojects__not-human_bacteria_viral_metagenome__BioProjectsANDSRAANDBioSamplesANDTaxonomy-WITHseqdataofinterest.csv") %>% dplyr::select(-X)
allrecords.slim <- allrecords.raw %>% filter(biosample_acc_sra %in% relevantbiosamps$biosample_acc_sra) %>% 
  dplyr::select(biosample_acc_sra, project_acc_bioprj, publicationdate_biosamp, registration_date_bioprj, date_biosamp)
allrecords.slim <- allrecords.slim %>% separate(., registration_date_bioprj, into = c("registration_date_bioprj"), sep = " ", remove = F)
allrecords.slim <- allrecords.slim %>% mutate(check = ifelse(date_biosamp == publicationdate_biosamp, "match", "don't match"))
allrecords.slim %>% group_by(check) %>% summarise(n=n())
#all publicationdate_biosamp and date_biosamp values are the same aka these columns contain duplicate info
allrecords.slim <- allrecords.slim %>% 
  mutate(check = ifelse(publicationdate_biosamp == registration_date_bioprj, "match", "don't match"))
allrecords.slim %>% group_by(check) %>% summarise(n=n())
allrecords.slim %>% dplyr::select(project_acc_bioprj, publicationdate_biosamp, registration_date_bioprj, check) %>%
  distinct() %>% View()
#bioprj usually registered earlier than biosamps
rm(allrecords.slim)

bioprj.dates <- allrecords.raw %>% dplyr::select(project_acc_bioprj, registration_date_bioprj) %>% distinct() %>% 
  separate(., registration_date_bioprj, into = c("registration_date_bioprj"), sep = " ", remove = F) %>% 
  mutate(registration_date_bioprj = as.Date(registration_date_bioprj))

#tack on bioprj dates
df <- merge(df, bioprj.dates, by = "project_acc_bioprj", all.x = T)


#***************************************
# get if author responded or not
df.nonmarine.response <- googlesheets4::range_read(nonmarineSRA, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(project_index,project_acc_bioprj,Author_Response_Date,Authors_Contacted) %>% mutate(datagroup = "nonmarine") %>% 
  mutate(Author_Response_Date = unlist(as.character(Author_Response_Date))) %>% 
  mutate(Authors_Contacted = unlist(as.character(Authors_Contacted)))

df.marine.response <- googlesheets4::range_read(marineSRA, sheet = 1) %>% as.data.frame() %>% 
  dplyr::select(project_index,project_acc_bioprj,Author_Response_Date,Authors_Contacted) %>% mutate(datagroup = "marine") %>%
  mutate(Author_Response_Date = unlist(as.character(Author_Response_Date))) %>% 
  mutate(Authors_Contacted = unlist(as.character(Authors_Contacted)))

bioprj.response <- rbind(df.nonmarine.response,df.marine.response) %>% 
  filter(project_index %in% df$project_index) %>%
  mutate(Author_Response_Date = gsub("NULL",NA,Author_Response_Date)) %>%
  mutate(author_response = ifelse(is.na(Author_Response_Date)==F & Author_Response_Date != "FALSE", "TRUE", Author_Response_Date)) %>% distinct()

#sanity check
bioprj.response %>% group_by(Authors_Contacted, Author_Response_Date, author_response) %>% summarise(n=n()) %>% View()

#keep just T/F col (and drop response date)
bioprj.response <- bioprj.response %>% distinct(project_acc_bioprj, author_response)
bioprj.response %>% group_by(author_response) %>% summarise(n=n())

#tack on if we got an author response or not
df <- merge(df, bioprj.response, by = "project_acc_bioprj", all.x = T)

#two projects have weird registration_date_bioprj of 0001-01-01
#looked them up manually on NCBI website and correcting here
df$registration_date_bioprj[df$project_acc_bioprj == "PRJNA357498"] = "2016-12-14"
df$registration_date_bioprj[df$project_acc_bioprj == "PRJNA361214"] = "2017-01-13"
#one has a date later then 2019, which doesn't jive, bc all datathon bioprjs are pre-2020
#looked back at 2019 version and date should be 9/5/2019
df$registration_date_bioprj[df$project_acc_bioprj == "PRJNA564091"] = "2019-09-05"



#***************************************
#***************************************
#save final parsed output
write.csv(df, "working_files/justrelevantdatathon-bioprjlevel-premidpost-metadatastatuses-1-24-2022.csv", row.names = FALSE)

