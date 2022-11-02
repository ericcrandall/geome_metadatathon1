#idea - got worried that formatting of terms we used to extract/search for metadata 
#for datathon 1 paper were too specific and excluding metadata that was actually present
#ran original search plus two more less specific searches and comapred number of matches
#overall no issues found
#also wanted to check if 5 bioprjs that are in datathon as relevant but not in our 2020 NCBI master list had metadata

#load libraries
library(dplyr)
library(tidyr)

# all records from NCBI (minus our filters like not human)
allrecords.raw <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/final_files_NCBI_records/11-NCBI_bioprojects__not-human_bacteria_viral_metagenome__BioProjectsANDSRAANDBioSamplesANDTaxonomy-WITHseqdataofinterest.csv") %>% dplyr::select(-X)

#get columns with metadata we care about
df.all <- allrecords.raw %>% dplyr::select(link,project_acc_bioprj,organism_biosamp,biosample_acc_sra,run_acc_sra,
                                           collection_date_biosamp,lat_long_biosamp,publication_ID_bioprj,DOI_pubmed,
                                           library_name_expxml_sra,samplename_sampledata_biosamp,
                                           identifiers_biosamp,samplename_identifiers_biosamp,infraspecies_biosamp,
                                           expxml_sra,sampledata_biosamp,
                                           date_this_sequence_first_added_to_our_records,publicationdate_biosamp,
                                           division_taxonomy, contains("via.taxize"),total_number_base_pairs_sra)

#lat/long
#sampledata_biosamp, display_name=\"latitude and longitude\">"
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"latitude and longitude\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"latitude and longitude\"")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "latitude and longitude")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "latitude and longitude")) %>% 
  filter (n_occurs>1) %>% dplyr::select(n_occurs,sampledata_biosamp) %>% View()
# !GOOD!
#when there are two occurances one is <Attribute attribute_name="latitude and longitude"

#publication
#full_fetched_record_bioprj, into = c("garbage","temp"), sep = "<Publication\ id="
#DOI fine
read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/working_files_to_build_NCBI_records/10.7-NCBI_bioprojects__not-human_bacteria_viral_metagenome__CLEAN-BIOPRJFULLRECORDS-WITHseqdataofinterest.csv") %>% 
  mutate(n_occurs = stringr::str_count(full_fetched_record_bioprj, "<Publication\ id=")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/working_files_to_build_NCBI_records/10.7-NCBI_bioprojects__not-human_bacteria_viral_metagenome__CLEAN-BIOPRJFULLRECORDS-WITHseqdataofinterest.csv") %>% 
  mutate(n_occurs = stringr::str_count(full_fetched_record_bioprj, "<Publication\ id=")) %>% 
  filter (n_occurs>1) %>% dplyr::select(n_occurs,full_fetched_record_bioprj) %>% View()
# !GOOD!

#locality
#sampledata_biosamp, "display_name=\"geographic location\">"))
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\"")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "geographic location")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs1 = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\">")) %>%
  mutate(n_occurs2 = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\"")) %>% 
  mutate(n_occurs3 = stringr::str_count(sampledata_biosamp, "geographic location")) %>% 
  group_by(n_occurs1,n_occurs2,n_occurs3) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs1 = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\">")) %>%
  mutate(n_occurs2 = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\"")) %>% 
  mutate(n_occurs3 = stringr::str_count(sampledata_biosamp, "geographic location")) %>% 
  filter(n_occurs1==0 & n_occurs2==0 & n_occurs3>0) %>% 
  dplyr::select(n_occurs3,sampledata_biosamp) %>% View()
# OKAY - missing 1 bioprj with 99 samples that did have locality/country

#country
#fine/same as locality

#date
#sampledata_biosamp, "display_name=\"collection date\">"))
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"collection date\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"collection date\"")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "collection date")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
# !GOOD!

#samp id
#sampledata_biosamp, into = c("garbage","temp"), sep = "sample name"
#identifiers_biosamp, into = c("garbage","temp1"), sep = "Sample name: "
#expxml_sra, into = c("garbage","temp1"), sep = "LIBRARY_NAME"

#derived data

#preservative

#permit

#habitat
#sampledata_biosamp, "display_name=\"broad-scale environmental context\">"))
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"broad-scale environmental context\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"broad-scale environmental context\"")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "broad-scale environmental context")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
# !GOOD!

#enviro medium
#sampledata_biosamp, "display_name=\"environmental medium\">"))
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"environmental medium\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"environmental medium\"")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "environmental medium")) %>% 
  group_by(n_occurs) %>% summarise(n=n())

# !GOOD!






# NOTE!!! below code was written to extract data from 2020 list
# formatting of these fields changed from 2019 to 2020, so 
# we cannot use 2020 code (from datathon paper 1) - bc it won't find any metadata!
# below show this discrepancy

#get 2019 NCBI list
df.all.raw <- read.csv("/Users/rachel/Desktop/DivDiv/collecting_genetic_data_divdiv/final_files_NCBI_records/older/11-NCBI_bioprojects__not-human_bacteria_viral_metagenome__BioProjectsANDSRAANDBioSamplesANDTaxonomy-WITHseqdataofinterest-pre11-9-2020.csv")

#keep just 5 bioprjs we lost in 2020
df.all <- df.all.raw %>% filter(project_acc_bioprj %in% c("PRJNA315895", "PRJNA324830", "PRJNA386149", "PRJNA429104", "PRJNA453553"))

# GET STATUS OF EACH TYPE OF METADATA -----------------------------------------------------
#add on col that says if diff types of metadata are present or not

#lat/long -----
#lat/long extracted from column" sampledata" that is returned by entrez_summary(), db = "biosample" 
#term we pulled out data for: display_name=\"latitude and longitude\
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"latitude and longitude\"")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "latitude")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
# all missing

#publication -----
#publication_ID_bioprj is a column returned by entrez_fetch(), db = "bioproject" 
#term we pulled out data for: <Publication\ id=
#DOI_pubmed is "doi" extracted from column "articleids" retured by entrez_summary(), db = "pubmed" 
#saying publication present if info present in publication_ID_bioprj and/or DOI_pubmed columns

# all missing - looked up bioprjs by hand in NCBI


#locality -----
#locality_sampledata_biosamp extracted from column" sampledata" that is returned by entrez_summary(), db = "biosample" 
#term we pulled out data for: display_name=\"geographic location\">
#figure out how many times "display_name=\"geographic location\">" occurs in sampledata col. (should be once) - so our code to extract it will work
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"geographic location\"")) %>% 
  group_by(project_acc_bioprj,n_occurs) %>% summarise(n=n())

# 4 of 5 bioprjs have country and maybe locality

#year collected -----
#collection_date_biosamp extracted from column" sampledata" that is returned by entrez_summary(), db = "biosample" 
#term we pulled out data for: display_name="collection date">
#re-extracting collection date bc original code seems to have had an issue and not grabbed all dates
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"collection date\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"collection date\"")) %>% 
  group_by(project_acc_bioprj,n_occurs) %>% summarise(n=n())
# 3 of 5 bioprjs have collection date

#material sample id -----

#derived genetic data -----

#preservative -----

#permit -----

#habitat -----
#envirobroad_sampledata_biosamp extracted from column "sampledata" that is returned by entrez_summary, db = "biosample"
#term we pulled out data for:display_name="broad-scale environmental context">
#figure out how many times "display_name=\"broad-scale environmental context\">" occurs in sampledata col. (should be once) - so our code to extract it will work
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"broad-scale environmental context\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"broad-scale environmental context\"")) %>% 
  group_by(project_acc_bioprj,n_occurs) %>% summarise(n=n())

# 1 of 5 bioprj has habitat

#enviro medium -----
#enviromedium_sampledata_biosamp extracted from column "sampledata" that is returned by entrez_summary, db = "biosample"
#term we pulled out data for:display_name="environmental medium"
#figure out how many times "display_name=\"environmental medium\">" occurs in sampledata col. (should be once) - so our code to extract it will work
df.all %>% 
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"environmental medium\">")) %>% 
  group_by(n_occurs) %>% summarise(n=n())
df.all %>%
  mutate(n_occurs = stringr::str_count(sampledata_biosamp, "display_name=\"environmental medium\"")) %>% 
  group_by(project_acc_bioprj,n_occurs) %>% summarise(n=n())

# all missing
