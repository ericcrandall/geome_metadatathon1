#idea: preparing lists of datasets for Eric Crandall to use in (DIPnet) datathon

#want to generate three lists, with no overlap:
# list1: divdiv datasets with lat/longs located outside of NCBI 
# list2: divdiv datasets without located lat/longs yet
# list3: divdiv datasets we know we need to contact author for if we want to try and get lat/long
# list4: non divdiv (aka micromacro) datasets with model organisms removed and without located lat/longs yet


#note that divdiv datasets filtered to 10+ individuals per biopj/species combo
#note that micromacro datasets filtering to 5+ individuals per bioprj/species combo

#NOTES FOR FUTURE ITERATIONS (not currently implemented in code below) - 
#want to do filtering to N indivs (e.g. 5) BEFORE we filter out lat/long - in case there are some cases
#where some samples have lat/long and some don't (currently we are filtering on groups of 5 indivs in only those that 
#don't have lat/long - but if we include samples with and without lat/long, there may actually be more than 5)

#want to think about if we want to group at biosample or SRA ID when filtering to N indivs
#in at least a few cases it seems like authors lumped a bunch of individuals that were sequenced individually
#together into one biosample (e.g. one biosample per population to represent all individuals that were sampled 
#and sequenced from that population)

#in a few cases it seemed that there was sampleID/name info that was helpful for linking lat and long in the 
#expxml_sra column under name "LIBRARY_NAME"
#bioprj PRJDB3777 is an example of this
#we might want to pull this info out in future iterations when searching for sample names to link sequences to a published paper

#load libraries -----------
library(dplyr)
library(tidyr)
library(stringr)


rm(list = ls())
gc()

# below is for list 4 (without filtering out other lists)
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

#keep just records we had at time of datathon
df.all <- df.all %>% filter(date_this_sequence_first_added_to_our_records == "10-28-2019 - 11-7-2019")

df.all <- df.all %>% mutate(haslatlong.temp = ifelse(lat_long_biosamp == "unknown", "no", "yes")) %>% 
  mutate(haslatlong = ifelse(is.na(haslatlong.temp)==T, "maybe in pub. paper", haslatlong.temp)) %>% dplyr::select(-haslatlong.temp)
df.all %>% group_by(haslatlong) %>% summarise(n=n())

#filter out more bacteria, enviro samples, and viruses that still snuck through
df.all <- df.all %>% filter(division_taxonomy != "Environmental samples") %>% 
  filter(division_taxonomy != "Bacteria") %>% 
  filter(division_taxonomy != "Viruses")

df.all %>% distinct(run_acc_sra) %>% nrow()
#346,353 sequences 
df.all %>% distinct(biosample_acc_sra) %>% nrow()
#288,300 biosamples
df.all %>% distinct(project_acc_bioprj) %>% nrow()
#4,751 bioprjs

#remove weird characters from species names
df.all <- df.all %>% mutate(organism_biosamp_clean = gsub("\\[","",organism_biosamp)) %>% 
  mutate(organism_biosamp_clean = gsub("\\]","",organism_biosamp_clean)) %>% 
  mutate(organism_biosamp_clean = gsub("\\(","",organism_biosamp_clean)) %>% 
  mutate(organism_biosamp_clean = gsub("\\)","",organism_biosamp_clean)) %>% 
  mutate(organism_biosamp_clean = gsub("\\'","",organism_biosamp_clean))

#get list of "species" to filter out (e.g. human pathogens and model orgs.)
species_to_drop <- read.delim("/Users/rachel/Desktop/geome_datathon/NonWildSpecies/nonWildSpecies_final_sources.tsv", header = T)
species_to_drop.models <- species_to_drop %>% filter(Category == "Model Species")
species_to_drop.hpaths <- species_to_drop %>% filter(Category == "Human Pathogen")

#filter out model orgs. and human pathogens
df.nomodels <- df.all %>% filter(!str_detect(organism_biosamp_clean, str_c(species_to_drop.models$Species, collapse="|")))
df <- df.nomodels %>% filter(!str_detect(organism_biosamp_clean, str_c(species_to_drop.hpaths$Species, collapse="|")))
df %>% distinct(run_acc_sra) %>% nrow()
#254,905 sequences
df %>% distinct(biosample_acc_sra) %>% nrow()
#214,119 biosamps
df %>% distinct(project_acc_bioprj) %>% nrow()
#3483 bioprjs

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

df <- df %>% filter(is_domestic == "no")
df %>% distinct(run_acc_sra) %>% nrow()
#177,611 sequences
df %>% distinct(biosample_acc_sra) %>% nrow()
#152,096 biosamps
df %>% distinct(project_acc_bioprj) %>% nrow()
#2,655 bioprjs


#filter to just those that don't have lat/long
df %>% distinct(run_acc_sra,haslatlong) %>% group_by(haslatlong) %>% summarise(n=n()) %>% mutate(total = sum(n))
df <- df %>% filter(haslatlong != "yes")
df %>% distinct(run_acc_sra) %>% nrow()
df %>% distinct(biosample_acc_sra) %>% nrow()
df %>% distinct(project_acc_bioprj) %>% nrow()

df <- df %>% filter(haslatlong != "no")
df %>% distinct(run_acc_sra) %>% nrow()
df %>% distinct(biosample_acc_sra) %>% nrow()
df %>% distinct(project_acc_bioprj) %>% nrow()



# set minimum number of indivs per bioprj/species combo
n.indiv.min = 5

#group at level of biorpj/species and get at least X indivs per combo
filterlist.Xindivs <- df %>%
  group_by(biosample_acc_sra,project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  group_by(project_acc_bioprj,organism_biosamp) %>% summarise(n=n()) %>% 
  filter(n >= n.indiv.min) %>% mutate(link = paste(project_acc_bioprj, organism_biosamp, sep = "_")) %>% 
  mutate(link = gsub(pattern = " ", replacement = "-", link))
dim(filterlist.Xindivs)

#1686 3-17-2022 bioprj/species combos

#get only above list from full records
dim(df)
dim(filterlist.Xindivs)
df.filtered.Xindivs <- df %>% filter(link %in% filterlist.Xindivs$link)
dim(df.filtered.Xindivs)

#105,399 rows (sequence records) 3-17-2022

df.filtered.Xindivs %>% distinct(run_acc_sra) %>% nrow()
#105,187 sequences
df.filtered.Xindivs %>% distinct(biosample_acc_sra) %>% nrow()
#97,685 biosamps
df.filtered.Xindivs %>% distinct(project_acc_bioprj) %>% nrow()
#904 bioprjs


df.filtered.Xindivs.tokeep <- df.filtered.Xindivs

#filter out rows without a run_acc_sra ID (these must be individuals that are listed in bioprjs but were not sequenced)
df.filtered.Xindivs.tokeep <- df.filtered.Xindivs.tokeep %>% filter(is.na(run_acc_sra)==F)
dim(df.filtered.Xindivs.tokeep)

#check for duplicate sequence IDs
#this returns the number of duplicate rows in the current list4 that we are making
df.filtered.Xindivs.tokeep %>% nrow() - df.filtered.Xindivs.tokeep %>% dplyr::select(run_acc_sra) %>% unique() %>% as.data.frame() %>% nrow()

#unclear how the same sequence cooresponds to multiple individuals but it seems like we don't want these sequence data anyways, or to sort out
#what is going on with these samples, so removing any rows with duplicate/identical run_acc_sra's
#below should return same number of rows if filtering code is working correctly
df.filtered.Xindivs.tokeep %>% group_by(run_acc_sra) %>% filter(n()>1) %>% dplyr::select(run_acc_sra,biosample_acc_sra,everything()) %>% arrange(run_acc_sra) %>% nrow() +
  df.filtered.Xindivs.tokeep %>% group_by(run_acc_sra) %>% filter(n()<2) %>% dplyr::select(run_acc_sra,biosample_acc_sra,everything()) %>% arrange(run_acc_sra) %>% nrow()
df.filtered.Xindivs.tokeep %>% nrow()
#if above return equal nrows, filter out duplicate run_acc_sras (all rows containing a duplicate, not leaving any rows with these run_acc_sra's in)
df.filtered.Xindivs.tokeep <- df.filtered.Xindivs.tokeep %>% group_by(run_acc_sra) %>% filter(n()<2) %>% ungroup(.)
dim(df.filtered.Xindivs.tokeep)

#check that this returns 0 now aka each row has a unique run_acc_sra ID
df.filtered.Xindivs.tokeep %>% nrow() - df.filtered.Xindivs.tokeep %>% dplyr::select(run_acc_sra) %>% unique() %>% as.data.frame() %>% nrow()


df.filtered.Xindivs.tokeep %>% distinct(run_acc_sra) %>% nrow()
#104,975 sequences
df.filtered.Xindivs.tokeep %>% distinct(biosample_acc_sra) %>% nrow()
#97,505 biosamps
df.filtered.Xindivs.tokeep %>% distinct(project_acc_bioprj) %>% nrow()
#901 bioprjs
