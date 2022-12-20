#idea - run some models to explore if age of bioprj predicts if we 
#recovered metadata (that we couldn't find on our own, aka we had to contact authors to try to get it)

#load libraries 
library(dplyr)
library(tidyr)
library(effects)
library(stringr)
library(ggplot2)

rm(list = ls())
gc()



#**************************************************************************************
#*#**************************************************************************************
# NEW SECTION - get data -----------------------------
#get parsed metadata statuses at bioprj level for paper 2
#filtered by: relevant = T, not a domestic or other species we don't care about, not a duplicated project (or one we lost btw 2019 and 2020)

df.full <- read.table("Supplemental_Materials_S6-Datathon_Anonymized_Author_Response.tsv", sep = "\t", header = T)

#recode date to continuous variable: year + julianDay/365 -> year.julianDayFraction
#	then recorde dataset age as (November 7, 2019) - (bioproject registration year)
presentDate <- as.numeric(paste0(2019,substr(as.numeric(format(as.Date("2019-11-07"), "%j"))/365,2,7)))


df.full <- df.full %>% mutate(registration_date_bioprj = as.Date(registration_date_bioprj, format = "%m/%d/%y")) %>% 
  mutate(registration_date_bioprj_julianday = format(registration_date_bioprj, "%j")) %>% 
  mutate(registration_date_bioprj_juliandayfrac = round(as.numeric(registration_date_bioprj_julianday)/365,5)) %>% 
  mutate(registration_date_bioprj_juliandayfracchar = substr(as.character(registration_date_bioprj_juliandayfrac),2,7)) %>% 
  mutate(registration_date_bioprj_year = format(registration_date_bioprj, "%Y")) %>% 
  mutate(registration_date_bioprj_julian = as.numeric(paste0(registration_date_bioprj_year,registration_date_bioprj_julianday))) %>% 
  mutate(registration_date_bioprj_julianCont = presentDate-as.numeric(paste0(registration_date_bioprj_year,registration_date_bioprj_juliandayfracchar)))



#**************************************************************************************
#**************************************************************************************
# NEW SECTION - model 1 - P of metadata present ~ bioprj age ---------------

# notes - 
# want to know, for all 492 bioprjs in our list, how does age of bioprj affect if metadata is present or not

# going to focus on spatiotemp metadata category but run one model per metadata category/type (for suppmat)

# Eric wants to have just metadata present/absent (as opposed to TRUE, MOST, SOME, FALSE coding we have now) so recoding
# aka collapsing these below

#Eric wants to recode to be binary metadata present or absent (present if at least 50% of biosamps have data, absent if not)
df.1 <- df.full %>% dplyr::select(registration_date_bioprj_julian,
                                  registration_date_bioprj_julianCont, Authors_Contacted,
                                  contains("MID", ignore.case = F)) %>%
  mutate_all(list(~str_replace(., "SOME", "FALSE"))) %>% 
  mutate_all(list(~str_replace(., "MOST", "TRUE")))
#add a derived spatiotemporal metadata category
#hasspatiotemp = collection date AND (lat/long OR locality)
df.1 <- df.1 %>% mutate(mid.space.tf = ifelse(hasMIDdatathoncoordinates == "TRUE" | hasMIDdatathonlocality == "TRUE", "TRUE", "FALSE")) %>% 
  mutate(hasMIDspatiotemp = ifelse(mid.space.tf == "TRUE" & hasMIDdatathonyearCollected == "TRUE", "TRUE", "FALSE")) %>% 
  dplyr::select(-mid.space.tf)

if(FALSE){
#sanity check 
df.1 %>% group_by(hasMIDdatathonyearCollected, hasMIDdatathoncoordinates, hasMIDdatathonlocality, hasMIDspatiotemp) %>% 
  summarise(n=n()) %>% as.data.frame()
#recode to be binary 0/1 and make long
df.1 <- df.1 %>% mutate_all(list(~str_replace(., "FALSE", "0"))) %>% 
  mutate_all(list(~str_replace(., "TRUE", "1"))) %>% 
  pivot_longer(., names_to = "metadata", values_to = "status", 6:ncol(.)) %>% 
  mutate(status = as.factor(status), 
         registration_date_bioprj_julianCont = as.numeric(registration_date_bioprj_julianCont))
str(df.1)

#run one non STAN model per metadata category/type quick
for (metadatatype in unique(df.1$metadata)) {
  
  #get subset of data
  df.sub <- df.1 %>% filter(metadata == metadatatype)
  #fit a quick model
  fit <- glm(status ~ registration_date_bioprj_julianCont, data = df.sub, family = binomial)
  print(paste0("modeling metadata type: ", metadatatype))
  print(df.sub %>% group_by(status) %>% summarise(n=n()))
  print(summary(fit))
  plot(allEffects(fit), main = paste0("modeling metadata type: ", metadatatype)) 
}
}

save(df.1,file="models/dataObjs/df1.Robj")

#**************************************************************************************
#**************************************************************************************
# NEW SECTION - model 2 - P of author response ~ bioprj age ---------------

# notes - 
# want to know, for just the bioprjs we contacted authors for, did the age of bioprj affect if 
# we got a response from authors or not

#get subsetted df - filtered to just bioprjs we contacted authors for
df.2 <- df.full %>% filter(Authors_Contacted == "TRUE") %>% 
  dplyr::select(Authors_Contacted, 
                registration_date_bioprj, registration_date_bioprj_julianCont, 
                author_response)
  

#recode response to 0/1
df.2$author_response[df.2$author_response == "FALSE"] = "0"
df.2$author_response[df.2$author_response == "TRUE"] = "1"
df.2 <- df.2 %>% mutate(author_response = as.factor(author_response))

#final checks
df.2 %>% group_by(Authors_Contacted, author_response) %>% summarise(n=n())
df.2 %>% dplyr::select(Authors_Contacted, author_response, registration_date_bioprj_julianCont) %>% str()

if (FALSE) {
#fit a non STAN model quick for fun
fit <- glm(author_response ~ registration_date_bioprj_julianCont, data = df.2, family = binomial)
summary(fit)
plot(allEffects(fit))
}

save(df.2,file="models/dataObjs/df2.Robj")
#**************************************************************************************
#**************************************************************************************
# NEW SECTION - model 3 - P of recovering ANY metadata from authors | they responded ~ bioprj age ---------------

#notes - 
# want to know, of the bioprjs that we contacted authors for and got a response, does age of bioprj
# predict if we gained ANY metadata or not. (for spatiotemp metdata category it's gained ANY amount of space OR time data)
# here counting any increase in metadata as a gain (aka from FALSE to SOME, SOME to TRUE, etc.)

# recorded for each bioprj and type of metadata if we gained any metadata by contacting authors

# if bioprj already had a type of metadata fully present (MID = 3 in below df), then we need to drop it in our models (bc none to gain)
# for spatiotemp metadata category only dropped projects if locality and coords and year were all complete present aka TRUE,
# otherwise there was possibility we could gain space OR time data from contacting authors, so leave it in

# built a spatiotemp category below that says metadata was gained by contacting authors if collection yr increased at all OR 
# locality OR coordinates increased at all

# publication metadata category could be excluded - because we didn't gain any publications from contacting authors

#get subsetted df - only keep datasets that authors responded for
df.3 <- df.full %>% filter(author_response == "TRUE") %>% 
  dplyr::select(registration_date_bioprj_julianCont, author_response, contains("MID"), contains("POST")) %>% 
  mutate(hasMIDdatathonpublication = as.factor(hasMIDdatathonpublication),
         hasPOSTdatathonpublication = as.factor(hasPOSTdatathonpublication))
#build col that says whether we gained metadata from MID to POST or not
df.3 <- df.3 %>% pivot_longer(., 5:ncol(.), names_to = "metadatadetailed", values_to = "status") %>% as.data.frame() %>% 
  mutate(stage = gsub("has|datathoncoordinates|datathoncountry|datathonderivedGeneticDataX|datathonenvironmentalMedium|datathonhabitat|datathonlocality|datathonmaterialSampleID|datathonpermitInformation|datathonpreservative|datathonpublication|datathonmaterialSampleID|datathonyearCollected|spatiotemp","",metadatadetailed)) %>% 
  mutate(metadata = gsub("has|MID|POST|PRE","",metadatadetailed)) %>% 
  dplyr::select(-metadatadetailed)
#build new numeric col so we can see if we gained metadata
df.3$status_num <- NULL
df.3$status_num[df.3$status == "FALSE"] = 0
df.3$status_num[df.3$status == "SOME"] = 1
df.3$status_num[df.3$status == "MOST"] = 2
df.3$status_num[df.3$status == "TRUE"] = 3
df.3 <- df.3 %>% dplyr::select(-status) %>% pivot_wider(., names_from = stage, values_from = status_num) %>% 
  mutate(metadata_gained = as.factor(ifelse(POST > MID, "1", "0")))
#build a correct/finer scale spatiotemp category here that counts metadata gained if any increase in collection yr 
#and any increase in coords or locality
df.3.spatiotemp <- df.3 %>% dplyr::select(registration_date_bioprj_julianCont, 
                                          author_response, metadata, metadata_gained, MID) %>% 
  filter(metadata %in% c("datathonyearCollected","datathoncoordinates","datathonlocality")) %>% distinct() %>% 
  #filter out projects that already had all time, coord, and locality info (no metadata to be gained)
  #and only put a value in for spatiotemp category for projs that didn't already have complete data
  mutate(sum = sum(MID)) %>% filter(sum < 9) %>% 
  ungroup() %>% dplyr::select(-MID, -sum) %>% 
  pivot_wider(., names_from = metadata, values_from = metadata_gained) %>% 
  mutate(gained_space = ifelse(datathoncoordinates == 1 | datathonlocality == 1, 1, 0)) %>% 
  mutate(metadata_gained = ifelse(datathonyearCollected == 1 | gained_space == 1, 1, 0)) %>% 
  mutate(MID = NA, POST = NA, metadata = "spatiotemp")
#sanity check
df.3.spatiotemp %>% group_by(datathonyearCollected, datathoncoordinates, datathonlocality, metadata_gained) %>% 
  summarise(n=n())
#merge back onto full df
df.3.spatiotemp <- df.3.spatiotemp %>% dplyr::select(registration_date_bioprj_julianCont, 
                                                     author_response, metadata, MID, POST, metadata_gained)
df.3 <- rbind(df.3, df.3.spatiotemp)
#check variable coding
df.3 %>% dplyr::select(metadata_gained, registration_date_bioprj_julianCont) %>% str()
# make wide
df.3 <- df.3 %>% filter(MID %in% c(0,1,2,NA)) %>% 
  dplyr::select(-MID,-POST) %>% 
  pivot_wider(., names_from = metadata, values_from = metadata_gained) %>% 
  dplyr::rename("datathonspatiotemp" = "spatiotemp")

if(FALSE){
#run one non STAN model per metadata category/type quick
for (metadatatype in unique(df.3$metadata)) {
  
  #get subset of data
  df.sub <- df.3 %>% filter(metadata == metadatatype)
  #drop projects that already had complete metadata of this type (not fair to count them as no metadata gained)
  if (metadatatype != "spatiotemp") {
  df.sub <- df.sub %>% filter(MID != 3) 
  }
  #fit a quick model
  fit <- glm(metadata_gained ~ registration_date_bioprj_julianCont, data = df.sub, family = binomial)
  print(paste0("modeling metadata type: ", metadatatype))
  print(df.sub %>% group_by(metadata_gained) %>% summarise(n=n()))
  print(summary(fit))
  plot(allEffects(fit), main = paste0("modeling metadata type: ", metadatatype))
  
}
}
  
save(df.3,file="models/dataObjs/df3.Robj")

#**************************************************************************************
#**************************************************************************************
# NEW SECTION - model 4 - P of recovering "COMPLETE" metadata from authors | they responded ~ bioprj age ---------------

# notes - 
# this model looks at ability to gain "complete" metadata aka we collapse
# FALSE and SOME into FALSE and MOST and TRUE into TRUE, then only count
# gains if metadata status switched from FALSE to TRUE after contacting author
# (unlike in Model 3 - where any gain in metadata is a gain, and for spatiotemp gains in space OR time counted)

# for spatiotemp, here we are counting space AND time
# so at MID which projects are missing majority of space and/or time metadata (hasMIDspatiotemp = F)
# and after contacting authors, which projects that were F for hasMIDspatiotemp now are T for hasPOSTspatiotemp
# aka have majority of space AND time metadata after contacting authors 
# (means we may have recovered space or time or both types of metadata from authors)

# like in model 3, need to drop rows where MID = 3, because this means metadata was already "complete"

#subset to just datasets we got a response from authors for
#recode to be binary metadata present or absent (present if at least 50% of biosamps have data, absent if not)
df.4 <- df.full %>% filter(author_response == "TRUE") %>% 
  dplyr::select(registration_date_bioprj_julianCont, Authors_Contacted,
                                  contains("MID", ignore.case = F),
                                  contains("POST", ignore.case = F)) %>%
  mutate_all(list(~str_replace(., "SOME", "FALSE"))) %>% 
  mutate_all(list(~str_replace(., "MOST", "TRUE")))
#add a derived spatiotemporal metadata category
#hasspatiotemp = collection date AND (lat/long OR locality)
df.4 <- df.4 %>% mutate(mid.space.tf = ifelse(hasMIDdatathoncoordinates == "TRUE" | hasMIDdatathonlocality == "TRUE", "TRUE", "FALSE"),
                        post.space.tf = ifelse(hasPOSTdatathoncoordinates == "TRUE" | hasPOSTdatathonlocality == "TRUE", "TRUE", "FALSE")) %>% 
  mutate(hasMIDspatiotemp = ifelse(mid.space.tf == "TRUE" & hasMIDdatathonyearCollected == "TRUE", "TRUE", "FALSE"),
         hasPOSTspatiotemp = ifelse(post.space.tf == "TRUE" & hasPOSTdatathonyearCollected == "TRUE", "TRUE", "FALSE")) %>% 
  dplyr::select(-mid.space.tf, -post.space.tf)
#sanity check 
df.4 %>% group_by(hasMIDdatathonyearCollected, hasMIDdatathoncoordinates, hasMIDdatathonlocality, hasMIDspatiotemp) %>% 
  summarise(n=n()) %>% as.data.frame()
df.4 %>% group_by(hasPOSTdatathonyearCollected, hasPOSTdatathoncoordinates, hasPOSTdatathonlocality, hasPOSTspatiotemp) %>% 
  summarise(n=n()) %>% as.data.frame()
#build col that says whether we gained metadata from MID to POST or not, now that we've collapsed to only TRUE or FALSE statuses
df.4 <- df.4 %>% pivot_longer(., 5:ncol(.), names_to = "metadatadetailed", values_to = "status") %>% as.data.frame() %>% 
  mutate(stage = gsub("has|datathoncoordinates|datathoncountry|datathonderivedGeneticDataX|datathonenvironmentalMedium|datathonhabitat|datathonlocality|datathonmaterialSampleID|datathonpermitInformation|datathonpreservative|datathonpublication|datathonmaterialSampleID|datathonyearCollected|spatiotemp","",metadatadetailed)) %>% 
  mutate(metadata = gsub("has|MID|POST|PRE","",metadatadetailed)) %>% 
  dplyr::select(-metadatadetailed)
#build new numeric col so we can see if we gained metadata
df.4$status_num <- NULL
df.4$status_num[df.4$status == "FALSE"] = 0
df.4$status_num[df.4$status == "TRUE"] = 3
df.4 <- df.4 %>% dplyr::select(-status) %>% pivot_wider(., names_from = stage, values_from = status_num) %>% 
  mutate(metadata_gained = as.factor(ifelse(POST > MID, "1", "0")),
         registration_date_bioprj_julianCont = as.numeric(registration_date_bioprj_julianCont))
#check variable coding
df.4 %>% dplyr::select(metadata_gained, registration_date_bioprj_julianCont) %>% str()
# make wide
df.4 <- df.4 %>% filter(MID %in% c(0,1,2,NA)) %>% 
  dplyr::select(-MID,-POST) %>% 
  pivot_wider(., names_from = metadata, values_from = metadata_gained) %>% 
  dplyr::rename("datathonspatiotemp" = "spatiotemp")


if(FALSE){
#run one non STAN model per metadata category/type quick
for (metadatatype in unique(df.4$metadata)) {
  
  #get subset of data
  df.sub <- df.4 %>% filter(metadata == metadatatype)
  #drop projects that already had complete metadata of this type (not fair to count them as no metadata gained)
  df.sub <- df.sub %>% filter(MID != 3)
  #fit a quick model
  fit <- glm(metadata_gained ~ registration_date_bioprj_julian, data = df.sub, family = binomial)
  print(paste0("modeling metadata type: ", metadatatype))
  print(df.sub %>% group_by(metadata_gained) %>% summarise(n=n()))
  print(summary(fit))
  plot(allEffects(fit), main = paste0("modeling metadata type: ", metadatatype))
  
}
}
  
save(df.4,file="models/dataObjs/df4.Robj")


# END


