#written by R.H. Toczydlowski (rhtoczydlowski@gmail.com, ORCID: 0000-0002-8141-2036)

#idea: make figure of metadata status for datathon paper #2

# NOTE - planning to state that we counted written out instances of "not applicable" etc. as missing
# NOTE - for material sample ID, planning to state there was a value in x, x, and/or x NCBI metadata columns and these may or may not be informative/match IDs in associated published papers

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
library(dplyr)          #data wrangling
library(tidyr)          #data wrangling
library(stringr)        #parsing
library(ggplot2)        #graphing


# PLOTTING ----------------------------------------------------------

#get df
rm(list = ls())
gc()

df <- read.csv("working_files/justrelevantdatathon-bioprjlevel-premidpost-metadatastatuses-1-24-2022.csv", header = T)

#convert wide to long for plotting
df.long <- df %>% dplyr::select(project_acc_bioprj,contains("has")) %>% mutate_all(as.character) %>%
  pivot_longer(., 2:ncol(.), names_to = "metadatadetailed", values_to = "status") %>% as.data.frame() %>% 
  mutate(stage = gsub("has|datathoncoordinates|datathoncountry|datathonderivedGeneticDataX|datathonenvironmentalMedium|datathonhabitat|datathonlocality|datathonmaterialSampleID|datathonpermitInformation|datathonpreservative|datathonpublication|datathonmaterialSampleID|datathonyearCollected","",metadatadetailed)) %>% 
  mutate(metadata = gsub("has|MID|POST|PRE","",metadatadetailed))

#reorder some factors
df.long <- df.long %>% 
  mutate(status_ordered = factor(status, levels=c("FALSE","SOME","MOST","TRUE"))) %>%
  mutate(stage_ordered = factor(stage, levels=c("PRE","MID","POST"), labels=c("Original INSDC metadata","Post scouring papers","Post contacting authors"))) %>% 
  mutate(metadata_ordered = factor(metadata, levels = c("datathonpermitInformation","datathonpreservative","datathonderivedGeneticDataX","datathonenvironmentalMedium","datathonhabitat","datathonpublication","datathonyearCollected","datathoncountry","datathonlocality","datathoncoordinates","datathonmaterialSampleID")))

#rename some factors for graphing
df.long <- df.long %>%
  mutate(status_ordered = factor(status_ordered, labels=c("Missing","Present for < 50%","Present for \u2265 50%","Present"))) %>% 
  mutate(metadata_ordered = factor(metadata_ordered, labels=c("Permit ID","Preservative used","Derived genetic data","Enviro. medium","Habitat","Publication DOI","Collection year","Country","Place name","Lat./long.","Sample ID")))


#plot - metadata gained at each step - FIGURE X ---------

#find all types of pre/mid/post combinations (e.g. FALSE_TRUE_TRUE)
df.gained <- df %>% dplyr::select(project_acc_bioprj,contains("has")) %>% mutate_all(as.character) %>%
  pivot_longer(., 2:ncol(.), names_to = "metadatadetailed", values_to = "status") %>% as.data.frame() %>% 
  mutate(stage = gsub("has|datathoncoordinates|datathoncountry|datathonderivedGeneticDataX|datathonenvironmentalMedium|datathonhabitat|datathonlocality|datathonmaterialSampleID|datathonpermitInformation|datathonpreservative|datathonpublication|datathonmaterialSampleID|datathonyearCollected","",metadatadetailed)) %>% 
  mutate(metadata = gsub("has|MID|POST|PRE","",metadatadetailed)) %>% 
  dplyr::select(project_acc_bioprj,status,stage,metadata) %>% 
  pivot_wider(., names_from = "stage",values_from = "status") %>% 
  mutate(stati = paste(PRE,MID,POST,sep = "_"))

key <- read.csv("working_files/key_for_where_we_gained_metadata_from.csv", header = T)

df.gained <- merge(df.gained, key, by = "stati", all.x = T)

#reorder and rename some factors for graphing
df.gained <- df.gained %>% 
  mutate(metadata_ordered = factor(metadata, levels = c("datathonpermitInformation","datathonpreservative","datathonderivedGeneticDataX","datathonenvironmentalMedium","datathonhabitat","datathonpublication","datathonyearCollected","datathoncountry","datathonlocality","datathoncoordinates","datathonmaterialSampleID"))) %>% 
  mutate(metadata_ordered = factor(metadata_ordered, labels=c("Permit ID","Preservative used","Derived genetic data","Enviro. medium","Habitat","Publication DOI","Collection year","Country","Place name","Lat./long.","Sample ID"))) %>% 
  mutate(obtained_from_simple_ordered = factor(obtained_from_simple, levels=c("missing","authors","paper","insdc"), labels=c("Still missing","Contacting authors","Published paper","INSDC"))) %>% 
  mutate(obtained_from_detailed_ordered = factor(obtained_from_detailed, levels=c("missing","authors","authors_most","authors_some","paper","paper_most","paper_some","insdc","insdc_most","insdc_some")))
  

df.gained %>% filter(metadata != "datathonmaterialSampleID") %>% 
  ggplot() + 
  geom_bar(aes(x=metadata_ordered, fill=obtained_from_simple_ordered, y=(..count..)/sum(..count..)), stat = "count", position = "fill", colour = "black", size = 0.15) + 
  scale_fill_manual(values = c("gray40","#3C9196","#97EEA7","#2B9133")) +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Metadata type", y = "Percent of BioProjects", fill = "Metadata\nobtained from:") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 8), 
        legend.key.size = unit(1,"line")) +
  coord_flip()

ggsave("figures/paper2/paper2-metadataobtainedfrom-simple_allmetadata-relevantstudies.pdf", width = 200, height = 150, dpi = 600, units = c("mm"))

#table of values
df.gained %>% group_by(metadata_ordered, obtained_from_simple_ordered) %>% 
  summarise(n=n()) %>% pivot_wider(., names_from = 2, values_from = 3) %>% replace(is.na(.), 0) %>% 
  mutate(total = rowSums(across(where(is.numeric)))) %>% 
  mutate(percent.stillmissing = (`Still missing`/total)*100, percent.authors = (`Contacting authors`/total)*100, 
         percent.papers = (`Published paper`/total)*100, percent.insdc = (INSDC/total)*100) %>% 
  dplyr::select(metadata_ordered, contains("percent"))






# graveyard ------- 


#plot - metadata pre,mid,post datathon - FIGURE X ---------------

#position = "stack" for absolute numbers
ggplot() + 
  geom_bar(data = df.long, aes(x=metadata_ordered, fill=status_ordered, y=(..count..)/sum(..count..)), stat = "count", position = "fill", colour = "black", size = 0.15) + 
  scale_fill_manual(values = c("gray40","#ADEBAD","#5BBA6F","#2A9134")) +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Metadata type", y = "Percent of BioProjects", fill = "Metadata\nstatus:") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 8), 
        legend.key.size = unit(1,"line")) +
  coord_flip() +
  facet_grid(~stage_ordered)

ggsave("figures/paper2/paper2-prepostmid_allmetadata-relevantstudies.pdf", width = 200, height = 150, dpi = 600, units = c("mm"))




# plot - metadata gained at each step - more detailed categories --------------
ggplot() + 
  geom_bar(data = df.gained, aes(x=metadata_ordered, fill=obtained_from_detailed_ordered, y=(..count..)/sum(..count..)), stat = "count", position = "fill", colour = "black", size = 0.15) + 
  scale_fill_manual(values = c("gray40",
                               "#D81159","#F04282","#F47BA7",
                               "#058ED9","#24AFF9","#60C5FB",
                               "#E36414","#FB8B24","#FCAB5F")) +
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Metadata type", y = "Percent of BioProjects", fill = "Metadata\nobtained from:") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 0),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 8), 
        legend.key.size = unit(1,"line")) +
  coord_flip()

ggsave("figures/paper2/paper2-metadataobtainedfrom-detailed_allmetadata-relevantstudies.pdf", width = 200, height = 150, dpi = 600, units = c("mm"))

#table of values
calc_percs <- function(x) {
  (x/total)*100
}

df.gained %>% group_by(metadata_ordered, obtained_from_detailed_ordered) %>% summarise(n=n()) %>%
  mutate(total = sum(n)) %>% mutate(percent = (n/total)*100) %>%
  dplyr::select(metadata_ordered, obtained_from_detailed_ordered, percent) %>% 
  pivot_wider(., names_from = obtained_from_detailed_ordered, values_from = percent) %>% replace(is.na(.), 0)

#





ggplot() + 
  geom_bar(data = df.long, aes(x=stage_ordered, fill=status_ordered, y=(..count..)/sum(..count..)), stat = "count", position = "fill", colour = "black", size = 0.15) + 
  scale_fill_manual(values = c("gray40","#ADEBAD","#5BBA6F","#2A9134")) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(limits = rev(levels(df.long$stage_ordered))) +
  labs(x = "Metadata type", y = "Percent of BioProjects", fill = "Metadata\nstatus:") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        axis.title = element_text(size = 8),
        strip.text = element_text(size = 8), 
        legend.key.size = unit(1,"line")) +
  coord_flip() +
  facet_grid(~metadata_ordered)
ggsave("figures/paper2/paper2-prepostmid_allmetadata-relevantstudies-FLIPPED.pdf", width = 250, height = 50, dpi = 600, units = c("mm"))
