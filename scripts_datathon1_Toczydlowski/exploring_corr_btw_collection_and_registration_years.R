#idea: reviewer asked how correlated deposition date and collection date are

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list = ls())
gc()


#read in cleaned but rawest / full form of data for paper2
bioprjs.finaldata <- read.csv("/Users/rachel/geome_datathon/datathon1_working_files/justrelevantdatathon-bioprjlevel-premidpost-metadatastatuses-1-24-2022.csv")
bioprjs.finaldata <- bioprjs.finaldata %>% mutate(registration_date_bioprj = as.Date(registration_date_bioprj)) %>% 
  mutate(year_registration_date_bioprj = as.numeric(format(registration_date_bioprj,'%Y'))) %>% 
  dplyr::select(project_acc_bioprj, registration_date_bioprj, year_registration_date_bioprj)
str(bioprjs.finaldata)

#get list of all BioSamp IDs in relevant datathon projects
dates.qc <- read.delim("/Users/rachel/geome_datathon/datathon1_working_files/paper2_all_QCd_biosample_list.tsv", header = T) %>% 
  dplyr::select(project_acc_bioprj, biosample_acc_sra, yearCollected) %>% distinct()
dates.qc <- dates.qc %>% filter(project_acc_bioprj %in% bioprjs.finaldata$project_acc_bioprj)
str(dates.qc)

#merge
df <- merge(bioprjs.finaldata, dates.qc, by = "project_acc_bioprj", all.x = T, all.y = T)

nrow(df)
df %>% filter(is.na(yearCollected)==T) %>% nrow()
#12632/42175 = 30% of biosamps still missing collection yr after datathon over

#drop NAs
df <- df %>% filter(is.na(yearCollected)==F)

df %>% 
  ggplot(aes(y = year_registration_date_bioprj, x = yearCollected)) + 
  geom_point() +
  geom_smooth(formula = y~x, method = "lm") + 
  theme_bw() + 
  theme(panel.grid = element_blank())
cor(x = df$yearCollected, 
    y = df$year_registration_date_bioprj, 
    use = "pairwise.complete.obs", method = c("pearson"))


just.distinct <- df %>% distinct(project_acc_bioprj, year_registration_date_bioprj, yearCollected)
just.distinct %>%
  ggplot(aes(y = year_registration_date_bioprj, x = yearCollected)) + 
  geom_point() +
  geom_smooth(formula = y~x, method = "lm") + 
  theme_bw() + 
  theme(panel.grid = element_blank())
cor(x = just.distinct$yearCollected, 
    y = just.distinct$year_registration_date_bioprj, 
    use = "pairwise.complete.obs", method = c("pearson"))


straight.mean <- df %>% 
  group_by(project_acc_bioprj) %>% 
  mutate(mean.yearCollected = mean(yearCollected)) %>%
  ungroup() %>% 
  distinct(project_acc_bioprj, mean.yearCollected, year_registration_date_bioprj)
straight.mean %>%
  ggplot(aes(y = year_registration_date_bioprj, x = mean.yearCollected)) + 
  geom_point() +
  geom_smooth(formula = y~x, method = "lm") + 
  theme_bw() + 
  theme(panel.grid = element_blank())
cor(x = straight.mean$mean.yearCollected, 
    y = straight.mean$year_registration_date_bioprj, 
    use = "pairwise.complete.obs", method = c("pearson"))

wt.mean <- df %>%
  group_by(project_acc_bioprj, yearCollected) %>% 
  mutate(n.records = n()) %>% ungroup() %>% 
  distinct(project_acc_bioprj, yearCollected, n.records, year_registration_date_bioprj) %>% 
  group_by(project_acc_bioprj) %>% mutate(n.rows = n()) %>% ungroup() %>% 
  mutate(wt = ifelse(n.rows == 1, 1, n.records)) %>%
  group_by(project_acc_bioprj) %>% 
  mutate(wtmean.yearCollected = weighted.mean(yearCollected,wt)) %>% 
  ungroup() %>% 
  distinct(project_acc_bioprj, year_registration_date_bioprj, wtmean.yearCollected)
wt.mean %>%
  ggplot(aes(y = year_registration_date_bioprj, x = wtmean.yearCollected)) + 
  geom_point() +
  geom_smooth(formula = y~x, method = "lm") + 
  theme_bw() + 
  theme(panel.grid = element_blank())
cor(x = wt.mean$wtmean.yearCollected, 
    y = wt.mean$year_registration_date_bioprj, 
    use = "pairwise.complete.obs", method = c("pearson"))

#zoom in out of curiosity on bulk of data
wt.mean.f <- wt.mean %>% filter(wtmean.yearCollected > 2000)
wt.mean.f %>% 
  ggplot(aes(y = year_registration_date_bioprj, x = wtmean.yearCollected)) + 
  geom_point() +
  geom_smooth(formula = y~x, method = "lm") + 
  theme_bw() + 
  theme(panel.grid = element_blank())
cor(x = wt.mean.f$wtmean.yearCollected, 
    y = wt.mean.f$year_registration_date_bioprj, 
    use = "pairwise.complete.obs", method = c("pearson"))


