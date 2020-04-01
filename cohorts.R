if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

# Libraries

library(tidyverse)
library(tidyselect)
library(readxl)
library(Hmisc)
library(lubridate)

cohorts <- read.csv(text = "A2,1,4,7,10,13,
                      A3,2,5,8,11,14,
                      B1,9,12,15,18,21,24
                      B2,10,13,16,19,22,25
                      B3,11,14,17,20,23,26
                      C1,15,21,27,33,,
                      C2,16,22,28,34,,
                      C3,17,23,29,35,,
                      D1,18,24,36,42,,
                      D2,19,25,37,43,,
                      D3,20,26,38,44,,
                      BSLERP1,3,6,9,12,24,
                      BSLERP2,4,7,19,13,24,
                      BSLERP3,5,8,11,14,24,
                      BSLERP4,6,9,12,15,24,", header = FALSE)

cohorts.names <- cohorts[, 1]
cohorts.values <- t(cohorts[, -1]) %>% as.data.frame()
colnames(cohorts.values) <- gsub(" ", "", cohorts.names)

mcdi_all <- read_csv("data/bcp-UMNUNC-mcdi-200131.csv")
s_dict_file <- "data/WS-example.csv"
g_dict_file <- "data/WG-example.csv"

# Clean up mcdi_all
colnames(mcdi_all) <- gsub("demographics,", "demo.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi,", "gest.", colnames(mcdi_all))
colnames(mcdi_all) <- gsub("mcdi_words_sentences,", "sent.", colnames(mcdi_all))

mcdi_all <- mcdi_all %>%
  select(demo.CandID, demo.Visit_label, demo.Gender, demo.Cohort,
         starts_with("gest."), starts_with("sent.")) %>%
  separate(demo.Visit_label, into = c(NA, "demo.ideal_age"),
           sep = "x") %>%
  mutate(demo.ideal_age = as.numeric(gsub("m", "",
                                          demo.ideal_age))) %>%
  rename(data_id = demo.CandID,
         age = demo.ideal_age,
         sex = demo.Gender)

mcdi <- mcdi_all %>%
          select(data_id, age, sex, demo.Cohort, ends_with("Administration"), 
                 ends_with(".Date_taken")) %>%
          filter(gest.Administration == "All" | sent.Administration == "All") %>%
          mutate_at(vars(ends_with(".Date_taken")), 
                    as.Date, format = "%Y-%m-%d") %>%
          mutate_at(vars(data_id, demo.Cohort), as.factor)

# Estimate DoB from date taken
mcdi$dob.g <- mcdi$gest.Date_taken %m-% months(mcdi$age)
mcdi$dob.s <- mcdi$sent.Date_taken %m-% months(mcdi$age)
mcdi$dob <- mcdi$dob.g 
mcdi$dob[is.na(mcdi$dob)] <- mcdi$dob.s[is.na(mcdi$dob)]

# Then calcuate age today from dobs and round
mcdi$age.today <- round((today() - mcdi$dob) / 30)

mcdi_today <- mcdi %>%
                group_by(data_id) %>%
                summarise(age.today = round(mean(age.today)))

# We are only wondering about people less than 36 mo
lt36 <- mcdi_today$data_id[mcdi_today$age.today <= 36]

mcdi.lt36 <- filter(mcdi, data_id %in% lt36)

for (i in levels(mcdi.lt36$demo.Cohort)) {
  
  # message(as.character(i))
  
  x <- filter(mcdi.lt36, demo.Cohort == i) %>%
        mutate(data_id = as.character(data_id))
  
  short_cohort <- gsub("BCP_", "", i) %>%
                    gsub("_", "", .)
  
  if (nrow(x) > 0){
    
    if(short_cohort %in% colnames(cohorts.values))
     lines <- na.omit(cohorts.values[, short_cohort])
    else
      lines <- 0
    
    p <- ggplot(x, 
                 aes(x = age, y = data_id)) +
            geom_point() +
            geom_point(data = filter(mcdi_today, data_id %in% x$data_id), 
                       aes(x = age.today, y = data_id), 
                       color = "red") +
            scale_x_continuous(limits = c(NA, 45)) +
            labs(title = short_cohort) +
            geom_vline(xintercept = lines, color = "blue") 
    print(p)}
  
}


