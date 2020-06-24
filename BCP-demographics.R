if(.Platform$OS.type == "unix") {
  setwd("/Volumes/GoogleDrive/My Drive/Research/MCDI/MCDI-analysis")
} else {
  setwd("G:/My Drive/Research/MCDI/MCDI-analysis/")
}

# Format demographics, including matching mother's/father's education 
# appropriately

na_omit <- function(x) {
  
  # My own NA omit function that returns NA instead of 'logical(0)' when used
  # on an all-NA list
  
  y <- na.omit(x)
  
  if (length(y) == 0) 
    y <- NA
  
  return(y)
  
}

get_education <- function(x) {
  
  result <- c(NA, NA)
  
  # Loop through in this order;
  for ( i in c("biological_mother", "nonbio_mother", "biological_father",
                "non_specified", "not_answered") ) {
    
    if (!is.na(x[i])) {
      result <- c(x[i], i)
      break
    }
    
  }
  
  return(unlist(unname(result)))
  
}

# Libraries

library(tidyverse)

date <- "200609"

# Get demographic/demographic-eligibility data, just the education for now
demo.eligb <- read_csv(paste0("data/bcp-UMNUNC-demoeligb-", date, ".csv")) %>%
                select_all(~gsub("demographics,", "demo.", .)) %>%
                select_all(~gsub("demographics_eligibility,", "de.", .)) %>%
                select(demo.CandID, de.Candidate_Age, demo.Sex,
                       ends_with("relationship"), ends_with("education")) %>%
                na.omit()

# Reshape a few times so that one column is the parent, and the second column
## is their education level.
demo.eligb.long <- demo.eligb %>%
                    pivot_longer(-c(demo.CandID, de.Candidate_Age)) %>%
                    mutate(name = gsub("de.", "", name)) %>%
                    separate(name, into = c("parent", "x")) 

# Separate into two dfs
del.educ <- filter(demo.eligb.long, x == "education") %>%
              rename(education = value) %>%
              select(-x)
del.rel  <- filter(demo.eligb.long, x == "relationship")%>%
              rename(relationship = value) %>%
              select(-x)

# Merge again, 'del' is demo.eligb.long
del <- left_join(del.rel, del.educ) %>%
        select(-parent) %>%
        distinct()

##

# Pivot wider ...
del.wide <- del %>%
              pivot_wider(id_cols = c(demo.CandID, de.Candidate_Age), 
                          names_from = relationship, 
                          values_from = education,
                          values_fn = list(education = length)) 

# Except there's a few individuals with mutliples, one person with 
## 'non_specified' twice (this is ok), and two with two biomom entries. 
## Presumably, those are errors
multiple <- del.wide %>%
              filter(biological_mother > 1 | non_specified > 1 | biological_father > 1 | nonbio_mother > 1 | not_answered > 1 | nonbio_father > 1)

# Take the higher education for these duplicates (so happens for all 3 to be
## 'grad'), will have to be fixed later
del2 <- del %>%
          filter(paste(demo.CandID, de.Candidate_Age) %in% paste(multiple$demo.CandID, multiple$de.Candidate_Age) &
                 education == "grad")
# Remove original entries
del3 <- del %>%
          filter(!(paste(demo.CandID, de.Candidate_Age) %in% paste(multiple$demo.CandID, multiple$de.Candidate_Age)))

# Create new, cleaned del
del4 <- rbind(del3, del2)

remove(del, del2, del3, del.wide, del.educ, del.rel, demo.eligb.long, multiple)

##

# Pivot wider, this time without duplicates
del.wide2 <- del4 %>%
              pivot_wider(id_cols = c(demo.CandID, de.Candidate_Age), 
                          names_from = relationship, 
                          values_from = education,
                          values_fn = list(education = na_omit)) 

# Lookup table to easily convert buckets into numbers
educ_lut <- tibble(mom_ed = c("jr_high", "some_high", "high", "some_college",
                              "college", "some_grad", "grad", "not_answered"),
                   mom_ed_n = c(8, 10, 12, 14, 16, 18, 20, NA))

# Get education levels
educ <- apply(del.wide2, 1, get_education) %>%
          t() %>%
          as_tibble(.name_repair = "unique") %>%
          rename(mom_ed = ...1, parent_ed = ...2) 

# Join with ID#s
educ <- left_join(educ, educ_lut) %>%
          cbind(del.wide2[, 1:2], .) %>%
          mutate(mom_ed = as_factor(mom_ed), parent_ed = as_factor(parent_ed))

#
# Gender
#

sex <- demo.eligb %>%
        select(demo.CandID, demo.Sex) %>% 
        distinct()

demographics <- left_join(educ, sex)

write_csv(demographics, path = paste0("data/BCP-demographics-", date, ".csv"))
