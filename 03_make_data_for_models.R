library(tidyverse)

# read in outcomes data
d.basc <- readRDS("yr12_basc_invalid_NA.rds")  # N = 344
d.basc.cc <- readRDS("yr12_basc_clean.rds") # N = 336 (8 invalid basc scores)

# read in covariate data
covariates <- read_csv('brain.csv') %>%  # N = 344
  select(id,
         ndvi = ndvi_avg,
         dep_ind = dep_index_avg,
         ecat = ecat_avg,
         bloodPb = Yr12Lead,
         parent_educ = momed1,
         male = gender,
         Age12)

# calculate N missing covariate data
missing_educ <- covariates %>%
  filter(is.na(parent_educ))        # 10 missing parental education

missing_ndvi <- covariates %>%
  filter(is.na(ndvi))               # 0 missing ndvi

missing_ecat <- covariates %>%
  filter(is.na(ecat))               # 0 missing ECAT

missing_dep <- covariates %>%
  filter(is.na(dep_ind))            # 1 missing dep_index

missing_bloodpb <- covariates %>%
  filter(is.na(bloodPb))            # 44 missing bloodPb

# read in lead exposure data
d.airpb.wide <- readRDS("air_pb_wide.rds") # N = 318

## Of the 310 with full BASC profile and air Pb profile, 263 had full covariate information

d_lead_all <- d.airpb.wide %>%
  right_join(covariates, by="id") %>%
  full_join(d.basc, by="id")

saveRDS(d_lead_all, "d_lead_all.rds")

# filter to complete cases
d_lead <- d_lead_all %>%
  na.omit()

saveRDS(d_lead, "d_lead.rds")
