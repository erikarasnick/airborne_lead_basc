library(tidyverse)

# read in outcome data
yr12basc <- readRDS(file="yr12_basc.rds")

# define BASC validity variables
d.basc.valid <- yr12basc[ ,c('Yr12_F_RAW','Yr12_CONS_RAW','Yr12_PTRN_RAW')]
names(d.basc.valid) <- c('F','C','R')

# # omit according to cutoff critera
invalid.F <- d.basc.valid$F > 6 | d.basc.valid$F < 0
invalid.R <- d.basc.valid$R > 125 | d.basc.valid$R < 66
invalid.C <- d.basc.valid$C > 17

d.basc <- yr12basc[!(invalid.F | invalid.R | invalid.C), ]
d.basc.missing <- yr12basc[(invalid.F | invalid.R | invalid.C), ]$ID

d.basc <- d.basc %>%
  select(id = ID,
         Anxiety = Yr12_ANX_GC_T,
         Depression = Yr12_DEP_GC_T,
         Somatization = Yr12_SOM_GC_T,
         Aggression = Yr12_AGG_GC_T,
         Conduct = Yr12_CND_GC_T,
         Hyperactivity = Yr12_HYP_GC_T,
         Attention = Yr12_ATN_GC_T,
         Atypicality = Yr12_ATP_GC_T,
         Withdrawal = Yr12_WDL_GC_T)

saveRDS(d.basc, "yr12_basc_clean.Rds")

# alternatively, convert score to NA if invalid
yr12basc[(invalid.F | invalid.R | invalid.C), 2:58] <- NA

yr12basc <- yr12basc %>%
  select(id = ID,
         Anxiety = Yr12_ANX_GC_T,
         Depression = Yr12_DEP_GC_T,
         Somatization = Yr12_SOM_GC_T,
         Aggression = Yr12_AGG_GC_T,
         Conduct = Yr12_CND_GC_T,
         Hyperactivity = Yr12_HYP_GC_T,
         Attention = Yr12_ATN_GC_T,
         Atypicality = Yr12_ATP_GC_T,
         Withdrawal = Yr12_WDL_GC_T)

saveRDS(yr12basc, "yr12_basc_invalid_NA.Rds")


