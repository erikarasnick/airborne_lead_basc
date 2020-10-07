library(tidyverse)

# read in long-format monthly airborne lead estimates
d.airpb <- readRDS("new_pb_age12.Rds")

# convert to wide
d.airpb.wide <- d.airpb %>%
  group_by_at(vars(-new_lead)) %>%  # group by everything other than the value column.
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=age_months, value=new_lead) %>%    # spread
  select(-row_id)

d.airpb.wide <- d.airpb.wide %>%
  select(id:`144`)

names(d.airpb.wide) <- c("id", paste0("age_month", 0:144))
saveRDS(d.airpb.wide, "air_pb_wide.rds")

# save only complete cases
d.airpb.wide.cc <- d.airpb.wide %>%
  na.omit()

saveRDS(d.airpb.wide.cc, "air_pb_wide_cc.rds")
