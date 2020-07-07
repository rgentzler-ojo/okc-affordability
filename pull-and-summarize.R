# Data: OSCN dispositions on small claims Forcible Entry and Detainer cases in Oklahoma County, 2018-2020
# Goal: Monthly filing and default counts, as well as counts of serial evictions (filings with same plaintiff and defendant in the previous 12 months)

# Notes: There are a few months (Oct 2018, for example) where there were hundreds of dispositions missing, which is why default judgments are so low those months. I defined "serial evictions" as filings in a month where the plaintiff and defendant had appeared in the same case in the previous 6 months. Those numbers are higher than I realized - about 10% of filings each month.

library(ojodb)
library(tibbletime)

### Query ####
connect_ojo()

d <- ojo_tbl("oscn_civ_disps") %>%
  filter(court == "OKLAHOMA",
         casetype == "SC",
         iss_desc %like% "FORC%",
         file_year >= 2017) %>%
  collect()

#### Identify previous cases with same defendant and plaintiff, calculate days since last filing ####
nas <- d %>%
  group_by(casenum) %>%
  mutate(mis = all(is.na(defname) | defname == "") | all(is.na(iss_plaint) | iss_plaint == "")) %>%
  filter(mis == TRUE) %>%
  mutate(caseseq = str_sub(casenum, 9, 13) %>%
           as.numeric)

p <- ojo_tbl("oscn_parties") %>%
  filter(court == "OKLAHOMA",
         casetype == "SC",
         casenum %in% !!nas$casenum) %>%
  collect()

p_short <- p %>%
  group_by(court, casenum) %>%
  summarize(iss_plaint = first(party_name[which(party_type == "Plaintiff")]),
            defname = first(party_name[which(party_type == "Defendant")]))

nas <- nas %>%
  select(-iss_plaint, -defname) %>%
  left_join(p_short)

ojo_disconnect_all()

d <- d %>%
  bind_rows(nas) %>%
  filter(!is.na(defname)) %>%
  group_by(file_date, iss_plaint, defname) %>%
  slice(1) %>%
  ungroup %>%
  group_by(iss_plaint, defname) %>%
  mutate(prev_file = lag(file_date)) %>% # Get previous file date, if available, of matching plaintiff-defendant cases
  mutate(days_since_prev = ymd(file_date) - ymd(prev_file))

## Summarize to case level, taking first plaintiff and defendant name, most recent disposition (and date), and days since last eviction ####
d_case <- d %>%
  group_by(court, casenum, file_date) %>%
  summarize(plaintiff = first(iss_plaint),
            def = first(defname),
            disp_date = last(disp_date),
            disp = last(disp),
            days_since_prev = max(days_since_prev, na.rm = TRUE))

# Fix the -Inf problem
d_case <- d_case %>%
  mutate(days_since_prev = if_else(days_since_prev == -Inf, as.numeric(NA), as.numeric(days_since_prev)))

#Summarize to monthly data by file month for filing variables
d_sum <- d_case %>%
  group_by(file_month = floor_date(ymd(file_date), "month")) %>%
  summarize(n_filed = n(),
            n_serial = sum(days_since_prev < 180, na.rm = TRUE))

# Summarize to monthly data by disposition month for defaults (since the disposition month can be different from file month)
o_sum <- d_case %>%
  filter(str_detect(disp, "DEFAULT")) %>%
  group_by(file_month = floor_date(ymd(disp_date), "month")) %>%
  summarize(n_default = n())

# Join and filter for only 2018 and beyond
d_sum <- left_join(d_sum, o_sum) %>%
  filter(year(file_month) >= 2018)

# Visual check
ggplot(d_sum, aes(file_month, n_filed)) +
  geom_line() +
  geom_line(aes(y = n_default), color = "red") +
  geom_line(aes(y = n_serial), color = "blue") +
  theme_ojo()

# Write csv of results
write_csv(d_sum, paste(Sys.Date(), "Eviction data for OKC affordability study.csv"))
