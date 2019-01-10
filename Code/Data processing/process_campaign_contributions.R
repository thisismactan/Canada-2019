#### Processing campaign contributions
source("Code/library.R")

## Read in
contribs <- fread_to_tbl("Data/Raw/contribs_2004_2015.csv") %>%
  filter(grepl("candidate", `Political Entity`, ignore.case = TRUE)) %>%
  dplyr::select(candidate_id = `Recipient ID`,
                candidate_name = Recipient,
                candidate_lastname = `Recipient last name`,
                party = `Political Party of Recipient`,
                district = `Electoral District`,
                election = `Electoral event`,
                amount = `Monetary amount`) %>%
  group_by(candidate_id, candidate_name, candidate_lastname, party, district, election) %>%
  summarise(contributions = sum(amount, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(election_year = case_when(election == "38th general election" ~ 2004,
                                   election == "39th general election" ~ 2006,
                                   election == "40th general election" ~ 2008,
                                   election == "41st general election" ~ 2011,
                                   election == "42nd general election" ~ 2015)) %>%
  na.omit() %>%
  mutate(party = case_when(party == "Conservative Party of Canada" ~ "Conservative",
                           party == "Liberal Party of Canada" ~ "Liberal",
                           party == "New Democratic Party" ~ "NDP",
                           grepl("Bloc", party) ~ "Bloc",
                           party == "Green Party of Canada" ~ "Green")) %>%
  na.omit() %>%
  arrange(election_year, district, party)

write.csv(contribs, "Data/Processed/campaign_contributions_2004_2015.csv", row.names = FALSE)
