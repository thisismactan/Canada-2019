#### Processing campaign contributions
source("Code/library.R")

## Read in
contribs <- fread_to_tbl("Data/Raw/contribs_2004_2015_submitted.csv") %>%  
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
  mutate(year = case_when(election == "38th general election" ~ 2004,
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
  arrange(year, district, party)

## Fix district names UGH
contribs <- contribs %>%
  mutate(name_english = gsub("--", "–", district),
         name_english = gsub("Ã©", "é", name_english),
         name_english = gsub("Ã¨", "è", name_english),
         name_english = gsub("ÃŽ", "Î", name_english),
         name_english = gsub("Ã‰", "É", name_english),
         name_english = gsub("Ã¢", "â", name_english),
         name_english = gsub("Ã´", "ô", name_english),
         name_english = gsub("Ã®", "î", name_english)) %>%
  mutate(candidate_name = gsub("--", "–", candidate_name),
         candidate_name = gsub("Ã©", "é", candidate_name),
         candidate_name = gsub("Ã¨", "è", candidate_name),
         candidate_name = gsub("ÃŽ", "Î", candidate_name),
         candidate_name = gsub("Ã‰", "É", candidate_name),
         candidate_name = gsub("Ã¢", "â", candidate_name),
         candidate_name = gsub("Ã´", "ô", candidate_name)) %>%
  mutate(candidate_lastname = gsub("--", "–", candidate_lastname),
         candidate_lastname = gsub("Ã©", "é", candidate_lastname),
         candidate_lastname = gsub("Ã¨", "è", candidate_lastname),
         candidate_lastname = gsub("ÃŽ", "Î", candidate_lastname),
         candidate_lastname = gsub("Ã‰", "É", candidate_lastname),
         candidate_lastname = gsub("Ã¢", "â", candidate_lastname),
         candidate_lastname = gsub("Ã´", "ô", candidate_lastname))

contribs_2004_2011 <- contribs %>%
  filter(year <= 2011) %>%
  mutate(name_english = gsub("^Argenteuil–Mirabel$", "Argenteuil–Papineau–Mirabel", name_english),
         name_english = gsub("^Athabasca$", "Fort McMurray–Athabasca", name_english),
         name_english = gsub("^Beauport$", "Beauport–Limoilou", name_english),
         name_english = gsub("^Bonavista–Exploits$", "Bonavista–Gander–Grand Falls–Windsor", name_english),
         name_english = gsub("^Calgary North Centre$", "Calgary Centre-North", name_english),
         name_english = gsub("^Calgary South Centre$", "Calgary Centre", name_english),
         name_english = gsub("^Carleton–Lanark$", "Carleton–Mississippi Mills", name_english),
         name_english = gsub("^Charlesbourg$", "Charlesbourg–Haute-Saint-Charles", name_english),
         name_english = gsub("^Charleswood–St. James$", "Charleswood–St. James–Assiniboia", name_english),
         name_english = gsub("^Charlevoix–Montmorency$", "Montmorency–Charlevoix–Haute-Côte-Nord", name_english),
         name_english = gsub("^Churchill River$", "Desnethé–Missinippi–Churchill River", name_english),
         name_english = gsub("^Clarington–Scugog–Uxbridge$", "Durham", name_english), 
         name_english = gsub("^Dauphin–Swan River$", "Dauphin–Swan River–Marquette", name_english),
         name_english = gsub("^Dewdney–Alouette$", "Pitt Meadows–Maple Ridge–Mission", name_english),
         name_english = gsub("^Edmonton–Beaumont$", "Edmonton–Mill Woods–Beaumont", name_english),
         name_english = gsub("^Fundy$", "Fundy Royal", name_english),
         name_english = gsub("^Grey–Bruce–Owen Sound$", "Bruce–Grey–Owen Sound", name_english),
         name_english = gsub("^Kamloops–Thompson$", "Kamloops–Thompson–Cariboo", name_english),
         name_english = gsub("^Kelowna$", "Kelowna–Lake Country", name_english),
         name_english = gsub("^Middlesex–Kent–Lambton$", "Lambton–Kent–Middlesex", name_english),
         name_english = gsub("^Laurier$", "Laurier–Sainte-Marie", name_english),
         name_english = gsub("^Longueuil$", "Longueuil–Pierre-Boucher", name_english),
         name_english = gsub("^Matapédia–Matane$", "Haute-Gaspésie–La Mitis–Matane–Matapédia", name_english),
         name_english = gsub("^North Nova$", "Cumberland–Colchester–Musquodoboit Valley", name_english),
         name_english = gsub("^North Okanagan–Shuswap$", "Okanagan–Shuswap", name_english),
         name_english = gsub("^^Nunavik–Eeyou$", "Abitibi–Baie-James–Nunavik–Eeyou", name_english),
         name_english = gsub("^Portneuf$", "Portneuf–Jacques-Cartier", name_english),
         name_english = gsub("^Richelieu$", "Bas-Richelieu–Nicolet–Bécancour", name_english),
         name_english = gsub("^Rimouski–Témiscouata$", "Rimouski-Neigette–Témiscouata–Les Basques", name_english),
         name_english = gsub("^Rivière-du-Loup–Montmagny$", "Montmagny–L'Islet–Kamouraska–Rivière-du-Loup", name_english),
         name_english = gsub("^Roberval$", "Roberval–Lac-Saint-Jean", name_english),
         name_english = gsub("^Southern Interior$", "British Columbia Southern Interior", name_english),
         name_english = gsub("^St. Croix–Belleisle$", "New Brunswick Southwest", name_english),
         name_english = gsub("^St. John's North$", "St. John's East", name_english),
         name_english = gsub("^St. John's South$", "St. John's South–Mount Pearl", name_english),
         name_english = gsub("^West Vancouver–Sunshine Coast$", "West Vancouver–Sunshine Coast–Sea to Sky Country", name_english),
         name_english = gsub("^Western Arctic$", "Northwest Territories", name_english)) %>%
  left_join(district_key_2003, by = "name_english")

contribs_2015 <- contribs %>%
  filter(year == 2015) %>%
  left_join(district_key_2013, by = "name_english")

write.csv(contribs_2004_2011, "Data/Processed/campaign_contributions_2004_2011.csv", row.names = FALSE)
write.csv(contribs_2015, "Data/Processed/campaign_contributions_2015.csv", row.names = FALSE)
