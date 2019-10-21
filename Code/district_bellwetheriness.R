## Estimating bellwetheriness
source("Code/bellwetheriness_functions.R")

# Identify districts where both Liberals and Conservatives have >10% of winning
competitive_district_inds <- which(district_probs$LPC_prob >= 0.15 & district_probs$CPC_prob >= 0.15)

LPC_bellwetherinesses <- all_districts_bellwetheriness("Liberal", competitive_district_inds)
CPC_bellwetherinesses <- all_districts_bellwetheriness("Conservative", competitive_district_inds)

bellwetheriness <- tibble(district_code = data_2019.simple$district_code[competitive_district_inds],
                          name_english = data_2019.simple$name_english[competitive_district_inds],
                          LPC_win_prob = district_probs$LPC_prob[competitive_district_inds],
                          CPC_win_prob = district_probs$CPC_prob[competitive_district_inds],
                          LPC = LPC_bellwetherinesses,
                          CPC = CPC_bellwetherinesses,
                          LPC_rel_logit = log(LPC_win_prob/CPC_win_prob)) %>%
  arrange(abs(LPC_rel_logit))

# Bellwether-o-gram
ggplot(bellwetheriness, aes(x = LPC, y = CPC, col = LPC_rel_logit)) +
  geom_text(aes(label = name_english), size = 3) +
  scale_colour_gradient(low = "blue", high = "red", name = "LPC rel. logit") +
  labs(title = "Bellwether-o-gram", x = "P(LPC gov|LPC district)", y = "P(CPC gov|CPC district)")

# Bellwether Power Index (BPI)
BPI <- bellwetheriness %>%
  mutate(BPI = LPC*CPC,
         description = paste0(name_english, "\nP(LPC): ", round(100*LPC_win_prob), "%\nP(CPC): ", round(100*CPC_win_prob), "%\nBPI: ", 
                              round(BPI, 3))) %>%
  arrange(desc(BPI))

write_rds(BPI, "Shiny-app/bpi.rds")
