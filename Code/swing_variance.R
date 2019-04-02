source("Code/shape_data.R")

## District swing data
district_swings <- results_pre2013 %>%
  dplyr::select(district_code, year, LPC_pct, CPC_pct, NDP_pct, Green_pct, Bloc_pct, LPC_pct_last, CPC_pct_last, NDP_pct_last,
                Green_pct_last, Bloc_pct_last, incumbent) %>%
  mutate(LPC_swing = LPC_pct - LPC_pct_last,
         CPC_swing = CPC_pct - CPC_pct_last,
         NDP_swing = NDP_pct - NDP_pct_last,
         Green_swing = Green_pct - Green_pct_last,
         Bloc_swing = Bloc_pct - Bloc_pct_last,
         province_code = floor(district_code/1000)) %>%
  dplyr::select(district_code, province_code, year, LPC_swing, CPC_swing, NDP_swing, Green_swing, Bloc_swing, incumbent) %>%
  left_join(national_results_pre2013, by = "year") %>%
  left_join(regional_results_pre2013, by = c("year", "province_code")) %>%
  group_by(district_code) %>%
  arrange(district_code, year) %>%
  mutate(LPC_swing_national = LPC_pct_national - lag(LPC_pct_national),
         CPC_swing_national = CPC_pct_national - lag(CPC_pct_national),
         NDP_swing_national = NDP_pct_national - lag(NDP_pct_national),
         Green_swing_national = Green_pct_national - lag(Green_pct_national),
         LPC_swing_regional = LPC_pct_regional - lag(LPC_pct_regional),
         CPC_swing_regional = CPC_pct_regional - lag(CPC_pct_regional),
         NDP_swing_regional = NDP_pct_regional - lag(NDP_pct_regional),
         Green_swing_regional = Green_pct_regional - lag(Green_pct_regional),
         Bloc_swing_regional = Bloc_pct_regional - lag(Bloc_pct_regional)) %>%
  dplyr::select(district_code, province_code, year, contains("swing"), incumbent)

## Linear mixed models
lmm.LPC <- lmer(LPC_swing~LPC_swing_national+I(incumbent == "Liberal")+(1|province_code), data = district_swings)
lmm.CPC <- lmer(CPC_swing~CPC_swing_national+I(incumbent == "Conservative")+(1|province_code), data = district_swings)
lmm.NDP <- lmer(NDP_swing~NDP_swing_national+I(incumbent == "NDP")+(1|province_code), data = district_swings)
lmm.Green <- lmer(Green_swing~NDP_swing_national+(1|province_code), data = district_swings)

summary(lmm.LPC)
summary(lmm.CPC)
summary(lmm.NDP)
summary(lmm.Green)
