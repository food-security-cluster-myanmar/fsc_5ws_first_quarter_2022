#code for doing the disaggregations 
# this was pretty maddening but you finally got it

fsc %>% 
  filter(humanitarian_or_development == "Humanitarian") %>% 
  group_by(strat_obj) %>%
  summarise_at(vars(child_male:elderly_female), ~ sum(., na.rm = TRUE)) %>% 
  mutate(male = child_male + adult_male + elderly_male,
         female = child_female + adult_female + elderly_female, 
         children = child_male + child_female, 
         adults = adult_male + adult_female, 
         elderly = elderly_male + elderly_female) %>% 
  select(-c(child_male:elderly_female)) %>%
  mutate_at(vars(male:elderly), ~ case_when(strat_obj == "so_1" ~ . * 0.300621067,
                                            strat_obj == "so_2" ~ . * 0.719499879,
                                            strat_obj == "so_3" ~ . * 0.001346787)) %>%
  group_by(strat_obj) %>% 
  # ungroup() %>% 
  summarise_at(vars(male:elderly), ~ sum(., na.rm = TRUE)) %>% 
  adorn_totals("row")

fsc %>% 
  filter(humanitarian_or_development == "Humanitarian") %>% 
  group_by(strat_obj) %>% 
  mutate(sum_disagg = child_male + child_female + adult_male + adult_female +
           elderly_male + elderly_female, 
         diff = beneficiaries - sum_disagg) %>%
  summarise(new_ben = sum(new_beneficiaries), 
            reached_ben = sum(beneficiaries), 
            sum_disagg = sum(sum_disagg, na.rm = TRUE)) %>% 
  mutate(mutliplier = (new_ben / reached_ben) * (sum_disagg / reached_ben))

# this is the plotly by prioritistion groupnp

ben_target <- fsc %>% 
  group_by(admin3_pcode_old) %>% 
  summarise(beneficiaries = sum(new_beneficiaries),
            partners = n_distinct(org_code)) %>% 
  left_join(fs_pin, by = c("admin3_pcode_old" = "admin3_pcode")) %>% 
  mutate(reached_pc = beneficiaries / fs_targeted,
         reached_pc = ifelse(is.infinite(reached_pc), 1, reached_pc),
         fs_targeted = ifelse(fs_targeted == 0 & beneficiaries > 0, 1, fs_targeted),
         fs_targeted = round(fs_targeted, digits = 0)) %>% 
  arrange(desc(reached_pc)) %>% 
  select(state, township, fs_pin, fs_targeted, beneficiaries, reached_pc, partners, group) %>%
  ggplot(aes(x = fs_targeted, y = beneficiaries, colour = group, 
             text = paste0(township, ",", "\n",
                           state, ",", "\n",
                           "beneficiaries: ", beneficiaries, "\n",
                           "target: ", fs_targeted, "\n",
                           "partners: ", partners, "\n",
                           "group: ", group))) + 
  geom_abline(intercept = 0, slope = 1, lty = 2, colour = "red") + 
  geom_point(aes(size = beneficiaries), alpha = 0.8) +
  scale_size_continuous(guide = "none") + 
  scale_x_continuous(trans = "log10", labels = comma) + 
  scale_y_continuous(trans = "log10", labels = comma) +
  scale_colour_manual(values = c("#575C6DFF", "#00204DFF", "#C4B56CFF", "#FFEA46FF")) +
  labs(x = "Beneficiaries", y = "Targeted population", 
       title = "Beneficiaries reached per township vs targeted population by prioritisation group, Q1 2022",
       subtitle = "The red line is 100% of target")

ggplotly(ben_target, tooltip = c("text")) %>% 
  config(displayModeBar = FALSE) %>% 
  layout(title = list(text = paste0("Beneficiaries reached vs targeted population by township, Q1 2022",
                                    "<br>",
                                    "<sup>",
                                    "The red line is 100% of target","</sup>")))

# barplot usd per person
# it's a good plot, just not sure how useful it is
fsc %>%
  filter(!is.na(total_value_usd)) %>% 
  group_by(usd_person_bin) %>% 
  summarise(beneficiaries = sum(new_beneficiaries)) %>% 
  mutate(`%_of_ben` = beneficiaries / sum(beneficiaries)) %>% 
  ggplot(aes(x = beneficiaries, y = usd_person_bin, fill = usd_person_bin)) + 
  geom_col() + 
  geom_text(aes(label = percent(`%_of_ben`)), hjust = "inward", size = 3) + 
  scale_x_continuous(breaks = seq(0, 200000, by = 20000), labels = number_format(scale = 1 / 1000, suffix = "K")) + 
  scale_fill_viridis_d(option = "mako", direction = -1) + 
  theme(legend.position = "none") + 
  labs(title = "Food distribution beneficiaries by USD transfer value per person",
       subtitle = "Excludes 1.7M persons reached through the in-kind modality", 
       x = "Number of beneficiaries", y = "USD value per person") 

# table that is not necessary

fsc %>% filter(location_type == "Camp/IDP site") %>% 
  group_by(state) %>% 
  summarise(count = n(), 
            beneficiaries = sum(new_beneficiaries)) %>% 
  arrange(desc(count)) %>% 
  kable(caption = "Number of camps/IDP sites reached in 2022, by state", format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped", full_width = FALSE, position = "left") 

# modalities plot that is not useful
# not useful at the moment
fsc %>%  
  filter(!is.na(delivery_modality)) %>% 
  group_by(org_code, delivery_modality) %>%
  summarise(beneficiaries = sum(beneficiaries)) %>% 
  mutate(pc = beneficiaries / sum(beneficiaries)) %>% 
  ggplot(aes(x = pc, y = delivery_modality, colour = delivery_modality)) + 
  geom_jitter()

# table activities by food insecurity status and evidence
fsc %>%
  filter(str_detect(activity, "moderate|severe")) %>% 
  mutate(has_evidence = ifelse(is.na(evidence), "no", "yes")) %>% 
  group_by(activity, has_evidence) %>% 
  summarise(beneficiaries = sum(beneficiaries)) %>% 
  group_by(activity) %>% 
  mutate(pc_of_beneficiaries = round(beneficiaries / sum(beneficiaries) * 100, digits = 2)) %>% 
  kable(caption = "Breakdown of activities by evidence provided", 
        format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped")

# comparison by prioritisation group
fsc %>% 
  group_by(admin3_pcode_old) %>% 
  summarise(beneficiaries = sum(new_beneficiaries),
            partners = n_distinct(org_code)) %>% 
  right_join(pin, by = c("admin3_pcode_old" = "admin3_pcode")) %>% 
  replace_na(list(beneficiaries = 0)) %>% 
  mutate(reached_pc = beneficiaries / fs_targeted,
         reached_pc = ifelse(is.infinite(reached_pc), 1, reached_pc),
         fs_targeted = ifelse(fs_targeted == 0 & beneficiaries > 0, 1, fs_targeted),
         fs_targeted = round(fs_targeted, digits = 0)) %>% 
  arrange(desc(reached_pc)) %>% 
  group_by(group) %>% 
  summarise(beneficiaries = sum(beneficiaries),
            fs_targeted = sum(fs_targeted)) %>% 
  mutate(pc_reached = round(beneficiaries / fs_targeted * 100, digits = 2), 
         pc_beneficiaries = round(beneficiaries / sum(beneficiaries) * 100, digits = 2),
         pc_target = round(fs_targeted / sum(fs_targeted) * 100, digits = 2)) %>% 
  select(group, beneficiaries, `%_beneficiaries` = pc_beneficiaries, target = fs_targeted, 
         `%_target` = pc_target, `%_reached` = pc_reached) %>% 
  kable(caption = "2022 Q1 Beneficiaries and percent reached by prioritisation group", 
        format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped")

fsc %>%  
  select(date, org_code, beneficiaries = new_beneficiaries, location, admin3_pcode) %>% 
  rbind(fsc_2021 %>% 
          filter(unique_beneficiaries == "Yes") %>% 
          select(date, org_code, beneficiaries, location, admin3_pcode)) %>% 
  group_by(location, admin3_pcode) %>% 
  slice(which.max(beneficiaries)) %>% 
  group_by(org_code, year = year(date)) %>% 
  summarise(beneficiaries = sum(beneficiaries)) %>%
  pivot_wider(names_from = year, values_from = beneficiaries, names_prefix = "ben_") %>% 
  rowwise() %>% 
  mutate(total_ben = sum(ben_2021, ben_2022, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(pc_diff = (ben_2022 - ben_2021) / ben_2022) %>% 
  arrange(desc(pc_diff))

# yoy changes in beneficiaries reached by partner
fsc %>%
  select(date, org_code, beneficiaries = new_beneficiaries, location, admin3_pcode) %>% 
  rbind(fsc_2021 %>% 
          filter(unique_beneficiaries == "Yes") %>% 
          select(date, org_code, beneficiaries, location, admin3_pcode)) %>% 
  group_by(location, admin3_pcode) %>% 
  slice(which.max(beneficiaries)) %>% 
  group_by(org_code, date) %>% 
  summarise(beneficiaries = sum(beneficiaries)) %>% 
  arrange(date) %>% 
  mutate(cum_ben = cumsum(beneficiaries), 
         year = year(date)) %>%
  select(-beneficiaries, -date) %>% 
  group_by(year, org_code) %>% 
  slice(which.max(cum_ben)) %>% 
  pivot_wider(names_from = year, values_from = cum_ben, names_prefix = "ben_") %>% 
  mutate(yoy = (ben_2022 - ben_2021) / ben_2021) %>% 
  arrange(desc(yoy)) 

# top partners by SO
# i don't know how useful this is, probably best to have it as a DT
cbind(
  fsc %>% 
    filter(strat_obj == "so_1") %>% 
    group_by(org_code) %>% 
    summarise(beneficiaries = sum(new_beneficiaries)) %>% 
    arrange(desc(beneficiaries)) %>%
    rename(`Partners, SO1` = org_code, 
           beneficiaries_SO1 = beneficiaries) %>% 
    head(10), 
  
  fsc %>% 
    filter(strat_obj == "so_2") %>% 
    group_by(org_code) %>% 
    summarise(beneficiaries = sum(new_beneficiaries)) %>% 
    arrange(desc(beneficiaries)) %>%
    rename(`Partners, SO2` = org_code, 
           beneficiaries_SO2 = beneficiaries) %>% 
    head(10),
  
  fsc %>% 
    filter(strat_obj == "so_3") %>% 
    group_by(org_code) %>% 
    summarise(beneficiaries = sum(new_beneficiaries)) %>% 
    arrange(desc(beneficiaries)) %>%
    rename(`Partners, SO3` = org_code, 
           beneficiaries_SO3 = beneficiaries) %>% 
    head(10)
) %>% 
  kable(caption = "Top 10 implementing partners by beneficiaries reached, by HRP indicator", format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped", full_width = FALSE, position = "left") %>%  
  footnote(general = "Figures reflect beneficiaries reached through direct implementation",
           general_title = "")

# cumsum for usd_hhd_bin
fsc %>%
  filter(!is.na(usd_per_hhd) & !is.na(new_beneficiaries)) %>% 
  filter(delivery_modality %in% c("CBT/CVA", "Hybrid (In-kind & CBT/CVA)")) %>%
  group_by(usd_hhd_bin) %>% 
  summarise(households = sum(households)) %>% 
  mutate(cumsum = cumsum(households), 
         rolling_pc = cumsum / max(cumsum))

# the boxplot is so much better than this
fsc %>% 
  group_by(org_code, activity_red) %>% 
  summarise(total_value_usd = sum(total_value_usd, na.rm = TRUE),
            beneficiaries = sum(beneficiaries, na.rm = TRUE), 
            households = sum(households, na.rm = TRUE)) %>% 
  mutate(per_person = round(total_value_usd / beneficiaries, digits = 2), 
         per_hhd = total_value_usd / households, 
         activity_red = str_remove_all(activity_red, "provision of ")) %>% 
  filter(total_value_usd > 0) %>%
  ggplot(aes(x = beneficiaries, y = per_person, colour = activity_red)) + 
  geom_point() + 
  scale_y_continuous(trans = "log10", breaks = c(0, 1, 3, 10, 30, 100, 300)) + 
  scale_x_continuous(trans = "log10", labels = comma) +
  facet_wrap(~activity_red, scales = "free")

# rounds are not need at the moment
fsc %>% filter(org_code == "org_2825" & activity_red == "food distribution") %>% 
  group_by(location) %>% 
  summarise(total_value_usd = sum(total_value_usd), 
            beneficiaries = sum(beneficiaries)) %>% 
  mutate(per_person = total_value_usd / beneficiaries)

fsc %>% 
  filter(!is.na(total_value_usd)) %>% 
  group_by(location, org_code, date, activity_red) %>% 
  summarise(beneficiaries = sum(beneficiaries, na.rm = TRUE), 
            total_value_usd = sum(total_value_usd, na.rm = TRUE)) %>% 
  group_by(location, org_code, activity_red) %>% 
  summarise(beneficiaries = sum(beneficiaries), 
            total_value_usd = sum(total_value_usd), 
            rounds = n()) %>% 
  group_by(org_code, activity_red) %>%  
  summarise(beneficiaries = sum(beneficiaries), 
            total_value_usd = sum(total_value_usd), 
            rounds = round(mean(rounds), digits = 2))

# Agriculture stuff

fsc %>%  count(activity_red)

fsc %>% filter(activity_red %in% c("crop, vegetable and seed kits", "FFS and farmer training", 
                                   "IGA and small grants", "livestock kits")) %>%  
  group_by(state, township, activity_red) %>%  
  summarise(beneficiaries = sum(new_beneficiaries), .groups = "drop") %>% 
  filter(beneficiaries != 0) %>%  
  arrange(desc(beneficiaries)) %>%  
  mutate(full_name = str_wrap(paste0(township, ", ", state))) %>% 
  ggplot(aes(x = reorder(str_wrap(full_name, 12), -beneficiaries), y = beneficiaries)) + 
  geom_col(aes(fill = activity_red)) + 
  theme(axis.text.x = element_text(angle = 45, vjust = .9, hjust = .9)) + 
  labs(x = "Township", y = "Beneficiaries", fill = "Acitvity", 
       title = "Beneficiaries reached by agicultural and livestock activities")

fsc %>% filter(activity_red %in% c("crop, vegetable and seed kits", "FFS and farmer training", 
                                   "IGA and small grants", "livestock kits")) %>%  
  group_by(activity_red) %>% 
  summarise(beneficiaries = sum(new_beneficiaries)) %>% 
  ggplot(aes(x = fct_reorder(activity_red, -beneficiaries), y = beneficiaries)) + 
  geom_col(aes(fill = activity_red)) + 
  geom_text(aes(label = comma(beneficiaries)), size = 3, vjust = -.5) + 
  theme(legend.position = "none") + 
  scale_y_continuous(labels = comma) + 
  labs(x = "", y = "Beneficiaries reached", 
       title = "Number of beneficiaries reached by agricultural activity")

fsc %>%  
  mutate(agricultural_activity = if_else(activity_red %in% c("crop, vegetable and seed kits", "FFS and farmer training", 
                                                             "IGA and small grants", "livestock kits"), "yes", "no"), 
         agricultural_activity = fct_rev(agricultural_activity)) %>% 
  group_by(agricultural_activity) %>% 
  summarise(beneficiaries = sum(new_beneficiaries), 
            states = n_distinct(admin1_pcode), 
            townships = n_distinct(admin3_pcode), 
            partners = n_distinct(org_code)) %>%
  mutate(`%_beneficiaries` = round(beneficiaries / sum(beneficiaries) * 100, digits = 2)) %>% 
  relocate(`%_beneficiaries`, .after = beneficiaries) %>% 
  kable(caption = "Breakdown of agricultural and non-agricultural activities", format.args = list(big.mark = ",")) %>% 
  kable_classic_2("striped")

# everyone in yangon just got 50kg of rice
fsc %>%  filter(state == "Yangon") %>% 
  count(unit)
sample_n(10) %>% 
  pull(unit, quantity)

fsc %>%  filter(state == "Yangon") %>% 
  mutate(kg = ifelse(unit == "MT", quantity * 1000, NA_real_), 
         kg_per_hhd = kg / households) %>% 
  ggplot(aes(x = kg_per_hhd)) + 
  geom_histogram()
summarise(kg_per_hhd = mean(kg_per_hhd))

