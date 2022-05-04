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