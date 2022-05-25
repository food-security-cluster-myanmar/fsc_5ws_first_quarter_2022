# exploraoty analysis

fsc %>% 
  filter(state == "Shan (South)") %>% 
  group_by(activity_red) %>% 
  summarise(partners = n_distinct(org_code), 
            beneficiaries = sum(new_beneficiaries),
            tsp = n_distinct(admin3_pcode),
            vt = n_distinct(admin4_pcode), 
            locations = n_distinct(location))

fsc %>% filter(state == "Shan (South)" & activity_red == "food distribution")

fsc %>% filter(state == "Shan (South)") %>% 
  group_by(beneficiary_type) %>% 
  summarise(beneficiaries = sum(new_beneficiaries))

# activity plot 

fsc %>% filter(state == "Shan (South)") %>%
  group_by(activity_red) %>% 
  summarise(beneficiaries = sum(new_beneficiaries)) %>% 
  mutate(activity_red = fct_reorder(activity_red, beneficiaries)) %>% 
  ggplot(aes(x = activity_red, y = beneficiaries, fill = activity_red)) + 
  geom_col() + 
  geom_text(aes(label = comma(beneficiaries)), vjust = -.5) + 
  scale_y_continuous(labels = comma) + 
  theme(legend.position = "none") + 
  labs(x = "", y = "Beneficiaries", 
       title = "Number of beneficiaries reached by activities in Shan (South)")

# beakdown by partner

fsc %>% filter(state == "Shan (South)") %>% 
  group_by(org_code) %>% 
  summarise(beneficiaries = sum(new_beneficiaries), 
            locations = n_distinct(location)) %>%  
  mutate(org_code = fct_reorder(org_code, beneficiaries)) %>% 
  ggplot(aes(x = beneficiaries, y = org_code, fill = locations)) + 
  geom_col() + 
  geom_text(aes(label = comma(beneficiaries)), hjust = -.3) + 
  scale_x_continuous(labels = comma) + 
  scale_fill_viridis(option = "mako", direction = -1, end = .8) + 
  labs(x = "Beneficiaries", y = "Partner code", 
       title = "Number of beneficiaries by implementing partner", 
       subtitle = "Colour indicates number of locations worked in")

# beneficiary types

fsc %>% filter(state == "Shan (South)") %>% 
  group_by(township, beneficiary_type) %>% 
  summarise(beneficiaries = sum(new_beneficiaries)) %>% 
  kable(caption = "Beneficiaries reached in Shan (South), by type and township", 
        format.args = list(big.mark = ","), table.attr = "style = 'width:60%;'") %>% 
  kable_classic_2("striped")

# plot of targets and pin

targets <- pin %>% 
  filter(state == "Shan (South)") %>% 
  mutate(pc_pin = round(fs_targeted / fs_pin, digits = 3), 
         is_target = "fs_targeted")

fsc %>% filter(state == "Shan (South)") %>% 
  group_by(admin3_pcode) %>% 
  summarise(beneficiaries_2022 = sum(new_beneficiaries)) %>% 
  right_join(pin %>% 
               filter(state == "Shan (South)"), by = "admin3_pcode") %>%
  mutate(pc_reach = beneficiaries_2022 / fs_targeted, 
         has_beneficiaries = if_else(is.na(beneficiaries_2022), "no", "yes"),
         non_target = fs_pin - fs_targeted, 
         pc_pin = round(fs_targeted / fs_pin, digits = 2), 
         township = fct_reorder(township, fs_pin)) %>% 
  pivot_longer(cols = c(fs_targeted, non_target), names_to = "is_target", values_to = "pin") %>% 
  ggplot(aes(x = township, y = pin, fill = is_target)) + 
  geom_col() + 
  geom_text(data = targets, aes(label = percent(pc_pin, accuracy = 0.01), y = fs_pin + 1500)) + 
  labs(x = "", y = "People in need", fill = "",
       title = "Number of people in need vs number of persons targeted", 
       subtitle = "Figures indicate the percentage of PIN targeted") + 
  scale_y_continuous(labels = comma, breaks = seq(0, 200000, by = 20000))