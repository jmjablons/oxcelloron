# save --------------------------------------------------------------------

d_hip_total %>%
  #tidyr::unite("name", c("structure", "name")) %>%
  group_split(name, .keep = T) %>%
  purrr::map(., ~tidyr::pivot_wider(.,names_from = structure, values_from = value)) %>%
  writexl::write_xlsx(paste0("all8hip/results/summary_",gender,"_hip_parv.xlsx"))

# stat --------------------------------------------------------------------

d_hip_total %>%
  group_by(name, structure) %>%
  rstatix::anova_test(value ~ pretreat * treat) %>%
  split(f = as.factor(.$name)) %>% 
  writexl::write_xlsx(paste0("all8hip/results/anova_",gender,"_hip_parv_total.xlsx"))

d_hip_total %>%
  tidyr::unite("gru", c(pretreat, treat), sep = "x", remove = F) %>%
  group_by(name, structure) %>%
  rstatix::kruskal_test(value ~ gru) %>%
  split(f = as.factor(.$name)) %>% 
  writexl::write_xlsx(paste0("all8hip/results/kruskal_",gender,"_hip_parv_total.xlsx"))

d_hip_total %>%
  group_by(name, structure) %>%
  rstatix::tukey_hsd(value ~ pretreat * treat) %>%
  split(f = as.factor(.$name)) %>%
  writexl::write_xlsx(paste0("all8hip/results/tukey_",gender,"_hip_parv_total.xlsx"))

d_hip_total %>%
  tidyr::unite("gru", c(pretreat, treat), sep = "x", remove = F) %>%
  group_by(name, structure) %>%
  rstatix::dunn_test(value ~ gru) %>%
  split(f = as.factor(.$name)) %>%
  writexl::write_xlsx(paste0("all8hip/results/dunn_",gender,"_hip_parv_total.xlsx"))
