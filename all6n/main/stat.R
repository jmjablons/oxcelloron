
require(readr)

# # COMMENT IN BATCH MODE ---------------------------------------------------
# source("init/main_init.R")
# # import ------------------------------------------------------------------
# temp_file <- choose.files()
# # exit --------------------------------------------------------------------
# temp$dir_subpath <- "all6n/results/stat"
# dir.create(temp$dir_subpath)
# settings ----------------------------------------------------------------
# temp <- list()

# version -----------------------------------------------------------------

temp_file_name <- temp_file %>% 
  stringr::str_split(pattern= "\\\\") %>% 
  purrr::map(~.[length(.)]) %>%
  unique() %>%
  sub(pattern = ".csv", replacement = "")

# read --------------------------------------------------------------------

r_fold_main <- read_delim(temp_file, 
                          delim = ";", 
                          escape_double = FALSE, 
                          col_types = cols(...1 = col_skip(), 
                                           sample_number = col_character()), 
                          locale = locale(decimal_mark = ","), 
                          trim_ws = TRUE)

# help --------------------------------------------------------------------

if(!any(grepl("name", names(r_fold_main)))){
  temp_names_info <- c(names(m_dat), "classifier", "what")
  temp_measure <- setdiff(c(names(r_fold_main)), 
                          c(temp_names_info, c("image", "structure")))
  r_fold_main = r_fold_main %>%
    select(c(all_of(temp_measure),image, structure)) %>%
    tidyr::pivot_longer(cols = all_of(temp_measure), 
                        names_to = "name", 
                        values_to = "value") %>%
    left_join(m_dat, by = "image") }

# stat --------------------------------------------------------------------

temp_stat <- list()

temp_stat$info = rbind(c(item = "source", value = temp_file),
                       c(item = "exit", value = temp_file_name)) %>%
  as_tibble()

temp_stat$anova = r_fold_main %>%
  left_join(m_dat) %>%
  group_by(name, structure) %>%
  rstatix::anova_test(value ~ pretreat*treat)

temp_stat$tukey = r_fold_main %>%
  left_join(m_dat) %>%
  group_by(name, structure) %>%
  rstatix::tukey_hsd(value ~ pretreat*treat)

temp_stat$kruskal = r_fold_main %>%
  left_join(m_dat) %>%
  mutate(gr = interaction(pretreat, treat, sep = ":")) %>%
  group_by(name, structure) %>%
  rstatix::kruskal_test(value ~ gr)

temp_stat$dunn = r_fold_main %>%
  left_join(m_dat) %>%
  mutate(gr = interaction(pretreat, treat, sep = ":")) %>%
  group_by(name, structure) %>%
  rstatix::dunn_test(value ~ gr)

# save stat ---------------------------------------------------------------

writexl::write_xlsx(temp_stat, 
                    path = paste(temp$dir_subpath,"/stat_",
                                 temp_file_name, ".xlsx", sep = ""))

# clear -------------------------------------------------------------------

rm(temp_file_name, r_fold_main, temp_stat)
gc()
