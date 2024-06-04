source("init/dependencies.R")
source("init/util.R")
source("init/values.R")
library(readr)
#library(readxl)

# source ------------------------------------------------------------------

source("all8hip/get_linked.R") #link$source_anno #link$source_metadata 
#link$source_dete

source("all8hip/value.R") #color #gender #temp #name

# custom ------------------------------------------------------------------

temp_code_anno <- function(a){a %>% 
    summarise(ncells_total = sum(n_cells_fixed, na.rm = T),
              area_total_um2 = sum(area_um2_fixed, na.rm = T),
              density_total_mm2 = ncells_total/(area_total_um2*1e-6),
              .groups = "drop") %>%
    tidyr::pivot_longer(cols = c(ncells_total, area_total_um2,
                                 density_total_mm2)) %>%
    left_join(m_dat %>% select(-image) %>% unique(), by = "sample_id") %>%
    arrange(pretreat, treat, sample_id)}

temp_mutate <- function(a){a %>%
    rowwise() %>%
    mutate(n_cells_fixed = sum(num_just_cell, num_light_cell, num_maybe_cell, 
                               num_piramid, na.rm = T)*multiplier,
           area_um2_fixed = `area_µm^2`*multiplier)}

temp_code_dete <- function(a){ a %>%
    summarise(cell_dab_mean = mean(nucleus_dab_od_mean, na.rm = T),
              cell_area_mean = mean(nucleus_area, na.rm = T), 
              .groups = "drop") %>%
    tidyr::pivot_longer(cols = c(cell_dab_mean, cell_area_mean)) %>%
    left_join(m_dat %>% select(-image) %>% unique(), by = "sample_id") %>%
    arrange(pretreat, treat, sample_id)}

# import ------------------------------------------------------------------

temp_code <- function(x){
  read_delim(x, 
             delim = ";", escape_double = FALSE, 
             col_types = cols(...1 = col_skip(), 
                              ...2 = col_skip()), 
             locale = locale(decimal_mark = ","), 
             trim_ws = TRUE)}

d_anno <- temp_code(link$source_anno)
d_dete <- temp_code(link$source_dete)
m_dat_source <- readxl::read_excel(link$source_metadata)

# metadata ----------------------------------------------------------------
#overwrite init m_dat

m_dat <- m_dat_source %>%
  select(image, marker, gender, roi, age, sample) %>%
  unique() %>%
  tidyr::separate(sample, into = c("pretreat","treat","sample")) %>%
  tidyr::separate(sample, into = c("sample_number", "part"), sep = 1) %>%
  select(-part) %>%
  tidyr::unite("sample_id", c(pretreat, treat, sample_number),remove = F) %>%
  mutate(pretreat = factor(pretreat, levels = name$meta_pretreat, ordered = T),
         treat = factor(treat, levels = name$meta_treat, ordered = T))

# anno --------------------------------------------------------------------

d_anno = d_anno %>%
  mutate(structure = ifelse(structure %in% temp_structure_pooled, 
                            stringr::str_c("d_",structure), structure))

d_anno_clean <- d_anno %>%
  left_join(m_dat, by = "image") %>%
  mutate(structure = factor(structure, levels = name$structure, 
                            ordered = T)) %>%
  temp_mutate() %>%
  group_by(sample_id, structure) %>%
  temp_code_anno()

d_anno_pooled <- d_anno %>%
  filter(structure %!in% c("ds","vs")) %>%
  mutate(structure = stringr::str_replace(structure, ".{1}_*", "")) %>%
  left_join(m_dat, by = "image") %>%
  temp_mutate() %>%
  group_by(sample_id, structure) %>%
  temp_code_anno()

d_anno_hf <- d_anno %>%
  left_join(m_dat, by = "image") %>%
  filter(structure %!in% c("ds","vs")) %>%
  temp_mutate() %>%
  group_by(sample_id, structure = "hf") %>%
  temp_code_anno()

d_anno_hfsub <- d_anno %>%
  left_join(m_dat, by = "image") %>%
  temp_mutate() %>%
  group_by(sample_id, structure = "hf+sub") %>%
  temp_code_anno()

d_anno_total <- d_anno_clean %>%
  bind_rows(d_anno_pooled) %>%
  bind_rows(d_anno_hf) %>%
  bind_rows(d_anno_hfsub)

# dete --------------------------------------------------------------------

d_dete = d_dete %>%
  mutate(structure = ifelse(structure %in% temp_structure_pooled, 
                            stringr::str_c("d_",structure), structure))

d_dete_clean <- d_dete %>%
  left_join(m_dat, by = "image") %>%
  #mutate(structure = factor(structure, levels = name$structure, 
  # ordered = T)) %>%
  filter(class %in% temp_get_those) %>%
  group_by(sample_id, structure) %>%
  temp_code_dete()

d_dete_pooled <- d_dete %>%
  filter(structure %!in% c("ds","vs")) %>%
  mutate(structure = stringr::str_replace(structure, ".{1}_*", "")) %>%
  left_join(m_dat, by = "image") %>%
  filter(class %in% temp_get_those) %>%
  group_by(sample_id, structure) %>%
  temp_code_dete()

d_dete_hf <- d_dete %>%
  filter(structure %!in% c("ds","vs")) %>%
  left_join(m_dat, by = "image") %>%
  filter(class %in% temp_get_those) %>%
  group_by(sample_id, structure = "hf") %>%
  temp_code_dete()

d_dete_hfsub <- d_dete %>%
  left_join(m_dat, by = "image") %>%
  filter(class %in% temp_get_those) %>%
  group_by(sample_id, structure = "hf+sub") %>%
  temp_code_dete()

d_dete_total <- d_dete_clean %>%
  bind_rows(d_dete_pooled) %>%
  bind_rows(d_dete_hf) %>%
  bind_rows(d_dete_hfsub)

# combine -----------------------------------------------------------------

d_hip_total <- d_anno_total %>%
  bind_rows(d_dete_total)
