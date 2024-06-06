
# dependency --------------------------------------------------------------

library(readr)

# import ------------------------------------------------------------------

d_dat <- 
  read_delim(
    temp_file, 
    delim = ";", 
    locale = locale(decimal_mark = ","),
    escape_double = FALSE, 
    show_col_types = FALSE,
    trim_ws = TRUE)

# values ------------------------------------------------------------------

temp <- list(switch_control = NA)

# versioning --------------------------------------------------------------

temp$data_version <- 
  temp_file %>% 
  stringr::str_split(pattern= "[\\,\\\\,//]") %>% 
  purrr::map(~.[length(.)]) %>%
  unique()

temp$data_source <- ifelse(grepl("abba", temp$data_version), 
                           "abba", "manual")
temp$data_gender <- ifelse(grepl("female", temp$data_version), 
                           "female", "male")

temp$data_classifier <- "classifier"
if(grepl("neuryty", temp$data_version)){temp$data_classifier = "neuropil"}
if(grepl("strong", temp$data_version)){temp$data_classifier = "strong"}
if(grepl("sharp", temp$data_version)){temp$data_classifier = "sharp"}

# control -----------------------------------------------------------------

temp$what <- "cells"
temp$what_init <- NA

if(temp$data_classifier %in% "neuropil"){
  #cat("\n","Check `neuryty?` class? y/n")
  #temp$what_init <- readline()
  #if(temp$what_init %in% "y"){temp$what <- "neuropil"}
  temp$what <- "neuropil"}

switch(temp$data_source, 
       abba={
         cat("\n","Combine subregions? y/n")
         temp$divide_subregions <- readline()
         if(temp$divide_subregions %in% "y"){d_dat$structure <- val$structures[6]}
         d_dat = d_dat %>% filter(structure %in% val$structures)}, 
       manual={
         d_dat$structure <- val$structures[7]}, 
       {d_dat$structure <- val$structures[7]})

# calculate dete ----------------------------------------------------------

temp$file_type <- NA

if(grepl("dete", temp_file)){
  temp$file_type <- "dete"
  temp$temp_measures <- c("nucleus_area","nucleus_dab_od_mean")
  temp$detection_class <- switch(temp$data_classifier, 
                                 strong = {"strong_stain2"}, 
                                 sharp = {"sharp"},
                                 neuropil = {"neuryty?"})
  r_main <- d_dat %>%
    filter(class %in% temp$detection_class) %>%
    select(image, position_rostral, hemi, structure, name, all_of(temp$temp_measures)) %>%
    group_by(image, structure) %>%
    summarise(mean_nucleus_area = mean(nucleus_area),
              mean_nucleus_dab_od_mean = mean(nucleus_dab_od_mean),
              .groups = "drop")}

# calculate anno ----------------------------------------------------------

if(grepl("anno", temp_file)){
  temp$file_type <- "anno"
  r_main <- d_dat %>%
    rowwise() %>%
    mutate(n_cells = NA) %>%
    mutate(n_cells = 
             ifelse(temp$data_classifier %in% "strong" && temp$what %in% "cells",
                    num_strong_stain2 + num_weak_stain2,
                    n_cells)) %>%
    mutate(n_cells = 
             ifelse(temp$data_classifier %in% "sharp" && temp$what %in% "cells",
                    num_sharp + num_smooth,
                    n_cells)) %>%
    mutate(n_cells =
             ifelse(temp$data_classifier %in% "neuropil" && temp$what %in% "neuropil",
                    `num_neuryty?`,
                    n_cells)) %>%
    mutate(n_cells_fix = n_cells * multiplier,
           area_fix = `area_µm^2` * multiplier,
           area_fix_mm2 = area_fix / 1e6,
           area_fix_um3 = (area_fix * 50), #val$slice_thickness
           area_fix_mm3 = (area_fix * 50)/1e9) %>% 
    group_by(image, structure) %>%
    summarise(n_cells_fix_total = sum(n_cells_fix), 
              area_fix_mm2_total = sum(area_fix_mm2),
              density_mm2 = n_cells_fix_total/area_fix_mm2_total, 
              .groups = "drop")}

# fold change -------------------------------------------------------------

r_fold_main <- util$get_fold(
  temp_measure =
    names(r_main)%>%
    stringr::str_subset("image", negate = T) %>%
    stringr::str_subset("structure", negate = T),
  a = r_main)

# add metadata ------------------------------------------------------------

r_main_full <- r_main %>%
  left_join(m_dat, by = c("image")) %>%
  mutate(classifier = temp$data_classifier,
         what = temp$what) %>%
  arrange(pretreat, treat, sample_number)

r_fold_main_full <- r_fold_main %>%
  left_join(m_dat, by = c("image")) %>%
  mutate(classifier = temp$data_classifier,
         what = temp$what) %>%
  arrange(name, pretreat, treat, sample_number)

# export ------------------------------------------------------------------

temp$dir_subpath <- paste(temp_path$dir_path)

#dir.create(temp$dir_subpath)

temp$file_name <- paste(                        
  temp$file_type, "_",
  temp$what, "_",
  temp$data_gender,"_",
  temp$data_source,"_",
  ifelse(temp$data_source %in% "abba", c(temp$divide_subregions, "_"),""),
  temp$data_classifier, sep = "")

# export results ----------------------------------------------------------

write.csv2(r_main_full, 
           file = paste(temp$dir_subpath,"/raw_",temp$file_name, ".csv", sep = ""))

write.csv2(r_fold_main_full,
           file = paste(temp$dir_subpath,"/fc_",temp$file_name, ".csv", sep = ""))

write.table(as_tibble(temp_file), paste(temp$dir_subpath,"/info_",temp$file_name, ".txt", sep = ""))

# clean -------------------------------------------------------------------

cat("\n", "DONE", "\n ")

rm(d_dat, 
   r_main_full, 
   r_main, 
   r_fold_main, 
   r_fold_main_full)
gc()
