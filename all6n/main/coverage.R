
# dependency --------------------------------------------------------------

library(readr)

# values ------------------------------------------------------------------

temp <- list(switch_control = NA,
             anno = list(),
             dete = list())

# input -------------------------------------------------------------------

stopifnot(grepl("detection", files_coverage$dete))
stopifnot(grepl("annotation", files_coverage$anno))

# ANNOTATIONS -------------------------------------------------------------

temp_file <- files_coverage$anno

d_dat_anno <- 
  read_delim(
    temp_file, 
    delim = ";", 
    locale = locale(decimal_mark = ","),
    show_col_types = FALSE,
    escape_double = FALSE, 
    trim_ws = TRUE)

temp$anno$data_version <- 
  temp_file %>% 
  stringr::str_split(pattern= "\\\\") %>% 
  purrr::map(~.[length(.)]) %>%
  unique() %>%
  gsub(pattern = "annotations", replacement = "") %>%
  gsub(pattern = "anno", replacement = "")

temp$data_source <- ifelse(grepl("abba", temp$anno$data_version), 
                           "abba", "manual")
temp$data_gender <- ifelse(grepl("female", temp$anno$data_version), 
                           "female", "male")

temp$data_classifier <- "classifier"
temp$data_classifier <- if(grepl("neuryty", temp$anno$data_version)){"neuropil"} else {temp$data_classifier}
temp$data_classifier <- if(grepl("strong", temp$anno$data_version)){"classic"} else {temp$data_classifier}
temp$data_classifier <- if(grepl("sharp", temp$anno$data_version)){"sharp"} else {temp$data_classifier}

# DETECTIONS --------------------------------------------------------------

cat("\n Detection file \n ")
temp_file <- files_coverage$dete

d_dat_dete <- 
  read_delim(
    temp_file, 
    delim = ";", 
    locale = locale(decimal_mark = ","),
    escape_double = FALSE, 
    show_col_types = FALSE,
    trim_ws = TRUE) 

# check -------------------------------------------------------------------

temp$dete$data_version <- 
  temp_file %>% 
  stringr::str_split(pattern= "\\\\") %>% 
  purrr::map(~.[length(.)]) %>%
  unique() %>%
  gsub(pattern = "detections", replacement = "") %>%
  gsub(pattern = "dete", replacement = "")

cat("\n Check: ")
stopifnot(all.equal(temp$anno$data_version, temp$dete$data_version))
cat("all right \n ")

# control -----------------------------------------------------------------

if(temp$data_source %in% "abba"){
  cat("\n","Combine subregions? y/n")
  temp$switch_control <- readline()
  d_dat_anno = d_dat_anno %>% filter(structure %in% val$structures)
  d_dat_dete = d_dat_dete %>% filter(structure %in% val$structures)
  if(temp$switch_control %in% "y"){
    d_dat_anno$structure <- val$structures[6]
    d_dat_dete$structure <- val$structures[6]}}

temp$what <- "cells"

if(temp$data_classifier %in% "neuropil"){temp$what <- "neurite"}

if(temp$data_source %in% "manual"){
  d_dat_anno$structure <- val$structures[7]
  d_dat_dete$structure <- val$structures[7]}

# calculate ---------------------------------------------------------------

temp$temp_measures <- c("nucleus_area")

if(temp$data_classifier %in% "neuropil" && temp$what %in% "neurite"){
  r_dete <- d_dat_dete %>%
    filter(class %in% c("neuryty?")) %>%
    select(image, position_rostral, hemi, structure, name, all_of(temp$temp_measures)) %>%
    group_by(image, structure) %>%
    summarise(total_area = sum(nucleus_area),
              .groups = "drop")}

r_anno <- d_dat_anno %>%
  rowwise() %>%
  mutate(structure_area = `area_µm^2` * multiplier) %>% 
  group_by(image, structure) %>%
  summarise(structure_area_total = sum(structure_area),
            .groups = "drop") 

r_coverage <- r_dete %>%
  left_join(r_anno, by = c("image", "structure")) %>%
  rowwise() %>%
  mutate(coverage = total_area/structure_area_total,
         coverage_percentage = coverage*100)

r_fold_coverage <- util$get_fold(
  temp_measure =
    names(r_coverage)%>%
    stringr::str_subset("image", negate = T) %>%
    stringr::str_subset("structure", negate = T),
  a = r_coverage)

# add metadata ------------------------------------------------------------

r_coverage_full <- r_coverage %>%
  left_join(m_dat, by = "image") %>%
  mutate(classifier = temp$data_classifier,
         what = temp$what) %>%
  arrange(pretreat, treat, sample_number)

r_fold_coverage_full <- r_fold_coverage %>%
  left_join(m_dat, by = "image") %>%
  mutate(classifier = temp$data_classifier,
         what = temp$what) %>%
  arrange(name, pretreat, treat, sample_number)

# export ------------------------------------------------------------------

# temp$dir_subpath <- paste(temp_path$dir_path)
# dir.create(temp$dir_subpath)

temp$file_name <- paste(                        
  "coverage", "_",
  temp$what, "_",
  temp$data_gender,"_",
  temp$data_source,"_",
  ifelse(temp$data_source %in% "abba", c(temp$divide_subregions, "_"),""),
  temp$data_classifier, sep = "")

# export results ----------------------------------------------------------

write.csv2(r_coverage_full, 
           file = paste(temp_path,"/raw_",temp$file_name, ".csv", sep = ""))

write.csv2(r_fold_coverage_full,
           file = paste(temp_path,"/fc_",temp$file_name, ".csv", sep = ""))

write.table(as_tibble(c(files_coverage$anno, files_coverage$dete)), 
            paste(temp_path,"/info_",temp$file_name, ".txt", sep = ""))


# clear -------------------------------------------------------------------

rm(d_dat_anno, 
   d_dat_dete,
   r_fold_coverage_full, 
   r_fold_coverage,
   r_coverage,
   r_coverage_full,
   files_coverage,
   r_dete, 
   r_anno)
gc()
