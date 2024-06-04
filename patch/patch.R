
# dependency --------------------------------------------------------------

library(readr)

# input -------------------------------------------------------------------

#temp_file <- c(choose.files()) #comment if batch mode

# settings ----------------------------------------------------------------

#temp_dir_path
#multiplier$multiplier_female
#multiplier$multiplier_male

# versioning --------------------------------------------------------------

patch$data_version <- 
  temp_file %>% 
  stringr::str_split(pattern= "\\\\") %>% 
  purrr::map(~.[length(.)]) %>%
  unique() %>%
  stringr::str_replace_all("data_","")

# inform ------------------------------------------------------------------

cat("File processed:")
cat(patch$data_version)

# read file ---------------------------------------------------------------

d_dat <- 
  read_delim(
    temp_file, 
    delim = ";", 
    locale = locale(decimal_mark = ","),
    escape_double = FALSE, 
    show_col_types = FALSE,
    trim_ws = TRUE)

# guide -------------------------------------------------------------------

if (grepl("_female", patch$data_version)) {
  m_multiplier <- multiplier$multiplier_female}

if (grepl("_male", patch$data_version)) {
  m_multiplier <- multiplier$multiplier_male}

# run ---------------------------------------------------------------------

if(grepl("annotations", patch$data_version)){
  d_dat = d_dat %>%
    left_join(m_multiplier) %>%
    filter(multiplier != 0)}

if (grepl("detections", patch$data_version)) {
  # WARNING: BE SURE MULTIPLIER IS AN INTEGER
  d_dat <- d_dat %>%
    left_join(m_multiplier %>% 
                select(image, position_rostral, hemi, multiplier)) %>%
    filter(multiplier != 0) %>% #remove
    bind_rows(util$get_multiplied_data(., .m_scheme = m_multiplier))}

# save --------------------------------------------------------------------

write.csv2(d_dat, file = paste(temp_dir_path, "data", "_",
                               patch$data_version,
                               patch$manual_tag, "_patched", sep = ""))

# inform ------------------------------------------------------------------

# temp_file %>%
#   as.data.frame() %>%
#   rename(source_filepath = '.') %>%
#   write.table(file = paste(temp_dir_path, 
#                            "info_source_data_", 
#                            util$today, ".txt", sep = ""))

cat("SUCCESS")

rm(d_dat)
