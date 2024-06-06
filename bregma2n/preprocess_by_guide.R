
# dependency --------------------------------------------------------------

library(readr)

# input -------------------------------------------------------------------

#temp_file <- c(choose.files()) #comment if batch mode

# versioning --------------------------------------------------------------

val$data_version <- 
  temp_file %>% 
  stringr::str_split(pattern= "[\\,\\\\,//]") %>% 
  purrr::map(~.[length(.)]) %>% 
  unlist() %>% 
  unique()

# inform ------------------------------------------------------------------

cat("File processed:\n")
cat(val$data_version, "\n")

# read file ---------------------------------------------------------------

d_dat <- 
  read_delim(
    temp_file, 
    delim = ";", 
    locale = locale(decimal_mark = ","),
    escape_double = FALSE, 
    show_col_types = FALSE,
    trim_ws = TRUE)

# fix ---------------------------------------------------------------------

if (grepl("annotations", val$data_version)) {
  d_dat= d_dat %>%
    left_join(m_guide) %>%
    filter(!is.na(multiplier))}

if (grepl("detections", val$data_version)) {
  # WARNING: BE SURE MULTIPLIER IS AN INTEGER
  d_dat <- d_dat %>%
    left_join(m_guide %>% 
                select(image, position_rostral, hemi, multiplier)) %>%
    filter(!is.na(multiplier)) %>% #remove
    bind_rows(util$get_multiplied_data(., .m_scheme = m_guide))} 

# save --------------------------------------------------------------------

write.csv2(d_dat, file = paste(path_data,"/", val$data_version,
                                  "_bregma2n_fixed", sep = ""))

# inform ------------------------------------------------------------------

rm(d_dat)
