
source("main_init.R")

# variable ----------------------------------------------------------------

source("bregma2n/get_linked.R") #link

# init input --------------------------------------------------------------

temp_list <- c(list.files(link$data_fixed, full.names = T))

# init export -------------------------------------------------------------

path_result <- "bregma2n/results"
dir.create(path_result)

# run ---------------------------------------------------------------------

for(temp_file in temp_list){
  cat("file: \n", temp_file, "\n")
  source("bregma2n/cells.R")}

# coverage ----------------------------------------------------------------

temp_path = path_result

temp_file_neurites <- temp_list %>% 
  stringr::str_split(pattern= "\\\\") %>% 
  purrr::map(~.[length(.)]) %>%
  stringr::str_replace_all(pattern = "^(\\w+?_){2}", replacement = "") %>%
  unique() %>%
  grep("neuryty",., value = T)

for(file_name in temp_file_neurites){
  temp_files_coverage <- grep(pattern = file_name, x = temp_list, value = T)
  files_coverage <- list(
    anno = grep(pattern = "anno", x = temp_files_coverage, value = T),
    dete = grep(pattern = "dete", x = temp_files_coverage, value = T))
  source("all6n/main/coverage.R")}
