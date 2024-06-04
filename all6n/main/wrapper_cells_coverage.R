
source("init/main_init.R")

# variable ----------------------------------------------------------------

temp_path <- list(dir_path = paste("all6n/","results", sep = ""))

# init export -------------------------------------------------------------

dir.create(paste(temp_path$dir_path))

# init input --------------------------------------------------------------

#temp_list <- c(list.files(, full.names = T))
temp_list <- choose.files()

# preprocess & analyse ----------------------------------------------------

for(temp_file in temp_list){
  cat("file: \n", temp_file, "\n")
  source("all6n/main/cells.R")}

cat("CELLS DONE\n\n")

# coverage ----------------------------------------------------------------

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

cat("COVERAGE DONE\n\n")
