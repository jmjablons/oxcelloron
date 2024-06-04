
source("init/main_init.R")
source("linked/get_multiplier.R") #edit to choose cx/hip

# input  ------------------------------------------------------------------

temp_files <- c(choose.files())

# output ------------------------------------------------------------------

temp_dir_path <- paste("data/data_fixed_hip/")
dir.create(temp_dir_path)

# settings ----------------------------------------------------------------

patch <- list(manual_tag = NULL)

# run ---------------------------------------------------------------------

for(temp_file in temp_files){
  source("patch/patch.R")}

# describe ----------------------------------------------------------------

temp_files %>%
  as.data.frame() %>%
  rename(filepath = '.') %>%
  write.table(file = paste(temp_dir_path, 
                           "info_source_data_",
                           util$today, ".txt", sep = ""))

multiplier[c("source_female","source_male")] %>% 
  cbind() %>%
  #as.data.frame() %>%
  write.table(file = paste(temp_dir_path, 
                           "info_source_multiplier_",
                           util$today, ".txt", sep = ""))
