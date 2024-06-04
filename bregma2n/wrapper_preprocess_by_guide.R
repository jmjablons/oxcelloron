
source("init/main_init.R")
library(readxl)

# val ---------------------------------------------------------------------

path_data <- "bregma2n/data/"
dir.create(path_data)
# temp_dir_path <- "bregma2n/data/data_fixed"
# dir.create(temp_dir_path)

# input -------------------------------------------------------------------

source("bregma2n/get_linked.R")
source("linked/get_guide.r")

# multiplier --------------------------------------------------------------

m_guide <- m_guide %>%
  select(image, position_rostral, hemi, multiplier = multiplier_in) %>%
  filter(multiplier != 0, !is.na(multiplier)) %>%
  mutate(multiplier  = as.integer(multiplier))

# fix data ----------------------------------------------------------------

temp_list <- list.files(link$data_raw, full.names = T)

for(temp_file in temp_list){
  source("bregma2n/preprocess_by_guide.R")}
