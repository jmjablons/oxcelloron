
source("init/main_init.R")
library(readxl)

# val ---------------------------------------------------------------------

path_data <- "bregma2n/data/"
dir.create(path_data)

# input -------------------------------------------------------------------

source("bregma2n/get_linked.R")

# multiplier --------------------------------------------------------------

m_guide <- m_guide %>%
  select(image, position_rostral, hemi, multiplier) %>%
  filter(multiplier != 0, !is.na(multiplier)) %>%
  mutate(multiplier  = as.integer(multiplier))

# fix data ----------------------------------------------------------------

for(temp_file in link$data_raw){
  source("bregma2n/preprocess_by_guide.R")}
