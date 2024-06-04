
source("init/main_init.R")
library(readxl)

# input  ------------------------------------------------------------------

m_bregma <- read_excel("bregmas/m_bregma_preprocessed/m_bregma_female.xlsx")

source("linked/get_multiplier.R")

# settings ----------------------------------------------------------------

temp <- list()

# run ---------------------------------------------------------------------

temp$df <- m_multiplier %>%
  full_join(m_bregma %>%
              mutate(image = as.numeric(image), 
                     position_rostral = as.numeric(position_rostral))) %>%
  mutate(whether_in = ifelse(bregma_min <= val$bregma_range$rostral &
                        bregma_max >= val$bregma_range$caudal, 1,0)) %>%
  filter(whether_in %in% 1) %>% 
  arrange(pretreat, treat, sample_number) %>% arrange(image) #choose

# export ------------------------------------------------------------------

temp$output <- "./bregma2n/metadata/"
dir.create(temp$output)

temp$df %>%
  writexl::write_xlsx(paste0(temp$output,"/guide_raw.xlsx", collapse = ""))

# MANUAL EDIT -------------------------------------------------------------

# COPY CHECK IMAGES -------------------------------------------------------
