
source("init/main_init.R")
source("bregmas/get_linked.R")
library(readxl)

# val ---------------------------------------------------------------------

temp <- list()

# read --------------------------------------------------------------------

m_bregma <- read_excel(link$mbregma_male, 
                       col_types = c("text", "text", "text", 
                                     "numeric", "text", "text", "text", 
                                     "text", "numeric", "numeric", "numeric"))

# chosen_one --------------------------------------------------------------

d_bregma <- m_bregma %>%
  #filter(position_rostral %!in% c(1,6,7)) %>%
  rowwise() %>%
  mutate(bregma_mean = mean(c(bregma_min, bregma_max)))

temp$interval <- c(d_bregma$bregma_min, d_bregma$bregma_max) %>% 
  unique() %>% sort(decreasing = T)

temp$range <- d_bregma %>%
  group_by(image) %>%
  summarise(min_min_bregma = min(bregma_min, na.rm = T),
            max_min_bregma = max(bregma_min, na.rm = T),
            min_max_bregma = min(bregma_max, na.rm = T),
            max_max_bregma = max(bregma_max, na.rm = T)) %>%
  summarise(rostral = min(max_max_bregma),
            caudal = max(min_min_bregma))

temp$range <- list(
  rostral = temp$interval[3], #3.72 #3.24
  caudal = temp$interval[5]) #2.76 #2.76

d_bregma %>%
  filter(bregma_mean >= temp$range$caudal,
         bregma_mean <= temp$range$rostral) %>%
  group_by(image) %>%
  summarise(n = n(), which_slice = paste0(position_rostral, collapse = "|"))

temp$final <- d_bregma %>%
  filter(bregma_max >= temp$range$caudal,
         bregma_max <= temp$range$rostral) %>%
  group_by(image) %>%
  arrange(-bregma_mean, .by_group = T) %>%
  filter(row_number() < 2)

# calculate ---------------------------------------------------------------

temp$dat <- m_bregma %>%
  group_by(image) %>%
  summarise(min_min_bregma = min(bregma_min, na.rm = T),
            max_min_bregma = max(bregma_min, na.rm = T),
            min_max_bregma = min(bregma_max, na.rm = T),
            max_max_bregma = max(bregma_max, na.rm = T))

# check -------------------------------------------------------------------

temp$dat %>%
  filter(max_min_bregma > max_max_bregma)

temp$dat %>%
  filter(min_min_bregma > min_max_bregma)

# calculate optimistic ----------------------------------------------------

temp$optimistic = temp$dat %>%
  summarise(rostral = min(max_max_bregma),
            caudal = max(min_min_bregma))

# check position rostral --------------------------------------------------

temp$result_optimistic <- m_bregma %>%
  filter(bregma_min < temp$optimistic$rostral,
         bregma_max > temp$optimistic$caudal) %>%
  group_by(image) %>%
  summarise(n = n(), which_slice = paste0(position_rostral, collapse = "|"))

temp$result_optimistic %>%
  group_by(n) %>%
  summarise(n_images = n())

m_bregma %>%
  mutate(dot = ifelse(bregma_min < temp$optimistic$rostral &
                        bregma_max > temp$optimistic$caudal, "in","out")) %>% 
  # filter(bregma_min < temp$optimistic$rostral,
  #        bregma_max > temp$optimistic$caudal) %>%
  rowwise() %>%
  mutate(val = paste0(c(bregma_min,bregma_max),collapse = "|")) %>%
  mutate(val_fix = ifelse(dot == "in", paste0(val,collapse = ""), paste0(c("(",val,")"), collapse = ""))) %>%
  select(-bregma_max, -bregma_min, -dot, -val) %>%
  tidyr::pivot_wider(names_from = position_rostral, values_from = val_fix) %>% View()

# find one  ---------------------------------------------------------------

temp$chosen_one = temp$dat %>%
  summarise(rostral = min(max_max_bregma),
            caudal = max(min_min_bregma))

temp$result_chosen_one <- m_bregma %>%
  filter(bregma_min <= temp$optimistic$rostral,
         bregma_max >= temp$optimistic$caudal) %>%
  group_by(image) %>%
  summarise(n = n(), which_slice = paste0(position_rostral, collapse = "|"))
