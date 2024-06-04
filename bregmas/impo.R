
source("init/main_init.R")
source("bregmas/get_linked.R")
library(readxl)

# val ---------------------------------------------------------------------

temp <- list(mark = "/")
gender = "female" #set here

# import ------------------------------------------------------------------

if(gender %in% "female"){
  sheet_extend = link$sheet_female_set_extend
  sheet_basic = link$sheet_female_set_basic
} else {
  sheet_extend = link$sheet_male_set_extend
  sheet_basic = link$sheet_male_set_basic}

m_bregma_source <- read_excel(link$metadata_bregma, 
                              sheet = sheet_extend) %>% 
  mutate(Plik = as.character(Plik)) %>%
  bind_rows(read_excel(link$metadata_bregma, 
                       sheet = sheet_basic) %>%
              mutate(Plik = as.character(Plik))) %>%
  mutate(skrawek_7 = as.character(skrawek_7))

# bregma metadata ---------------------------------------------------------

m_bregma <- m_bregma_source %>%
  tidyr::pivot_longer(cols = paste0("skrawek_",val$numberslices_min:val$numberslices_max),
                      names_to = "position_rostral", 
                      values_to = "bregma") %>%
  select(
    sample = `nazwa szkiełka`,
    image = Plik,
    gender = Plec,
    ihc = Bialko,
    anatomy = Struktura,
    age = Wiek,
    position_rostral,
    bregma) %>%
  mutate(image = as.character(image)) %>%
  tidyr::separate_wider_delim(cols = sample, 
                              delim = " ", 
                              names = c("pretreat", "treat", "sample_number")) %>%
  mutate(bregma = gsub(x = bregma, pattern = ",",replacement = ".")) %>%
  tidyr::separate(col = bregma, 
                  sep = "/", 
                  into=c("bregma_1","bregma_2"), 
                  remove = T,
                  fill = "right",
                  extra = "warn") %>%
  mutate(bregma_2 = gsub("\\?","", bregma_2)) %>%
  mutate(bregma1 = as.numeric(bregma_1),
         bregma2 = as.numeric(bregma_2)) %>%
  mutate(image = ifelse(grepl(pattern = paste0("\\",temp$mark), x = image),
                        (stringr::str_match(image, pattern = paste0(temp$mark,".{1,}+")) %>%
                           stringr::str_replace(pattern = temp$mark, replacement = "")),image)) %>%
  # beware. only second part of "sth1/sth2" string will remain
  mutate(image = 
           stringr::str_replace(
             pattern = " IHC", 
             replacement = "", image))

# check -------------------------------------------------------------------

m_bregma %>%
  mutate(dif_bregma = bregma1 - bregma2) %>%
  filter(dif_bregma == 0)

m_bregma %>%
  mutate(dif_bregma = bregma1 - bregma2) %>%
  filter(dif_bregma < 0)

# final -------------------------------------------------------------------

m_bregma = m_bregma %>% 
  rowwise() %>%
  mutate(bregma2 = ifelse(is.na(bregma2), bregma1, bregma2)) %>%
  mutate(bregma1 = ifelse(is.na(bregma1), bregma2, bregma1)) %>%
  mutate(bregma_min = min(c_across(c('bregma1', 'bregma2')))) %>%
  mutate(bregma_max = max(c_across(c('bregma1', 'bregma2')))) %>%
  mutate(position_rostral = gsub("skrawek_","", position_rostral)) %>%
  select(-bregma_1, -bregma_2, -bregma1, -bregma2)

# check -------------------------------------------------------------------

m_bregma %>%
  mutate(dif_bregma = bregma_max - bregma_min) %>%
  filter(dif_bregma < 0)

# save --------------------------------------------------------------------

temp$dir = "./bregmas/m_bregma"
dir.create(paste0(temp$dir))
writexl::write_xlsx(m_bregma, 
                    path = paste0(temp$dir,"/m_bregma_",gender,".xlsx"))

# clean -------------------------------------------------------------------

rm(m_bregma_source, m_bregma)
