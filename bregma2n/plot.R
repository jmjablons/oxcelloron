require(readxl)
require(readr)
require(patchwork)
require(ggbeeswarm)

source("init/main_init.R")
source("all6n/main/custom_plot.R")

# val ---------------------------------------------------------------------

gender <- c("male","female")[2]
dir.create("bregma2n/figure/")

# custom ------------------------------------------------------------------

custom_get_plot <- function(temp_measure){
  temp_dat <- temp_data %>% 
    filter(name %in% temp_measure) %>%
    left_join(m_dat) %>%
    util$rename_group() 
  custom_plot(temp_dat, util$signif_reference(temp_dat, .plot_interval = 0.04))}

custom_set_axis <- function(which_plot, min, max, by = 0.1){
  temp_plots[[which_plot]] <<- 
    temp_plots[[which_plot]] + 
    scale_y_continuous(limits = c(min, max), breaks = seq(min, max, by), expand = c(0,0))
  return()}

# basic -------------------------------------------------------------------

temp_file <- c(paste0("bregma2n/results/fc_anno_cells_",gender,"_manual_sharp.csv"), 
               paste0("bregma2n/results/fc_dete_cells_",gender,"_manual_sharp.csv"))

temp_data <- tibble(path = temp_file) %>%
  rowwise() %>%
  do(read_delim(.$path, 
                delim = ";", 
                escape_double = FALSE, 
                col_types = cols(...1 = col_skip(), 
                                 sample_number = col_character()), 
                locale = locale(decimal_mark = ","), 
                trim_ws = TRUE)) %>%
  mutate(name = stringr::str_replace_all(
    name, pattern = "fold_mean of ", replacement = ""))

temp_measures <- temp_data$name %>% unique()

# get stat ----------------------------------------------------------------

temp_file <- c(paste0("bregma2n/stat/stat_fc_anno_cells_",gender,"_manual_sharp.xlsx"), 
               paste0("bregma2n/stat/stat_fc_dete_cells_",gender,"_manual_sharp.xlsx"))

temp_stat <- list()

temp_stat$anova <- tibble(path = temp_file) %>%
  rowwise() %>%
  do(read_excel(.$path, sheet = "anova")) %>%
  mutate(name = stringr::str_replace_all(
    name, pattern = "fold_mean of ", replacement = ""))

temp_stat$posthoc <- tibble(path = temp_file) %>%
  rowwise() %>%
  do(read_excel(.$path, sheet = "tukey")) %>%
  mutate(name = stringr::str_replace_all(
    name, pattern = "fold_mean of ", replacement = ""))

# final plot --------------------------------------------------------------

if(gender %in% "male"){
  accent_color = "#7DD8F0"  
} else {
  accent_color = "#F07DA5"
}

#accent_color = "#F0E67D" 

temp_plots <- lapply(temp_measures, custom_get_plot)

custom_set_axis(1, 0.5, 1.6)
custom_set_axis(2, 0.5, 1.6)
custom_set_axis(3, 0.5, 1.6)
custom_set_axis(4, 0.8, 1.6)
custom_set_axis(5, 0.8, 1.3)

p<-wrap_plots(temp_plots, nrow = 1) +
  patchwork::plot_annotation(tag_levels = "A")

ggsave(plot = p, filename = paste0("bregma2n/figure/plot_",gender,".pdf"), 
       height = 7, width = 14, scale = 1.75, units = "cm")
