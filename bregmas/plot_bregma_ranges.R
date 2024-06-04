
# visualize bregma ranges 
# indicate union part

source("init/main_init.R")
source("bregmas/get_linked.R")
source("linked/get_multiplier.R")

# input -------------------------------------------------------------------

m_bregma <- read_excel(link$mbregma_female, #set here
                       col_types = c("text", "text", "text", 
                                     "numeric", "text", "text", "text", 
                                     "text", "numeric", "numeric", "numeric"))

# val ---------------------------------------------------------------------

temp <- list()

temp$interval <- c(m_bregma$bregma_min, m_bregma$bregma_max) %>% 
  unique() %>% 
  sort(decreasing = T)

# plot --------------------------------------------------------------------

temp_data <- m_bregma %>%
  mutate(image = as.numeric(image), position_rostral = as.numeric(position_rostral)) %>% 
  left_join(m_multiplier) %>%
  mutate(whether_in = "n") %>%
  mutate(whether_in = ifelse(
    whether_in %!in% "y", "n", whether_in)) %>%
  #mutate(n_hemi = ifelse(is.na(n_hemi), NA, n_hemi)) %>%
  mutate(image = as.factor(image),
          n_hemi = factor(n_hemi)) %>%
  rowwise() %>%
  mutate(position_rostral_fix = position_rostral) %>%
  mutate(position_rostral_fix = ifelse(
    hemi %in% "a", position_rostral_fix - 0.2, position_rostral_fix)) %>%
  mutate(position_rostral_fix = ifelse(
    hemi %in% "b", position_rostral_fix + 0.2, position_rostral_fix)) %>%
  mutate(bregma_point = ifelse(bregma_min == bregma_max, bregma_min, NA)) %>%
  # mutate(dot = ifelse(bregma_min < val$bregma_range$rostral &
  #                       bregma_max > val$bregma_range$caudal, "in","out")) %>% 
  # mutate(dot = ifelse(!is.na(bregma_point) && bregma_point <= val$bregma_range$rostral &
  #                       bregma_point >= val$bregma_range$caudal, "in", dot)) %>% 
  util$rename_group()

temp_data %>%
  ggplot(aes(y= position_rostral_fix, group = image, 
             shape = n_hemi, linetype = n_hemi, 
             fill = whether_in, colour = whether_in)) +
  annotate("rect", 
           ymin = 1, ymax = 7, 
           xmin = val$bregma_range$rostral, xmax = val$bregma_range$caudal,
           fill = "#DF76A0", alpha = 0.4) + ##DF76A0
  geom_vline(xintercept = temp$interval, colour = "white", linetype = "dotted")+
  geom_rect(aes(ymin = position_rostral_fix, ymax = position_rostral_fix, 
                xmin = bregma_min, xmax = bregma_max), linewidth = 1)+
            #colour = "black")+
  geom_point(aes(x = bregma_point)) +
  facet_wrap(group~image, ncol = 4,
             scales = "fixed", 
             dir = "v", strip.position = "left")+
  scale_x_reverse(breaks = seq(2, 5,  by = 0.2))+
  scale_y_continuous(breaks = c(1,3,5,7))+
  labs(x = "bregma", y = "slice within slide")+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "lightgray", colour = "lightgray"),
        strip.background = element_rect(colour = "lightgray"),
        panel.spacing.y = unit(0.1, "lines"),
        panel.grid.minor.y = element_line(linewidth = 0.1),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        #axis.title = element_blank(),
        axis.ticks.x = element_line(colour = "gray"),
        axis.text.y = element_text(size = 5),
        axis.text.x = element_text(size = 7),
        axis.line.y = element_blank(),
        strip.text.y.left = element_text(angle = 0))+
  scale_color_manual(values = c("#687578","#182E33"))+
  scale_fill_manual(values = c("#687578", "#182E33"))+
  scale_linetype_manual(values = c("dashed","dotted","solid"))
