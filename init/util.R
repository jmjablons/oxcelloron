
util <- list()

util$sem <- function (x) {
  x <- stats::na.omit(x)
  data_frame(y = mean(x), 
             ymin = mean(x) - sd(x)/sqrt(length(x)), 
             ymax = mean(x) + sd(x)/sqrt(length(x)), 
             .size = 1)}

util$sem_solo <- function(x) {sd(x)/sqrt(length(x))}

util$min_max <- list(
  min = ~min(.x, na.rm = TRUE), 
  max = ~max(.x, na.rm = TRUE))

util$today <- Sys.Date() %>% format("%Y%m%d")

util$rename_group <- function(a, temp_group = val$name_group){
  a %>% mutate(group = stringr::str_c(pretreat, treat, sep = " x ")) %>%
    mutate(group = factor(group, levels = temp_group))}

`%!in%` <- function(a,b) ! a %in% b

util$var_summary <- function(a, var) {
  a %>% summarise("n_{{var}}" := n(), 
                  "min_{{var}}" := min({{ var }}), 
                  "max_{{var}}" := max({{ var }}),
                  "median_{{var}}" := median({{ var }}),
                  "mean_{{var}}" := mean({{var}}))}

util$vars_summary <- function(data, vars) {
  data %>% summarise(across(
                       {{vars}},
                       .fns = list(
                         n_na = ~ sum(is.na(.x)),
                         mean = mean 
                         #min = min,
                         #max = max
                         #median = median
                         )), 
                     n = n(), 
                     #n_row = nrow(.)
                     )}

util$rename_structure <- function(a, temp = val$structures){
  a %>% mutate(structure = factor(structure, levels = temp, ordered = T))}

temp_plot <- function(a, ...) {a %>%
    left_join(m_dat) %>%
    util$rename_structure(...) %>%
    util$rename_group() %>%
    ggplot(aes(x = group, y = value))+
    stat_summary(fun = mean, size = 1,
                 geom="crossbar", fill = 'gray')+
    stat_summary(fun = mean,
                 geom = "errorbar",
                 fun.max = function(x) mean(x) + sd(x),size =1,
                 fun.min = function(x) mean(x) - sd(x), width = 0.5)+
    geom_hline(yintercept = 1, linetype = "dotted")+
    facet_wrap(name ~ structure, ncol = 4, labeller = label_wrap_gen(width = 4))+
    labs(y = "", x = "")+
    scale_y_continuous(limits = c(0.2, 1.8), expand = c(0,0))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust =1),
          axis.line.y = element_line(),
          legend.position="bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(), 
          panel.background = element_rect(fill = "lightgray", colour = NA),
          axis.ticks.y = element_line())}

get_fold <- function(temp_measure, a = r_annotations){
  
  r_temp <- a %>%
    select(c(all_of(temp_measure),image)) %>%
    tidyr::pivot_longer(cols = all_of(temp_measure), names_to = "measure", values_to = "value") %>%
    group_by(image, measure) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup()
  
  temp <- r_temp %>%
    left_join(m_dat) %>%
    util$rename_group() %>%
    filter(group %in% "VEH x CON") %>%
    group_by(group, stain) %>%
    summarise(mean = mean(value, na.rm = T)) %>%
    ungroup() %>%
    select(-c("group"))
  
  temp_out <- r_temp %>%
    left_join(m_dat %>% select(image, stain)) %>%
    left_join(temp, by = c("stain")) %>%
    mutate(fold_mean = value/mean,
           #relative_change = (value-mean)/mean
    )%>%
    select(-c("mean","value")) %>%
    tidyr::pivot_longer(!c("image","stain","measure"), 
                        names_to = "name", values_to = "value") %>%
    tidyr::unite(col = "name", 
                 c("name", "measure"), 
                 sep = " of ", 
                 remove = TRUE, 
                 na.rm = FALSE)
  
  return(temp_out)}

get_fold_n <- function(a = r_annotations){
  
  r_temp <- a %>%
    group_by(image) %>%
    summarise(value = n()) %>%
    ungroup() %>%
    mutate(measure = "n")
  
  temp <- r_temp %>%
    left_join(m_dat) %>%
    util$rename_group() %>%
    filter(group %in% "VEH x CON") %>%
    group_by(group, stain) %>%
    summarise(mean = mean(value, na.rm = T)) %>%
    ungroup() %>%
    select(-c("group"))
  
  temp_out <- r_temp %>%
    left_join(m_dat %>% select(image, stain)) %>%
    left_join(temp, by = c("stain")) %>%
    mutate(fold_mean = value/mean,
           #relative_change = (value-mean)/mean
    )%>%
    select(-c("mean","value")) %>%
    tidyr::pivot_longer(!c("image","stain","measure"), 
                        names_to = "name", values_to = "value") %>%
    tidyr::unite(col = "name", 
                 c("name", "measure"), 
                 sep = " of ", 
                 remove = TRUE, 
                 na.rm = FALSE)
  
  return(temp_out)}

get_stat <- function(a = r_combo){
  temp_stat <- list()
  
  temp_data <- a %>%
    left_join(m_dat) %>%
    util$rename_group() %>%
    ungroup()
  
  if("structure" %in% colnames(temp_data))
  {} else {
    temp_data$structure = "total"
  }
  
  temp_stat$anova <- temp_data %>%
    group_by(structure, name) %>%
    rstatix::anova_test(value ~ pretreat * treat)
  
  temp_stat$tukey <- temp_data %>%
    group_by(structure, name) %>%
    rstatix::tukey_hsd(value ~ pretreat * treat) %>%
    filter(p.adj < 0.05)
  
  temp_stat$kruskal <- temp_data %>%
    group_by(structure, name) %>%
    rstatix::kruskal_test(value ~ group)
  
  temp_stat$dunn <- temp_data %>%
    group_by(structure, name) %>%
    rstatix::dunn_test(value ~ group)
  
  return(temp_stat)}

# help data ---------------------------------------------------------------

util$get_multiplied_data <- function(a = d_dat, .m_scheme = m_multiplier_female){
  m_scheme = .m_scheme %>%
    filter(!is.na(multiplier)) %>%
    filter(multiplier > 1)
  d_output <- list()
  i = 1
  n = 1
  for(i in 1:nrow(m_scheme)){
    temp_slice <- m_scheme[i,]
    for(j in 1:(temp_slice$multiplier-1)){
      d_output[[n]] <- a %>%
        filter(image %in% temp_slice$image &
                 position_rostral %in% temp_slice$position_rostral &
                 hemi %in% temp_slice$hemi)
      n = n + 1}
    i = i + 1}
  bind_rows(d_output)}

# get fold change ---------------------------------------------------------

util$get_fold <- function(temp_measure = 
                                       names(r_annotations)%>%
                                       stringr::str_subset("image", negate = T) %>%
                                       stringr::str_subset("structure", negate = T), 
                                     a = r_annotations){
  
  r_temp <- a %>%
    select(c(all_of(temp_measure),image, structure)) %>%
    tidyr::pivot_longer(cols = all_of(temp_measure), 
                        names_to = "measure", 
                        values_to = "value")
  
  temp <- r_temp %>%
    left_join(m_dat, by = "image") %>%
    util$rename_group() %>%
    filter(group %in% "VEH x CON") %>%
    group_by(group, structure, measure, stain) %>%
    summarise(mean = mean(value, na.rm = T)) %>%
    ungroup() %>%
    select(-c("group"))
  
  temp_out <- r_temp %>%
    left_join(m_dat %>% select(image, stain)) %>%
    left_join(temp, by = c("stain", "measure", "structure")) %>%
    mutate(fold_mean = value/mean,
           #relative_change = (value-mean)/mean
    )%>%
    select(-c("mean","value")) %>%
    tidyr::pivot_longer(!c("image","stain","measure","structure"), 
                        names_to = "name", values_to = "value") %>%
    tidyr::unite(col = "name", 
                 c("name", "measure"), 
                 sep = " of ", 
                 remove = TRUE, 
                 na.rm = FALSE)
  
  return(temp_out)}

# plot signif reference  --------------------------------------------------

util$signif_reference <- function(a = r_fold_annotations,
                                  .plot_interval = 0.05,
                                  .stat_general = temp_stat$anova,
                                  .stat_posthoc = temp_stat$posthoc,
                                  .transform_from = 0){
  temp_filter <- unique(a$name)
  
  temp_signif_measure_filter <- .stat_general %>%
    as_tibble() %>%
    filter(name %in% temp_filter) %>%
    filter(p < 0.05) %>%
    select(name, structure, p)
  
  temp_signif_levels <- a %>%
    group_by(name, structure) %>%
    summarise(
      value_max = max(value),
      value = list(seq((value_max + (.plot_interval * value_max)),
                       by = (.plot_interval * value_max),
                       length.out = val$plot_levels_max)-.transform_from),
      lp = list(seq(1:val$plot_levels_max))) %>%
    tidyr::unnest_longer(col = c("value", "lp"))
  
  temp_signif_test_filtered <-
    .stat_posthoc %>%
    filter(name %in% temp_signif_measure_filter$name) %>%
    filter(p.adj < 0.05) %>% #or p
    mutate(p_final = "*") %>%
    group_by(name, structure) %>%
    mutate(lp = row_number()) %>%
    ungroup() %>%
    filter(grepl("\\:", group1) | grepl("\\:", group2) ) %>%
    mutate(group1 = stringr::str_replace_all(string = group1, pattern = "\\:", replacement = " x "),
           group2 = stringr::str_replace_all(string = group2, pattern = "\\:", replacement = " x ")) %>%
    mutate(group1 = factor(group1, levels = val$name_group),
           group2 = factor(group2, levels = val$name_group)) %>%
    left_join(temp_signif_levels)
  
  return(temp_signif_test_filtered)}


# check df ----------------------------------------------------------------

util$check <- function(a, .len = 10L){str(lapply(a, unique), vec.len = .len)}
