
#library(ggbeeswarm)

custom_plot <- function(a = util$get_fold(), 
                     .stat_reference = util$signif_reference(util$get_fold())) {

  temp_info  = a %>%
    group_by(anatomy, age, gender, classifier) %>%
    summarise() %>%
    stringr::str_flatten(collapse = " | ") %>%
    tolower()
  
  a %>%
    ggplot(aes(x = group, y = value))+
    
    stat_summary(geom = "crossbar",
                 fun.data = function(x) {
                   data_frame(y = mean(x), 
                              ymin = ifelse(x < 1, mean(x), 1), 
                              ymax = ifelse(x < 1, 1, mean(x)))}, 
                 width = val$plot_bar_width,
                 fill = accent_color,
                 colour = NA )+ #"black"

    stat_summary(aes(colour = group),
      fun = mean,
      geom = "errorbar", 
      #colour = 'darkgrey',
      width = 0.618*val$plot_bar_width,
      colour = "black",#"#636363",
      fun.max = function(x) mean(x) + util$sem_solo(x),#size =1,
      fun.min = function(x) mean(x) - util$sem_solo(x))+
    
    geom_quasirandom(
      varwidth = TRUE,
      colour = "black", #"#636363",
      fill = accent_color,
      width = 0.1,
      pch = 21,
      size = 2)+

    geom_hline(yintercept = 1, linetype = "dotted")+
    
    #facet_wrap(name~structure, labeller = label_wrap_gen(width = 4))+
    
    labs(y = "fold change", x = "", subtitle = paste(unique(a$name), temp_info, sep = " \n "))+
    
    theme_minimal()+
    
    # scale_y_continuous(
    #   expand = val$plot_y_expand
    #   # limits = #c(0,2),
    #   #   c(ifelse(
    #   #     min(a$value) > (1 - (pretty(max(.stat_reference$value), shrink.sml = 0.1)[2] - 1)),
    #   #     (1 - (pretty(max(.stat_reference$value), shrink.sml = 0.1)[2] - 1)), 0),
    #   #     pretty(max(.stat_reference$value), shrink.sml = 0.1)[2])
    #   )+
    
    ggsignif::geom_signif(
      data = .stat_reference,
      aes(xmin = group1, xmax = group2, annotations = "*", y_position = value),
      textsize = 9, vjust = val$plot_signif_vjust, tip_length = val$plot_signif_tip_length, 
      manual = TRUE)+
    
    theme(axis.text.x = element_text(angle = 45, hjust =1),
          axis.text.y = element_text(face = "plain"),
          axis.line.y = element_line(), 
          aspect.ratio = val$aspect_ratio, 
          plot.subtitle = element_text(face = "bold"),
          legend.position="none",
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(), 
          axis.ticks.y = element_line())}
