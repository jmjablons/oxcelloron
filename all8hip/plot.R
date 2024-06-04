require(beeswarm)
# plot --------------------------------------------------------------------

(d_hip_total %>%
   tidyr::unite("gru", c(pretreat, treat), sep = "x", remove = F) %>%
   mutate(gru = factor(gru, levels = temp$meta_gru, ordered = T),
          structure = factor(structure, levels = temp$structure, ordered = T)) %>%
   ggplot(aes(x = gru, y = value))+
   geom_boxplot(outlier.colour = NA, fill = color)+
   geom_jitter(width = 0.1, pch = 21, fill = color)+
   #facet_grid(name~structure, scales = "free")+
   facet_wrap(name~structure, scales = "free_y", nrow = 5)+
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         axis.title = element_blank())) %>%
  ggsave(., file = paste0("./all8hip/results/", gender ,"_boxplot_wrap.pdf"), 
         scale = 2, width = 36, height = 16, units = "cm")

(d_hip_total %>%
    tidyr::unite("gru", c(pretreat, treat), sep = "x", remove = F) %>%
    mutate(gru = factor(gru, levels = temp$meta_gru, ordered = T),
           structure = factor(structure, levels = temp$structure, ordered = T)) %>%
    ggplot(aes(x = gru, y = value))+
    geom_boxplot(outlier.colour = NA, fill = color)+
    geom_jitter(width = 0.1, pch = 21, fill = color)+
    facet_grid(name~structure, scales = "free")+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank())) %>%
  ggsave(., file = paste0("./all8hip/results/", gender, "_boxplot_grid.pdf"), 
         scale = 2, width = 24, height = 16, units = "cm")
