# filter & copy image files -----------------------------------------------

path_source <- choose.dir()
path_into <- choose.dir()

temp <- m_male_filtered %>%
  rowwise() %>% 
  mutate(path_file = paste0(c(path_source, image, "_", position_rostral, 
                              ifelse(!is.na(hemi),hemi,""), ".ome.tif.png")))

file.copy(temp$path_file, 
          path_into,
          overwrite = TRUE, 
          recursive = FALSE, 
          copy.mode = TRUE)
