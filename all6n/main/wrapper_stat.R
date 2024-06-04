
source("init/main_init.R")

# settings ----------------------------------------------------------------

temp <- list()

# exit --------------------------------------------------------------------

temp$dir_subpath <- "all6n/stat"
dir.create(temp$dir_subpath)

# init input --------------------------------------------------------------

temp_list <- choose.files()

# preprocess & analyse ----------------------------------------------------

for(temp_file in temp_list){
  cat("file: \n", temp_file, "\n")
  source("all6n/main/stat.R")}

cat("DONE\n\n")
