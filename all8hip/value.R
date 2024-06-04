# val ---------------------------------------------------------------------

temp <- list(
  structure = c("ca1", "ca2", "ca3", "dentate",  
                "d_ca1", "d_ca2", "d_ca3", "d_dentate", "ds",
                "v_ca1", "v_ca2", "v_ca3", "v_dentate", "vs", 
                "hf", "hf+sub"),
  meta_pretreat = c("VEH", "MAM"),
  meta_treat = c("CON", "JQ"),
  meta_gru = c("VEHxCON", "VEHxJQ", "MAMxCON", "MAMxJQ"))

#color = "lightblue"; gender = "male
color = "pink"; gender = "female"

name <- list(
  structure = c("ca1", "ca2", "ca3", "dentate",  
                "d_ca1", "d_ca2", "d_ca3", "d_dentate", "ds",
                "v_ca1", "v_ca2", "v_ca3", "v_dentate", "vs", 
                "hf", "hf+sub"),
  meta_pretreat = c("VEH", "MAM"),
  meta_treat = c("CON", "JQ"),
  meta_gru = c("VEHxCON", "VEHxJQ", "MAMxCON", "MAMxJQ"))

temp_get_those <- c("piramid", "just_cell", "maybe_cell", "light_cell")

temp_structure_pooled <- c("ca1", "ca2", "ca3", "dentate")
