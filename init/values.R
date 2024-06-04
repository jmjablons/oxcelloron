
# custom ------------------------------------------------------------------

val <- list()

# init settings -----------------------------------------------------------

val$file_suffix <- "detections"

val$name_annotation <- ifelse(val$file_suffix %in% "detections",
                              "parent", "name")

val$temp_measures <- c("Nucleus: Area", "Nucleus: DAB OD mean")

val$chosen_class = c("strong_stain2") #"weak_stain2"

val$chosen_measures = val$temp_measures

val$structures <- c("Cg","Cg1","Cg2","IL", "PrL","PFC")

val$structures_fix <- c("Cg1", "PrL", "IL", "total")

# experimental values -----------------------------------------------------

val$slice_thickness <- 50
val$numberslices_min <-1
val$numberslices_max <-7
val$interval_slices <- 4 # 4 denotes every 5th slice
val$distance_slices <- val$interval_slices * val$slice_thickness
val$distance_slices_mm <- val$distance_slices / 1e3

temp_gender <- c("female", "Female", "male", "Male")
temp_group <- c("VEH x CON", "MAM x CON", "VEH x JQ", "MAM x JQ")

# bregmaxn ----------------------------------------------------------------

val$bregma_range <- list(rostral = 3.24, caudal = 2.76)

# combined analysis -------------------------------------------------------

val$structures <- c("Cg","Cg1","Cg2","IL", "PrL","abba_PFC","PFC")
val$n_groups <- length(unique(temp_group))
val$plot_levels_max <- factorial(val$n_groups)/(factorial(2)*factorial(val$n_groups-2))
val$name_group <- c("VEH x CON", "MAM x CON", "VEH x JQ", "MAM x JQ")

# plot with signif --------------------------------------------------------

val$plot_signif_vjust = 0.618 #0.7
val$plot_signif_tip_length = 0.618^9 #0.025 
val$plot_y_limits = c(0, 2)
val$plot_y_breaks = c(0, 0.5, 1, 1.5, 2)
val$plot_y_expand = c(0,0)
val$plot_bar_width = 0.7
val$aspect_ratio = 2
