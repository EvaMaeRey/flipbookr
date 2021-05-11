





define_css <- function(
  chunk_name = "example",
  break_type = "auto",
  widths = c(32, 32, 33),
  heights = c(32, 32, 33),
  color = c("black", "black", "black"),
  float = "left",
  spacing = 1,
  font_size_code = 80
){

  active_in_100 <- 100 - spacing*(length(widths))
  normalized_widths <- active_in_100*widths/sum(widths)

  width_left <- NULL
  width_right <- NULL
  width_middle <- NULL

  try(width_1 <- normalized_widths[1])
  try(width_2 <- normalized_widths[2])
  try(width_3 <- normalized_widths[3])

  active_in_100 <- 100 - spacing*(length(heights))
  normalized_heights <- active_in_100*heights/sum(heights)

  height_left <- NULL
  height_right <- NULL
  height_middle <- NULL

  try(height_1 <- normalized_heights[1])
  try(height_2 <- normalized_heights[2])
  try(height_3 <- normalized_heights[3])


  id <- paste0(chunk_name, "-", break_type)

  knitr::asis_output(glue::glue(
    "<style>
    .panel1-<<<id>>> {
      color: <<<color[1]>>>;
      width: <<<width_1>>>%;
      hight: <<<height_1>>>%;
      float: <<<float>>>;
      padding-left: 1%;
      font-size: <<<font_size_code>>>
    }
    .panel2-<<<id>>> {
      color: <<<color[2]>>>;
      width: <<<width_2>>>%;
      hight: <<<height_2>>>%;
      float: <<<float>>>;
      padding-left: 1%;
      font-size: <<<font_size_code>>>
    }
    .panel3-<<<id>>> {
      color: <<<color[3]>>>;
      width: <<<width_3>>>%;
      hight: <<<height_3>>>%;
      float: <<<float>>>;
      padding-left: 1%;
      font-size: <<<font_size_code>>>
    }
    </style>
    ",
    .open = "<<<",
    .close = ">>>",
    .sep = "\n"
  ))
}





#### historical define CSS ####


# choose_css_components <- function(display_type = "both"){
#
# left <- ".left-panel-<<<id>>> {
#       color: #777;
#       width: <<<width_left>>>;
#       height: 92%;
#       float: left;
#       font-size: <<<font_size_code>>>
#     }"
#
# right <- ".right-panel-<<<id>>> {
#       width: <<<width_right>>>;
#       float: right;
#       padding-left: 1%;
#     }"
#
# middle <- ".middle-panel-<<<id>>> {
#       width: <<<width_middle>>>;
#       float: center;
#       padding-left: 1%;
#     }"
#
# if (display_type[1] == "both" | length(display_type) == 2) {
#   paste0(left, right)
# } else if (length(display_type) == 2) {
#   paste0(left, right)
# } else if (length(display_type) == 3) {
#   paste0(left, middle, right)
# }
#
# }

