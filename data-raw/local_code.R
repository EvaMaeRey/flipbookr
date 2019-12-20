local_code <- # for testing w/o knitting
  "cars %>%             # the data  #REVEAL
filter(speed > 4) %>%  # subset
ggplot() +              # pipe to ggplot
aes(x = speed) +
aes(y = dist) +
# Describing what follows
geom_point(alpha = .3) + #REVEAL
geom_point(alpha = 1) + #REVEAL2
geom_jitter(alpha = .5) + #REVEAL3
aes(color =
speed > 14
) %+%
cars ->
my_plot  #REVEAL"

local_code_regular_assignment <- # for testing w/o knitting
  "my_cars <- cars %>%             # the data  #REVEAL
filter(speed > 4) %>%  # subset
ggplot() +              # pipe to ggplot
aes(x = speed) +
aes(y = dist) +
# Describing what follows
geom_point(alpha = .3) + #REVEAL
aes(color =
paste(\"speed\",
speed > 14)
) %+%
cars"

save(local_code, file = "../data/local_code.rda")
save(local_code_regular_assignment, file = "../data/local_code_regular_assignment.rda")

cat("#' Local Code",
    "#'",
    "#' Code for examples in the flipbookr package.",
    "#'",
    "#' @name local_code",
    "NULL",
    "",
    "#' @rdname local_code",
    "\"local_code\"",
    "",
    "#' @rdname local_code",
    "\"local_code_regular_assignment\"",
    sep = "\n",
    file = "../R/local_code.R")

