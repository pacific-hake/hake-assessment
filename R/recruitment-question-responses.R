# Code to tidy up the results from the Dec 2021 Google Survey regarding
#  recruitment. No need to be called from all.R, as will create a new .csv file
#  to use for plots.

# Question was: Figure e in the 2021 Pacific Hake stock assessment document
# shows the estimated recruitment each year. From this figure (copied below),
# what do you think the chance (probability) is that the 2014 cohort is at least
# as large as the 2010 cohort? Please give a percentage or a range, and feel
# free to add any text if you like. All answers are anonymous. Thanks.

# Survey was: https://docs.google.com/forms/d/120jjAf-O8ya1RhzDk9dZbIQ0FHQ32DRwAnqOWn9aTjU

# Responses saved as "../data/recruitment-question-responses-orig.csv", but not
#  pushed to GitHub (since contains Timestamps), Andy has it and then stripped
#  off the timestamps and added Andy did a Low, High column (as a percentage)
#  for each response, based on the replies.

# For write-up - an answer of "Low" was excluded, and 0.4 was
# assumed to be 40% (not 0.4%).

library(dplyr)
library(tidyr)
orig <- read.csv("../data/recruitment-question-responses.csv",
                 comment.char = "#") %>%
  as_tibble()


res <- dplyr::select(orig,
                     Low,
                     High) %>%
  na.omit() %>%
  arrange(desc(Low))

# From OA paper code, adapting:

n <- nrow(res)

plot(-1, -1,
     xlim = c(0,100),
     ylim = c(1, n),
     xlab = "Estimated percentage",
     ylab = "Respondent number")

segments(x0 = res$Low,
         y0 = 1:n,
         x1 = res$High,
         y1 = 1:n,
         lwd = 2)

points(res$Low,
       1:n,
       pch = 19)

points(res$High,
       1:n,
       pch = 19)
