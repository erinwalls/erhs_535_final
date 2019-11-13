library(readr)
jeopardy_s1 <- read_tsv("https://raw.githubusercontent.com/jwolle1/jeopardy_clue_dataset/master/season1.tsv")
write_tsv(jeopardy_s1, "season1.tsv")