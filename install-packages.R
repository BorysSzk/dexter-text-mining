required_packages <- c(
  "pdftools", "tidyverse", "tm", "tidytext", "stopwords", "textstem",
  "hunspell", "wordcloud", "wordcloud2", "plotrix", "dplyr", "stringr",
  "tidyr", "ggplot2", "scales", "qdap", "topicmodels", "dendextend",
  "dbscan", "clValid", "lsa", "factoextra", "skmeans", "fpc", "clue",
  "lemon", "ggdendro", "shiny"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)

if (length(to_install)) install.packages(to_install)

invisible(lapply(required_packages, library, character.only = TRUE))