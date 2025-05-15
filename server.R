server <- function(input, output) {
  
  output$wordcloud_no2 = renderWordcloud2(wordcloud2_1)
} 