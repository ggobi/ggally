## code to prepare `nba_ppg_2008` dataset goes here

# https://flowingdata.com/2010/01/21/how-to-make-a-heatmap-a-quick-and-easy-solution/
nba_ppg_2008 <- read.csv(
  "http://datasets.flowingdata.com/ppg2008.csv",
  sep = ","
)

usethis::use_data(nba_ppg_2008, overwrite = TRUE)
