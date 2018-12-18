#This file exists to experiment with stuff and put some notes for later
#so we can easily test stuff without rendering the app each time

# Explore tab example: https://shiny.rstudio.com/gallery/datatables-options.html

library(tidyverse)

payments <- read_csv("./ChicagoPI/Payments.csv")

vendors <- select(payments, `VENDOR NAME`) %>% distinct()
