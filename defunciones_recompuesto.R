
library(tidyverse)
library(readxl)
library(lubridate)

x <- "C:\\Users\\bodega\\Downloads\\1998-2018_defunciones_totales.xlsx"

total_defun <- read_excel(x, sheet = "Hoja1")
tdefun <- total_defun %>% 
  mutate(year = years(`Entidad Federativa`))

r.ts <- ts(tdefun, start = 1998, frequency = 1)

is.Date(tdefun$year)
