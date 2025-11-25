# Exploration
library(tidyverse)

thepath <- "data/raw/Frailty and CAR T - Frailty Data - 09Jul2025.xlsx"

hojas <- readxl::excel_sheets(thepath)

lit <- map(hojas, ~readxl::read_xlsx(thepath, sheet = .))

names(lit) <- janitor::make_clean_names(hojas)
names(lit)
names(lit) <- c("base", "long")

lit$base
