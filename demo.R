library(modelsummary)
library(tidyverse)
library(palmerpenguins)

mod <- lm(bill_length_mm ~ body_mass_g, data = penguins)

modelsummary(mod, output = "markdown")
?modelsummary
modelsummary(mod)
modelplot(mod)

datasummary_skim(penguins)
datasummary_correlation(mtcars)
