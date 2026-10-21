## ----include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE, 
  comment = '#>'
)

old = options(width = 200)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library(dplyr)
library(tidyr)
library(emmeans)
library(powerutilities)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data("greenfeeds")

greenfeeds |> 
  pivot_wider(names_from = 'Rep', 
              values_from = 'Yield', 
              names_prefix = 'Rep:')

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# First, fit the model
greenfeeds_fit = lm(Yield ~ Treatment, data = greenfeeds)

# Calculate the marginal means
(greenfeeds_emm = emmeans(greenfeeds_fit, specs = ~ Treatment))

# Define our contrasts using a named list
contr_list = list('Mangel vs. Fodder Beets' = c(0.25,  0.25,  0.25,  0.25,
                                               -0.25, -0.25, -0.25, -0.25))

# Perform the test
contrast(greenfeeds_emm, contr_list)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define our contrasts using a named list
contr_list = list('Mangels vs. Fodder Beets' = c(0.25,  0.25,  0.25,  0.25,
                                                -0.25, -0.25, -0.25, -0.25), 
                  'Globe vs. Brigadier Mangels' = c(-1, 0.5, 0.5, 0,
                                                     0, 0,   0,   0))

# Perform the test
contrast(greenfeeds_emm, contr_list)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
orth_contr_list = list('Mangel vs Fodder'       = c( 1, 1,  1,  1,  -1,   -1,  -1, -1)/4, 
                       
                       'Globe vs Non-Globe'     = c(-1, 1,  1, -1,   0,    0,   0,  0)/2,
                       'Yellow vs Orange Globe' = c( 0, 1, -1,  0,   0,    0,   0,  0), 
                       'Brigadier vs Red Int.'  = c( 1, 0,  0, -1,   0,    0,   0,  0), 
                       
                       'Mono vs Daeno Fodder'   = c( 0, 0,  0,  0, 1/3,  1/3, 1/3, -1), 
                       'Non-Bomba vs Bomba'     = c( 0, 0,  0,  0, 1/2,  1/2,  -1,  0), 
                       'Rosa vs Blanc Fodder'   = c( 0, 0,  0,  0, 1/2, -1/2,   0,  0))

contrast(greenfeeds_emm, orth_contr_list)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Define treatments (and rep numbers)
barley = expand.grid(Superphosphate = c(0, 250), 
                     Nitrolime = c(0, 250), Rep = 1:5) |> 
  mutate(across(1:3, factor),
         Yield = c(19.2, 18.2, 20.0, 23.6, 
                   18.4, 19.8, 21.6, 21.6,
                   17.0, 19.4, 22.0, 23.2, 
                   17.6, 19.0, 20.8, 21.4, 
                   17.2, 19.8, 20.4, 21.2)) |> 
  dplyr::select(Rep, Nitrolime, Superphosphate, Yield)

head(barley)

## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fit the model
barley_fit = lm(Yield ~ Superphosphate*Nitrolime, data = barley)

(barley_emm = emmeans(barley_fit, ~ Superphosphate:Nitrolime))

fac_contr = list('Main Effects Superphosphate' = c(-0.5,  0.5, -0.5, 0.5), 
                 'Main Effects Nitrolime'      = c(-0.5, -0.5,  0.5, 0.5), 
                 'P x N Interaction'           = c(   1,   -1,   -1,   1))
contrast(barley_emm, fac_contr)

## ----include=FALSE------------------------------------------------------------
options(old)

