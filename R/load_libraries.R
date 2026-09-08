# this script simply loads all the packages used in running the function of the shiny app
pacman::p_load(tidyverse,
               readxl,
               writexl,
               fixest,
               modelsummary,
               scales,
               patchwork,
               purrr,
               bslib,
               gridlayout,
               DT,
               gt,
               tidytransit,
               units,
               lwgeom,
               sf,
               leaflet,
               leaflet.extras2,
               viridis,
               tidycensus,
               tigris,
               lubridate,
               zoo,
               shinyjs,
               pak)


# in case I want to source all my functions.
# But this is automatic when the app is published, so this line
# is just for myself when testing locally.
# lapply(list.files("R", full.names = T), source)
