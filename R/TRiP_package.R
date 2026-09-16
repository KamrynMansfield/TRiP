#' TRiP: Transit Ridership Prediction Tool
#'
#' A Shiny application that forecasts bus ridership for small- to medium-sized
#' transit agencies. TRiP combines historical route-level ridership with service
#' levels, demographic and economic indicators, and telecommuting rates to fit a
#' fixed-effects regression model and project ridership forward.
#'
#' @section Main workflow:
#' The app proceeds through the following stages, each backed by functions in
#' this package:
#' \enumerate{
#'   \item Load and screen agency ridership data
#'   \item Attach GTFS-derived service characteristics
#'   \item Join ACS demographics and gas price series
#'   \item Fit the regression model
#'   \item Forecast and visualize future ridership
#' }
#'
#' @section Getting started:
#' Launch the application with \code{\link{run_app}()}.
#'
#' @keywords internal
#'
#' ---------------------------------------------------------------------------
#' Imports
#' ---------------------------------------------------------------------------
#' Every symbol below is called WITHOUT a `pkg::` prefix somewhere in R/.
#' Symbols already written as `pkg::name()` in the source do not need to be
#' listed, but harmless duplicates are kept where the package uses both styles.
#'
#' @import shiny
#'
#' @importFrom dplyr %>% across all_of any_of arrange bind_rows c_across
#' @importFrom dplyr case_when cur_group_id distinct filter group_by if_else
#' @importFrom dplyr join_by left_join mutate n pull recode rowwise select
#' @importFrom dplyr summarise summarize ungroup
#' @importFrom tidyr expand_grid pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @importFrom stringr str_pad str_sub
#' @importFrom zoo na.approx na.locf
#'
#' Dates
#' @importFrom lubridate interval month months time_length year years ym ymd
#'
#' Spatial
#' @importFrom sf sf_use_s2 st_area st_as_sf st_buffer st_coordinates
#' @importFrom sf st_drop_geometry st_intersection st_intersects st_is_empty
#' @importFrom sf st_make_valid st_read st_simplify st_transform
#' @importFrom units set_units
#' @importFrom tigris tracts
#' @importFrom tidycensus census_api_key get_acs
#' @importFrom tidytransit read_gtfs shapes_as_sf
#'
#' Modeling
#' @importFrom fixest feols fixef pvalue
#' @importFrom stats as.formula coef
#'
#' Plotting and tables
#' @importFrom ggplot2 aes facet_wrap geom_line geom_sf ggplot ggsave labs
#' @importFrom ggplot2 theme_bw theme_void
#' @importFrom viridis viridis
#' @importFrom leaflet addPolygons addPolylines addProviderTiles colorFactor
#' @importFrom leaflet highlightOptions labelOptions leaflet leafletOutput
#' @importFrom leaflet renderLeaflet
#' @importFrom DT coerceValue dataTableProxy datatable DTOutput formatStyle
#' @importFrom DT renderDT replaceData
#' @importFrom gt cell_fill cells_body cols_hide gt gt_output render_gt tab_style
#'
#' UI layout
#' @importFrom bslib bs_theme card card_body card_header input_task_button
#' @importFrom bslib nav_panel nav_select page_navbar
#' @importFrom gridlayout grid_card grid_container
#'
#' File I/O
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom utils head read.csv write.csv zip
"_PACKAGE"
