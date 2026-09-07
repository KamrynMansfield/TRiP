#' Application User Interface
#'
#' Builds the top-level UI. Not intended to be called directly; use
#' [run_app()] instead.
#'
#' @return A [shiny::tagList()] containing the full UI definition.
#'
#' @keywords internal
app_ui <- function() {
  shiny::page_navbar(
    id = "main_nav",
    title = "TRiP App",
    selected = "pan_1",
    theme = bslib::bs_theme(),

    tags$head(
      tags$style(HTML("\
      .screening-warning { border: 1px solid #e5b94f; border-radius: 6px; overflow: hidden; }\
      .screening-warning-header { background: #ffc107; color: #212529; font-weight: 700; padding: 10px 14px; }\
      .screening-warning-body { background: #fff8df; color: #212529; padding: 12px 14px; }\
      .screening-warning-body ul { margin: 0; padding-left: 20px; }\
    "))
    ),

    ##### 1. screening #####
    nav_panel(
      value = "pan_1",
      title = "1. Screening",
      grid_container(
        layout = c("area0 area1", "area0 area1"),
        row_sizes = c("1fr", "1fr"),
        col_sizes = c("1fr", "1fr"),
        gap_size = "10px",

        grid_card(
          area = "area0",
          card_header(
            markdown(mds = c(
              "The **TRiP App** was created to help small- to medium-sized transit agencies create short-term forecasts to help with their yearly budgets.",
              "<br>",
              "<br>",
              "Please answer the following questions to see if this app is for your agency."
            ))
          ),
          card_body(
            radioButtons(
              "qRedesign",
              "Has your agency had a system redesign within the last three years?",
              choices = c("Yes" = "yes", "No" = "no"),
              width = "100%"
            ),
            radioButtons(
              "qUniversity",
              "Is a majority of your system ridership from a university or a single employer?",
              choices = c("Yes" = "yes", "No" = "no"),
              width = "100%"
            ),
            radioButtons(
              "qRail",
              "Does your city have light- or heavy-rail transit? If so, have there been any major rail transit investments in the last three years?",
              choices = c("Yes" = "yes", "No" = "no"),
              width = "100%"
            ),
            actionButton("compatibility_button", "Check Compatibility", class = "btn-primary")
          )
        ),

        grid_card(
          area = "area1",
          card_body(
            uiOutput("textWarn1"),
            uiOutput("screening_button_placeholder")
          )
        )
      )
    ),
    ##### 2. rider data #####
    nav_panel(
      value = "pan_2",
      title = "2. Ridership Data Upload",
      grid_container(
        layout = c(
          "area1 vrm",
          "area1 vrm"
        ),
        row_sizes = c(
          "1fr",
          "1fr"
        ),
        col_sizes = c(
          "1fr",
          "1fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "vrm",
          card_body(
            div(
              style = "height: 375px; overflow-y: auto; width: 100%; max-width: 100%; overflow-x: auto; border: 1px solid #cccccc; border-radius: 4px; padding: 10px;",
              strong("Imported File Preview"),
              DTOutput(outputId = "input_data", width = "98%")
            ),
            "Just one more questions before creating your model for forecasting.",
            radioButtons(
              inputId = "has_fare_changes",
              label = "Have you changed your adult base fare in the past 5 years?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected = "no",
              inline = TRUE
            ),
            conditionalPanel(
              condition = "input.has_fare_changes === 'yes'",
              h5("Inputing Changes to Adult Base Fare"),
              div(
                style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                dateInput("new_date", "Date of change", width = "150px",
                          value = Sys.Date(),
                          min = Sys.Date() - 10000,
                          max = Sys.Date()),
                numericInput("new_prev", "Previous fare", value = NA, min = 0, step = 0.25, width = "150px"),
                numericInput("new_new", "New fare", value = NA, min = 0, step = 0.25, width = "150px")
              ),
              div(
                style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                actionButton("add", "Submit fare increase", class = "btn-primary"),
                "You may select a row from the table below if you want to delete it.",
                actionButton("delete", "Delete selected row in fare table", class = "btn-danger")
              )
            ),
            conditionalPanel(
              condition = "input.has_fare_changes === 'yes'",
              DTOutput("tbl")
            ),
            uiOutput(outputId = "rider_data_next_placeholder")
          )
        ),
        grid_card(
          area = "area1",
          card_body(
            "Upload a properly formatted excel file with monthly weekday Unlinked Passenger Trips (UPT) and Vehicle Revenue Miles (VRM) for each route.",
            "(seen example below for data format) \n",
            "Due to unusual ridership patterns during the COVID19 pandemic, it is recommended that your data not go back past the year 2023.",
            fileInput("upload_data", "",
                      accept = ".xlsx",
                      width = "100%"),
            hr(),
            h5("Input Data Example"),

            # tableOutput(outputId = "example_input"),
            tags$img(src = "dataexample.png",),
            "The uploaded data must match the format shown above",
            "The column names must match exactly.",
            "If you have routes classified as Bus Rapid Transit, you may optionally add a brt column.",
            "The values in the route_id column must match the route id in the GTFS files.",
            "If you have you own variables you would also like to model, you may add columns that contain positive integers. The model
          will take the natural logarithm of any added variables, so, if you add any variables,
          just list actual values and avoid putting 'log' in the column name."
          )
        )
      )
    ),

    ##### 3. gtfs #####
    nav_panel(
      value = "pan_3",
      title = "3. GTFS Upload",
      grid_container(
        layout = c(
          "area1 plot1  ",
          "area1 plot1"
        ),
        row_sizes = c(
          "1fr",
          "1fr"
        ),
        col_sizes = c(
          "0.5fr",
          "1.5fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "plot1",
          card_body(leafletOutput("route_map"))
        ),
        grid_card(
          area = "area1",
          card_header("Route Data Upload"),
          card_body(
            "Upload your agency's GTFS zip file",
            fileInput("upload_routes","GTFS Upload", accept = ".zip"),
            textOutput(outputId = "acs_description"),
            uiOutput(outputId = "api_key_placeholder"),
            uiOutput(outputId = "api_button_placeholder"),
            uiOutput(outputId = "acs_button_placeholder")
          )
        )
      )
    ),
    ##### 4. Create Model #####
    nav_panel(
      value = "pan_4",
      title = "4. Model Creation",
      card(
        full_screen = TRUE,
        card_body(
          grid_container(


            layout = c(
              "desc       model1 model2",
              "create_mod model1 model2",
              "create_mod model1 model2"
            ),
            row_sizes = c(
              "1.66fr",
              "0.67fr",
              "0.67fr"
            ),
            col_sizes = c(
              "550px",
              "1fr",
              "1fr"
            ),
            gap_size = "1rem",
            grid_card(
              style = "background-color: #f0f0f0;",
              area = "desc",
              card_body(
                markdown("
              *Once the data is uploaded, follow these steps to choose a
              linear regression model to forecast your ridership.*

              **View Default Model:** The default model will be displayed in the middle of the page.
              It was generated by the app using a
              backwards stepwise method (see note below).

              **Create Your Own Model:** The selection pane below contains every variable that is
              available to put in the model (see below for variable descriptions). The pre-selected varaibles
              are those belonging to the default model.
              You may click the selection pane to update which variables you want to test in the model.
              Note that VRM, Month, and Year variables are require to be selected.
              Once you have selected variables you want to see in the model, click the Create New Model Button.

              **Compare Models:** The most recent model you created will be
              displayed next to the default model so you can compare your new model with
              the model the app created.

              **Choose Model:** You may continue with the model you have created,
              or you may choose to continue with the model that was created by the app.
              Select the model that you feel best reflects the variables affecting your
              local context.

              ***How to interpret the models:*** *A positive coefficient means an increase or presence of the
              variable is associated with an increase in UPT. A negative coefficient means an increase or presence of the
              variable is associated with an decrease in UPT. The magnitude of the coefficient
              indicates how much effect the variable has on the UPT.*

              ***Note:*** *To produce the default model, the app creates a large
              model using every variable, and then
              iteratively removes the variables with the highest p-values
              until they all fall within a 10% confidence threshold.*

              **-- Variable Descriptions --**

              ***Provided by Agency***

              - *VRM (required in model):*

              - *Month (required in model)*

              - *Year (required in model)*

              - *Additional variables uploaded by agency (optional)*

              ***Retrieved from Census***

              - *Gas price at the regional level (US Energy Information Administration)*

              - *Total workers 16 years and over (American Community Survey)*

              - *Number of workers working from home (American Community Survey)*

              - *Number of workers commuting by taxicab, motorcycle or other means (American Community Survey)*

              - *Number of workers commuting by car (American Community Survey)*

              - *Number of households with no vehicle available (American Community Survey)*

              - *Number of workers with no vehicle available (American Community Survey)*

              - *Percent of workers who are below 100 percent of the poverty level (American Community Survey)*

              - *Percent of workers who are between 100 to 149 percent of the poverty level (American Community Survey)*

              - *Percent of workers who are female (American Community Survey)*

              - *Percent workers in households in renter-occupied housing units (American Community Survey)*

              - *Total population per census tract (American Community Survey)*

              - *Labor force participation rate per census tract (American Community Survey)*

              - *Unemployment rate per census tract (American Community Survey)*
                       "
                )
              )
            ),
            grid_card(
              # style = "background-color: #f0f0f0;",
              area = "create_mod",
              card_body(
                selectizeInput(inputId = "variables_forced",
                               label = "Varaible Selection",
                               choices = NULL,
                               selected = NULL,
                               multiple = TRUE,
                               width = "100%"
                ),
                div(
                  style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                  input_task_button("run_model_forced", "Create New Model")
                )
              )
            ),
            grid_card(
              area = "model2",
              card_header("Alternative Model Results"),
              card_body(
                input_task_button("use_this_model_button_forced", "Continue With This Model"),
                gt_output(outputId = "tbl_mod_forced")
              )
            ),
            grid_card(
              area = "model1",
              card_header("Default Model Results"),
              card_body(
                input_task_button("use_this_model_button", "Continue With This Model"),
                gt_output(outputId = "tbl_mod_stepwise")
              )
            )


          )
        )
      )
    ),
    ##### 5. Review Model #####
    nav_panel(
      value = "pan_5",
      title = "5. Model Review",
      grid_container(
        layout = c(
          "area0 area1 area2",
          "area0 area1 area2"
        ),
        row_sizes = c(
          "1fr",
          "1fr"
        ),
        col_sizes = c(
          "1fr",
          "1fr",
          "1fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "area0",
          card_body(
            strong("Selected model"),
            gt_output("coefficients_review")
          ),
          card_body()
        ),
        grid_card(
          area = "area1",
          card_body(
            strong("Pre-forecasting questions"),
            radioButtons(
              inputId = "brt_question",
              label = "In the next year, do you plan to convert any of your routes to BRT?",
              choices = list("No" = "no","Yes" = "yes"),
              selected = "no",
              inline = TRUE
            ),
            radioButtons(
              inputId = "fare_question",
              label = "In the next year, do you plan to increase the adult base fare?",
              choices = list("No" = "no","Yes" = "yes"),
              selected = "no",
              inline = TRUE
            ),
            # uiOutput(outputId = "brt_question_placeholder"),
            conditionalPanel(
              condition = "input.brt_question === 'yes'",
              hr(),
              strong("Input BRT Changes"),
              "Please select the route and the approximate date that it will be converted to BRT",
              div(
                style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                uiOutput(outputId = "brt_date"),
                uiOutput(outputId = "brt_routes"),
                div(
                  style = "display:flex; gap:.5rem; flex-wrap:wrap;",
                  actionButton("add_brt", "Submit BRT change", class = "btn-primary"),
                  "You may select a row from the table below if you want to delete it.",
                  actionButton("delete_brt", "Delete selected row in BRT table", class = "btn-danger")
                )
              )
            ),
            conditionalPanel(
              condition = "input.brt_question === 'yes'",
              DTOutput("tbl_brt")
            )
          ),
          card_body(),
          card_body()
        ),
        grid_card(
          area = "area2",
          card_body(
            markdown(
              "Research has cited the following elasticities for the coefficients below.
          If you would prefer to replace or add these coefficients to your model, you can check the corresponding boxes.
          ***Only replace a coefficient if you have good reason to believe the
          coefficient listed below is a better representation of your local
          context than the calculated coefficient.***"
            ),
            uiOutput(outputId = "forced_coef_placeholder"),
            "If you have reviewed the coefficients, added any forced coefficients you wanted, and decided this is the model you want to use, click the button below.",
            actionButton(
              inputId = "proceed_to_forecast",
              label = "Proceed to Forecasting",
              class = "btn-primary"
            )
          ),
          card_body(),
          card_body()
        )
      )
    ),
    ##### 6. Forecasting #####
    nav_panel(
      value = "pan_6",
      title = "6. Forecasting Inputs",
      grid_container(
        layout = c(
          "instr area1",
          "area0 area1"
        ),
        row_sizes = c(
          ".6fr",
          "1.4fr"
        ),
        col_sizes = c(
          "1fr",
          "1fr"
        ),
        gap_size = "10px",
        grid_card(
          style = "background-color: #f0f0f0;",
          area = "instr",
          card_body(
            markdown("
          **Scenario Estimation:** Estimate three scenarios based on
          realistic assumptions of the direction of each variable (growth/decline)
          and the magnitude of the expected annual percent change in each variable.
          For example...

          - *Low Estimate* might represent a scenario with small decreases in VRM and other variables.

          - *Mid Estimate* might represent a status quo scenario with no changes in VRM and minimal changes in other variables.

          - *High Estimate* might represent a large increase in VRM and changes in other variables.

          In many cases, the same set of scenarios will apply to all the routes, but there are
          some cases where individual routes will have different scenarios
          (e.g. you know one route will double it's service but the rest will stay the same).

          In the case where all your routes have the same scenarios, you can select *All remaining routes* and click save.
          This will save the same scenario estimates for each route.

          If some of your routes will have different scenarios, it is easiest to make those estimates first,
          save those routes, and then make the scenario estimates that will apply to *All remaining routes*.

          **Existing Routes Overwrite:** You may overwrite any of the saved routes if you need to make adjustments.
          Note that some users first create general scenarios and apply it to each route.
          Then they go back and overwrite the few routes that have different estiamtes.
                   ")
          )
        ),
        grid_card(
          area = "area0",
          card_body(
            # DTOutput(outputId = "dtScenarios", width = "100%"),
            radioButtons(
              "route_mode",
              "Apply scenario to:",
              choices = c("New routes (not yet saved)" = "new",
                          "Existing routes (overwrite saved routes)" = "overwrite"),
              selected = "new",
              inline = TRUE
            ),

            selectInput("route_selected", "Route", choices = NULL),

            actionButton("save_route_scenario", "Save", class = "btn-primary"),

            DTOutput("dtScenarios")
          )
        ),
        grid_card(
          area = "area1",
          card_body(
            input_task_button("buttonRun","Run Forecasts"),
            "The assumed forecasts for each route will be displayed below.
          Check your estimate values once more before proceeding ",
            # plotOutput(outputId = "forcast_plot"),
            # Optional: show saved results
            DTOutput("dtSavedScenarios")
          )
        )
      )
    ),
    ##### 7. Visualize #####
    nav_panel(
      value = "pan_7",
      title = "7. Visualization",
      grid_container(
        layout = c(
          "area0 area1",
          "area0 area1"
        ),
        row_sizes = c(
          "1fr",
          "1fr"
        ),
        col_sizes = c(
          "0.5fr",
          "1.5fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "area0",
          card_body(
            style = "display: flex; flex-direction: column; height: 100%;",
            selectInput(
              inputId = "input_route_to_plot",
              label = "Choose a route to plot",
              choices = "Waiting for forecast..."
            ),
            downloadButton("download_plots", "Download Plots (.zip)", class = "btn-success"),
            input_task_button("go_to_export", "Export Forecast Data",style = "margin-top: auto;")
          )
        ),
        grid_card(area = "area1",
                  plotOutput("viz_plot") #,
                  # plotOutput("viz_plot_2")
        )
      )
    ),
    ##### 8. Export #####
    nav_panel(
      value = "pan_8",
      title = "8. Export",
      grid_container(
        layout = c(
          "area1 area0",
          "desc area0"
        ),
        row_sizes = c(
          ".8fr",
          "1.2fr"
        ),
        col_sizes = c(
          "1fr",
          "1fr"
        ),
        gap_size = "10px",
        grid_card(
          area = "area0",
          card_body(
            strong("Raw Forecast Data"),
            DTOutput(outputId = "outputExample", width = "100%")
          )
        ),
        grid_card(
          area = "area1",
          card_body(
            "You can download the raw forecast data as either an Excel or a CSV file.
          (See a description of each of the varaibles below.)",
            downloadButton("download_csv", "Download as CSV"),
            downloadButton("download_xlsx", "Download as Excel")
          )
        ),
        grid_card(
          style = "background-color: #f0f0f0;",
          area = "desc",
          card_body(
            h5("Data Description"),
            markdown("
                   **route_id** = Route ID

                   **year** = Year

                   **month** = Month

                   **avg_daily_upt** = Average Weekday Unlinked Passenger Trips (UPT)

                   **tot_weekday_upt** = Average Weekday UPT multiplied by total weekdays in the month.

                   **forecast** = True/False indicating whether the value was forecasted or not.

                   **scenario** = The name of the scenario that the forecasted value belongs to. If it is not a forecasted value, it will be labeled *observed*.

                   **date** = Combined month and year in a date format for easier plotting.


                   ")
          )
        )
      )
    )
  )
}
