# app_server.R ------------------------------------------------------------
# All server-side logic for the eight-step workflow defined in app_ui.R.
#
# Data flow through the reactive graph, top to bottom:
#
#   input$upload_data   -> processed_data()  -> check_names()/check_numeric()/
#                                               check_log() -> data_check()
#   input$upload_routes -> route_sf()        -> county_sf()
#   input$get_acs       -> acs_data()        (tracts -> buffers -> ACS -> monthly)
#   processed_data() +
#   acs_data()          -> first_model()     (stepwise, tab 4 middle column)
#                       -> model_forced()    (user-selected, tab 4 right column)
#                       -> selected_model()  -> final_coefs()
#   saved$by_route +
#   final_coefs()       -> forecast_df()     -> plots, zip, csv/xlsx exports
#
# A few conventions used throughout:
#   * Validation failures surface as showModal() dialogs rather than errors.
#   * Navigation between tabs is driven by bslib::nav_select(), never by the
#     user clicking a tab, which keeps the workflow strictly sequential.
#   * Several outputs are rendered inside observeEvent() blocks so that buttons
#     only exist once their prerequisites are met.

#' Application Server Logic
#'
#' Implements every reactive, observer, and output for the TRiP app. Not called
#' directly; [run_app()] passes it to [shiny::shinyApp()].
#'
#' @param input,output,session Internal Shiny parameters.
#'
#' @return Called for side effects. Returns `NULL` invisibly.
#'
#' @seealso [app_ui()] for the interface these handlers are bound to.
#'
#' @keywords internal
#'
#' @export
app_server <- function(input, output, session){

  #### 1. SCREENING ####

  # Evaluates the three screening questions and returns both a flag and the
  # text of any warnings triggered. Kept as a reactive so that both the warning
  # panel and the continue button can read the same result.
  screening_result <- reactive({
    warnings <- c(
      redesign = "Because your agency has implemented a system redesign within the last three years,
            historic route ridership data may not accurately reflect current or future service patterns.
            As a result, this tool may not provide reliable ridership forecasts for your system.",
      university = "Because most of your system ridership comes from a university or a single employer,
            the variables used in this app may not be the largest predictors of ridership,
            and, therefore, the app's predictions may not be accurate.
            Note that if you have your own data to capture these unique circumstances,
            there is an option to add those to the model.",
      rail = "This tool is intended for cities and demographics served primarily by bus. If there have been no recent investments in rail transit, you might be able to use this app if you exclude non-bus routes."
    )

    selected_warnings <- warnings[c(
      input$qRedesign == "yes",
      input$qUniversity == "yes",
      input$qRail == "yes"
    )]

    list(has_warnings = length(selected_warnings) > 0, warnings = unname(selected_warnings))
  })

  # Renders either a green all-clear or an amber list of caveats.
  # bindEvent() on the button means the message only updates when the user
  # explicitly asks to check compatibility, not as they toggle radio buttons.
  output$textWarn1 <- renderUI({
    result <- screening_result()

    if (!result$has_warnings) {
      return(tags$div(
        class = "alert alert-success",
        "Your responses are compatible with the intended use of this tool."
      ))
    }

    tags$div(
      class = "screening-warning",
      tags$div(class = "screening-warning-header", "Warning"),
      tags$div(
        class = "screening-warning-body",
        tags$p("One or more responses may affect the reliability of this tool:"),
        tags$ul(lapply(result$warnings, tags$li))
      )
    )
  }) |>
    bindEvent(input$compatibility_button, ignoreInit = TRUE)

  # The continue button is generated rather than static so its label and color
  # can reflect the screening outcome. Either way it advances the user; the
  # screening is advisory, not a hard gate.
  output$screening_button_placeholder <- renderUI({
    if (screening_result()$has_warnings) {
      bslib::input_task_button(
        "screening_button",
        "Continue Anyways",
        class = "btn-warning"
      )
    } else {
      bslib::input_task_button(
        "screening_button",
        "Continue",
        class = "btn-primary"
      )
    }
  }) |>
    bindEvent(input$compatibility_button, ignoreInit = TRUE)

  # advance to the Ridership Data Upload tab
  observeEvent(input$screening_button, {
    bslib::nav_select("main_nav", "pan_2")
  })


  # output$textWarn1 <- renderText({
  #   if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
  #     paste("This app might be useful to your agency :)")
  #   } else{
  #     paste(input$qRedesign)
  #   }
  # })
  #
  # output$textWarn2 <- renderText({
  #   if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
  #     paste("")
  #   } else{
  #     paste(input$qUniversity)
  #   }
  # })
  #
  # output$textWarn3 <- renderText({
  #   if (input$qRedesign == "" & input$qUniversity == "" & input$qRail == ""){
  #     paste("")
  #   } else{
  #     paste(input$qRail)
  #   }
  # })

  #### 2. RIDERSHIP DATA UPLOAD ####

  # Read and modify the uploaded data
  # Parses the uploaded .xlsx. Route IDs are coerced to numeric when they
  # convert cleanly, purely so routes sort in a sensible order (2 before 10)
  # rather than alphabetically. Non-numeric IDs are left as-is.
  # Anything other than .xlsx is rejected with a modal.
  processed_data <- reactive({
    req(input$upload_data) # Ensure a file is uploaded
    path <- input$upload_data$datapath
    is_xlsx <- grepl("\\.xlsx$", tolower(path))

    if (is_xlsx){
      df <- read_excel(path)

      numeric_vec <- tryCatch(
        {
          as.numeric(df$route_id)
        },
        error = function(e) {
          NULL
        },
        warning = function(w) {
          NULL
        }
      )

      if (!is.null(numeric_vec)){
        df$route_id <- numeric_vec

        df <- df |>
          arrange(route_id)
      }

      return(df)
    } else{
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "File must be an excel file (.xlsx)"
        )
      )
      return(NULL)
    }


  })

  # checking to make sure the data is formatted correctly
  # Validation 1: are all five required columns present and spelled correctly?
  check_names <- reactive({
    req(processed_data())
    df <- processed_data()

    needed_cols <- c("route_id", "month", "year", "upt", "vrm")
    # must_contain route_id, month,  year,   upt, and vrm columns
    check_names_long <- needed_cols %in% names(df)

    sum(check_names_long) == 5

  })

  # Validation 2: can every column except route_id be coerced to numeric?
  # tryCatch traps the coercion warning that as.numeric() raises on text, which
  # is what makes a bad column detectable.
  # NOTE: names(df) != c("route_id") compares a vector to a length-1 vector and
  # relies on recycling; it works here but `!= "route_id"` is the safer form.
  check_numeric <- reactive({
    req(processed_data())
    df <- processed_data()

    # all columns except rout_id must be able to be numeric
    numeric_cols <- names(df)[names(df) != c("route_id")]
    for (col in numeric_cols){
      df[,col] <- tryCatch(
        {
          as.numeric(unlist(df[,col]))
        },
        warning = function(w) {
          return("warning")
        },
        error = function(e) {
          return("error")
        }
      )
    }

    check_numeric_long <- sapply(df, is.numeric)

    sum(check_numeric_long[-1]) == length(check_numeric_long[-1])
  })

  # Validation 3: reject columns whose names suggest they are already logged.
  # make_model_data_frame() logs every extra column it finds, so a pre-logged
  # column would be logged twice.
  check_log <- reactive({
    req(processed_data())
    df <- processed_data()

    col_names <- names(df)

    has_log <- grepl("log_",col_names, ignore.case = TRUE)

    sum(has_log) == 0

  })

  # All three validations combined; gates the Census data pull.
  data_check <- reactive({
    req(check_names())
    req(check_numeric())

    check_names() == TRUE & check_numeric() == TRUE & check_log() == TRUE
  })


  # Dispatches a specific error modal for each combination of failed checks,
  # and renders either the continue button or a nudge to re-upload.
  # Worth knowing: the branches test combinations in a fixed order, and the
  # three-way branch requires all three checks to fail simultaneously, so some
  # combinations fall through to a message that only describes part of the
  # problem. That is likely the behavior behind the TODO below.
  # TODO: This doesn't seem to be working properly.
  # send a message if the data is not formatted correctly
  observe({
    req(processed_data())

    check_names <- check_names()
    check_numeric <- check_numeric()
    check_log <- check_log()

    if (check_names == FALSE & check_numeric == FALSE & check_log == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It appears you are missing or have incorrectly spelled some of the required columns (route_id, month, year, upt, vrm). You also have some extra columns that can't be converted to numbers. Any extra columns in the data must be numeric. You also have some unacceptable column names that suggest logarithmic values. The modeling process will take the logarithm of any added variables, so please put original values in any extra columns you add."
        )
      )
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    } else if (check_names == FALSE & check_log == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It appears you are missing or have incorrectly spelled some of the required columns (route_id, month, year, upt, vrm). You also have some unacceptable column names that suggest logarithmic values. The modeling process will take the logarithm of any added variables, so please put original values in any extra columns you add."
        )
      )
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    } else if (check_log == FALSE & check_numeric == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "You have some extra columns that can't be converted to numbers. Any extra columns in the data must be numeric. You also have some unacceptable column names that suggest logarithmic values. The modeling process will take the logarithm of any added variables, so please put original values in any extra columns you add."
        )
      )
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    } else if (check_names == FALSE & check_numeric == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It appears you are missing or have incorrectly spelled some of the required columns (route_id, month, year, upt, vrm). Additionally, you have some extra columns that can't be converted to numbers. Any extra columns in the data must be numeric."
        )
      )
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    } else if(check_names == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It appears you are missing or have incorrectly spelled some of the required columns (route_id, month, year, upt, vrm)."
        )
      )
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    } else if(check_log == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "You have some unacceptable column names that suggest logarithmic values. The modeling process will take the logarithm of any added variables, so please put original values in any extra columns you add."
        )
      )
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    } else if(check_numeric == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "You have some extra columns that can't be converted to numbers. Any extra columns in the data must be numeric."
        )
      )
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    } else if(check_numeric & check_names) {
      # maybe I should put another notification saying the file looks good
      # showNotification("File Received and Processed",
      #                  type = "message",
      #                  duration = 1)

      output$rider_data_next_placeholder <- renderUI({
        input_task_button("rider_data_next", "Continue")
      })


    } else{
      showNotification("ERROR: There was an unknown error with your file. Please double check to make sure it follows the correct formatting",
                       type = "error",
                       duration = 15)
      output$rider_data_next_placeholder <- renderUI({
        em("*Upload a compatible file to continue")
      })
    }

  }) |>
    bindEvent(processed_data())

  observe({
    bslib::nav_select("main_nav", "pan_3")
  }) |>
    bindEvent(input$rider_data_next)

  # Any column beyond the five required ones is treated as a user-supplied
  # predictor. Returns a named vector mapping the original column name to its
  # logged counterpart, which is spliced into the variable lookups used by the
  # model tables, check_coefficients(), and the scenario table.
  addnl_vars <- reactive({
    req(processed_data())
    df <- processed_data()

    extra_variables <- names(df)[!names(df) %in% c("route_id","month","year","upt","vrm")]

    if (length(extra_variables) > 0){
      addnl_vars <- paste0("log_",extra_variables)
      names(addnl_vars) <- extra_variables
    } else{
      addnl_vars <- NULL
    }

    addnl_vars
  })


  ##### Copied AI code for fare change table #####


  # --- Fare change table (tab 2) -------------------------------------------
  # A small editable CRUD table: add rows with the date/prev/new inputs, edit
  # cells inline, delete selected rows. Stored in a reactiveVal so edits persist
  # across re-renders.
  # Current agency working table (no agency column yet; add on save)
  current <- reactiveVal(
    data.frame(
      change_date = as.Date(character()),
      prev_fare   = numeric(),
      new_fare    = numeric(),
      stringsAsFactors = FALSE
    )
  )

  # append a fare change row, keeping the table sorted by date
  observeEvent(input$add, {
    req(input$new_date)
    req(!is.na(input$new_prev), !is.na(input$new_new))

    df <- current()
    df <- rbind(df, data.frame(
      change_date = as.Date(input$new_date),
      prev_fare   = as.numeric(input$new_prev),
      new_fare    = as.numeric(input$new_new),
      stringsAsFactors = FALSE
    ))

    # Optional: keep sorted by date
    df <- df[order(df$change_date), ]

    current(df)
  })

  # Render current table (editable)
  # dom = "t" shows just the table body, no search or paging chrome
  output$tbl <- renderDT({
    datatable(
      current(),
      rownames = FALSE,
      selection = "multiple",
      editable = list(target = "cell", disable = list(columns = NULL)),
      options = list(dom = "t", paging = FALSE)
    ) |>
      formatStyle('change_date', backgroundColor = 'lightgrey') |>
      formatStyle('prev_fare', backgroundColor = 'lightgrey') |>
      formatStyle('new_fare', backgroundColor = 'lightgrey')
  })

  # Apply cell edits from DT to current()
  # DT reports edits with 0-based column indexes, hence col + 1.
  # Values that fail to coerce are ignored rather than written back as NA.
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    df <- current()

    i <- info$row
    j <- info$col + 1
    v <- info$value

    colname <- names(df)[j]

    if (colname == "change_date") {
      # Expect yyyy-mm-dd; coerce to Date
      v2 <- as.Date(v)
      if (is.na(v2)) return()  # ignore invalid edits
      df[i, j] <- v2
    } else {
      v2 <- suppressWarnings(as.numeric(v))
      if (is.na(v2)) return()
      df[i, j] <- v2
    }

    # Optional: re-sort after editing date
    df <- df[order(df$change_date), ]

    current(df)
  })

  # Delete selected rows
  # drop whichever rows the user has selected
  observeEvent(input$delete, {
    sel <- input$tbl_rows_selected
    if (length(sel) == 0) return()
    df <- current()
    df <- df[-sel, , drop = FALSE]
    current(df)
  })

  # switching the question back to "no" clears any rows already entered, so a
  # stale fare history can't silently feed the model
  observeEvent(input$has_fare_changes, {
    if (input$has_fare_changes == "no") {
      # clear your current fare-change table
      current(current()[0, , drop = FALSE])
      # optionally reset the add-row inputs too
      updateNumericInput(session, "new_prev", value = NA)
      updateNumericInput(session, "new_new",  value = NA)
      updateDateInput(session, "new_date", value = Sys.Date())
    }
  })

  # Normalizes the fare table for downstream use: an empty table becomes NULL,
  # which is what make_model_data_frame() expects when there is no fare history.
  fare_tbl <- reactive({
    req(current())

    fare_df <- current()

    if (nrow(fare_df) == 0){
      updated_fare_df <- NULL
    } else {
      updated_fare_df <- fare_df
    }

    updated_fare_df

  })


  # fare_tbl <- data.frame(change_date = "2024-06-20",
  #                        prev_fare = 2,
  #                        new_fare = 2.5)
  #
  # brt_tbl <- data.frame(change_date_brt = "2023-04-14",
  #                       routes_brt = "14")


  # Show a preview of the input that was just uploaded
  # five-row preview of the upload, read-only
  output$input_data <- renderDT({
    req(processed_data())
    processed_data() |>
      datatable(rownames = FALSE,
                options = list(
                  pageLength = 5,                         # Sets default view to 5 rows
                  lengthChange = FALSE,                  # doesn't allow changing the length
                  searching = FALSE                     # doesn't allow searching
                ))
  })


  #### 3. GTFS UPLOAD ####

  # getting routes sf from gtfs
  # Parses the GTFS zip into one geometry per route. Returns the string
  # "error" (not a condition) when tidytransit can't read the feed, so callers
  # test with inherits(route_sf(), "character") before using it.
  route_sf <- reactive({
    req(input$upload_routes) # Ensure a file is uploaded
    routes <- input$upload_routes$datapath

    is_zip <- grepl("\\.zip$", tolower(routes))

    if (is_zip == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "File must be a GTFS zip file (.zip)"
        )
      )
      return(NULL)
    }

    tryCatch(
      {
        get_gtfs_routes(routes)
      },
      error = function(e) {
        return("error")
      }
    )

  })

  # display a message stating that the plot is rendering
  observe({
    showNotification(
      paste("Plotting routes. Please wait a moment."),
      type = "message",
      duration = 20
    )
  }) |>
    bindEvent(input$upload_routes)

  # getting the counties it touches
  # Counties the routes pass through. Drives both the map's background layer
  # and the state/county FIPS codes sent to tigris and tidycensus.
  county_sf <- reactive({
    req(!inherits(route_sf(), "character"))
    find_overlapping_counties(route_sf())
  })

  # once someone uploads the gtfs,
  # On a successful upload, draw the map and reveal the API key input and the
  # Census pull button. These are rendered here rather than defined statically
  # so they can't be clicked before a valid feed exists.
  observeEvent(input$upload_routes, {
    req(!inherits(route_sf(), "character"))

    output$route_map <- renderLeaflet({
      make_route_leaflet(route_sf(),county_sf())
    })

    output$acs_description <- renderText({
      "Your transit routes and the counties they cross are displayed to the right. If these are the routes and counties you expected, you are ready for the next step. Input your Census API keye, and click the button to pull census data."
    })


    output$api_key_placeholder <- renderUI({
      passwordInput("api_key", "Census API Key", placeholder = "40-character key")
    })

    output$api_button_placeholder <- renderUI({
      actionButton("submit_key", "Submit API Key")
    })

    # Once finished, render the button to pull acs data
    output$acs_button_placeholder <- renderUI({
      input_task_button("get_acs", "Get Census Data")
    })


  })

  # check to make sure it is a good file
  observe({
    req(route_sf())

    if (inherits(route_sf(), "character")){

      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "Unable to process file. Make sure it is a valid GTFS formatted file."
        )
      )

    } else{

    }

  }) |>
    bindEvent(input$upload_routes)


  # --- Census retrieval -----------------------------------------------------
  # The longest-running step in the app. Validates the API key, checks that the
  # ridership and GTFS route IDs agree, then runs the full geography pipeline
  # inside a progress bar.
  ##### GET AND PREPARE CENSUS DATA #####

  acs_data <- reactiveVal(NULL)

  key_rv <- reactiveVal(NULL)

  # Validates the key with one cheap, known-good query before accepting it, so
  # a typo is caught here rather than midway through the expensive pull.
  observeEvent(input$submit_key, {
    k <- trimws(input$api_key)
    req(nzchar(k))

    # validate with a cheap call before accepting it
    test <- tryCatch(
      tidycensus::get_acs(geography = "state", variables = "B01003_001",
                          year = 2022, state = "TN", key = k),
      error = function(e) e
    )

    if (inherits(test, "error")) {
      key_rv(NULL)
      showNotification("Invalid API key — check it and try again.", type = "error")
    } else {
      key_rv(k)
      showNotification("API key accepted.", type = "message")
    }
  })


  observeEvent(input$get_acs, {  #TODO: If they haven't input ridership data, this won't run, but in that case I need to put a popup to let users know that.

    if (is.null(input$upload_data)){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It looks like you have nut uploaded your riderhsip data. Can not retrieve census data without riderhsip data."
        )
      )

      return()
    } else if(data_check() == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "It looks like there is something wrong with your ridership data. Can not retrieve census data without proper riderhsip data"
        )
      )
    }


    # The spreadsheet and the GTFS feed must name routes identically, otherwise
    # the ACS data could never be joined to ridership. Block if they disagree.
    excel_routes <- unique(processed_data()$route_id)
    gtfs_routes <- unique(route_sf()$route_id)

    if (all(excel_routes %in% gtfs_routes) == FALSE){
      showModal(
        modalDialog(
          title = "ERROR",
          easy_close = TRUE,
          "All route_id values from ridership data must be present
          in GTFS data, but some of your route IDs are not the same.
          Make sure you have the correct GTFS file and that it is naming
          the routes the same way you name them in your ridership file."
        )
      )
      return()
    }


    req(data_check() == TRUE)

    if (!is.null(key_rv())) {
      tidycensus::census_api_key(key_rv(), install = FALSE, overwrite = TRUE)
    }

    vrm_data <- processed_data()

    year_start <- min(vrm_data$year, na.rm = T)
    month_start <- vrm_data |>
      dplyr::filter(year == year_start) |>
      dplyr::pull(month) |>
      min(na.rm = TRUE)
    month_start <- paste0(year_start, "-", month_start)


    year_end <- max(vrm_data$year, na.rm = T)
    month_end <- vrm_data |>
      dplyr::filter(year == year_end) |>
      dplyr::pull(month) |>
      max(na.rm = TRUE)
    month_end <- paste0(year_end, "-", month_end)

    # This prevents it from looking for ACS data above 2024.
    # TODO: It sill need to be updated to 2025 when the ACS 2025 data is available
    if(year_end > 2024){
      year_end_val <- 2024
    } else {
      year_end_val <- year_end
    }


    # The geography pipeline, in order:
    #   get_tract_geometry()  -> tract boundaries per year
    #   create_intersecting_tract_percentages() -> buffer overlap weights
    #   pull_acs_data()       -> raw ACS variables per tract
    #   combine_acs_data()    -> one column per readable variable
    #   create_final_acs_data() -> route-level monthly series
    # Each step that can fail shows a modal and halts via req().
    res <- withProgress(message = "Organizing Census Data...",
                        detail = "this could take a minute or two", value = 0, {

                          incProgress(0.05, detail = "Starting process")
                          route_geom  <- route_sf()
                          county_info <- county_sf()

                          state_fps  <- unique(county_info$STATEFP)
                          county_fps <- unique(county_info$COUNTYFP)
                          year_vals  <- (year_start-1):year_end_val

                          incProgress(0.20, detail = "Pulling in census tracts")
                          census_tract_geom <- get_tract_geometry(state_fps, county_fps, year_vals)

                          if (is.null(census_tract_geom)){
                            showModal(
                              modalDialog(
                                title = "ERROR",
                                easy_close = TRUE,
                                "Unable to retrieve census data.
                                If you are offline or there is a government shutdown, the data is unable to be accessed.
                                If your computer has no storage space on its hard drive, this could also be the problem.
                                If you have good internet connection and storage, and if www.census.gov seems to be working properly,
                                you might find success by simply trying this function again."
                              )
                            )
                          }

                          req(census_tract_geom)

                          incProgress(0.15, detail = "Finding tracts that intersect bus routes")
                          tract_buffer_data <- create_intersecting_tract_percentages(
                            census_tract_geom, route_geom
                          )

                          incProgress(0.30, detail = "Pulling ACS data")
                          pulled_acs <- pull_acs_data(county_sf = county_info, years = year_vals)

                          if ("errors" %in% names(pulled_acs)){
                            showModal(
                              modalDialog(
                                title = "ERROR",
                                easy_close = TRUE,
                                "Unable to retrieve census data.
                                If you are offline or there is a government shutdown, the data is unable to be accessed.
                                If you have good internet connection and www.census.gov seems to be working properly,
                                you might find success by simply trying this function again."
                              )
                            )
                          }

                          req(!"errors" %in% names(pulled_acs))

                          incProgress(0.05, detail = "Organizing ACS data")
                          organized_acs <- combine_acs_data(pulled_acs)

                          incProgress(0.20, detail = "Preparing data for model")
                          acs_data <- create_final_acs_data(
                            combined_acs_data   = organized_acs,
                            intersecting_tracts = tract_buffer_data,
                            start_month         = month_start,
                            end_month           = month_end
                          )

                          acs_data
                        })

    # cache the finished ACS data and move the user on to Model Creation
    acs_data(res)  # store result so outputs can use it

    bslib::nav_select("main_nav", "pan_4")
  })


  #### 4. MODEL CREATION ####
  # get first model after user inputs the files
  # The default model: every available variable is offered to
  # create_regression_model(), which prunes back to a 10% significance
  # threshold. The named vector below is the full candidate set, with display
  # labels as names.
  first_model <- reactive({ # first model
    req(input$upload_data$datapath)
    req(acs_data())
    acs <- acs_data()
    # acs <- acs_data
    xl_data <- processed_data()
    vars <- c("[VRM]" = "log_vrm",
              "[Month]" = "factor(month)",
              "[Year]" = "year_cent",
              "[Year Squared]" = "year_cent^2",
              "[Gas Price]" = "log_gas_price",
              "[% No Vehicle Households]" = "log_perc_hshlds_noveh",
              "[% Workers Below Federal Poverty Line]" = "log_below_fpl",
              "[% Commuting by Car]" = "log_perc_car",
              "[% Commuting by Taxi]" = "log_perc_taxicab",
              "[% Work From Home]" = "log_perc_wfh",
              "[% Female Workers]" = "log_perc_female",
              "[% Workers Between 100-150% of Federal Povery Level]" = "log_fpl_100_150",
              "[% Workers in Renter Occupied Housing Units]" = "log_perc_renter_occupied",
              "[Labor Participation Rate]" = "log_labor_part_rate",
              "[Unemployment Rate]" = "log_unemp_rate",
              "[Bus Rapid Transit]" = "brt",
              "[Adult Base Fare]" = "log_fare",
              addnl_vars())
    fare_tbl <- fare_tbl()


    create_regression_model(data_xlsx = xl_data,
                            acs_data = acs,
                            gas_data = gas,
                            variables = vars,
                            fare_df = fare_tbl) # TODO: this doesn't seem to be working
  })

  # Renders the default model's coefficient table: raw names swapped for
  # readable labels, stars appended by significance, and rows shaded green to
  # yellow by p-value. The numeric pval helper column is hidden at the end.
  output$tbl_mod_stepwise <- render_gt({
    created_model <- first_model() # first model

    name_key <- c("VRM" = "log_vrm",
                  "February" = "factor(month)2",
                  "March" = "factor(month)3",
                  "April" = "factor(month)4",
                  "May" = "factor(month)5",
                  "June" = "factor(month)6",
                  "July" = "factor(month)7",
                  "August" = "factor(month)8",
                  "September" = "factor(month)9",
                  "October" = "factor(month)10",
                  "November" = "factor(month)11",
                  "December" = "factor(month)12",
                  "Year" = "year_cent",
                  "Year Squared" = "I(year_cent^2)",
                  "Gas Price" = "log_gas_price",
                  "% No Vehicle Households" = "log_perc_hshlds_noveh",
                  "% Workers Below Federal Poverty Line" = "log_below_fpl",
                  "% Commuting by Car" = "log_perc_car",
                  "% Commuting by Taxi" = "log_perc_taxicab",
                  "% Work From Home" = "log_perc_wfh",
                  "% Female Workers" = "log_perc_female",
                  "% Workers Between 100-150% of Federal Povery Level" = "log_fpl_100_150",
                  "% Workers in Renter Occupied Housing Units" = "log_perc_renter_occupied",
                  "Labor Participation Rate" = "log_labor_part_rate",
                  "Unemployment Rate" = "log_unemp_rate",
                  "Is Bus Rapid Transit" = "brtTRUE",
                  "Fare" = "log_fare",
                  addnl_vars())

    var_table <- data.frame("Variable" = names(name_key),
                            "vars" = unname(name_key))

    coef_table <- data.frame("vars" = names(created_model$coefficients),
                             "Coeff" = round(created_model$coefficients,3),
                             "P.value" = round(fixest::pvalue(created_model), 3))

    table <- coef_table |>
      left_join(var_table, by = "vars") |>
      select("Variable", "Coeff", "P.value")

    table <- table |>
      mutate(pval = P.value) |>
      mutate(P.value = as.character(P.value)) |>
      mutate(P.value = case_when(
        P.value <= 0.1 & P.value > 0.05  ~ paste0(P.value, "*"),
        P.value <= 0.05 & P.value > 0.01  ~ paste0(P.value, "**"),
        P.value <= 0.01  ~ paste0(P.value, "***"),
        TRUE ~ P.value
      ))

    table |>
      # left_join(var_table, by = "vars") |>
      select("Variable", "Coeff", "P.value", "pval") |>
      gt() |>
      # when it is above 10% threshold
      tab_style(
        style = cell_fill(color = "#FEFFEB"),
        locations = cells_body(rows = pval > 0.1)
      ) |>
      # when it is below 10% but above 5% threshold
      tab_style(
        style = cell_fill(color = "#FFFFFF"),
        locations = cells_body(rows = pval <= .1 & P.value > .05)
      ) |>
      # when it is below 5% but above 1% threshold
      tab_style(
        style = cell_fill(color = "#F2FFF9"),
        locations = cells_body(rows = pval <= .05 & P.value > .01)
      ) |>
      # when it is below 1% threshold
      tab_style(
        style = cell_fill(color = "#E6FFF2"),
        locations = cells_body(rows = pval <= .01)
      ) |>
      cols_hide(columns = pval)
  })

  # Create the proxy handle for the output table
  # Proxy handle created for in-place table updates. Not currently used; the
  # table is re-rendered rather than patched.
  proxy <- dataTableProxy("tbl_mod_stepwise")

  # once acs is uploaded, then it will update the selections for the model creation on the next page
  # Once ACS data exists, populate the variable picker on tab 4 with every
  # candidate variable, pre-selecting the ones the default model kept. Month
  # dummies and the squared year term are collapsed back to their formula form
  # ("factor(month)", "year_cent^2") so the selection round-trips correctly.
  observeEvent(acs_data(),
               {
                 req(acs_data())
                 variables_used <- names(first_model()$coefficients) #first model

                 all_vars <- c("[VRM]" = "log_vrm",
                               "[Month]" = "factor(month)",
                               "[Year]" = "year_cent",
                               "[Year Squared]" = "year_cent^2",
                               "[Gas Price]" = "log_gas_price",
                               "[% No Vehicle Households]" = "log_perc_hshlds_noveh",
                               "[% Workers Below Federal Poverty Line]" = "log_below_fpl",
                               "[% Commuting by Car]" = "log_perc_car",
                               "[% Commuting by Taxi]" = "log_perc_taxicab",
                               "[% Work From Home]" = "log_perc_wfh",
                               "[% Female Workers]" = "log_perc_female",
                               "[% Workers Between 100-150% of Federal Povery Level]" = "log_fpl_100_150",
                               "[% Workers in Renter Occupied Housing Units]" = "log_perc_renter_occupied",
                               "[Labor Participation Rate]" = "log_labor_part_rate",
                               "[Unemployment Rate]" = "log_unemp_rate",
                               "[Bus Rapid Transit]" = "brt",
                               "[Adult Base Fare]" = "log_fare",
                               addnl_vars())

                 if (TRUE %in% grepl("month",variables_used)){
                   new_selected <- c(variables_used[grepl("month",variables_used) == FALSE], "factor(month)")
                 } else{
                   new_selected <- variables_used
                 }
                 if (TRUE %in% grepl("year_cent^2",new_selected,fixed = TRUE)){
                   new_selected <- c(new_selected[grepl("year_cent^2",new_selected,fixed = TRUE) == FALSE], "year_cent^2")
                 }

                 new_vars <- all_vars[all_vars %in% new_selected]

                 # updateSelectizeInput(inputId = "variables",
                 #                      choices = new_vars,
                 #                      selected = new_vars)

                 updateSelectizeInput(inputId = "variables_forced",
                                      choices = all_vars,
                                      selected = new_vars)

               })

  # value to make sure the model has been run
  # Left over from an earlier two-model flow; model_forced() is now the source
  # of truth for whether a custom model exists.
  model_ran <- reactiveVal(FALSE)

  model_forced <- reactiveVal(NULL)



  # Fits the user's hand-picked specification with no stepwise elimination.
  # NOTE: gas_data = gas is passed here, but create_regression_model_forced()
  # declares that parameter as gas_csv, so this call will error with
  # "unused argument" until the two names agree.
  observeEvent(input$run_model_forced,
               ignoreNULL = TRUE,
               {
                 req(input$upload_data$datapath)
                 req(acs_data())

                 mod <- create_regression_model_forced(data_xlsx = processed_data(),
                                                       acs_data = acs_data(),
                                                       gas_data = gas,
                                                       variables = input$variables_forced,
                                                       fare_df = fare_tbl())
                 model_forced(mod)
               })


  # show the coefficients that were generated from regression model function
  # Same coefficient table treatment as the default model, for side-by-side
  # comparison on tab 4.
  output$tbl_mod_forced <- render_gt({
    req(model_forced())

    created_model <- model_forced()

    name_key <- c("VRM" = "log_vrm",
                  "February" = "factor(month)2",
                  "March" = "factor(month)3",
                  "April" = "factor(month)4",
                  "May" = "factor(month)5",
                  "June" = "factor(month)6",
                  "July" = "factor(month)7",
                  "August" = "factor(month)8",
                  "September" = "factor(month)9",
                  "October" = "factor(month)10",
                  "November" = "factor(month)11",
                  "December" = "factor(month)12",
                  "Year" = "year_cent",
                  "Year Squared" = "I(year_cent^2)",
                  "Gas Price" = "log_gas_price",
                  "% No Vehicle Households" = "log_perc_hshlds_noveh",
                  "% Workers Below Federal Poverty Line" = "log_below_fpl",
                  "% Commuting by Car" = "log_perc_car",
                  "% Commuting by Taxi" = "log_perc_taxicab",
                  "% Work From Home" = "log_perc_wfh",
                  "% Female Workers" = "log_perc_female",
                  "% Workers Between 100-150% of Federal Povery Level" = "log_fpl_100_150",
                  "% Workers in Renter Occupied Housing Units" = "log_perc_renter_occupied",
                  "Labor Participation Rate" = "log_labor_part_rate",
                  "Unemployment Rate" = "log_unemp_rate",
                  "Is Bus Rapid Transit" = "brtTRUE",
                  "Fare" = "log_fare",
                  addnl_vars())

    var_table <- data.frame("Variable" = names(name_key),
                            "vars" = unname(name_key))

    coef_table <- data.frame("vars" = names(created_model$coefficients),
                             "Coeff" = round(created_model$coefficients,3),
                             "P.value" = round(fixest::pvalue(created_model), 3))


    coef_table <- coef_table |>
      mutate(pval = P.value) |>
      mutate(P.value = as.character(P.value)) |>
      mutate(P.value = case_when(
        P.value <= 0.1 & P.value > 0.05  ~ paste0(P.value, "*"),
        P.value <= 0.05 & P.value > 0.01  ~ paste0(P.value, "**"),
        P.value <= 0.01  ~ paste0(P.value, "***"),
        TRUE ~ P.value
      ))

    coef_table |>
      left_join(var_table, by = "vars") |>
      select("Variable", "Coeff", "P.value", "pval") |>
      gt() |>
      # when it is above 10% threshold
      tab_style(
        style = cell_fill(color = "#FEFFEB"),
        locations = cells_body(rows = pval > 0.1)
      ) |>
      # when it is below 10% but above 5% threshold
      tab_style(
        style = cell_fill(color = "#FFFFFF"),
        locations = cells_body(rows = pval <= .1 & P.value > .05)
      ) |>
      # when it is below 5% but above 1% threshold
      tab_style(
        style = cell_fill(color = "#F2FFF9"),
        locations = cells_body(rows = pval <= .05 & P.value > .01)
      ) |>
      # when it is below 1% threshold
      tab_style(
        style = cell_fill(color = "#E6FFF2"),
        locations = cells_body(rows = pval <= .01)
      ) |>
      cols_hide(columns = pval)
  })

  # Whichever model the user commits to; everything downstream reads this.
  selected_model <- reactiveVal(NULL)

  # "Continue With This Model" under the default model
  observeEvent(input$use_this_model_button, {
    req(first_model())

    # if (model_ran() == FALSE){ # if the second model was not run, then use the first model
    #   m <- isolate(first_model())
    # } else{
    #   m <- isolate(model())
    # }

    m <- isolate(first_model())

    selected_model(m)

    bslib::nav_select("main_nav", "pan_5")
  })

  # "Continue With This Model" under the alternative model
  observeEvent(input$use_this_model_button_forced, {
    fm <- isolate(model_forced())
    req(fm)
    selected_model(fm)
    bslib::nav_select("main_nav", "pan_5")
  })


  #### 5. REVIEW MODEL ####
  # Coefficient sanity check, colored red for impossible signs and yellow for
  # questionable ones.
  output$coefficients_review <- render_gt({
    req(!is.null(selected_model()))
    check_coefficients(selected_model(),addnl_vars())
  })


  ##### BRT CHNAGES CODE #####


  # add inputs for a brt change
  # Builds the BRT entry inputs on demand. The route choices come from the
  # uploaded ridership data so the user can only pick routes that exist.
  observeEvent(input$brt_question, {

    if (input$brt_question == "yes"){

      # render the date input
      output$brt_date <- renderUI({
        dateInput(
          inputId = "brt_change_date",
          label = "Date of change",
          value = Sys.Date(),
          min = Sys.Date() - 10000,
          max = Sys.Date() + 10000,
          width = "150px")
      })

      input_df <- processed_data()
      routes <- input_df$route_id

      if (!is.null(routes)){
        route_list <- as.list(routes)
      } else{
        route_list <- list("no","routes","found")
      }

      output$brt_routes <- renderUI({
        selectizeInput(
          "select_brt_routes",
          "Routes converted to BRT",
          route_list,
          multiple = TRUE,
          width = "200px"
        )
      })


    } else{
      # render the date input
      output$brt_date <- renderUI({
        ""
      })

      output$brt_routes <- renderUI({
        ""
      })



    }

  })


  ### COPIED AI RESPONSE FOR BRT STUFF ###

  # Current agency working table
  # --- BRT conversion table (tab 5) ----------------------------------------
  # Same add/edit/delete pattern as the fare table. Multiple routes sharing one
  # conversion date are stored as a single comma-separated string and split back
  # out in brt_tbl() below.
  current_brt <- reactiveVal(
    data.frame(
      change_date_brt = as.Date(character()),
      routes_brt = character(),
      stringsAsFactors = FALSE
    )
  )

  observeEvent(input$add_brt, {
    req(input$brt_change_date)
    req(!is.na(input$select_brt_routes))

    df <- current_brt()
    df <- rbind(df, data.frame(
      change_date_brt = as.Date(input$brt_change_date),
      routes_brt   = paste(as.character(input$select_brt_routes), collapse = ","),
      stringsAsFactors = FALSE
    ))

    # Optional: keep sorted by date
    df <- df[order(df$change_date_brt), ]

    current_brt(df)
  })

  # Render current_brt table (editable)
  output$tbl_brt <- renderDT({
    datatable(
      current_brt(),
      rownames = FALSE,
      selection = "multiple",
      editable = list(target = "cell", disable = list(columns = NULL)),
      options = list(dom = "t", paging = FALSE)
    )  |>
      formatStyle('change_date_brt', backgroundColor = 'lightgrey') |>
      formatStyle('routes_brt', backgroundColor = 'lightgrey')
  })

  # Apply cell edits from DT to current_brt()
  observeEvent(input$tbl_brt_cell_edit, {
    info <- input$tbl_brt_cell_edit
    df <- current_brt()

    i <- info$row
    j <- info$col + 1
    v <- info$value

    colname <- names(df)[j]

    if (colname == "change_date_brt") {
      # Expect yyyy-mm-dd; coerce to Date
      v2 <- as.Date(v)
      if (is.na(v2)) return()  # ignore invalid edits
      df[i, j] <- v2
    }

    # Optional: re-sort after editing date
    df <- df[order(df$change_date_brt), ]

    current_brt(df)
  })

  # Delete selected rows
  observeEvent(input$delete_brt, {
    sel <- input$tbl_brt_rows_selected
    if (length(sel) == 0) return()
    df <- current_brt()
    df <- df[-sel, , drop = FALSE]
    current_brt(df)
  })

  observeEvent(input$brt_question, {
    if (input$brt_question == "no") {
      # clear your current_brt fare-change table
      current_brt(current_brt()[0, , drop = FALSE])
      # optionally reset the add-row inputs too
      updateDateInput(session, "brt_change_date", value = Sys.Date())
    }
  })



  # Expands the comma-separated route strings into one row per route-date pair,
  # which is the shape forecast_ridership() expects. Returns NULL when empty.
  brt_tbl <- reactive({
    req(current_brt())

    brt_df <- current_brt()

    if (nrow(brt_df) == 0){
      updated_brt_df <- NULL
    } else {
      df_list <- list()
      for (row_num in 1:nrow(brt_df)){
        new_df <- data.frame(change_date_brt = brt_df$change_date_brt[[row_num]], routes_brt = unlist(strsplit(brt_df$routes_brt[[row_num]],",")))
        df_list[[row_num]] <- new_df
      }

      updated_brt_df <- bind_rows(df_list)
    }

    updated_brt_df

  })



  # Builds the list of literature elasticities the user may substitute for
  # their own estimates. VRM is always offered; gas, fares, and BRT appear only
  # when relevant to the chosen model or the user's stated plans.
  foreced_coef_choices <- reactive({
    req(input$brt_question)
    req(input$fare_question)
    req(selected_model())

    coefs <- coef(selected_model())

    choices <- c("VRM (0.45)" = "vrm")
    if ("log_gas_price" %in% names(coefs)){
      choices <- c(choices, "Gas Price (0.15)" = "gas")
    }

    if ("log_fare" %in% names(coefs) | input$fare_question == "yes"){
      choices <- c(choices, "Fares (-0.3)" = "fares")
    } else{
      choices <- choices[!choices %in% "fares"]
    }

    if (input$brt_question == "yes"){
      choices <- c(choices, "BRT (0.3)" = "brt")
    } else{
      choices <- choices[!choices %in% "brt"]
    }

    choices |> as.list()

  })

  output$forced_coef_placeholder <- renderUI({
    checkboxGroupInput(
      inputId = "forced_coef_checkbox",
      label = "Common Coefficient Values",
      choices = foreced_coef_choices()
    )
  })


  # getting a vector with the final coefficients that will be used in the model
  # Merges the model's estimated coefficients with any literature values the
  # user chose to force. The `map` vector translates between the checkbox IDs
  # ("vrm", "gas") and the model's variable names ("log_vrm", "log_gas_price"),
  # then the estimated versions of the overridden variables are dropped and the
  # forced values appended in their place.
  final_coefs <- reactive({
    req(selected_model())

    if (is.null(input$forced_coef_checkbox)){
      final_coefs <- coef(selected_model())
    } else{
      updated_coefs <- input$forced_coef_checkbox
      # updated_coefs <- c("vrm","gas","fares")

      new_coefs <- c(vrm = .45, gas = .15, fares = -0.3, brt = -0.3)
      new_coefs <- new_coefs[names(new_coefs) %in% updated_coefs]

      coefs_og <- coef(selected_model())

      # map coefs_og names -> new_coef names
      map <- c(log_vrm = "vrm",
               log_gas_price = "gas",
               log_fare = "fares",
               brt = "brt")
      new_names <- map[map %in% names(new_coefs)]
      names(new_coefs) <- names(new_names)

      # replace v2 values where mapping exists (and key exists in v1)
      idx <- !names(coefs_og) %in% names(new_coefs)
      final_coefs <- c(coefs_og[idx],new_coefs)
    }


    final_coefs

  })


  #### 6 FORECAST AND VIZUALIZATION ####

  # every route present in the uploaded ridership data
  routes <- reactive({
    req(input$upload_data$datapath)
    ridership_df <- processed_data()

    unique(ridership_df$route_id)
  })

  # the editable Low/Mid/High scenario table currently on screen
  v <- reactiveValues(data = NULL)

  # NEW: store saved scenarios (all routes)
  saved <- reactiveValues(by_route = NULL)

  # NEW: for overwrite confirmation workflow
  pending <- reactiveValues(routes_to_save = NULL, mode = NULL)

  # NEW: your full set of routes (replace routes() with your real source)
  all_routes <- reactive({
    req(routes())     # routes() should return a character vector of route ids/names
    routes()
  })

  # NEW: which routes already have saved scenarios
  saved_routes <- reactive({
    if (is.null(saved$by_route)) character(0) else sort(unique(saved$by_route$Route))
  })

  # NEW: remaining (unsaved) routes
  unsaved_routes <- reactive({
    setdiff(all_routes(), saved_routes())
  })

  # Use an observer to get the model they choose to use
  # and create a data frame for the forecasting inputs
  # Seeds the scenario table with one row per model variable and placeholder
  # assumptions of -1% / 2% / 5%, then moves to the Forecasting Inputs tab.
  observeEvent(input$proceed_to_forecast, {
    req(final_coefs())
    elasticities <- get_elasticity_varaibles(final_coefs(), addnl_vars())

    elast_table <- data.frame("Variable" = names(elasticities),
                              "Low" = "-1%",
                              "Mid" = "2%",
                              "High" = "5%")
    elast_table <- elast_table[,2:4]
    rownames(elast_table) <- names(elasticities)
    names(elast_table) <- c("Low Estimate","Mid Estimate","High Estimate")

    # Update the reactiveValues with the elasticities table
    v$data <- elast_table

    bslib::nav_select("main_nav", "pan_6")
  })

  # The route dropdown's contents depend on the mode: unsaved routes (plus an
  # "all remaining" shortcut) when adding, already-saved routes when overwriting.
  # NEW: keep the route dropdown updated based on mode and what's already saved
  observe({
    req(input$route_mode)

    if (input$route_mode == "new") {
      rem <- unsaved_routes()
      choices <- c("All remaining routes" = "__ALL_REMAINING__", rem)

      updateSelectInput(
        session, "route_selected",
        choices = choices,
        selected = if (length(rem)) rem[1] else "__ALL_REMAINING__"
      )

    } else { # overwrite mode
      existing <- saved_routes()
      choices <- c( "All saved routes" = "__ALL_SAVED__", existing)

      updateSelectInput(
        session, "route_selected",
        choices = choices,
        selected = if (length(existing)) existing[1] else "__ALL_SAVED__"
      )
    }
  })

  # 2. Render the table with editable = TRUE
  # The editable scenario grid. Users type values like "5%" directly into cells.
  output$dtScenarios <- renderDT({
    req(v$data)
    datatable(v$data, editable = 'cell', selection = 'none',
              options = list(
                dom = 't',         # Only show the Table (hides search, paging, etc.)
                paging = FALSE,    # Show all data at once
                ordering = FALSE,  # Disable column sorting
                searching = FALSE  # Remove the search box
              ))
  })

  # 3. Use a proxy to update the table without a full re-render
  proxy_scenarios <- dataTableProxy('dtScenarios')

  # 4. Observe the 'cell_edit' event
  # Writes an edited cell back into v$data and refreshes the table through the
  # proxy, avoiding a full re-render that would lose scroll position.
  observeEvent(input$dtScenarios_cell_edit, {
    info <- input$dtScenarios_cell_edit

    # Extract row and column (DT uses 0-based indexing for columns)
    i <- info$row
    j <- info$col # they say i need to have +1, but I don't htink I do
    k <- info$value

    v$data[i, j] <- DT::coerceValue(k, v$data[i, j])

    replaceData(proxy_scenarios, v$data, resetPaging = FALSE)
  })



  # NEW: helper that performs the save (overwrites only targeted routes)
  # Stamps the current scenario grid onto each target route and merges it into
  # the saved store, replacing any existing rows for those routes.
  save_routes <- function(routes_to_save) {
    base <- data.frame(
      Variable = rownames(v$data),
      as.data.frame(v$data, stringsAsFactors = FALSE),
      row.names = NULL
    )

    to_save <- do.call(rbind, lapply(routes_to_save, function(rt) cbind(Route = rt, base)))

    if (is.null(saved$by_route)) {
      saved$by_route <- to_save
    } else {
      saved$by_route <- subset(saved$by_route, !(Route %in% routes_to_save))
      saved$by_route <- rbind(saved$by_route, to_save)
    }
  }

  # NEW: Save button handler (with confirmation when overwriting)
  # Save handler. Overwriting existing routes requires an explicit confirmation
  # modal; saving to new routes happens immediately.
  observeEvent(input$save_route_scenario, {
    req(v$data, input$route_selected, input$route_mode)

    routes_to_save <- if (input$route_mode == "new") {
      if (identical(input$route_selected, "__ALL_REMAINING__")) unsaved_routes() else input$route_selected
    } else {
      if (identical(input$route_selected, "__ALL_SAVED__")) saved_routes() else input$route_selected
    }

    validate(need(length(routes_to_save) > 0, "No routes selected."))

    # Require confirmation for overwrite mode
    if (input$route_mode == "overwrite") {
      pending$routes_to_save <- routes_to_save
      pending$mode <- "overwrite"

      showModal(modalDialog(
        title = "Confirm overwrite",
        paste0(
          "You are about to overwrite saved scenarios for ",
          length(routes_to_save), " route(s):\n",
          paste(head(routes_to_save, 10), collapse = ", "),
          if (length(routes_to_save) > 10) ", ..." else ""
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_overwrite", "Yes, overwrite", class = "btn-danger")
        )
      ))
      return()
    }

    # New routes mode: save immediately
    save_routes(routes_to_save)
  })

  # NEW: confirm overwrite
  observeEvent(input$confirm_overwrite, {
    removeModal()
    req(pending$routes_to_save)
    save_routes(pending$routes_to_save)
    pending$routes_to_save <- NULL
    pending$mode <- NULL
  })

  # view saved scenarios
  # running list of every route that now has saved assumptions
  output$dtSavedScenarios <- renderDT({
    req(saved$by_route)
    datatable(saved$by_route, options = list(pageLength = 25),
              rownames = FALSE)
  })



  #### RUNNING FORECASTS ####

  # The forecast run. Routes that share identical Low/Mid/High assumptions are
  # grouped and forecast together (one call per distinct assumption set rather
  # than one per route), then the results are filtered back down to the routes
  # in each group and stacked.
  #
  # Note the guard near the bottom: the forecast only runs when every route has
  # saved assumptions, otherwise final_df is NULL and the downstream outputs
  # stay empty.
  forecast_df <- eventReactive(input$buttonRun, {

    req(final_coefs())
    req(acs_data())
    req(!is.null(v$data))
    req(saved$by_route)

    saved_predictions <- saved$by_route

    # TODO: Delete code below
    # this is me manually creating the saved predictions to test things.

    # saved_predictions <- expand.grid("Route" = routes,
    #   "Variable" = names(elasticities),
    #                           "Low.Estimate" = "-1",
    #                           "Mid.Estimate" = "2%",
    #                           "High.Estimate" = "5%")
    #
    # saved_predictions$Route <- as.character(saved_predictions$Route)
    # saved_predictions$Variable <- as.character(saved_predictions$Variable)
    # saved_predictions$Low.Estimate <- as.character(saved_predictions$Low.Estimate)
    # saved_predictions$Mid.Estimate <- as.character(saved_predictions$Mid.Estimate)
    # saved_predictions$High.Estimate <- as.character(saved_predictions$High.Estimate)
    #
    # which(saved_predictions$Route == 9)
    # saved_predictions[1,"Low.Estimate"] <- -10
    # saved_predictions[28,"Low.Estimate"] <- -5
    # saved_predictions[55,"Low.Estimate"] <- -3


    # TODO: Delete code above ^

    routes <- routes()

    coefs <- final_coefs()
    acs <- acs_data()

    processed_data <- processed_data()

    # Collapse each route's assumptions into a single string per scenario, then
    # assign a shared group ID to routes whose strings all match.
    grouped_routes <- saved_predictions |>
      group_by(Route) |>
      summarize(low = paste(Low.Estimate, collapse = ", "),
                mid = paste(Mid.Estimate, collapse = ", "),
                high = paste(High.Estimate, collapse = ", ")) |>
      ungroup() |>
      group_by(low, mid, high) |>
      mutate(route_group_id = cur_group_id()) |>
      ungroup() |>
      select(Route, route_group_id)

    grouped_predictions <- saved_predictions |>
      left_join(grouped_routes, by = join_by(Route))


    if (length(unique(saved_predictions$Route)) == length(routes)){ # TODO: I think I should make it so that it will work even if they don't make a prediction for all the routes
      forecast_dfs <- list()
      for (group_id in unique(grouped_predictions$route_group_id)){
        filtered_grouped_predictions <- grouped_predictions |>
          filter(route_group_id == group_id)

        one_route_in_group <- filtered_grouped_predictions$Route[[1]]

        scenario_df <- grouped_predictions |>
          filter(Route == one_route_in_group)

        if (input$select_1_or_5 == "one"){
          fiveyear <- FALSE
        } else{
          fiveyear <- TRUE
        }

        df_unfiltered <- forecast_ridership(coefs = coefs,
                                            data_xlsx = processed_data,
                                            acs_data = acs,
                                            gas_data = gas,
                                            scenario_inputs_df = scenario_df,
                                            start_year = NULL,
                                            start_month = NULL,
                                            five_year = fiveyear)


        df <- df_unfiltered |>
          filter(route_id %in% unique(filtered_grouped_predictions$Route))

        forecast_dfs[[as.character(group_id)]] <- df
      }

      final_df <- bind_rows(forecast_dfs)

      # system-wide totals, recomputed after the per-group forecasts are stacked
      df_all_routes <- final_df |>
        summarize(route_id = "all_routes",
                  avg_daily_upt = sum(avg_daily_upt, na.rm = T),
                  tot_weekday_upt = sum(tot_weekday_upt, na.rm = T),
                  .by = c(year, month, scenario)) |>
        mutate(date = ym(paste(year, month,sep = "/")))

      final_df <- bind_rows(final_df, df_all_routes)


    } else{
      final_df <- NULL
    }

    final_df

  })

  # Advances to the Visualization tab.
  observeEvent(input$buttonRun, {
    bslib::nav_select("main_nav", "pan_7")
  })


  #### 7. VISUALIZATION PAGE ####

  observeEvent(input$buttonRun, {
    req(forecast_df())
    df <- forecast_df()
    updateSelectInput(
      session, "input_route_to_plot",
      choices = unique(df$route_id),
      selected = unique(df$route_id)[1]
    )
  }, ignoreInit = TRUE)

  # redraws whenever the user picks a different route from the dropdown
  output$viz_plot <- renderPlot({
    req(forecast_df())
    df <- forecast_df()
    plot_forecast(df, route = input$input_route_to_plot)
  }, res = 120)

  # go to the next tab when clicked
  observe({
    bslib::nav_select("main_nav", "pan_8")
  }) |>
    bindEvent(input$go_to_export)

  # Handle the ZIP download
  # Writes one PNG per route to a temp directory and zips them. The "-j" flag
  # flattens the archive so it contains files rather than nested temp folders.
  output$download_plots <- downloadHandler(
    filename = function() {
      paste0("ridership_plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(forecast_df())

      forecast_df <- forecast_df()
      routes <- c(unique(forecast_df$route_id),"all_routes")

      files_to_zip <- get_files_to_zip(forecast_df, routes)

      # Zip the files. 'file' is the target path Shiny provides for the final download.
      # use zip::zip() or R's native utils::zip()
      utils::zip(zipfile = file, files = files_to_zip, flags = "-j")
      # Note: The "-j" flag junk-paths, meaning it zips just the files without keeping the absolute temp folder structure
    },
    contentType = "application/zip"
  )


  #### 8. FINAL DOWNLOAD PAGE ####

  # the exact column set exported to CSV and Excel, documented on tab 8
  output_df <- reactive({
    forecast_df() |>
      select(route_id, year, month, avg_daily_upt, tot_weekday_upt, forecast, scenario, date)
  })

  # Show a preview of the output that is about to be downloaded
  # rounded preview of the export; rounding is display-only and does not
  # affect the downloaded files
  output$outputExample <- renderDT({
    req(forecast_df())
    output_df <- output_df()
    output_df |>
      mutate(avg_daily_upt = round(avg_daily_upt,1),
             tot_weekday_upt = round(tot_weekday_upt,1))
  })

  # Handle the csv download
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("TRiP-forcasted-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(output_df(), file, row.names = FALSE)
    }
  )

  # Handle the xlsx download
  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste("TRiP-forcasted-data-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(output_df(), file)
    }
  )

  invisible(NULL)
}
