# app.R
# AirQuality Explorer — a multi-tab Shiny app for the built-in `airquality` dataset

# ---- Packages ----
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(bslib)

# ---- Data prep ----
aq <- airquality |>
  mutate(
    MonthName = factor(Month, levels = 5:9, labels = month.name[5:9])
  )

numeric_vars <- c("Ozone", "Solar.R", "Wind", "Temp")
month_choices <- setNames(5:9, month.name[5:9])

# ---- UI ----
ui <- navbarPage(
  title = "AirQuality Explorer",
  theme = bs_theme(bootswatch = "flatly"),
  collapsible = TRUE,
  inverse = TRUE,
  
  tabPanel(
    "Overview",
    fluidRow(
      column(
        width = 4,
        br(),
        wellPanel(
          h4("Dataset at a glance"),
          uiOutput("meta_cards")
        ),
        wellPanel(
          h4("Filter snapshot"),
          tags$small("Adjust filters in 'Filter & Data'"),
          tableOutput("filter_echo")
        )
      ),
      column(
        width = 8,
        br(),
        plotOutput("overview_plot", height = "420px"),
        br(),
        wellPanel(
          h5("NA Summary (entire dataset)"),
          tableOutput("na_summary")
        )
      )
    )
  ),
  
  tabPanel(
    "Filter & Data",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        checkboxGroupInput(
          "months", "Months",
          choices = month_choices, selected = 5:9, inline = FALSE
        ),
        sliderInput("day_range", "Day of Month", min = 1, max = 31, value = c(1, 31), step = 1),
        checkboxInput(
          "drop_na",
          "Drop rows with any NA in Ozone, Solar.R, Wind, Temp",
          value = TRUE
        ),
        hr(),
        downloadButton("dl_data", "Download filtered data (CSV)")
      ),
      mainPanel(
        DTOutput("data_table")
      )
    )
  ),
  
  tabPanel(
    "Summaries",
    fluidRow(
      column(
        6,
        br(),
        h4("Summary by Month"),
        DTOutput("by_month_tbl")
      ),
      column(
        6,
        br(),
        h4("Average Ozone by Month"),
        plotOutput("ozone_month_plot", height = "380px")
      )
    ),
    fluidRow(
      column(
        12,
        br(),
        h4("Correlation Matrix (filtered data)"),
        plotOutput("corr_plot", height = "360px")
      )
    )
  ),
  
  tabPanel(
    "Scatter",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput("xvar", "X variable", choices = numeric_vars, selected = "Temp"),
        selectInput("yvar", "Y variable", choices = numeric_vars, selected = "Ozone"),
        selectInput("color_by", "Color by", choices = c("None", "Month"), selected = "Month"),
        checkboxInput("add_smooth", "Add LOESS smoother", value = TRUE),
        sliderInput("pt_alpha", "Point alpha", min = 0.2, max = 1, value = 0.8, step = 0.05),
        sliderInput("pt_size", "Point size", min = 1, max = 5, value = 2, step = 0.5)
      ),
      mainPanel(
        plotOutput("scatter_plot", height = "480px")
      )
    )
  ),
  
  tabPanel(
    "Trends",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput("trend_var", "Variable", choices = numeric_vars, selected = "Ozone"),
        radioButtons(
          "trend_gran",
          "Aggregate by",
          choices = c("Day (within selected months)" = "day", "Month (mean per month)" = "month"),
          selected = "day"
        ),
        checkboxInput("trend_smooth", "Add smoother", TRUE)
      ),
      mainPanel(
        plotOutput("trend_plot", height = "460px")
      )
    )
  ),
  
  tabPanel(
    "Distributions",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput("dist_var", "Variable", choices = numeric_vars, selected = "Ozone"),
        sliderInput("bins", "Histogram bins", min = 5, max = 50, value = 20),
        checkboxInput("show_density", "Overlay density curve", TRUE),
        checkboxInput("show_box_by_month", "Show boxplots by month (instead of histogram)", FALSE)
      ),
      mainPanel(
        plotOutput("dist_plot", height = "460px")
      )
    )
  ),
  
  tabPanel(
    "Modeling",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        selectInput("resp", "Response", choices = numeric_vars, selected = "Ozone"),
        selectizeInput("preds", "Predictors", choices = setdiff(numeric_vars, "Ozone"),
                       selected = c("Temp", "Wind"), multiple = TRUE),
        helpText("Fits a simple linear model on the filtered data (with rows containing NAs dropped for selected variables).")
      ),
      mainPanel(
        verbatimTextOutput("model_summary"),
        plotOutput("model_plot", height = "420px")
      )
    )
  ),
  
  tabPanel(
    "Missingness",
    fluidRow(
      column(
        5,
        br(),
        h4("Missingness by variable (after month/day filters, before NA drop)"),
        tableOutput("miss_table")
      ),
      column(
        7,
        br(),
        h4("Missingness map"),
        plotOutput("miss_map", height = "480px")
      )
    ),
    br(),
    tags$small(
      "Note: This tab ignores the 'Drop NAs' toggle so you can inspect what's being removed."
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Data filtered by month/day only (pre-NA-drop) for missingness inspection
  filtered_pre_na <- reactive({
    req(input$months, input$day_range)
    aq |>
      filter(
        Month %in% input$months,
        Day >= input$day_range[1],
        Day <= input$day_range[2]
      )
  })
  
  # Data filtered by UI, with optional NA drop
  filtered <- reactive({
    df <- filtered_pre_na()
    if (isTRUE(input$drop_na)) {
      df <- df |>
        drop_na(all_of(numeric_vars))
    }
    df
  })
  
  # ---- Overview tab ----
  output$meta_cards <- renderUI({
    total_rows <- nrow(aq)
    total_cols <- ncol(aq)
    total_na <- sum(is.na(aq[numeric_vars]))
    tagList(
      div(
        strong("Rows: "), total_rows, br(),
        strong("Columns: "), total_cols, br(),
        strong("Numeric vars: "), paste(numeric_vars, collapse = ", "), br(),
        strong("Total NA (numeric vars): "), total_na
      )
    )
  })
  
  output$filter_echo <- renderTable({
    data.frame(
      Months = paste(names(month_choices[as.character(input$months)]), collapse = ", "),
      Day_Range = paste(input$day_range, collapse = " – "),
      Drop_NA = ifelse(isTRUE(input$drop_na), "Yes", "No"),
      check.names = FALSE
    )
  })
  
  output$overview_plot <- renderPlot({
    aq |>
      group_by(MonthName) |>
      summarize(
        mean_Ozone = mean(Ozone, na.rm = TRUE),
        mean_Temp  = mean(Temp,  na.rm = TRUE),
        .groups = "drop"
      ) |>
      ggplot(aes(x = MonthName, y = mean_Ozone)) +
      geom_col() +
      geom_line(aes(y = mean_Temp * (max(mean_Ozone, na.rm = TRUE) / max(mean_Temp, na.rm = TRUE)), group = 1)) +
      geom_point(aes(y = mean_Temp * (max(mean_Ozone, na.rm = TRUE) / max(mean_Temp, na.rm = TRUE)))) +
      labs(
        x = NULL,
        y = "Avg Ozone (ppb) — bars; Avg Temp scaled — line",
        title = "Average Ozone and Temperature by Month"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$na_summary <- renderTable({
    tibble(
      Variable = numeric_vars,
      `NA Count` = sapply(aq[numeric_vars], function(x) sum(is.na(x))),
      `NA %` = round(100 * sapply(aq[numeric_vars], function(x) mean(is.na(x))), 1)
    )
  }, striped = TRUE, bordered = TRUE, align = "lrr", digits = 1)
  
  # ---- Filter & Data tab ----
  output$data_table <- renderDT({
    datatable(
      filtered(),
      extensions = "Buttons",
      options = list(
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "print"),
        pageLength = 10
      )
    )
  })
  
  output$dl_data <- downloadHandler(
    filename = function() {
      paste0("airquality_filtered_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
  
  # ---- Summaries tab ----
  output$by_month_tbl <- renderDT({
    filtered() |>
      group_by(MonthName) |>
      summarize(
        n = n(),
        mean_Ozone = round(mean(Ozone, na.rm = TRUE), 1),
        mean_SolarR = round(mean(Solar.R, na.rm = TRUE), 1),
        mean_Wind = round(mean(Wind, na.rm = TRUE), 2),
        mean_Temp = round(mean(Temp, na.rm = TRUE), 1),
        .groups = "drop"
      ) |>
      datatable(options = list(dom = "t", pageLength = 5))
  })
  
  output$ozone_month_plot <- renderPlot({
    filtered() |>
      group_by(MonthName) |>
      summarize(mean_Ozone = mean(Ozone, na.rm = TRUE), .groups = "drop") |>
      ggplot(aes(MonthName, mean_Ozone)) +
      geom_col() +
      labs(x = NULL, y = "Average Ozone (ppb)") +
      theme_minimal(base_size = 12)
  })
  
  output$corr_plot <- renderPlot({
    df <- filtered()
    if (nrow(df) < 2) return(NULL)
    mat <- df |> select(all_of(numeric_vars)) |> cor(use = "pairwise.complete.obs")
    # simple correlation heatmap
    mat |>
      as.data.frame() |>
      mutate(var1 = rownames(mat)) |>
      tidyr::pivot_longer(-var1, names_to = "var2", values_to = "corr") |>
      ggplot(aes(var2, var1, fill = corr)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%.2f", corr))) +
      scale_fill_gradient2(limits = c(-1, 1)) +
      labs(x = NULL, y = NULL, fill = "r") +
      theme_minimal(base_size = 12)
  })
  
  # ---- Scatter tab ----
  output$scatter_plot <- renderPlot({
    df <- filtered()
    if (!nrow(df)) return(NULL)
    p <- ggplot(df, aes_string(x = input$xvar, y = input$yvar))
    
    if (identical(input$color_by, "Month")) {
      p <- p + aes(color = MonthName)
    }
    
    p <- p +
      geom_point(alpha = input$pt_alpha, size = input$pt_size) +
      labs(x = input$xvar, y = input$yvar, color = NULL) +
      theme_minimal(base_size = 12)
    
    if (isTRUE(input$add_smooth)) {
      p <- p + geom_smooth(se = FALSE, method = "loess")
    }
    
    p
  })
  
  # ---- Trends tab ----
  output$trend_plot <- renderPlot({
    df <- filtered()
    if (!nrow(df)) return(NULL)
    
    if (identical(input$trend_gran, "day")) {
      agg <- df |>
        group_by(MonthName, Day) |>
        summarize(val = mean(.data[[input$trend_var]], na.rm = TRUE), .groups = "drop")
      ggplot(agg, aes(Day, val, color = MonthName, group = MonthName)) +
        geom_line() +
        { if (isTRUE(input$trend_smooth)) geom_smooth(se = FALSE, method = "loess") } +
        labs(x = "Day of Month", y = paste("Avg", input$trend_var), color = NULL) +
        theme_minimal(base_size = 12)
    } else {
      agg <- df |>
        group_by(MonthName) |>
        summarize(val = mean(.data[[input$trend_var]], na.rm = TRUE), .groups = "drop")
      ggplot(agg, aes(MonthName, val, group = 1)) +
        geom_line() + geom_point() +
        { if (isTRUE(input$trend_smooth)) geom_smooth(se = FALSE, method = "loess") } +
        labs(x = NULL, y = paste("Avg", input$trend_var)) +
        theme_minimal(base_size = 12)
    }
  })
  
  # ---- Distributions tab ----
  output$dist_plot <- renderPlot({
    df <- filtered()
    if (!nrow(df)) return(NULL)
    
    if (isTRUE(input$show_box_by_month)) {
      ggplot(df, aes(x = MonthName, y = .data[[input$dist_var]])) +
        geom_boxplot() +
        labs(x = NULL, y = input$dist_var, title = paste("Distribution of", input$dist_var, "by Month")) +
        theme_minimal(base_size = 12)
    } else {
      ggplot(df, aes(x = .data[[input$dist_var]])) +
        geom_histogram(bins = input$bins) +
        { if (isTRUE(input$show_density)) geom_density(linewidth = 1) } +
        labs(x = input$dist_var, y = "Count", title = paste("Histogram of", input$dist_var)) +
        theme_minimal(base_size = 12)
    }
  })
  
  # ---- Modeling tab ----
  output$model_summary <- renderPrint({
    df <- filtered()
    vars_needed <- unique(c(input$resp, input$preds))
    df <- df |> select(all_of(vars_needed)) |> drop_na()
    if (!nrow(df) || !length(input$preds)) {
      cat("Not enough data or no predictors selected.")
      return()
    }
    fml <- as.formula(paste(input$resp, "~", paste(input$preds, collapse = " + ")))
    fit <- lm(fml, data = df)
    summary(fit)
  })
  
  output$model_plot <- renderPlot({
    df <- filtered()
    resp <- input$resp
    preds <- input$preds
    if (length(preds) != 1) {
      plot.new(); title("Model plot shown when exactly 1 predictor is selected."); return()
    }
    df <- df |> select(all_of(c(resp, preds))) |> drop_na()
    if (!nrow(df)) return(NULL)
    fml <- as.formula(paste(resp, "~", preds))
    fit <- lm(fml, data = df)
    ggplot(df, aes(x = .data[[preds]], y = .data[[resp]])) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = preds, y = resp, title = paste("Linear fit:", resp, "~", preds)) +
      theme_minimal(base_size = 12)
  })
  
  # ---- Missingness tab ----
  output$miss_table <- renderTable({
    df <- filtered_pre_na()
    tibble(
      Variable = numeric_vars,
      Missing = sapply(df[numeric_vars], function(x) sum(is.na(x))),
      `% Missing` = round(100 * sapply(df[numeric_vars], function(x) mean(is.na(x))), 1)
    )
  }, digits = 1)
  
  output$miss_map <- renderPlot({
    df <- filtered_pre_na()
    if (!nrow(df)) return(NULL)
    df_map <- df |>
      mutate(Row = row_number()) |>
      select(Row, all_of(numeric_vars)) |>
      pivot_longer(-Row, names_to = "Variable", values_to = "Value") |>
      mutate(Missing = is.na(Value))
    
    ggplot(df_map, aes(x = Variable, y = Row, fill = Missing)) +
      geom_tile() +
      scale_y_reverse() +
      labs(x = NULL, y = "Row (filtered)", fill = "Is NA",
           title = "Missingness map by variable and row") +
      theme_minimal(base_size = 12)
  })
}
# this is a test again again
# ---- Run ----
shinyApp(ui, server)
