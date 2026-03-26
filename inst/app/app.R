library(shiny)
library(shinyjs)
library(arrow)

data_dir <- "//wpia-hn2.hpc.dide.ic.ac.uk/vimc_stochastics"

source("utils.R")

app_ui <- function() {
  useShinyjs()
  fluidPage(
    tags$style(HTML("
      .selectize-input {
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .shiny-options-group {
        display: grid;
        grid-template-columns: repeat(2, 1fr);  /* 2 per row */
        gap: 4px 12px;  /* row gap, column gap */
      }

      .shiny-options-group .checkbox label {
        display: flex;
        align-items: centre;
        margin: 0;
        padding-left: 0;
        gap: 6px;
      }

      .control-label {
        margin-bottom: 2px;
        font-size: 12px;
      }

      .selectize-input {
        min-height: 30px;
        padding: 4px 8px;
        font-size: 13px;
      }

      .form-group {
        margin-bottom: 8px;
      }

      .shiny-plot-output {
        padding-top: 5px;
      }
    ")),
    titlePanel("Stochastic Explorer"),
    tabsetPanel(
      tabPanel("Burden",
        sidebarLayout(
          sidebarPanel(
            selectInput("b_touchstone", "Touchstone:",
                        choices = get_touchstones(), selected = NULL),
            selectInput("b_disease", "Disease:",
                        choices = character(0)),
            selectInput("b_group", "Group:",
                        choices = character(0)),
            selectInput("b_scenario", "Scenario:",
                        choices = character(0)),
            selectInput("b_country", "Country:",
                        choices = character(0)),
            selectInput("b_outcome", "Y-Axis:",
                        choices = character(0)),
            radioButtons("b_year", "X-Axis:", inline = TRUE,
                         choices = c("Calendar", "Cohort")),
            radioButtons("b_ages", "Age:", inline = TRUE,
                         choices = c("All", "Under 5")),
            checkboxGroupInput(
              "b_options", "Plot Options:",
              choices = c("Quantiles", "Median", "Mean", "Log-Y"),
              selected = c("Quantiles", "Median", "Mean", "Log-Y"),
              inline = TRUE),
            actionButton("b_plot_btn", "Plot")
          ), mainPanel(
            plotOutput("b_main_plot")
          )
        )
      ),
      tabPanel("Burden/TS",
        sidebarLayout(
          sidebarPanel(
            selectInput("bts_touchstone1", "Touchstone 1:",
              choices = get_touchstones(), selected = NULL),
            selectInput("bts_touchstone2", "Touchstone 2:",
              choices = get_touchstones(), selected = NULL),
            selectInput("bts_disease", "Disease:",
              choices = character(0)),
            selectInput("bts_group", "Group:",
              choices = character(0)),
            selectInput("bts_scenario", "Scenario:",
              choices = character(0)),
            selectInput("bts_country", "Country:",
              choices = character(0)),
            selectInput("bts_outcome", "Y-Axis:",
              choices = character(0)),
            radioButtons("bts_year", "X-Axis:", inline = TRUE,
              choices = c("Calendar", "Cohort")),
            radioButtons("bts_ages", "Age:", inline = TRUE,
              choices = c("All", "Under 5")),
            checkboxGroupInput("bts_options", "Plot Options:",
              choices = c("Quantiles", "Median", "Mean", "Log-Y"),
                       selected = c("Quantiles", "Median", "Mean", "Log-Y"),
                       inline = TRUE),
            actionButton("bts_plot_btn", "Plot")
          ), mainPanel(
            plotOutput("bts_main_plot1"),
            plotOutput("bts_main_plot2")
          )
        )
      ),
      tabPanel("Burden/MG",
        sidebarLayout(
          sidebarPanel(
            selectInput("bmg_touchstone", "Touchstone:",
                        choices = get_touchstones(), selected = NULL),
            selectInput("bmg_disease", "Disease:",
                        choices = character(0)),
            selectInput("bmg_group1", "Group 1:",
                        choices = character(0)),
            selectInput("bmg_group2", "Group 2:",
                        choices = character(0)),
            selectInput("bmg_scenario", "Scenario:",
                        choices = character(0)),
            selectInput("bmg_country", "Country:",
                        choices = character(0)),
            selectInput("bmg_outcome", "Y-Axis:",
                        choices = character(0)),
            radioButtons("bmg_year", "X-Axis:", inline = TRUE,
                         choices = c("Calendar", "Cohort")),
            radioButtons("bmg_ages", "Age:", inline = TRUE,
                         choices = c("All", "Under 5")),
            checkboxGroupInput("bmg_options", "Plot Options:",
                         choices = c("Quantiles", "Median", "Mean", "Log-Y"),
                         selected = c("Quantiles", "Median", "Mean", "Log-Y"),
                         inline = TRUE),
            actionButton("bmg_plot_btn", "Plot")
          ), mainPanel(
            plotOutput("bmg_main_plot1"),
            plotOutput("bmg_main_plot2")
          )
        )
      ),
      tabPanel("Impact",
        sidebarLayout(
          sidebarPanel(
            selectInput("i_touchstone", "Touchstone:",
                       choices = get_touchstones(), selected = NULL),
            selectInput("i_disease", "Disease:",
                       choices = character(0)),
            selectInput("i_group", "Group:",
                       choices = character(0)),
            selectInput("i_scenario1", "Base Scenario:",
                       choices = character(0)),
            selectInput("i_scenario2", "Compare Scenario:",
                       choices = character(0)),
            selectInput("i_country", "Country:",
                       choices = character(0)),
            selectInput("i_outcome", "Y-Axis:",
                       choices = character(0)),
            radioButtons("i_year", "X-Axis:", inline = TRUE,
                        choices = c("Calendar", "Cohort")),
            radioButtons("i_ages", "Age:", inline = TRUE,
                        choices = c("All", "Under 5")),
            checkboxGroupInput(
                 "i_options", "Plot Options:",
                 choices = c("Quantiles", "Median", "Mean", "Log-Y"),
                 selected = c("Quantiles", "Median", "Mean", "Log-Y"),
                 inline = TRUE),
            actionButton("i_plot_btn", "Plot")
          ), mainPanel(
           plotOutput("i_main_plot")
          )
        )
      )
    )
  )
}

app_server <- function(input, output, session) {

  # Events for the basic burden tab

  observeEvent(input$b_touchstone, {
    update_touchstone(session, input$b_touchstone, input) })
  observeEvent(input$b_disease, {
    update_disease(session, input$b_touchstone, input$b_disease, input) })
  observeEvent(input$b_group, {
    update_group(session, input$b_touchstone, input$b_disease, input$b_group, input) })
  observeEvent(input$b_scenario, {
    update_scenario(session, input$b_touchstone, input$b_disease, input$b_group,
                    input$b_scenario, input) })
  observeEvent(input$b_country, {
    update_country(session, input$b_touchstone, input$b_disease, input$b_group,
                   input$b_scenario, input$b_country, input) })


  plot_obj_b <- eventReactive(input$b_plot_btn, {
    ages <- NULL
    if (input$b_ages == "Under 5") {
      ages <- 0:4
    }
    stone_stochastic_graph(
      base = data_dir,
      touchstone = input$b_touchstone,
      disease = input$b_disease,
      group = input$b_group,
      country = input$b_country,
      scenario = input$b_scenario,
      outcome = input$b_outcome,
      by_cohort = input$b_year == "Cohort",
      ages = ages,
      log = "Log-Y" %in% input$b_options,
      include_median = "Median" %in% input$b_options,
      include_quantiles = "Quantiles" %in% input$b_options,
      include_mean = "Mean" %in% input$b_options,
    )
  })

  output$b_main_plot <- renderPlot({
    plot_obj_b()
  })

  # Events for the impact tab

  observeEvent(input$i_touchstone, {
    update_touchstone(session, input$i_touchstone, input, "i") })
  observeEvent(input$i_disease, {
    update_disease(session, input$i_touchstone, input$i_disease, input, "i") })
  observeEvent(input$i_group, {
    update_group(session, input$i_touchstone, input$i_disease,
                 input$i_group, input, "i", "1")
    update_group(session, input$i_touchstone, input$i_disease,
                 input$i_group, input, "i", "2")
  })
  observeEvent(input$i_scenario1, {
    update_scenario(session, input$i_touchstone, input$i_disease, input$i_group,
                    input$i_scenario1, input, "i",
                    extra_scenario = input$i_scenario2) })
  observeEvent(input$i_scenario2, {
    update_scenario(session, input$i_touchstone, input$i_disease, input$i_group,
                    input$i_scenario2, input, "i",
                    extra_scenario = input$i_scenario2) })
  observeEvent(input$i_country, {
    update_country(session, input$i_touchstone, input$i_disease, input$i_group,
                   input$i_scenario1, input$i_country, input, "i") })

  plot_obj_i <- eventReactive(input$i_plot_btn, {

    if (input$i_scenario1 == input$i_scenario2) {
      showModal(modalDialog(
        title = "Warning",
        "Same scenario selected - impact will be zero.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }

    ages <- NULL
    if (input$i_ages == "Under 5") {
      ages <- 0:4
    }
    stone_stochastic_graph(
      base = data_dir,
      touchstone = input$i_touchstone,
      disease = input$i_disease,
      group = input$i_group,
      country = input$i_country,
      scenario = input$i_scenario1,
      scenario2 = input$i_scenario2,
      outcome = input$i_outcome,
      by_cohort = input$i_year == "Cohort",
      ages = ages,
      log = "Log-Y" %in% input$i_options,
      include_median = "Median" %in% input$i_options,
      include_quantiles = "Quantiles" %in% input$i_options,
      include_mean = "Mean" %in% input$i_options,
    )
  })

  output$i_main_plot <- renderPlot({
    plot_obj_i()
  })

  # Events for comparing burdens on different touchstones

  observeEvent(input$bts_touchstone1, {
    update_touchstone_ts(session, input$bts_touchstone1, input$bts_touchstone2,
                         input, "bts") })
  observeEvent(input$bts_touchstone2, {
    update_touchstone_ts(session, input$bts_touchstone1, input$bts_touchstone2,
                         input, "bts") })
  observeEvent(input$bts_disease, {
    update_disease_ts(session, input$bts_touchstone1, input$bts_touchstone2,
                      input$bts_disease, input, "bts") })
  observeEvent(input$bts_group, {
    update_group_ts(session, input$bts_touchstone1, input$bts_touchstone2,
                    input$bts_disease, input$bts_group, input, "bts")
  })
  observeEvent(input$bts_scenario, {
    update_scenario_ts(session, input$bts_touchstone1, input$bts_touchstone2,
                       input$bts_disease, input$bts_group, input$bts_scenario,
                       input, "bts") })
  observeEvent(input$bts_country, {
    update_country_ts(session, input$bts_touchstone1, input$bts_touchstone2,
                      input$bts_disease, input$bts_group, input$bts_scenario,
                      input$bts_country, input, "bts") })

  plot_obj_bts <- eventReactive(input$bts_plot_btn, {
    if (input$bts_touchstone1 == input$bts_touchstone2) {
      showModal(modalDialog(
        title = "Warning",
        "Same touchstones selected.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    ages <- NULL
    if (input$i_ages == "Under 5") {
      ages <- 0:4
    }
    plots <- list()
    for (ts in 1:2) {
      plots[[ts]] <-
        stone_stochastic_graph(
          base = data_dir,
          touchstone = if (ts == 1) input$bts_touchstone1 else input$bts_touchstone2,
          disease = input$bts_disease,
          group = input$bts_group,
          country = input$bts_country,
          scenario = input$bts_scenario,
          outcome = input$bts_outcome,
          by_cohort = input$bts_year == "Cohort",
          ages = ages,
          log = "Log-Y" %in% input$bts_options,
          include_median = "Median" %in% input$bts_options,
          include_quantiles = "Quantiles" %in% input$bts_options,
          include_mean = "Mean" %in% input$bts_options,
        )
    }
    list(plot1 = plots[[1]], plot2 = plots[[2]])
  })

  output$bts_main_plot1 <- renderPlot({
    replayPlot(plot_obj_bts()$plot1)
  })

  output$bts_main_plot2 <- renderPlot({
    replayPlot(plot_obj_bts()$plot2)
  })

  # Compare burden estimates for different groups

  observeEvent(input$bmg_touchstone, {
    update_touchstone(session, input$bmg_touchstone, input, "bmg") })
  observeEvent(input$bmg_disease, {
    update_disease_mg(session, input$bmg_touchstone, input$bmg_disease,
                   input, "bmg") })
  observeEvent(input$bmg_group1, {
    update_group_mg(session, input$bmg_touchstone, input$bmg_disease,
                    input$bmg_group1, input$bmg_group2, input, "bmg")
  })
  observeEvent(input$bmg_group2, {
    update_group_mg(session, input$bmg_touchstone, input$bmg_disease,
                    input$bmg_group1, input$bmg_group2, input, "bmg")
  })
  observeEvent(input$bmg_scenario, {
    update_scenario_mg(session, input$bmg_touchstone, input$bmg_disease,
                       input$bmg_group1, input$bmg_group2, input$bmg_scenario,
                       input, "bmg") })
  observeEvent(input$bmg_country, {
    update_country_mg(session, input$bmg_touchstone, input$bmg_disease,
                      input$bmg_group1, input$bmg_group2, input$bmg_scenario,
                      input$bmg_country, input, "bmg") })

  plot_obj_bmg <- eventReactive(input$bmg_plot_btn, {
    if (input$bmg_group1 == input$bmg_group2) {
      showModal(modalDialog(
        title = "Warning",
        "Same groups selected.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    ages <- NULL
    if (input$i_ages == "Under 5") {
      ages <- 0:4
    }
    plots <- list()
    for (ts in 1:2) {
      plots[[ts]] <-
        stone_stochastic_graph(
          base = data_dir,
          touchstone = input$bmg_touchstone,
          disease = input$bmg_disease,
          group = if (ts == 1) input$bmg_group1 else input$bmg_group2,
          country = input$bmg_country,
          scenario = input$bmg_scenario,
          outcome = input$bmg_outcome,
          by_cohort = input$bmg_year == "Cohort",
          ages = ages,
          log = "Log-Y" %in% input$bmg_options,
          include_median = "Median" %in% input$bmg_options,
          include_quantiles = "Quantiles" %in% input$bmg_options,
          include_mean = "Mean" %in% input$bmg_options,
        )
    }
    list(plot1 = plots[[1]], plot2 = plots[[2]])
  })

  output$bmg_main_plot1 <- renderPlot({
    replayPlot(plot_obj_bmg()$plot1)
  })

  output$bmg_main_plot2 <- renderPlot({
    replayPlot(plot_obj_bmg()$plot2)
  })
}

shinyApp(app_ui, app_server)
