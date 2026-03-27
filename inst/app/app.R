library(shiny)
library(shinyjs)
library(arrow)

source("utils.R")

app_ui <- function() {
  useShinyjs()

  make_multi_input <- function(prefix, name, count, choices,
                               n1 = name, n2 = NULL) {
    if (count == 1) {
      return(list(selectInput(sprintf("%s_%s", prefix, name),
                              paste0(name, ":"),
                              choices = choices, selected = NULL)))
    } else {
      return(list(selectInput(sprintf("%s_%s1", prefix, name), n1,
                              choices = choices, selected = NULL),
                  selectInput(sprintf("%s_%s2", prefix, name), n2,
                              choices = choices, selected = NULL)))
    }
  }

  make_panel <- function(title, prefix, n_touchstones, n_scenarios, n_groups) {
    c0 <- character(0)
    ts <- get_touchstones()

    touchstones <- make_multi_input(prefix, "touchstone", n_touchstones, ts,
                                    "Touchstone 1:", "Touchstone 2:")
    scenarios <- make_multi_input(prefix, "scenario", n_scenarios, c0,
                                    "Base Scenario:", "Compare Scenario:")
    groups <- make_multi_input(prefix, "group", n_groups, c0,
                               "Group 1:", "Group 2:")

    if ((n_touchstones > 1) || (n_groups > 1)) {
      graphs <- list(
        plotOutput(sprintf("%s_main_plot1", prefix)),
        plotOutput(sprintf("%s_main_plot2", prefix)))
    } else {
      graphs <- list(
        plotOutput(sprintf("%s_main_plot", prefix)))
    }

    sidebar <- append(touchstones, list(
      selectInput(sprintf("%s_disease", prefix), "Disease:", choices = c0)))
    sidebar <- append(sidebar, groups)
    sidebar <- append(sidebar, scenarios)
    sidebar <- append(sidebar, list(
      selectInput(sprintf("%s_country", prefix), "Country:", choices = c0),
      selectInput(sprintf("%s_outcome", prefix), "Y-Axis:", choices = c0),
      radioButtons(sprintf("%s_year", prefix), "X-Axis:", inline = TRUE,
                   choices = c("Calendar", "Cohort")),
      radioButtons(sprintf("%s_ages", prefix), "Age:", inline = TRUE,
                   choices = c("All", "Under 5")),
      checkboxGroupInput(
        sprintf("%s_options", prefix), "Plot Options:",
        choices = c("Quantiles", "Median", "Mean", "Log-Y"),
        selected = c("Quantiles", "Median", "Mean", "Log-Y"),
        inline = TRUE),
      actionButton(sprintf("%s_plot_btn", prefix), "Plot")
    ))

    tabPanel(title, sidebarLayout(sidebarPanel(sidebar), mainPanel(graphs)))
  }

  fluidPage(
    tags$style(HTML("
      .shiny-busy {
        cursor: wait !important;
      }
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
      make_panel("Burden", "b", 1, 1, 1),
      make_panel("Burden/TS", "bts", 2, 1, 1),
      make_panel("Burden/MG", "bmg", 1, 1, 2),
      make_panel("Impact", "i", 1, 2, 1),
      make_panel("Impact/TS", "its", 2, 2, 1),
      make_panel("Impact/MG", "img", 1, 2, 2)
    )
  )
}

app_server <- function(input, output, session) {

  add_observers <- function(prefix, n_touchstone = 1, n_group = 1,
                            n_scenario = 1) {
    if (n_touchstone == 1) {
      it1 <- sprintf("%s_touchstone", prefix)
      it2 <- NULL
      observeEvent(input[[it1]], {
                   update_touchstone(session,
                                     input[[it1]],
                                     NULL,
                                     input,
                                     prefix)})

    } else {
      it1 <- sprintf("%s_touchstone1", prefix)
      it2 <- sprintf("%s_touchstone2", prefix)

      observeEvent(input[[it1]], {
                   update_touchstone(session,
                                      input[[it1]],
                                      input[[it2]],
                                      input,
                                      prefix)})

      observeEvent(input[[it2]], {
                   update_touchstone(session,
                                     input[[it1]],
                                     input[[it2]],
                                     input,
                                     prefix)})
    }

    id <- sprintf("%s_disease", prefix)
    observeEvent(input[[id]], {
                 update_disease(session,
                                input[[it1]],
                                if (is.null(it2)) NULL else input[[it2]],
                                input[[id]],
                                input,
                                prefix)})

    if (n_group == 1) {
      ig1 <- sprintf("%s_group", prefix)
      ig2 <- NULL
      observeEvent(input[[ig1]], {
                   update_group(session,
                                input[[it1]],
                                if (is.null(it2)) NULL else input[[it2]],
                                input[[id]],
                                input[[ig1]],
                                NULL,
                                input,
                                prefix)})

    } else {
      ig1 <- sprintf("%s_group1", prefix)
      ig2 <- sprintf("%s_group2", prefix)
      observeEvent(input[[ig1]], {
                   update_group(session,
                                input[[it1]],
                                if (is.null(it2)) NULL else input[[it2]],
                                input[[id]],
                                input[[ig1]],
                                input[[ig2]],
                                input,
                                prefix)})

      observeEvent(input[[ig2]], {
                   update_group(session,
                                input[[it1]],
                                if (is.null(it2)) NULL else input[[it2]],
                                input[[id]],
                                input[[ig1]],
                                input[[ig2]],
                                input,
                                prefix)})
    }

    if (n_scenario == 1) {
      is1 <- sprintf("%s_scenario", prefix)
      is2 <- NULL
      observeEvent(input[[is1]], {
                   update_scenario(session,
                                   input[[it1]],
                                   if (is.null(it2)) NULL else input[[it2]],
                                   input[[id]],
                                   input[[ig1]],
                                   if (is.null(ig2)) NULL else input[[ig2]],
                                   input[[is1]],
                                   NULL,
                                   input, prefix)})
    } else {
      is1 <- sprintf("%s_scenario1", prefix)
      is2 <- sprintf("%s_scenario2", prefix)
      observeEvent(input[[is1]], {
                   update_scenario(session,
                                   input[[it1]],
                                   if (is.null(it2)) NULL else input[[it2]],
                                   input[[id]],
                                   input[[ig1]],
                                   if (is.null(ig2)) NULL else input[[ig2]],
                                   input[[is1]],
                                   input[[is2]],
                                   input,
                                   prefix)})

      observeEvent(input[[is2]], {
                   update_scenario(session,
                                   input[[it1]],
                                   if (is.null(it2)) NULL else input[[it2]],
                                   input[[id]],
                                   input[[ig1]],
                                   if (is.null(ig2)) NULL else input[[ig2]],
                                   input[[is1]],
                                   input[[is2]],
                                   input,
                                   prefix)})
    }

    ic <- sprintf("%s_country", prefix)
    observeEvent(input[[ic]], {
                 update_country(session,
                                input[[it1]],
                                if (is.null(it2)) NULL else input[[it2]],
                                input[[id]],
                                input[[ig1]],
                                if (is.null(ig2)) NULL else input[[ig2]],
                                input[[is1]],
                                if (is.null(is2)) NULL else input[[is2]],
                                input[[ic]],
                                input,
                                prefix)})

    n_graphs <- 1 + ((n_touchstone * n_group) > 1)
    if (n_graphs == 1) {
      graphs <- sprintf("%s_main_plot", prefix)
    } else {
      graphs <- sprintf("%s_main_plot%d", prefix, 1:2)
    }
    button <- sprintf("%s_plot_btn", prefix)

    plot_reactive <- eventReactive(input[[button]], {
      ages <- NULL
      if (input[[sprintf("%s_ages", prefix)]] == "Under 5") {
        ages <- 0:4
      }

      check <- function(pre, x1, x2, type) {
        if (is.null(x2)) return(TRUE)
        if ((grepl(pre, prefix)) && (input[[x1]] == input[[x2]])) {
          showModal(modalDialog(
            title = "Warning",
            sprintf("Same %s selected. Dull comparison.", type),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(FALSE)
        }
        TRUE
      }

      if (!check("mg", ig1, ig2, "group")) return(NULL)
      if (!check("ts", it1, it2, "touchstone")) return(NULL)
      if (!check("i", is1, is2, "scenario")) return(NULL)

      list(
        ages = ages,
        opts = input[[sprintf("%s_options", prefix)]],

        touchstone = c(input[[it1]], if (!is.null(it2)) input[[it2]] else NULL),
        group      = c(input[[ig1]], if (!is.null(ig2)) input[[ig2]] else NULL),
        scenario   = c(input[[is1]], if (!is.null(is2)) input[[is2]] else NULL),

        disease = input[[id]],
        country = input[[ic]],
        outcome = input[[sprintf("%s_outcome", prefix)]],
        year    = input[[sprintf("%s_year", prefix)]]
      )
    })

    for (g in seq_along(graphs)) {
      local({
        gg <- g

        output[[graphs[gg]]] <- renderPlot({
          pr <- plot_reactive()
          req(pr)

          it <- pr$touchstone[min(gg, length(pr$touchstone))]
          ig <- pr$group[min(gg, length(pr$group))]

          stoner::stone_stochastic_graph(
            base = data_dir,
            touchstone = it,
            disease = pr$disease,
            group = ig,
            country = pr$country,
            scenario = pr$scenario[1],
            scenario2 = if (length(pr$scenario) > 1) pr$scenario[2] else NULL,
            outcome = pr$outcome,
            by_cohort = pr$year == "Cohort",
            ages = pr$ages,
            log = "Log-Y" %in% pr$opts,
            include_median = "Median" %in% pr$opts,
            include_quantiles = "Quantiles" %in% pr$opts,
            include_mean = "Mean" %in% pr$opts
          )
        })
      })
    }
  }
  add_observers("b")
  add_observers("bts", 2, 1, 1)
  add_observers("bmg", 1, 2, 1)
  add_observers("i", 1, 1, 2)
  add_observers("its", 2, 1, 2)
  add_observers("img", 1, 2, 2)
}

shinyApp(app_ui, app_server)
