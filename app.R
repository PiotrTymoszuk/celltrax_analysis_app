# This function provides the interface and server functions for an web app
# for import and pre-processing of microscopy cell tracking data.

# Tools ------

  library(plyr)
  library(tidyverse)
  library(shiny)
  library(shinyWidgets)
  library(shinyjs)
  library(celltrax)
  library(waiter)
  library(writexl)
  library(rlang)
  library(clustTools)
  library(stringi)
  library(shinyBS)

  source('./tools/styles.R')
  source('./tools/main_panels.R')
  source('./tools/tabsets.R')
  source('./tools/utils.R')

  options(shiny.usecairo = FALSE)

# User interface -----

  ui <- fluidPage(

    useShinyjs(),

    ## progress bar

    autoWaiter(html = spin_rotating_plane()),

    ## some styling

    styles(),

    ## Title panel with the logos and names

    title_panel(),

    ## Side panel with user's entries.
    ## Contains uploads handlers and analysis launch button

    sidebarLayout(

      side_panel(),

      ## Main panel to hold the dynamic output

      mainPanel(
        tabsetPanel(id = 'tab_status',
                    general_info(),
                    track_stats(),
                    motility(),
                    autocov_plots(),
                    pair_analysis(),
                    dir_analysis(),
                    hotellings(),
                    hetero_analysis(),
                    get_data(),
        )
      )
    )
  )

# Define server logic ----

  server <- function(input, output, session) {

    ## refresh option -----

    observeEvent(input$refresh,{

      session$reload()

    })

    ## Input-specific GUI -----

    spy_results <- eventReactive(input$data_entry, {

      file <- input$data_entry

      spy_file(file$datapath)

    })

    observeEvent(input$data_entry, {

      enable('launcher')

    })

    output$selectors <- renderUI({

      if(is.null(input$data_entry)) return(NULL)

      tagList(selectInput('id_name',
                          label = 'track ID column',
                          choices = spy_results()$col_names,
                          selected = spy_results()$id_col),
              selectInput('t_name',
                          label = 'time column',
                          choices = spy_results()$col_names,
                          selected = spy_results()$t_col),
              selectInput('x_name',
                          label = 'X coordinate column',
                          choices = spy_results()$col_names,
                          selected = spy_results()$x_col),
              selectInput('y_name',
                          label = 'Y coordinate column',
                          choices = spy_results()$col_names,
                          selected = spy_results()$y_col),
              selectInput('z_name',
                          label = 'Z coordinate column',
                          choices = c('', spy_results()$col_names),
                          selected = NULL))

    })

    ## Table with input tracks, catching entry errors ----

    input_data <- eventReactive(input$launcher, {

      file <- input$data_entry

      z_entry <- if(input$z_name == '') NULL else input$z_name

      output <- try(read_trax_text(file = file$datapath,
                                   id_name = input$id_name,
                                   t_name = input$t_name,
                                   x_name = input$x_name,
                                   y_name = input$y_name,
                                   z_name = z_entry),
                    silent = TRUE)

      if(any(class(output) == 'try-error')) {

        return('Input error: incorrect path? wrong file format?')

      }

      if(length(output) > 1500) {

        output <- structure('Input error: data limit reached.
                            The app can process up to 1500 tracks.',
                            class = 'try-error')

      } else {

        total_size <- map_dbl(output, nrow) %>%
          sum

        if(total_size > 40000) {

          output <- structure('Input error: data limit reached.
                              The app can process up to 40000 steps.',
                              class = 'try-error')

        }

      }

      hide('data_entry')
      hide('selectors')

      output

    })

    qc <- observe({

      if(!is_trax(input_data())) {

        showNotification(input_data(),
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 1,
                        total = 8,
                        title = 'Data imported')

    })

    ## Track statistic plots -------

    ### speed and displacement plots ----

    disp_plots <- reactive({

      disp_title <- switch(input$disp_aggregate,
                           total = 'Total displacement vector length',
                           mean = 'Mean displacement per step',
                           median = 'Median displacement per step')

      speed_title <- switch(input$disp_aggregate,
                            total = 'Mean speed per step',
                            mean = 'Mean speed per step',
                            median = 'Median speed per step')

      disp_plots <- list(plot_title = c(disp_title, speed_title),
                         stat = c('displacements', 'speeds')) %>%
        pmap(plot,
             x = input_data(),
             color = input$disp_color,
             type = 'statistic',
             geom = input$disp_geom,
             cust_theme = theme_shiny(),
             aggregate = input$disp_aggregate)

      if(input$disp_geom != 'histogram') {

        disp_plots %>%
          map(~.x + expand_limits(y = 0))

      } else {

        disp_plots

      }

    })

    output$disp_plot <- renderPlot({

      disp_plots()[[1]]

    })

    output$speed_plot <- renderPlot({

      disp_plots()[[2]]

    })

    observe({

      if(!is.null(input$disp_hover)) {

        show('download_disp1')

      } else {

        hide('download_disp1')

      }

    })

    output$download_disp <- downloadHandler(

      filename = function() {

        return(paste0('displacement_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = disp_plots()[[1]] + theme_trax(),
               device = cairo_pdf,
               width = 90,
               height = 90,
               units = 'mm')

      }

    )

    output$download_speed <- downloadHandler(

      filename = function() {

        return(paste0('speed_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = disp_plots()[[2]] + theme_trax(),
               device = cairo_pdf,
               width = 90,
               height = 90,
               units = 'mm')

      }

    )

    ### straightness and asphericity -----

    straight_plots <- reactive({

      plots <- list(stat = c('straightness', 'asphericity'),
                    plot_title = c('Track straightness', 'Track asphericity')) %>%
        pmap(plot,
             x = input_data(),
             color = input$straight_color,
             type = 'statistic',
             geom = input$straight_geom,
             cust_theme = theme_shiny())

      if(input$straight_geom != 'histogram') {

        plots %>%
          map(~.x +
                scale_y_continuous(limits = c(0, 1)))

      } else {

        plots

      }

    })

    output$straight_plot <- renderPlot({

      straight_plots()[[1]]

    })

    output$aspher_plot <- renderPlot({

      straight_plots()[[2]]

    })

    output$download_straight <- downloadHandler(

      filename = function() {

        return(paste0('straightness_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = straight_plots()[[1]] + theme_trax(),
               device = cairo_pdf,
               width = 90,
               height = 90,
               units = 'mm')

      }

    )

    output$download_aspher <- downloadHandler(

      filename = function() {

        return(paste0('asphericity_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = straight_plots()[[2]] + theme_trax(),
               device = cairo_pdf,
               width = 90,
               height = 90,
               units = 'mm')

      }

    )

    qc <- observe({

      if(!all(c(is.ggplot(disp_plots()[[1]]),
                is.ggplot(straight_plots()[[1]])))) {

        showNotification('Track statistic computation error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 2,
                        total = 8,
                        title = 'Track statistic')

    })

    ## delta BIC --------

    bic_plots <- reactive({

      list(y_stat = c('displacements', 'asphericity'),
           plot_title = c('Total displacement vs delta BIC',
                          'Asphericity vs delta BIC'),
           y_args = list(list(aggregate = 'total'),
                         list2())) %>%
        pmap(plot,
             x = input_data(),
             x_stat = 'delta_BIC',
             type = 'stat_pair',
             coverage = input$bic_coverage/100,
             color = input$bic_color,
             cust_theme = theme_shiny()) %>%
        map(~.x +
              geom_vline(xintercept = 0,
                         linetype = 'dashed') +
              scale_x_continuous(trans = input$bic_x_scale) +
              scale_y_continuous(trans = input$bic_y_scale))

    })

    output$bic_displ <- renderPlot({

      bic_plots()[[1]]

    })

    output$bic_aspher <- renderPlot({

      bic_plots()[[2]]

    })

    output$download_bic_disp <- downloadHandler(

      filename = function() {

        return(paste0('deltaBIC_displacement_plot', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = bic_plots()[[1]] + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    output$download_bic_aspher <- downloadHandler(

      filename = function() {

        return(paste0('deltaBIC_asphericity_plot', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = bic_plots()[[2]] + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    ### plots quality control

    qc <- observe({

      if(!all(c(is.ggplot(disp_plots()[[1]]),
                is.ggplot(straight_plots()[[1]]),
                is.ggplot(bic_plots()[[1]])))) {

        showNotification('Motility modeling error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 3,
                        total = 7,
                        title = 'Motility modeling')

    })

    ## Motion persistence: autocovariance --------

    autocov_stats <- reactive({

      list(method = c('dot_product', 'angle')) %>%
        pmap(calculate,
             x = input_data(),
             type = 'autocov',
             aggregate = TRUE,
             na.rm = TRUE) %>%
        reduce(full_join, by = 'i') %>%
        set_names(c('Step i',
                    'Displacement vector dot product',
                    'Displacement vector angle, radians'))


    })

    autocov_plots <- reactive({

      list(method = c('dot_product', 'angle'),
           plot_title = c('Auto-covariance: displacement vectors',
                          'Auto-covariance: angles'),
           plot_subtitle = c('Mean dot product between displacement vectors',
                             'Mean overall angle between displacement vectors')) %>%
        pmap(plot,
             x = input_data(),
             type = 'autocov',
             color = NULL,
             mean_color = input$autocov_color,
             cust_theme = theme_shiny()) %>%
        map2(., c('displacement vector dot product',
                  'displacement vector angle, radians'),
             ~.x +
               labs(y = .y) +
               scale_y_continuous(trans = input$autocov_y_scale)) %>%
        map2(., c(0, 1.57079633),
             ~.x +
               geom_hline(yintercept = .y, linetype = 'dashed'))

    })

    output$autocov_dot <- renderPlot({

      autocov_plots()[[1]]

    })

    output$autocov_angle <- renderPlot({

      autocov_plots()[[2]]

    })

    output$download_auto_dot <- downloadHandler(

      filename = function() {

        return(paste0('autocovariance_dot_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = autocov_plots()[[1]] + theme_trax(),
               device = cairo_pdf,
               width = 120,
               height = 90,
               units = 'mm')

      }

    )

    output$download_auto_angle <- downloadHandler(

      filename = function() {

        return(paste0('autocovariance_angle_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = autocov_plots()[[2]] + theme_trax(),
               device = cairo_pdf,
               width = 120,
               height = 90,
               units = 'mm')

      }

    )

    qc <- observe({

      if(!all(c(is.ggplot(disp_plots()[[1]]),
                is.ggplot(straight_plots()[[1]]),
                is.ggplot(bic_plots()[[1]]),
                is.ggplot(autocov_plots()[[1]])))) {

        showNotification('Autocovariance computation error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 4,
                        total = 7,
                        title = 'Displacement autocovariance')

    })

    ## Pair analysis --------

    pair_plots <- reactive({

      list(method = c('cells', 'steps'),
           plot_title = c('Cell-wise pair analysis',
                          'Step-wise pair analysis'),
           plot_subtitle = c('Distances and angles between cell pairs',
                             'Distances and angles between cell pairs at each step')) %>%
        pmap(plot,
             x = input_data(),
             type = 'pair_analysis',
             coverage = input$pair_coverage/100,
             cust_theme = theme_shiny(),
             color = input$pair_color,
             line_color = input$gam_color,
             trend_method = 'gam') %>%
        map(~.x +
              geom_hline(yintercept = 90,
                         linetype = 'dashed'))

    })

    output$pair_cells <- renderPlot({

      pair_plots()[[1]]

    })

    output$pair_steps <- renderPlot({

      pair_plots()[[2]]

    })

    output$download_cells <- downloadHandler(

      filename = function() {

        return(paste0('pair_cells_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = pair_plots()[[1]] + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

    }

    )

    output$download_steps <- downloadHandler(

      filename = function() {

        return(paste0('pair_steps_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = pair_plots()[[2]] + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    qc <- observe({

      if(!all(c(is.ggplot(disp_plots()[[1]]),
                is.ggplot(straight_plots()[[1]]),
                is.ggplot(bic_plots()[[1]]),
                is.ggplot(autocov_plots()[[1]]),
                is.ggplot(pair_plots()[[1]])))) {

        showNotification('Pair analysis computation error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 5,
                        total = 8,
                        title = 'Cell pair analysis')

    })

    ## Directional motion analysis -------

    ### track plots ------

    #### generating the plots

    track_plots <- reactive({

      track_color <- if(input$monochrome == 'yes') input$track_color else NULL

      list(normalize = c(FALSE, TRUE),
           show_zero = c(FALSE, TRUE),
           plot_title = c('Cell tracks',
                          'Normalized cell tracks')) %>%
        pmap(plot,
            type = 'tracks',
            x = input_data(),
            color = track_color,
            coverage = input$track_coverage/100,
            cust_theme = theme_shiny())



    })

    #### setting the initial values of the axes ranges

    init_track_ranges <- reactiveValues()

    observe({

      init_track_ranges$raw_range <- track_plots()[[1]]$data[c('x', 'y')] %>%
        map(range)

      init_track_ranges$norm_range <- track_plots()[[2]]$data[c('x', 'y')] %>%
        map(range)

    })

    isolate(init_track_ranges$raw_range)
    isolate(init_track_ranges$norm_range)

    #### dynamic update of the scale ranges

    observe({

      raw_range <- track_plots()[[1]]$data[c('x', 'y')] %>%
        map(range)

      updateNumericInputIcon(inputId = 'x_min_tracks_raw',
                             value = raw_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_tracks_raw',
                             value = raw_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_tracks_raw',
                             value = raw_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_tracks_raw',
                             value = raw_range[[2]][2])

    })

    observe({

      norm_range <- track_plots()[[2]]$data[c('x', 'y')] %>%
        map(range)

      updateNumericInputIcon(inputId = 'x_min_tracks_norm',
                             value = norm_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_tracks_norm',
                             value = norm_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_tracks_norm',
                             value = norm_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_tracks_norm',
                             value = norm_range[[2]][2])


    })

    observeEvent(input$tracks_raw_refresh, {

      updateNumericInputIcon(inputId = 'x_min_tracks_raw',
                             value = init_track_ranges$raw_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_tracks_raw',
                             value = init_track_ranges$raw_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_tracks_raw',
                             value = init_track_ranges$raw_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_tracks_raw',
                             value = init_track_ranges$raw_range[[2]][2])

    })

    observeEvent(input$tracks_norm_refresh, {

      updateNumericInputIcon(inputId = 'x_min_tracks_norm',
                             value = init_track_ranges$norm_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_tracks_norm',
                             value = init_track_ranges$norm_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_tracks_norm',
                             value = init_track_ranges$norm_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_tracks_norm',
                             value = init_track_ranges$norm_range[[2]][2])

    })

    track_plot_raw <- reactive({

      track_plots()[[1]] +
        scale_x_continuous(limits = c(input$x_min_tracks_raw,
                                      input$x_max_tracks_raw)) +
        scale_y_continuous(limits = c(input$y_min_tracks_raw,
                                      input$y_max_tracks_raw))

    })

    track_plot_norm <- reactive({

      track_plots()[[2]] +
        scale_x_continuous(limits = c(input$x_min_tracks_norm,
                                      input$x_max_tracks_norm)) +
        scale_y_continuous(limits = c(input$y_min_tracks_norm,
                                      input$y_max_tracks_norm))

    })

    ## plotting

    output$tracks_raw <- renderPlot({

      track_plot_raw()

    })

    output$tracks_norm <- renderPlot({

      track_plot_norm()

    })

    #### download

    output$download_tracks_raw <- downloadHandler(

      filename = function() {

        return(paste0('track_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = track_plot_raw() + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )


    output$download_tracks_norm <- downloadHandler(

      filename = function() {

        return(paste0('track_plot_normalized_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = track_plot_norm() + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    ### vector plots ------

    vector_plots <- reactive({

      track_color <- if(input$monochrome == 'yes') input$track_color else NULL

      list(normalize = c(FALSE, TRUE),
           show_zero = c(FALSE, TRUE),
           plot_title = c('Total displacement vectors',
                          'Normalized total displacement vectors')) %>%
        pmap(plot,
             type = 'vectors',
             x = input_data(),
             color = track_color,
             coverage = input$track_coverage/100,
             mean_color = input$velo_color,
             cust_theme = theme_shiny())

    })

    #### initial scale ranges

    init_vector_ranges <- reactiveValues()

    observe({

      init_vector_ranges$raw_range <- vector_plots()[[1]]$data[c('x', 'y')] %>%
        map(range)

      init_vector_ranges$norm_range <- vector_plots()[[2]]$data[c('x', 'y')] %>%
        map(range)

    })

    isolate(init_vector_ranges$raw_range)
    isolate(init_vector_ranges$norm_range)

    #### dynamic ranges of the axes

    observe({

      raw_range <- vector_plots()[[1]]$data[c('x', 'y')] %>%
        map(range)

      updateNumericInputIcon(inputId = 'x_min_vectors_raw',
                             value = raw_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_vectors_raw',
                             value = raw_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_vectors_raw',
                             value = raw_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_vectors_raw',
                             value = raw_range[[2]][2])

    })

    observe({

      norm_range <- vector_plots()[[2]]$data[c('x', 'y')] %>%
        map(range)

      updateNumericInputIcon(inputId = 'x_min_vectors_norm',
                             value = norm_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_vectors_norm',
                             value = norm_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_vectors_norm',
                             value = norm_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_vectors_norm',
                             value = norm_range[[2]][2])

    })

    observeEvent(input$vectors_raw_refresh, {

      updateNumericInputIcon(inputId = 'x_min_vectors_raw',
                             value = init_vector_ranges$raw_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_vectors_raw',
                             value = init_vector_ranges$raw_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_vectors_raw',
                             value = init_vector_ranges$raw_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_vectors_raw',
                             value = init_vector_ranges$raw_range[[2]][2])

    })

    observeEvent(input$vectors_norm_refresh, {

      updateNumericInputIcon(inputId = 'x_min_vectors_norm',
                             value = init_vector_ranges$norm_range[[1]][1])

      updateNumericInputIcon(inputId = 'x_max_vectors_norm',
                             value = init_vector_ranges$norm_range[[1]][2])

      updateNumericInputIcon(inputId = 'y_min_vectors_norm',
                             value = init_vector_ranges$norm_range[[2]][1])

      updateNumericInputIcon(inputId = 'y_max_vectors_norm',
                             value = init_vector_ranges$norm_range[[2]][2])

    })

    vector_plot_raw <- reactive({

      vector_plots()[[1]] +
        scale_x_continuous(limits = c(input$x_min_vectors_raw,
                                      input$x_max_vectors_raw)) +
        scale_y_continuous(limits = c(input$y_min_vectors_raw,
                                      input$y_max_vectors_raw))

    })

    vector_plot_norm <- reactive({

      vector_plots()[[2]] +
        scale_x_continuous(limits = c(input$x_min_vectors_norm,
                                      input$x_max_vectors_norm)) +
        scale_y_continuous(limits = c(input$y_min_vectors_norm,
                                      input$y_max_vectors_norm))

    })

    ## plotting

    output$vectors_raw <- renderPlot({

      vector_plot_raw()

    })

    output$vectors_norm <- renderPlot({

      vector_plot_norm()

    })

    output$download_vectors_raw <- downloadHandler(

      filename = function() {

        return(paste0('displ_vector_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = vector_plot_raw() + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    output$download_vectors_norm <- downloadHandler(

      filename = function() {

        return(paste0('displ_vector_plot_normalized_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = vector_plot_norm() + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    qc <- observe({

      if(!all(c(is.ggplot(disp_plots()[[1]]),
                is.ggplot(straight_plots()[[1]]),
                is.ggplot(bic_plots()[[1]]),
                is.ggplot(autocov_plots()[[1]]),
                is.ggplot(pair_plots()[[1]]),
                is.ggplot(track_plots()[[1]])))) {

        showNotification('Directionality analysis error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 6,
                        total = 8,
                        title = 'Directional movement analysis')

    })

    ## Hotelling's plot --------

    test_results <- reactive({

      calculate(input_data(), 'hotellings', step_spacing = input$step_spacing)

    })

    hotellings_plot <- reactive({

      plot_cap <- test_results() %>%
        mutate(plot_cap = paste('t =', signif(statistic, 2)),
               significance = ifelse(p_value < 0.05,
                                     paste('p =', signif(p_value, 2)),
                                     paste0('ns (p = ', signif(p_value, 2), ')')),
               plot_cap = paste(plot_cap, significance, sep = ', '))

      plot(input_data(),
           type = 'hotellings',
           plot_title = "Hotelling's test",
           plot_subtitle = plot_cap$plot_cap[1],
           color = input$hotellings_color,
           coverage = input$hotellings_coverage,
           cust_theme = theme_shiny(),
           step_spacing = input$step_spacing)

    })

    output$hotellings_plot <- renderPlot({

      hotellings_plot()

    })

    output$download_hotellings <- downloadHandler(

      filename = function() {

        return(paste0('hotellings_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = hotellings_plot() + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    qc <- observe({

      if(!all(c(is.ggplot(disp_plots()[[1]]),
                is.ggplot(straight_plots()[[1]]),
                is.ggplot(bic_plots()[[1]]),
                is.ggplot(autocov_plots()[[1]]),
                is.ggplot(pair_plots()[[1]]),
                is.ggplot(track_plots()[[1]]),
                is.ggplot(hotellings_plot())))) {

        showNotification("Hotelling's test error",
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 7,
                        total = 8,
                        title = "Hotelling's test")

    })

    ## dimensionality reduction --------

    sum_stats <- reactive({

      summary.trax(input_data())

    })

    norm_stats <- reactive({

      sum_stats() %>%
        select(id,
               steps,
               duration,
               delta_BIC,
               total_displacement,
               mean_speed,
               overall_angle,
               overall_dot,
               asphericity,
               x_displ,
               y_displ) %>%
        column_to_rownames('id') %>%
        center_data(type = input$center_method,
                    complete_cases = TRUE)

    })

    red_object <- reactive({

      norm_stats() %>%
        reduce_data(distance_method = input$dist_method,
                    kdim = input$dim_number,
                    red_fun = input$red_fun)

    })

    red_plots <- reactive({

      list(type = c('scree', 'scores')) %>%
        pmap(plot,
             x = red_object(),
             label_points = FALSE,
             cust_theme = theme_shiny(),
             point_color = input$red_color) %>%
        map(~.x +
              labs(tag = stri_extract(.x$labels$tag,
                                      regex = 'n = \\d+') %>%
                     paste0('\n', .)))

    })

    output$scree_plot <- renderPlot({

      red_plots()[[1]] +
        expand_limits(y = 0)

    })

    output$red_plot <- renderPlot({

      red_plots()[[2]]

    })

    output$download_scree <- downloadHandler(

      filename = function() {

        return(paste0('dimensionality_reduction_scree_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = red_plots()[[1]] + theme_trax(),
               device = cairo_pdf,
               width = 180,
               height = 180,
               units = 'mm')

      }

    )

    output$download_red <- downloadHandler(

      filename = function() {

        return(paste0('dimensionality_reduction_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = red_plots()[[2]] + theme_trax(),
               device = cairo_pdf,
               width = 180,
               height = 180,
               units = 'mm')

      }

    )

    qc <- observe({

      if(!all(c(is.ggplot(disp_plots()[[1]]),
                is.ggplot(straight_plots()[[1]]),
                is.ggplot(bic_plots()[[1]]),
                is.ggplot(autocov_plots()[[1]]),
                is.ggplot(pair_plots()[[1]]),
                is.ggplot(track_plots()[[1]]),
                is.ggplot(red_plots()[[1]])))) {

        showNotification('Dimensionality reduction error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 8,
                        total = 8,
                        title = 'Dimensionality reduction')

    })

    ## downloading the results ------

    output$download_trax <- downloadHandler(

      filename = function() {

        return(paste0('trax_table_', Sys.Date(), '.tsv'))

      },

      content = function(con) {

        write_trax(input_data(),
                   file = con)

      }

    )

    summary_output <- reactive({

      tab_names <- c(id = 'Track ID',
                     steps = 'step number',
                     length = 'track length',
                     duration = 'track duration',
                     delta_BIC = 'motility type, delta BIC',
                     total_displacement = 'total displacement',
                     mean_displacement = 'mean displacement per step',
                     median_displacement = 'median displacement per step',
                     mean_speed = 'mean speed per step',
                     median_speed = 'median speed per step',
                     overall_angle = 'overall displacement angle, radians',
                     mean_angle = 'mean displacement angle per step, radians',
                     overall_dot = 'overall displacement vector dot product',
                     normal_dot = 'normalized displacement vector dot product',
                     straightness = 'straightness',
                     asphericity = 'asphericity',
                     x_displ = 'X displacement',
                     y_displ = 'Y displacement',
                     z_displ = 'Z displacement')

      tab_names <- tab_names[names(sum_stats())]

      set_names(sum_stats(),
                unname(tab_names))


    })

    output$download_stats <- downloadHandler(

      filename = function() {

        return(paste0('track_stats_table_', Sys.Date(), '.xlsx'))

      },

      content = function(con) {

        list(`Summary statistics` = summary_output(),
             `Autocovariance` = autocov_stats(),
             `Hotelling's test` = test_results() %>%
               set_names(c('t statistic',
                           'df1', 'df2', 'df3',
                           'p value')) %>%
               mutate(`step spacing` = input$step_spacing)) %>%
          write_xlsx(path = con)

      }

    )

    dim_red_report <- reactive({

      tibble(Preprocessing = paste('normalization with',
                                   input$center_method,
                                   'centering'),
             Dimensions = input$dim_number,
             Algorithm = switch(input$red_fun,
                                pca = 'PCA',
                                umap = 'UMAP',
                                mds = 'MDS'),
             `Distance metric` = ifelse(input$red_fun == 'pca',
                                        'none',
                                        switch(input$dist_method,
                                               euclidean = 'Euclidean',
                                               manhattan = 'Manhattan',
                                               cosine = 'Cosine')))


    })

    output$download_red_tbl <- downloadHandler(

      filename = function() {

        return(paste0('dimensionality_reduction_table_', Sys.Date(), '.xlsx'))

      },

      content = function(con) {

        list(`Analysis paramaters` = dim_red_report(),
             `Score table` = red_object()$component_tbl %>%
               mutate(`Track ID`= observation) %>%
               select( - observation)) %>%
          write_xlsx(path = con)

      }

    )

    ## download RDa ------

    analysis_bundle <- reactive({

      plot_list <- list(disp_plots() %>%
                          set_names(c('displacement', 'speed')),
                        straight_plots() %>%
                          set_names(c('straightness', 'asphericity')),
                        bic_plots() %>%
                          set_names(c('bic_displacement',
                                      'bic_asphericity')),
                        autocov_plots() %>%
                          set_names(c('autocov_dot', 'autocov_angle')),
                        pair_plots() %>%
                          set_names(c('pair_analysis_cells',
                                      'pair_analysis_steps')),
                        track_plots() %>%
                          set_names(c('tracks', 'tracks_normalized')),
                        vector_plots() %>%
                          set_names(c('vectors', 'vectors_normalized')),
                        list(hotellings_plot = hotellings_plot()),
                        red_plots() %>%
                          set_names(c('dimension_red_scree',
                                      'dimension_red_scores'))) %>%
        unlist(recursive = FALSE) %>%
        map(~.x + theme_trax())

      list(trax_object = input_data(),
           stat_summary = sum_stats(),
           autocovariance = autocov_stats() %>%
             set_names(c('id', 'dot_product', 'angle_radians')),
           hotellings_test = test_results(),
           dim_reduction_params = dim_red_report(),
           dim_reduction_scores = red_object()$component_tbl %>%
             mutate(id = observation) %>%
             select( - observation),
           plots = plot_list)

    })


    output$download_bundle <- downloadHandler(

      filename = function() {

        return(paste0('analysis_result_bundle_', Sys.Date(), '.RDa'))

      },

      content = function(con) {

        analysis <- analysis_bundle()

        save(analysis, file = con)

      }

    )




  }

# Run the app ----

  shinyApp(ui = ui, server = server)
