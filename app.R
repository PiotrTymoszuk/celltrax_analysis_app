# This function provides the interface and server functions for an web app
# for import and pre-processing of microscopy cell tracking data.

# Tools ------

  library(plyr)
  library(tidyverse)
  library(shiny)
  library(shinyWidgets)
  library(celltrax)
  library(waiter)
  library(writexl)
  library(rlang)
  library(clustTools)
  library(stringi)

  source('./tools/styles.R')
  source('./tools/main_panels.R')
  source('./tools/tabsets.R')

  options(shiny.usecairo = FALSE)

# User interface -----

  ui <- fluidPage(

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
                    autocov_plots(),
                    pair_analysis(),
                    dir_analysis(),
                    hetero_analysis(),
                    get_data()
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

    ## Table with input tracks, catching entry errors ----

    input_data <- eventReactive(input$launcher, {

      file <- input$data_entry

      output <- try(read_trax(file$datapath), silent = TRUE)

      if(any(class(output) == 'try-error')) {

        return('Input error: incorrect path? wrong file format?')

      }

      if(length(output) > 1000) {

        output <- structure('Input error: data limit reached.
                            The app can process up to 1000 tracks.',
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
                        total = 5,
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
                            total = 'Total velocity vector length',
                            mean = 'Mean speed per step',
                            median = 'Median speed per step')

      disp_plots <- list(plot_title = c(disp_title, speed_title),
                         stat = c('displacements', 'speeds')) %>%
        pmap(plot,
             x = input_data(),
             color = input$disp_color,
             type = 'statistic',
             geom = input$disp_geom,
             cust_theme = theme_shiny())

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

    ### delta BIC

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

        showNotification('Track statistic computation error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 2,
                        total = 5,
                        title = 'Track statistic')

    })

    ## Motion persistence: autocovariance --------

    autocov_stats <- reactive({

      list(method = c('dot_product', 'angle')) %>%
        pmap(calculate,
             x = input_data(),
             type = 'autocov',
             aggregate = TRUE) %>%
        reduce(full_join, by = 'i') %>%
        set_names(c('Step i',
                    'Displacement vector dot product',
                    'Displacement vector angle, radians'))


    })

    autocov_plots <- reactive({

      list(method = c('dot_product', 'angle'),
           plot_title = c('Autocovariance: displacement vectors',
                          'Autocovariance: angles'),
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
               scale_y_continuous(trans = input$autocov_y_scale))

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
                        value = 3,
                        total = 5,
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
                        value = 3,
                        total = 5,
                        title = 'Cell pair analysis')

    })

    ## Directional motion analysis -------

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

    vector_plots <- reactive({

      track_color <- if(input$monochrome == 'yes') input$track_color else NULL

      list(normalize = c(FALSE, TRUE),
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

    output$tracks_raw <- renderPlot({

      track_plots()[[1]]

    })

    output$vectors_raw <- renderPlot({

      vector_plots()[[1]]

    })

    output$tracks_norm <- renderPlot({

      track_plots()[[2]]

    })

    output$vectors_norm <- renderPlot({

      vector_plots()[[2]]

    })

    output$hotellings_plot <- renderPlot({

      hotellings_plot()

    })

    output$download_tracks_raw <- downloadHandler(

      filename = function() {

        return(paste0('track_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = track_plots()[[1]] + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

    output$download_vectors_raw <- downloadHandler(

      filename = function() {

        return(paste0('displ_vector_plot_', Sys.Date(), '.pdf'))

      },

      content = function(con) {

        ggsave(filename = con,
               plot = vector_plots()[[1]] + theme_trax(),
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
               plot = track_plots()[[2]] + theme_trax(),
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
               plot = vector_plots()[[2]] + theme_trax(),
               device = cairo_pdf,
               width = 160,
               height = 160,
               units = 'mm')

      }

    )

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
                is.ggplot(track_plots()[[1]])))) {

        showNotification('Directionality analysis error',
                         duration = NULL,
                         closeButton = TRUE,
                         type = 'error')

      }

      updateProgressBar(id = 'pb',
                        value = 4,
                        total = 5,
                        title = 'Directional movement analysis')

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

      red_plots()[[1]]

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
                        value = 5,
                        total = 5,
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

    output$download_stats <- downloadHandler(

      filename = function() {

        return(paste0('track_stats_table_', Sys.Date(), '.xlsx'))

      },

      content = function(con) {

        list(`Summary statistics` = sum_stats() %>%
               set_names(c('Track ID',
                           'step number',
                           'track length',
                           'track duration',
                           'motility type, delta BIC',
                           'total displacement',
                           'mean displacement per step',
                           'median displacement per step',
                           'mean speed per step',
                           'median speed per step',
                           'overall displacement angle, radians',
                           'mean displacement angle per step, radians',
                           'overall displacement vector dot product',
                           'normalized displacement vector dot product',
                           'straightness',
                           'asphericity',
                           'X displacement',
                           'Y displacement',
                           'Z displacement')),
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
                        hotellings_plot = hotellings_plot(),
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
