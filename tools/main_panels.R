# The title and side panels

# Title panel ------

  title_panel <- function() {

     titlePanel(title =  HTML("<div class = 'title'>
                                   <strong>Celltrax:</strong>
                                   analyze tracking data
                                   <img src = '' width = 45%>
                                   <img src = 'mui_logo.png' width = 5%><br/>
                                   <div/>
                                   <hr style = 'height: 5px'>"),
                windowTitle = 'Celltrax tools')

   }

# Side panel ------

  side_panel <- function() {

    sidebarPanel(h4('Step 1'),
                 h5('Upload pre-processed tracking data in a tab-separated text file'),
                 fileInput(inputId = 'data_entry',
                           label = 'Choose the file',
                           multiple = FALSE,
                           accept = c('.tsv', '.csv', '.txt')),
                 uiOutput('selectors'),
                 br(),
                 h4('Step 2'),
                 fluidRow(column(width = 6,
                                 h5('Start the analysis')),
                          column(width = 6,
                                 align = 'center',
                                 disabled(actionBttn(inputId = 'launcher',
                                            icon = icon(name = 'play',
                                                        lib = 'glyphicon'),
                                            style = 'unite',
                                            color = 'primary')))),
                 br(),
                 br(),
                 h4('Step 3'),
                 h5('Adjust the analysis parameters, step by step'),
                 br(),
                 h4('Step 4'),
                 h5('Download the statistics, plots and R project'),
                 br(),
                 h4('Step 5'),
                 fluidRow(column(width = 6,
                                 h5('Start with a new sample')),
                          column(width = 6,
                                 align = 'center',
                                 actionBttn(inputId = 'refresh',
                                            icon = icon(name = 'refresh',
                                                        lib = 'glyphicon'),
                                            style = 'unite',
                                            color = 'primary'))),
                 br(),
                 br(),
                 h4('Analysis progress'),
                 progressBar(id = 'pb',
                             value = 0,
                             total = 8,
                             title = ''),
                 br(),
                 width = 3,
                 bsTooltip(id = 'launcher',
                           title = 'start the analysis once a file is provided'),
                 bsTooltip(id = 'refresh',
                           title = 'start over with a new sample'))

  }

# END -----
