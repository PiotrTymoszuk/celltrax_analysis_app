# Tabsets for the main panel

# Tab 1: general information ------

  general_info <- function() {

    tabPanel('General information',
             textOutput('test'),
             h3('Welcome to celltrax tracing analysis tools!'),
             hr(),
             p("The celltrax analysis application is an interactive implementation of ",
               a(href = 'https://github.com/ingewortel/celltrackR', 'celltrackR'),
               " and ",
               a(href = 'https://github.com/PiotrTymoszuk/celltrax', 'celltrax'),
               " R packages for pre-processing and analysis of microscopy cell tracing data.
               This online Shiny tool provides a platform for analysis of user-provided
               cell tracking data in text form with most commonly used settings.
               Ideally, the input data should be pre-processed to repair hardware
               and software faults, short tracks or drift, e. g. with the ",
               a(href = 'https://im2-ibk.shinyapps.io/celltrax_preprocess/',
                 'celltrax preprocessing tools app.'),
               "The upper size limit of the input data is 1000 tracks or 40000 steps.
               For analysis of multiple samples, rich or untypical track
               sets (i.e. motile macroscopic objects), please resort to the seminal R packages."),
             br(),
             p("For help, please refer to the ",
               a(href = 'Manual.pdf', 'manual.'),
               "You may be also interested in experimenting with our ",
               a(href = 'well_3_tracks.tsv', 'macrophage demo data'),
               " or ",
               a(href = 'b_cells.tsv', 'B cells'),
               ", ",
               a(href = 't_cells.tsv', 'T cells'),
               " and ",
               a(href = 'neutros.tsv', 'neutrophils'),
               " provided with the celltrackR package."),
             br(),
             p("The R code of the application is open and available ",
               a(href = 'https://github.com/PiotrTymoszuk/celltrax_analysis_app', 'here.')),
             p("The app developers put all efforts to develop and maintain qualitative cell tracing
                solutions but carry no responsibility for correctness and error-free functioning of
                the application. This tool may not be used for diagnostic and treatment purposes.
                For scientific use only."),
             hr(),
             em('By using the application you accept',
                a('the terms of use and licensing', href = 'Readme.pdf')),
             br(),
             HTML("<div style =  'text-align: right'>
                                  <img src = '' width = 80%>
                                  <p>Powered by </p>
                                  <a href = 'http://www.daas.tirol'>
                                  <img src = 'logo_large.png' width = 60 alt = 'daas.tirol'>
                                  </a>
                                  <img src = '' width = 30>
                                   <img src = 'shiny_logo.png' width = 60></div>"))

  }

# Tab 2: track statistics -----

  track_stats <- function() {

    tabPanel('Track statistics',
             h3('Basic cell track statistics'),
             br(),
             p("This analysis enables you to exoplore cell displacements and speed
               (step-wise mean and lengths of total displacement/speed vectors),
               tracks straightness and asphericity as estimates of directional
               movement and the mode or motility (passive or active)."),
             hr(),
             h4('Cell displacement and speed'),
             fluidRow(column(width = 4,
                             selectInput(inputId = 'disp_aggregate',
                                         label = 'statistic type',
                                         choices = list('total' = 'total',
                                                        'mean per step' = 'mean',
                                                        'median per step' = 'median'),
                                         selected = 'total')),
                      column(width = 4,
                             selectInput(inputId = 'disp_geom',
                                         label = 'statistic type',
                                         choices = list('histogram' = 'histogram',
                                                        'violin plot' = 'violin',
                                                        'box plot' = 'boxplot'),
                                         selected = 'violin')),
                      column(width = 4,
                             selectInput(inputId = 'disp_color',
                                         label = 'plot color',
                                         choices = plot_colors(),
                                         selected = 'steelblue'))),
             br(),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'disp_plot',
                                        height = '500px',
                                        width = '80%'),
                             downloadButton(outputId = 'download_disp',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput(outputId = 'speed_plot',
                                        height = '500px',
                                        width = '80%'),
                             downloadButton(outputId = 'download_speed',
                                            label = 'download plot'))),
             hr(),
             h4('Track straightness'),
             fluidRow(column(width = 4,
                             selectInput(inputId = 'straight_geom',
                                         label = 'statistic type',
                                         choices = list('histogram' = 'histogram',
                                                        'violin plot' = 'violin',
                                                        'box plot' = 'boxplot'),
                                         selected = 'violin')),
                      column(width = 4,
                             selectInput(inputId = 'straight_color',
                                         label = 'plot color',
                                         choices = plot_colors(),
                                         selected = 'steelblue')),
                      column(width = 4)),
             br(),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'straight_plot',
                                        height = '500px',
                                        width = '80%'),
                             downloadButton(outputId = 'download_straight',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput(outputId = 'aspher_plot',
                                        height = '500px',
                                        width = '80%'),
                             downloadButton(outputId = 'download_aspher',
                                            label = 'download plot'))),
             hr(),
             h4('Type of motion'),
             br(),
             p("Non-motile objects in your sample may represent dead cells.
               In turn, non-adherent floating cells may underlie random,
               Brownian-like motion. By principle, non-motile or passively moving
               cells may be well modeled by fitting a single Gaussian distribution
               to their position coordinates. Yet, this simple model fails for
               actively moving cells. See: ",
               a('celltraceR manual',
                 href = 'https://cran.rstudio.com/web/packages/celltrackR/vignettes/QC.html'),
               " for details. The difference in the active and passive motility model fits
               is described by 'delta BIC' parameter. Low delta BIC values indicate
               Brownian-like motility, high delta BIC values suggest active movement and
               can be used to identify passively moving cells in your sample and eliminate them.
               The 'sigma' parameter estimates the passive motility radius around a central point:
               approximate cell diameter is usually a good starting value."),
             fluidRow(column(width = 4,
                             numericInput(inputId = 'sigma',
                                          label = 'sigma',
                                          value = 10,
                                          min = 0)),
                      column(width = 4,
                             selectInput(inputId = 'bic_color',
                                         label = 'plot color',
                                         choices = plot_colors(),
                                         selected = 'steelblue')),
                      column(width = 4,
                             numericInput(inputId = 'bic_coverage',
                                          label = '% plotted cells',
                                          value = 100,
                                          min = 10,
                                          max = 100,
                                          step = 10))),
             fluidRow(column(width = 4,
                             selectInput(inputId = 'bic_x_scale',
                                         label = 'X axis transformation',
                                         choices = list('identity' = 'identity',
                                                        'pseudo log' = 'pseudo_log'),
                                         selected = 'identity')),
                      column(width = 4,
                             selectInput(inputId = 'bic_y_scale',
                                         label = 'Y axis transformation',
                                         choices = list('identity' = 'identity',
                                                        'pseudo log' = 'pseudo_log'),
                                         selected = 'identity')),
                      column(width = 4)),
             br(),
             fluidRow(column(width = 6,
                             plotOutput('bic_displ',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_bic_disp',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput('bic_aspher',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_bic_aspher',
                                            label = 'download plot')))
             )

  }

# Tab 3: autocovariance ------

  autocov_plots <- function() {

    tabPanel('Auto-covariance',
             h3('Movement persistence: autoc-ovariance of displacement vectors'),
             br(),
             p("By investigating of covariance of subsequent displacement vectors of a track,
               persistent, directional motion of a cell may be investigated. This tool
               offers two methods of auto-covariance calculation: by computing dot products
               and by measuring angles between the subsequent displacement vectors of each
               track. Mean statistic per step is presented."),
             hr(),
             h4('Auto-covariance'),
             br(),
             fluidRow(column(width = 4,
                             selectInput(inputId = 'autocov_color',
                                         label = 'plot color',
                                         choices = plot_colors(),
                                         selected = 'steelblue')),
                      column(width = 4,
                             selectInput(inputId = 'autocov_y_scale',
                                         label = 'Y axis transformation',
                                         choices = list('identity' = 'identity',
                                                        'pseudo log' = 'pseudo_log'),
                                         selected = 'identity')),
                      column(width = 4)),
             br(),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'autocov_dot',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_auto_dot',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput(outputId = 'autocov_angle',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_auto_angle',
                                            label = 'download plot'))))

  }

# Tab 4: pair analysis ------

  pair_analysis <- function() {

    tabPanel('Pair analysis',
             h3('Analysis of cell pairs'),
             br(),
             p("By analysing distances and angles for all cell pairs in the sample,
               a tendency towards concomitant directional motility can be discerned.
               If the cells of your sample move randomly, the angles between cell pair's
               displacement vectors are expected to be evenly distrubuted between 0
               and 180 degrees and mean of approximately 90 degrees. If a specific
               directional motility takes place, such angles are expected to be low,
               between 0 and 90 degrees, independently of the cell - cell distance.
               This tools calculates distances and angles between displacement
               vectors for each cell pair or single steps
               in the track. The expected angle value as a function
               of the cell - cell distance is estimated by a GAM trand
               (generalized additive model) with 95% confidence interval."),
             hr(),
             h4('Distances and angles between cell pairs'),
             br(),
             fluidRow(column(width = 4,
                             selectInput(inputId = 'pair_color',
                                         label = 'point color',
                                         choices = plot_colors(),
                                         selected = 'steelblue')),
                      column(width = 4,
                             selectInput(inputId = 'gam_color',
                                         label = 'trend line color',
                                         choices = plot_colors(),
                                         selected = 'orangered3')),
                      column(width = 4,
                             numericInput(inputId = 'pair_coverage',
                                          label = '% analyzed tracks',
                                          value = 100,
                                          min = 10,
                                          max = 100))),
             br(),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'pair_cells',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_cells',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput(outputId = 'pair_steps',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_steps',
                                            label = 'download plot'))))


  }

# Tab 5: directional movement --------

  dir_analysis <- function() {

    tabPanel('Directional movement analysis',
             h3('Analysis of directional movement'),
             br(),
             p("By visual analysis of cell tracks and total displacement vectors,
               a tendency towards directional movement could be observed.
               Formally, the such tendency could be assessed by a bi-variate
               (or, generally, multivariate) variant of the T test, the Hotelling's test.
               In the Hotelling's test, the step spacing parameter indicates
               how many positions are to be left out between the steps that are
               considered for the test. It allows you to account for
               stimulus-independent persistent motion."),
             hr(),
             h4('Movement tracks and total displacement vectors'),
             br(),
             fluidRow(column(width = 4,
                             radioButtons(inputId = 'monochrome',
                                          label = 'monochrome',
                                          choices = list('no' = 'no',
                                                         'yes' = 'yes'),
                                          selected = 'no',
                                          inline = TRUE)),
                      column(width = 4,
                             selectInput(inputId = 'track_color',
                                         label = 'track/vector color',
                                         choices = plot_colors(),
                                         selected = 'steelblue')),
                      column(width = 4,
                             selectInput(inputId = 'velo_color',
                                         label = 'mean velocity vector color',
                                         choices = plot_colors(),
                                         selected = 'orangered3'))),
             fluidRow(column(width = 4,
                             numericInput(inputId = 'track_coverage',
                                          label = '% plotted tracks',
                                          value = 100,
                                          min = 10,
                                          max = 100)),
                      column(width = 4),
                      column(width = 4)),
             br(),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'tracks_raw',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_tracks_raw',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput(outputId = 'vectors_raw',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_vectors_raw',
                                            label = 'download plot'))),
             br(),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'tracks_norm',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_tracks_norm',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput(outputId = 'vectors_norm',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_vectors_norm',
                                            label = 'download plot'))),
             h4('Hotellings plot'),
             br(),
             fluidRow(column(width = 4,
                             numericInput(inputId = 'step_spacing',
                                          label = 'step spacing',
                                          value = 2,
                                          min = 0,
                                          max = 20)),

                      column(width = 4,
                             selectInput(inputId = 'hotellings_color',
                                         label = 'point color',
                                         choices = plot_colors(),
                                         selected = 'steelblue')),
                      column(width = 4,
                             numericInput(inputId = 'hotellings_coverage',
                                          label = '% plotted tracks',
                                          value = 100,
                                          min = 10,
                                          max = 100))),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'hotellings_plot',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_hotellings',
                                            label = 'download plot')),
                      column(width = 6)))


  }

# Tab 6: heterogenity ------

  hetero_analysis <- function() {

    tabPanel('Movement heterogeneity',
             h3('Heterogenous mode of motility in the sample'),
             br(),
             p("Dimensionality reduction of multiple track measures with
               PCA (principal component analysis),
               UMAP (uniform manifold approximation and Projection, see: DOI 10.1038/nbt.4314
               and ArXiv ID 1802.03426) or MDS (multi-dimensional scaling) may help
               to identify populations of cells in the sample differing in modes of
               their motility. This tool enables two - five dimensional PCA, UMAP or MDS analysis
               of the following track statistics: step number, tracking duration,
               delta BIC (passive versus active motility), total displacement, mean speed,
               overall angle between total displacement vectors, dot product of total
               displacement vectors, track asphericity, X and Y unit vector displacements.
               Prior to analysis, the data set is subjected to normalization
               with median or mean centering."),
             hr(),
             h4('Dimensionality reduction: variance and scores'),
             br(),
             fluidRow(column(width = 4,
                             selectInput(inputId = 'center_method',
                                         label = 'data centering',
                                         choices = list('mean' = 'mean',
                                                        'median' = 'median'),
                                         selected = 'mean')),
                      column(width = 4,
                             numericInput(inputId = 'dim_number',
                                          label = 'dimensions',
                                          value = 2,
                                          min = 2,
                                          max = 5,
                                          step = 1)),
                      column(width = 4)),
             fluidRow(column(width = 4,
                             selectInput(inputId = 'red_fun',
                                         label = 'reduction method',
                                         choices = list('PCA' = 'pca',
                                                        'UMAP' = 'umap',
                                                        'MDS' = 'mds'),
                                         selected = 'pca')),
                      column(width = 4,
                             selectInput(inputId = 'dist_method',
                                         label = 'distance metric',
                                         choices = list('Euclidean' = 'euclidean',
                                                        'Manhattan' = 'manhattan',
                                                        'Cosine' = 'cosine'),
                                         selected = 'euclidean')),
                      column(width = 4,
                             selectInput(inputId = 'red_color',
                                         label = 'point color',
                                         choices = plot_colors(),
                                         selected = 'steelblue'))),
             br(),
             fluidRow(column(width = 6,
                             plotOutput(outputId = 'scree_plot',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_scree',
                                            label = 'download plot')),
                      column(width = 6,
                             plotOutput(outputId = 'red_plot',
                                        height = '500px',
                                        width = '100%'),
                             downloadButton(outputId = 'download_red',
                                            label = 'download plot'))))

  }

# Tab 7: results download ------

  get_data <- function() {

    tabPanel('Fetch data',
             h3('Download track statistics and result plots'),
             br(),
             p("This tool enables you to save the analysis results in tabular
             form including tracks data table, a table with track statistics,
             auto-covariance, Hotelling's test and dimensionality reduction
             results. Finally, you may save the entire analysis bundle
             (tracks/trax object, plots and statistic tables) as a .RDa
             R data storage file and continue with analysis and plot adjustment
             in R."),
             hr(),
             h4('Download result tables and R data bundle'),
             br(),
             fluidRow(column(width = 4,
                             p('Get tracks data as a tab-delimited text file'),
                             downloadButton(outputId = 'download_trax',
                                            label = 'download tracks')),
                      column(width = 4,
                             p("Download summary of statistics"),
                             downloadButton(outputId = 'download_stats',
                                            label = 'download stats')),
                      column(width = 4,
                             p('Download dimensionality reduction results'),
                             downloadButton(outputId = 'download_red_tbl',
                                            label = 'download results'))),
             br(),
             fluidRow(column(width = 4,
                             p('Download an R data bundle'),
                             downloadButton(outputId = 'download_bundle',
                                            label = 'download R data'))))

  }

# END -----
