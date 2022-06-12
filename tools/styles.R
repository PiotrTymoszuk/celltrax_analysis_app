# Retuns CSS and ggplot styling information

# CSS -----

  styles <- function() {

    tags$head(
      tags$style(
        "
            .title {
                height: 140px;
                background: url('banner_trax.png');
                background-repeat: no-repeat;
                background-size: cover;
                font-size: 50px;
                color: #4682B4;
                font-family: Bahnschrift, Verdana, Helvetica;
                text-shadow: 1px 1px #91b6d4;
                padding-left: 3%;
                padding-top: 1%;
                padding-bottom: 0.05%
            }

            h2 {
               font-size: 30;
               font-weight: bold;
               font-family: Bahnschrift, Verdana, Helvetica
               }

             h3 {
               font-size: 26;
               font-weight: bold;
               font-family: Bahnschrift, Verdana, Helvetica
               }

            h4 {
               font-size: 22;
               font-family: Bahnschrift, Verdana, Helvetica
            }

            .shiny-text-output {
              font_size: 18,
              font-family: Bahnschrift, Verdana, Helvetica
            }

            .shiny-html-output {
              font_size: 18,
              font-family: Bahnschrift, Verdana, Helvetica
            }

            "
      )
    )

  }

# ggplot ------

  theme_shiny <- function() {

    common_text <- element_text(size = 14,
                                face = 'plain',
                                color = 'black')

    common_margin <- ggplot2::margin(t = 4, l = 3, r = 2, unit = 'mm')

    theme_classic() + theme(axis.text = common_text,
                            axis.title = common_text,
                            plot.title = element_text(size = 14,
                                                      face = 'bold',
                                                      color = 'black',
                                                      hjust = 0),
                            plot.subtitle = common_text,
                            plot.tag = element_text(size = 14,
                                                    face = 'plain',
                                                    color = 'black',
                                                    hjust = 0),
                            plot.tag.position = 'bottom',
                            legend.text = common_text,
                            legend.title = common_text,
                            strip.text = common_text,
                            strip.background = element_rect(fill = 'gray95',
                                                            color = 'gray80'),
                            plot.margin = common_margin,
                            panel.grid.major = element_line(color = 'gray90'))


  }

# plot colors ------

  plot_colors <- function() {

    list('steelblue' = 'steelblue',
         'darkolivegreen' = 'darkolivegreen4',
         'coral' = 'coral3',
         'gray' = 'gray60',
         'cornsilk' = 'cornsilk2',
         'firebrick' = 'firebrick3',
         'plum' = 'plum3',
         'orangered' = 'orangered3',
         'black' = 'black')

  }



