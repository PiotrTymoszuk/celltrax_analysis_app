# Some functional utilities

# Check a text file for track id, time, coordinates and sample ID variables

  spy_file <- function(file, delim = '\t', first = TRUE, ...) {

    ## read the file heading

    text_entry <- readr::read_delim(file = file,
                                    delim = delim,
                                    col_names = TRUE,
                                    n_max = 2,
                                    show_col_types = FALSE, ...) %>%
      names

    ## candidate columns

    detect_regex <- list(sample_col = '(W|w)ell|(S|s)ample',
                         id_col = '(T|t)rack|(T|t)race|ID|id',
                         t_col = '(T|t)ime|^T(\\s+|$)|^t(\\s+|$)',
                         x_col = '^X|^x',
                         y_col = '^Y|^y',
                         z_col = '^Z|^z')

    col_list <- map(detect_regex,
                    ~stri_detect(text_entry, regex = .x)) %>%
      map(~text_entry[.x])

    if(first) {

      col_list <- map(col_list,
                      function(x) if(length(x) > 0) x[[1]] else NULL)

    } else {

      col_list <- col_list

    }

    return(c(col_list,
             list(col_names = text_entry)))

  }
