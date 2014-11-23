#' Frequency counts
#'
#' Check the frequencies of values in a vector.
#' @param ordered If TRUE the data frame is returned with most frequent items at top Defaults to FALSE.
#' @keywords table
#' @export
#' @examples
#' freq(c(1, 1, 2, 3))

freq = function(vector, ordered = FALSE) { 
    df = data.frame(table(vector, useNA = "ifany"))
    names(df) = c("value", "freq")
    if (ordered) { df = df[order(-df$freq), ] }
    df
}


#' Count unique values in a vector
#'
#' A short cut to typing length(unique()), nothing more.
#' @param None
#' @keywords length unique
#' @export
#' @examples
#' lu(c(1, 1, 2, 3)

lu = function(x) { length(unique(x)) }  


#' Calculate the number of calendar months between dates
#'
#' Author: http://stackoverflow.com/users/143305/dirk-eddelbuettel
#' @param None
#' @keywords date month
#' @export
#' @examples
#' month_diff("1980-03-21", today())

month_diff = function(date1, date2) {
    if (!is.Date(date1)) { date1 = as.Date(date1) }
    if (!is.Date(date2)) { date2 = as.Date(date2) }
    # turn a date into a 'monthnumber' relative to an origin
    month_num = function(date) {
        lt <- as.POSIXlt(as.Date(date, origin = "1900-01-01"))
        lt$year * 12 + lt$mon
    }
    # return the month difference between two monthnumbers
    month_num(date2) - month_num(date1)
}


#' Re-create ggplot colour palettes
#'
#' Get a vector of hexadecimal colours as used by ggplot2 for a given number of groups in a plot.
#' @param None
#' @keywords ggplot2 palette
#' @export
#' @examples
#' gg_colour_hue(3)

gg_colour_hue = function(n) { hues = seq(15, 375, length=n+1); hcl(h=hues, l=65, c=100)[1:n] }


#' Classify dates according to year and quarter
#'
#' Returns a factor vector of same length as input.
#' @param None
#' @keywords date quarter
#' @export
#' @examples
#' year_quarter(today())

year_quarter = function(x) {
    if (!is.Date(x)) { date = as.Date(x) }
    x = factor(quarter(x, with_year = TRUE))
    levels(x) = paste0(substr(levels(x), 1, 4), "-Q", substr(levels(x), 6, 6))
    x
}


#' Obtain yearmon object as factor for given dates
#'
#' Shortcut for converting to yearmon and then factor (useful for plots).
#' @param None
#' @keywords month year
#' @export
#' @examples
#' month_year(today())

month_year = function(x) { 
    if (!is.Date(x)) { date = as.Date(x) }
    factor(as.yearmon(x))
}


#' Return first Monday of week for any given date
#'
#' Basically floor_date but with week start taken to be Monday, rather than Sunday.
#' @param None
#' @keywords floor_date monday
#' @export
#' @examples
#' week_start(today())

week_start <- function(date) {
    if (!is.Date(date)) { date = as.Date(date) }
    as.Date(ifelse(date == floor_date(date, "week"), 
                   floor_date(date - 1, "week") + 1,
                   floor_date(date, "week") + 1), 
            origin = "1970-01-01")
}


#' Clean up data frame column names
#'
#' Remove unsightly characters from character vector. Intended for use on data frame column names.
#' @param None
#' @keywords column names
#' @export
#' @examples
#' names(df) = make_names_nice(names(df))

make_names_nice = function(names) {
    names = tolower(names)           # lower case
    names = gsub("\\.", "_", names)  # replace all . with _
    names = gsub("__", "_", names)   # replace __ with _
    names = gsub("_$", "", names)    # lose trailing _
    names = gsub("_$", "", names)    # lose any remaining trailing _
    names = gsub(" ", "", names)     # remove whitespace
    names
}


#' Connect to local database
#'
#' This connects me to a local MySQL database - you might need to change host and root to work on your computer.
#' @param None.
#' @keywords local database MySQL
#' @export
#' @examples
#' connect_to_local("my_db")

connect_to_local = function(name) {
    print(paste0("Connecting to local db: ", name))
    dbConnect(
        MySQL(),
        host = "127.0.0.1",
        port = 3306,
        user = "root",
        dbname = name
    )
}


#' Multiple plot function
#'
#' Plot multiple ggplot objects on one page.  
#' 
#' Author: Winston Chang: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)
#' 
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#'
#' If the layout is something like matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @param cols Number of columns in layout,
#'        layout A matrix specifying the layout. If present, 'cols' is ignored.
#' @keywords ggplot2 grid
#' @export
#' @examples
#' multiplot(plot1, plot2, cols = 2)

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                    ncol = cols, nrow = ceiling(numPlots / cols))
  }

 if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx = as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#' Create ggplot object to make chart more pretty
#'
#' A custom theme that can be added to ggplot2 objects to make them look less scientific (useful when presenting to non-analysts).
#' @param None
#' @keywords ggplot2 theme
#' @export
#' @examples
#' ggplot(df, aes(x, y)) + geom_point() + nice_chart

nice_chart = theme(axis.ticks   = element_blank(),
                   axis.title   = element_text(size = 25),
                   axis.text    = element_text(size = 25),  
                   strip.text   = element_text(size = 25),
                   legend.text  = element_text(size = 25),
                   legend.key   = element_blank(),
                   legend.title = element_text(size = 25),
                   plot.title   = element_text(size = 25, face = "bold")
                   )
