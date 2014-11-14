#' Frequency counts
#'
#' Check the frequencies of values in a vector.
#' @param ordered If TRUE the returned data frame is returned with most frequent items at top Defaults to FALSE.
#' @keywords table
#' @export
#' @examples
#' freq()

freq = function(vector, ordered = FALSE) { 
    df = data.frame(table(vector, useNA = "ifany"))
    names(df) = c("value", "freq")
    if (ordered) { df = df[order(-df$freq), ] }
    df
}


#' Count unique values in a vector
#'
#' A short cut to typing length(unique()), nothing more.
#' @param None.
#' @keywords length unique
#' @export
#' @examples
#' lu()

lu = function(x) { length(unique(x)) }  


#' Calculate the number of calender months between dates
#'
#' Author: http://stackoverflow.com/users/143305/dirk-eddelbuettel
#' @param None.
#' @keywords date month
#' @export
#' @examples
#' month_diff("1980-03-31", Sys.Date())

month_diff = function(date1, date2) {
    # turn a date into a 'monthnumber' relative to an origin
    month_num = function(date) {
        lt <- as.POSIXlt(as.Date(date, origin = "1900-01-01"))
        lt$year * 12 + lt$mon
    }
    # return the month difference between two monthnumbers
    month_num(date2) - month_num(date1)
}













gg_colour_hue = function(n) { hues = seq(15, 375, length=n+1); hcl(h=hues, l=65, c=100)[1:n] }

year_quarter = function(x) {
    #require(lubridate)
    x = factor(quarter(x, with_year = TRUE))
    levels(x) = paste(substr(levels(x), 1, 4), substr(levels(x), 6, 6), sep = "-Q")
    x
}

month_year = function(x) { factor(as.yearmon(x)) }

week_start <- function(date) { 
    as.Date(ifelse(date == floor_date(date, "week"), 
                   floor_date(date - 1, "week") + 1,
                   floor_date(date, "week") + 1))
}

make_names_nice = function(names) {
    names = tolower(names)           # lower case
    names = gsub("\\.", "_", names)  # replace all . with _
    names = gsub("__", "_", names)   # replace __ with _
    names = gsub("_$", "", names)    # lose trailing _
    names = gsub("_$", "", names)    # lose any remaining trailing _
    names
}

# local connection

connect_to_local = function(name="analytics_copy") {
    print(paste0("Connecting to local db: ", name))
    dbConnect(
        MySQL(),
        host="127.0.0.1",
        port=3306,
        user="root",
        dbname=name
    )
}


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

nice_chart = theme(axis.ticks = element_blank(),
                   axis.title.x = element_text(size = 25, vjust = 0.5),
                   axis.title.y = element_text(size = 25, vjust = 0.5),
                   axis.text.x = element_text(size = 20, vjust = 0.5),
                   axis.text.y = element_text(size = 20),  
                   strip.text = element_text(size = 25),
                   legend.text = element_text(size = 20),
                   legend.key = element_blank(),
                   legend.title = element_text(size = 25),
                   plot.title = element_text(size = 25, face = "bold")
                   )


# add excelify!
