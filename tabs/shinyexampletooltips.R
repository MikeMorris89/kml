plot.tooltips <- function(bins){
  
  # generate bins based on input$bins from ui.R
  x    <- faithful[, 2]
  bins <- seq(min(x), max(x), length.out = bins + 1)
  
  # draw the histogram with the specified number of bins
  return(hist(x, breaks = bins, col = 'darkgray', border = 'white'))
  
}
tt.popover<-paste0("Waiting time between ",
                   "eruptions and the duration of the eruption for the Old Faithful geyser ",
                   "in Yellowstone National Park, Wyoming, USA.

Azzalini, A. and ",
                   "Bowman, A. W. (1990). A look at some data on the Old Faithful geyser. ",
                   "Applied Statistics 39, 357-365.

")

createtext<-function(b){   
  txt = ""
  if((b > 5 & b < 15) | b > 35) {
    txt = "That was dangerous."
  } else if((b > 5 & b < 20) | b > 30) {
    txt = "I warned you about that."
  } else if(b >= 20 &  b <= 30) {
    txt = "You have choosen... wisely."
  }
  return(txt)
}

eval.function<-function(num1,num2,session){
  if(is.na(num1) | is.na(num2)) {
    createAlert(session, "alert", "exampleAlert", title = "Oops",
                content = "Both inputs should be numeric.", append = FALSE)
  } else if(num2 == 0) {
    createAlert(session, "alert", "exampleAlert", title = "Oops",
                content = "You cannot divide by 0.", append = FALSE)
  } else {
    closeAlert(session, "exampleAlert")
    return(num1/num2)
  }
}

create.dt<-function(bins){
  x    <- faithful[, 2]
  bins <- seq(min(x), max(x), length.out = bins + 1)
  
  # draw the histogram with the specified number of bins
  tab <- hist(x, breaks = bins, plot = FALSE)
  tab$breaks <- sapply(seq(length(tab$breaks) - 1), function(i) {
    paste0(signif(tab$breaks[i], 3), "-", signif(tab$breaks[i+1], 3))
  })
  tab <- as.data.frame(do.call(cbind, tab))
  colnames(tab) <- c("Bins", "Counts", "Density")
  return(tab[, 1:3])
}