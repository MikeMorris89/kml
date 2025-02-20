tabItem.improvingyourpredictions<-tabItem(
  tabName = "improvingyourpredictions",
  fluidRow(
    fluidRow(
       column(1)
      ,column(10
        ,h1("Improving your predictions ")
        ,hr()
        ,h3("What is a Random Forest")
        ,p("A detailed study of Random Forests would take this tutorial a bit too far. However, since it's an often used machine learning technique, a general understanding and an illustration in R won't hurt.")
        ,p("In layman's terms, the Random Forest technique handles the overfitting problem you faced with decision trees. It grows multiple (very deep) classification trees using the training set. At the time of prediction, each tree is used to come up with a prediction and every outcome is counted as a vote. For example, if you have trained 3 trees with 2 saying a passenger in the test set will survive and 1 says he will not, the passenger will be classified as a survivor. This approach of overtraining trees, but having the majority's vote count as the actual classification decision, avoids overfitting.")
        ,p("Before starting with the actual analysis, you first need to meet one big condition of Random Forests: no missing values in your data frame.")
        ,p("First Review missing values on both the test and train sets by selecting 'initial: review.missing(all_data)'")
        ,p("Next review the updates to apply in the definition of the funtion add.factors by selecting 'define: apply updates add.factors(all_data)'")
        ,p("Last review the changes by using the 'review.missing' function agin by selecting 'updated: review.missing(all_data)'")
        ,p("*you can also view the review.missing definition with 'define: review.missing(all_data)'")
        ,fluidRow(
          selectizeInput(label="Pick Function or output:",inputId = "func",choices  = list('initial: review.missing(all_data)'
                                          ,'define: add.factors'
                                          ,'updated: review.missing(all_data)'
                                          ,'define: review.missing(all_data)')
                         ,selected='initial: review.missing(all_data)'),
          verbatimTextOutput('ex_out')
        )
      )
      ,column(1)
    )
  )
)
