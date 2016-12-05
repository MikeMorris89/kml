tabItem.fromicebergstotrees<-tabItem(
  tabName = "fromicebergstotrees",
  fluidRow(
    column(1)
    ,column(10
    ,h2("From icebergs to trees")
    ,p("In the previous chapter you did all the slicing and dicing yourself to find subsets that have a higher chance of surviving. A decision tree automates this process for you, and outputs a flowchart-like structure that is easy to interpret (you'll make one yourself in the next exercise).")
    ,p("Conceptually, the decision tree algorithm starts with all the data at the root node and scans all the variables for the best one to split on. Once a variable is chosen, you do the split and go down one level (or one node) and repeat. The final nodes at the bottom of the decision tree are known as terminal nodes, and the majority vote of the observations in that node determine how to predict for new observations that end up in that terminal node.")
    ,p("To create your first decision tree, you'll make use of R's rpart package. Instead of needing to writing an algo yourself you can use this package to build a decision tree.")
    )
    ,column(1)
  )
)