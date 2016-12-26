tabItem.explorecaretnotes<-
  tabItem(tabName = "explorecaret",
          hr("Notes"),
                     fluidPage(
                       h4("RMSE is commonly calculated in-sample on your training set. What's a potential drawback to calculating training set error?")
                       ,p("You have no idea how well your model generalizes to new data (i.e. overfitting).")
                       ,h4("What is the advantage of using a train/test split rather than just validating your model in-sample on the training set?")
                       ,p("It gives you an estimate of how well your model performs on new data.")
                       ,h4("What is the advantage of cross-validation over a single train/test split?")
                       ,p("It gives you multiple estimates of out-of-sample error, rather than a single estimate.")
                     )
          )