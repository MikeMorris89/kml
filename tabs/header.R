dashboardHeader.main<-dashboardHeader(
  title = "Kaggle Machine Learning"
  
  # Dropdown menu for messages
  ,dropdownMenu(type = "messages", badgeStatus = "success",
               messageItem("Support Team",
                           "This is the content of an example message.",
                           time = "5 mins"
               ),
               messageItem("Support Team",
                           "This is the content of another example message.",
                           time = "2 hours"
               ),
               messageItem("New User",
                           "Can I get some help?",
                           time = "Today"
               )
  ),
  
  # Dropdown menu for notifications
  dropdownMenu(type = "notifications", badgeStatus = "warning",
               notificationItem(icon = icon("users"), status = "info",
                                "5 new members joined today"
               ),
               notificationItem(icon = icon("warning"), status = "danger",
                                "Resource usage near limit."
               ),
               notificationItem(icon = icon("shopping-cart", lib = "glyphicon"),
                                status = "success", "25 sales made"
               ),
               notificationItem(icon = icon("user", lib = "glyphicon"),
                                status = "danger", "You changed your username"
               )
  ),
  
  # Dropdown menu for tasks, with progress bar
  dropdownMenu(type = "tasks", badgeStatus = "danger",
               taskItem(value = 20, color = "aqua",
                        "Refactor code"
               ),
               taskItem(value = 40, color = "green",
                        "Design new layout"
               ),
               taskItem(value = 60, color = "yellow",
                        "Another task"
               ),
               taskItem(value = 80, color = "red",
                        "Write documentation"
               )
  )
)