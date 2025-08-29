install.packages("rsconnect")
library(rsconnect)
rsconnect::setAccountInfo(name = "jameselsner",
                          token = "3C92183CA1B1099E8103D01DC641077D",
                          secret = "RioG4oKcktVcb8qrNR8r1/oUIv918FgessS+tkFj")
rsconnect::deployApp(
  appDir   = "storm-mortality",
  appName  = "storm-mortality",   # pick a short unique name
  account  = "jameselsner"
)

