# DistributionVisualizer

Code for Shiny web application that visualizes distributions based on specified moments (mean, standard deviation, skewness, kurtosis)

# Deployment

```r
library(rsconnect)

rsconnect::setAccountInfo(
  name = "<ACCOUNT>",
  token = "<TOKEN>",
  secret = "<SECRET>")
  
rsconnect::deployApp()
```
