library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)
set.seed(1)

#load pre-computed data
kingcounty = readRDS("resources/kingcounty.rds")
lm = readRDS("resources/lm.rds")

#HELPER FUNCTIONS
#helper function to calculate the distance between two lat/longs
haversine = function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in km
}

#helper function to return the 30 houses nearest to the user-input lat/long
getNNgeo = function(lat, long) {
  #create a matrix where each row is a house in our dataset, and $dist corresponds to the distance between that house and the user-input lat/long.
  dist_matrix = data.frame("i"=c(1:nrow(kingcounty)),"dist"=rep(NA,nrow(kingcounty)) )
  for (i in c(1:nrow(kingcounty))) {
    dist_matrix[i,"dist"] = haversine(long, lat, kingcounty$long[i], kingcounty$lat[i])
  }
  return(head(arrange(dist_matrix, dist), n = 30)$i)
}

#TBD given a set of ids, return the most frequently-occuring neighborhood
getNeighborhood = function(ids) {
  neighborhoods = kingcounty[ids,"neighborhood"]
  uniqueIds = unique(neighborhoods)
  uniqueIds[which.max(tabulate(match(neighborhoods, uniqueIds)))]
}

#helper function to inflate prices from 11/2014 to 7/2020 according to the Seattle Price Home Index (https://fred.stlouisfed.org/series/SEXRNSA)
inflate = function(x) {
  x = x * (1+ (273.94 - 169.66363)/169.66363)
}

#helper function that takes a user-input house and returns a price prediction
runModel = function(input, neighborhood) {
  #read in user-input data from UI
  df = data.frame("bedrooms" = input$bedrooms, 
                  "bathrooms" = input$bathrooms, 
                  "sqft_lot"=input$sqft_lot, 
                  "floors"=input$floors, 
                  "waterfront"=as.integer(as.logical(input$waterfront)), 
                  "view"=input$view, 
                  "condition"=input$condition, 
                  "grade"=input$grade, 
                  "sqft_above"=input$sqft_above, 
                  "sqft_basement"=input$sqft_basement, 
                  "zipcode" = as.factor(input$zipcode),
                  "bedrooms_2" = input$bedrooms^2, 
                  "bedrooms_3" = input$bedrooms^3, 
                  "bathrooms_2" = input$bathrooms^2, 
                  "bathrooms_3" = input$bathrooms^3,
                  "bed.bath" = input$bathrooms * input$bedrooms,
                  "floors_2" = input$floors^2,
                  "floors_3" = input$floors^3,
                  "sqft_lot_2"=input$sqft_lot^2, 
                  "sqft_lot_3"=input$sqft_lot^3, 
                  "sqft_above_2"=input$sqft_above^2,
                  "sqft_above_3"=input$sqft_above^3,
                  "sqft_basement_2"=input$sqft_basement^2, 
                  "sqft_basement_3"=input$sqft_basement^3, 
                  "basement" = replace(input$sqft_basement, input$sqft_basement>0, 1),
                  "view.waterfront" = input$view * as.integer(as.logical(input$waterfront)),
                  "view_2"=input$view^2,
                  "condition_2" = input$condition^2,
                  "grade_2"=input$grade^2, 
                  "age" = 2015 - input$yr_built, 
                  "condition.age" = input$condition * (2015 - input$yr_built),
                  "grade.age" = input$grade * (2015 - input$yr_built),
                  "age_2" = (2015 - input$yr_built)^2, 
                  "age_3" = (2015 - input$yr_built)^3, 
                  "age_since_ren_or_build" = 2015 - pmax(input$yr_built,input$yr_renovated),
                  "age_since_ren_or_build_2" = (2015 - pmax(input$yr_built,input$yr_renovated))^2,
                  "renovated" = replace(input$yr_renovated, input$yr_renovated>0, 1),
                  "renovated.age2" = (2015 - input$yr_built)^2 * replace(input$yr_renovated, input$yr_renovated>0, 1),
                  "price" = 0,
                  "lat" = 0,
                  "long" = 0,
                  "neighborhood" = neighborhood)

  #predict housing price using our pre-computed model
  pred_log_price = predict(lm, newdata = df, interval="confidence", level = 0.95)
  
  #inflate the estimated price (and its confidence interval)
  pred_price = inflate(exp(pred_log_price[1]))
  pred_price_lwr = inflate(exp(pred_log_price[2]))
  pred_price_upr = inflate(exp((pred_log_price[3])))
  
  return(c(pred_price,pred_price_lwr,pred_price_upr))
}

#helper function that implements the k-nearest neighbors method: it finds the k observations in train tnat are most similar to new_obs in covariate space
nearest_neighbors = function(train, new_obs, k) {
  X = rbind(train,new_obs)
  X = scale(X)
  
  dist = data.frame("i" = c(1:nrow(X)), "distance" = rep(NA,nrow(X)))
  for(i in 1:nrow(X)) {
    z = as.matrix(rbind(X[i,],X[nrow(X),]))
    matrix = dist(z, method = "euclidean", diag = FALSE, upper = TRUE, p = 2)
    dist[i,"distance"] = matrix[c(1)]
  }
  top_n = head(arrange(dist, distance), k+1)
  return(top_n[c(2:k+1),"i"])
}

#helper function that takes the user-input home and finds the most-similar homes using the k-nearest neighbors method.
getKNN = function(input) {
  df = data.frame("bedrooms" = input$bedrooms, "bathrooms" = input$bathrooms, "sqft_lot"=input$sqft_lot, "floors"=input$floors, "waterfront"=as.integer(as.logical(input$waterfront)), "view"=input$view, "condition"=input$condition, "grade"=input$grade, "sqft_above"=input$sqft_above, "sqft_basement"=input$sqft_basement, "basement" = replace(input$sqft_basement, input$sqft_basement>0, 1), "age" = 2015 - input$yr_built, "age_2" = (2015 - input$yr_built)^2, "age_since_ren_or_build" = 2015 - pmax(input$yr_built,input$yr_renovated), "renovated" = replace(input$yr_renovated, input$yr_renovated>0, 1), "lat"=input$lat, "long"=input$long)
  df.X = df[,c("bedrooms","bathrooms","sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","basement","age","age_2","age_since_ren_or_build","lat","long")]
  
  kc_select = kingcounty[,c("logprice","bedrooms","bathrooms","sqft_lot","floors","waterfront","view","condition","grade","sqft_above","sqft_basement","basement","age","age_2","age_since_ren_or_build","lat","long")]
  kingcounty.X = kc_select[,-1]
  kingcounty.Y = kc_select[,1]
  
  return(nearest_neighbors(kingcounty.X, df.X, 20))
}

ui <- fluidPage(
  
  # App title ----
  titlePanel("King County Home Price Predictor"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      helpText("As you input information on your home below, the charts to the right will update automatically. Performance is slow on the initial load and anytime you adjust the location; it should be fast otherwise."),
      
      # Input: Slider for the number of bins ----
      numericInput("bedrooms", 
                   h4("# Bedrooms"), 
                   value = median(kingcounty$bedrooms)),
      numericInput("bathrooms", 
                   h4("# Bathrooms"), 
                   value = round(median(kingcounty$bathrooms),0)),
      numericInput("sqft_above", 
                   h4("Square footage of interior above ground"), 
                   value = median(kingcounty$sqft_above)),
      numericInput("sqft_basement", 
                   h4("Basement square footage"), 
                   value = median(kingcounty$sqft_basement)),
      numericInput("sqft_lot", 
                   h4("Land square footage"), 
                   value = median(kingcounty$sqft_lot)),
      numericInput("floors", 
                   h4("# Floors"), 
                   value = round(median(kingcounty$floors),0)),
      h4("Waterfront?"),
      checkboxInput("waterfront", "Yes", value = FALSE),
      sliderInput("view", h4("View"),
                  min = 0, max = 4, value = 0),
      sliderInput("condition", h4("Condition"),
                  min = 1, max = 5, value = 3),
      p("Scale described "),
      a("here",href="https://info.kingcounty.gov/assessor/esales/Glossary.aspx?type=r"),
      sliderInput("grade", h4("Grade"),
                  min = 1, max = 13, value = 7),
      p("Scale described "),
      a("here",href="https://info.kingcounty.gov/assessor/esales/Glossary.aspx?type=r"),
      numericInput("yr_built", 
                   h4("Year Built"), 
                   value = 1975),
      numericInput("yr_renovated", 
                   h4("Year Renovated"), 
                   value = 0),
      p("0 if the house has not been renovated."),
      numericInput("zipcode", 
                   h4("Zipcode"), 
                   value = 98005),
      numericInput("lat", 
                   h4("lat"), 
                   value = 47.591985),
      numericInput("long", 
                   h4("long"), 
                   value = -122.175203),
      p("You can get this information by searching your address on google maps.")
      #actionButton("newLoc", "Submit New Location")
      #submitButton("Submit")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Predicted Selling Price",
                 plotOutput(outputId = "distPlot"),
                 br(),
                 textOutput("pred_price")),
        tabPanel("Nearby Homes", 
                 br(),
                 leafletOutput(outputId = "mapPlotNear"),
                 tableOutput("nearTable")),
        tabPanel("Similar Homes", 
                 br(),
                 actionButton("calcSim", "Find Similar Homes"),
                 br(),
                 br(),
                 leafletOutput(outputId = "mapPlotSim"),
                 br(),
                 tableOutput("simTable")),
        tabPanel("Methodology", 
                 br(),
                 strong("Source Data: "),
                 span("Kaggle's "),
                 a("housing sales in King County", href="https://www.kaggle.com/harlfoxem/housesalesprediction"),
                 span(" dataset. Data dictionary available "),
                 a("here", href="https://rstudio-pubs-static.s3.amazonaws.com/155304_cc51f448116744069664b35e7762999f.html"),
                 span(". "),
                 span("Note that since the dataset only includes home sales between May 2014 and May 2015, we inflate predicted housing prices by a factor of 1.495842 based on the growth in the "),
                 a("S&P/Case Shiller Seattle Housing Price Index", href="https://fred.stlouisfed.org/series/SEXRNSA"),
                 span("between November 2014 (the midpoint of the dataset) and October 2019 (the most recently-published index. Prices shown in tables and maps are actual 2014-2015 prices."),
                 br(),
                 br(),
                 span("For details on the model, please see "),
                 a("https://github.com/ericknudson/kingcounty", href="https://github.com/ericknudson/kingcounty"),
                 span(".")
         )
      )
    )
  )
)

server <- function(input, output) {
  
  #to run our model, we first need to figure out which neighborhood the user-input house is in, since that's an input into our model
  #1) whenever the user adjusts the lat/long, find the 30 closest houses
  NN = reactive({
    getNNgeo(input$lat, input$long)
  })
  
  #2) of those 30 houses, figure out the most frequently-occurring neighborhood
  neighborhood = reactive({
    getNeighborhood(NN())
  })
  
  #now that we know the neighborhood, we can run our model
  model = reactive({
    runModel(input, neighborhood())
  })
  
  #take our model's output and convert it into the content the UI needs: lower and upper confidence intervals, the perentile, etc.
  output$pred_price <- renderText({ 
    model_output = model()
    pred_price = model_output[1]
    
    #calculate 95% confidence interval
    pred_price_lwr = model_output[2] 
    pred_price_upr = model_output[3] 
    
    #calculate percentile for entire dataset and just the zip code
    percentile = round(sum(kingcounty$price < pred_price)/nrow(kingcounty)*100,0)
    zipneighbors = subset(kingcounty, zipcode = input$zipcode)
    zipneighbors_percentile = round(sum(zipneighbors$price < pred_price)/nrow(zipneighbors)*100,0)
    
    #send text to the UI
    paste0("Your home's estimated value is between $", 
           format(round(as.numeric(pred_price_lwr), 0), big.mark=","),
           " and $",
           format(round(as.numeric(pred_price_upr), 0), big.mark=","),
           ", putting it in the ", 
           percentile, 
           ifelse(percentile %% 10 == 1,"st",ifelse(percentile %% 10 == 2,"nd",ifelse(percentile %% 10 == 3,"rd","th"))),
           " percentile of King County and the ",
           zipneighbors_percentile,
           ifelse(zipneighbors_percentile %% 10 == 1,"st",ifelse(zipneighbors_percentile %% 10 == 2,"nd",ifelse(zipneighbors_percentile %% 10 == 3,"rd","th"))),
           " for your zip code")
  })
  
  #plot the histogram of home prices, using a vertical line for the user's estimated home value
  output$distPlot <- renderPlot({
    model_output = model()
    pred_price = model_output[1]
    ggplot(kingcounty,aes(x=price)) + 
      geom_histogram(data=kingcounty,fill = "blue", alpha = 0.5, bins = 200) +
      geom_histogram(data=subset(kingcounty,zipcode == input$zipcode),fill = "blue", alpha = .8, bins = 100) +
      coord_cartesian(xlim = c(0, 2000000)) +
      xlab("Home sale prices for King County (your zip code in dark blue)") +
      geom_vline(xintercept=pred_price, 
                 color = "black", size=1.5) +
      geom_text(aes(x=pred_price + 30000, 
                    label=paste("Your Home: $",format(round(as.numeric(pred_price), 0), big.mark=",")), 
                    y=1500, hjust = 0), 
                colour="black")
  })
  
  #plot nearby homes on a map
  output$mapPlotNear = renderLeaflet({
    n = NN()
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=kingcounty[n,"long"], 
                 lat=kingcounty[n,"lat"], 
                 label=paste0("$",format(round(as.numeric(kingcounty[n,"price"]), 0), big.mark=","))
      )
    m 
  })
  
  #show a table of those nearby homes
  output$nearTable = renderTable({
    neighbors = NN()
    kingcounty[neighbors,c("price",
                           "bedrooms",
                           "bathrooms",
                           "sqft_lot",
                           "sqft_above",
                           "sqft_basement",
                           "floors",
                           "waterfront",
                           "view",
                           "condition",
                           "grade",
                           "age",
                           "age_since_ren_or_build",
                           "lat",
                           "long",
                           "neighborhood")]
  })
  
  #button for calculating most-similar homes (placing behind a button since this is a heavy operation)
  calcSim <- eventReactive(input$calcSim, {
    getKNN(input)
  })
  
  #table of most-similar homes
  output$simTable = renderTable({
    top_neighbors = calcSim()
    kingcounty[top_neighbors,c("price",
                           "bedrooms",
                           "bathrooms",
                           "sqft_lot",
                           "sqft_above",
                           "sqft_basement",
                           "floors",
                           "waterfront",
                           "view",
                           "condition",
                           "grade",
                           "age",
                           "age_since_ren_or_build",
                           "lat",
                           "long",
                           "neighborhood")]
  })
  
  #map of most-similar homes
  output$mapPlotSim = renderLeaflet({
    top_neighbors = calcSim()
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addMarkers(lng=kingcounty[top_neighbors,"long"], 
                 lat=kingcounty[top_neighbors,"lat"], 
                 label=paste0("$",format(round(as.numeric(kingcounty[top_neighbors,"price"]), 0), big.mark=","))
      )
    m 
  })
}
shinyApp(ui = ui, server = server)