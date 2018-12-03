# load the preprocessed and labelled data
load(paste("../",image_analysis,sep=""))

library(shiny)
library(leaflet)
library(dplyr)

###### Functions to implement user selection

#==================================================================
do_inclusive_list_selection <- function(mylist,the_data)
{

  # select entries by summing up the list of columns in mylist
  # entries selection must have sum_of_columns > 0

  selection <- rep(TRUE,nrow(the_data))

  if(length(mylist) > 0) {
    if(length(mylist) == 1) {
      selection <- selection & (the_data[,mylist[1]] > 0)
    }
    else {
      selection <- selection & (rowSums(the_data[,mylist]) > 0)
    }
  }

  return(selection)

}
#==================================================================
do_exclusive_list_selection <- function(mylist,the_data)
{

  # select entries by multiplying the list of columns in mylist
  # entries selection must have prod_of_columns > 0

  selection <- rep(TRUE,nrow(the_data))

  if(length(mylist) > 0) {
    for(i in 1:length(mylist)) {
      selection <- selection & (the_data[,mylist[i]] > 0)
    }
  }

  return(selection)

}
#==================================================================
do_colvalues_selection <- function(mylist,variable)
{

  # selection entries
  #if(length(mylist) == 0) {
  if(is.null(mylist)) {
    return(rep(FALSE,length(variable)))
  }
  else {
    return(variable %in% mylist)
  }

}
#==================================================================
do_range_selection <- function(myrange,variable)
{

  # select entries from a range in a variable

  return((variable >= myrange[1]) & (variable <= myrange[2]))

}
#==================================================================

# the data
the_data <- labelled.df.geo

# controid for map coordinates
long_m <- 0.5*sum(range(labelled.df.geo$long))
lat_m  <- 0.5*sum(range(labelled.df.geo$lat))

# sources names
source.names            <- as.character(unique(labelled.df.geo$source))

# sentiment names
sentiment.names         <- c("negative","neutral","positive")

# allergen enquiries names
allergy_enquiries.name  <- c("allergy_enquiries")

# food labelling names
food_labelling.name     <- c("food_labelling")

# reporting reactions names
reporting_reaction.name <- as.character(unique(labelled.df.geo$reactions_report))

# Get the time range from the data
time_range_moth      <- range(labelled.df.geo$Month)
time_range_moth_init <- c(time_range_moth[1],time_range_moth[1] + 1)
if(time_range_moth_init[2] > time_range_moth[2]) time_range_moth_init[2] <- time_range_moth[2]

ui <- fluidPage(
  # Creation of a page with a Tab
  navbarPage("FSA Dashboard",
             # First tab of the page
             tabPanel("Map",
                      sidebarPanel(
                        # 14 allergens listing
                        selectInput("fourteen", "14 Allergens", fourteen.allergen.names, selected = vector(),
                                    multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

                        # other allergens listing
                        selectInput("other", "Other Allergens", other.allergen.names, selected = vector(),
                                    multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

                        # Sources listing
                        selectInput("source", "Sources", source.names, selected = source.names,
                                    multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

                        # Sentiment listing
                        selectInput("sentiment", "Sentiment", sentiment.names, selected = sentiment.names,
                                    multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

                        # Allergy enquiries
                        selectInput(allergy_enquiries.name, "Allergy enquiries", allergy_enquiries.name, selected = vector(),
                                    multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

                        # Food labelling
                        selectInput("food_labelling", "Food labelling", food_labelling.name, selected = vector(),
                                    multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

                        # Reporting reactions
                        selectInput("reporting_reaction", "Reporting reactions", reporting_reaction.name, selected = reporting_reaction.name,
                                    multiple = TRUE,selectize = TRUE, width = NULL, size = NULL),

                        # Normalization
                        radioButtons("norm", "Normalization", choices = list("Raw","N. Businesses","Population"), selected = "Raw"),

                        # Time range slider
                        sliderInput("timerange", "Time interval",
                                    min   = time_range_moth[1],
                                    max   = time_range_moth[2],
                                    value = time_range_moth_init)
                      ),
                      mainPanel(
                        leafletOutput("map", height = 1000)
                      )
             ),
             # Second tab to display raw data
             tabPanel("Data",
                    tags$div(class = "container",
                      # adding a row
                      fluidRow(
                        column(3,
                               # 14 allergens listing
                               selectInput("fourteen2", "14 Allergens", fourteen.allergen.names, selected = vector(),
                                           multiple = TRUE,selectize = TRUE, width = NULL, size = NULL)
                        ),
                        column(3,
                               # other allergens listing
                               selectInput("other2", "Other Allergens", other.allergen.names, selected = vector(),
                                           multiple = TRUE,selectize = TRUE, width = NULL, size = NULL)
                        ),
                        column(3,
                               # Sentiment listing
                               selectInput("sentiment2", "Sentiment", sentiment.names, selected = sentiment.names,
                                           multiple = TRUE,selectize = TRUE, width = NULL, size = NULL)
                        ),
                        column(3,
                               # Sources listing
                               selectInput("source2", "Sources", source.names, selected = c("Twitter"),
                                           multiple = TRUE,selectize = TRUE, width = NULL, size = NULL)
                        )
                      ),
                      fluidRow(
                        # diplaying the table with data
                        dataTableOutput("raw_data")
                      )
                    )
             )
  )
)



server <- function(input, output) {

  # Create the map
  output$map <- renderLeaflet({
    leaflet()     %>%
      addTiles()  %>%
      setView(lng = long_m, lat = lat_m, zoom = 6)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  dataInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(the_data[FALSE,])

    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east,  bounds$west)

    subset(the_data,
           lat  >= latRng[1] & lat  <= latRng[2] &
             long >= lngRng[1] & long <= lngRng[2])
  })



  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    selection <- rep(TRUE,nrow(the_data))

    # Allergens
    # 14 allergens selection
    selection <- selection & do_inclusive_list_selection(input$fourteen,the_data)
    # other allergens selection
    selection <- selection & do_inclusive_list_selection(input$other,the_data)

    # Stream 1 issues
    # Allergy enquiries
    selection <- selection & do_inclusive_list_selection(input$allergy_enquiries,the_data)
    # Food labelling
    selection <- selection & do_inclusive_list_selection(input$food_labelling,the_data)
    # Reporting reactions
    selection <- selection & do_colvalues_selection(input$reporting_reaction,the_data$reactions_report)

    # source selection
    selection <- selection & do_colvalues_selection(input$source,the_data$source)

    # sentiment selection
    selection <- selection & do_colvalues_selection(input$sentiment,the_data$sentiment_class)

    # time selection
    selection <- selection & do_range_selection(input$timerange,the_data$Month)

    # organize the data grouped by District name
    summary_data <- the_data[selection,] %>%
      group_by(long, lat, District) %>%
      summarise(count = n())

    # Normalization variables
    summary_data$Norm     <- rep(1,nrow(summary_data))  # raw numbers ==> no Normalization
    summary_data$District <- as.character(summary_data$District)

    mytitle <- "Raw mentions"

    if(input$norm == "N. Businesses") {
      # set up Normalization by number of businesses if requires by the user
      mytitle <- paste("mentions per ",norm_factor_businesses," Establishments",sep="")

      restaurants_per_local_authority.df$District <- as.character(restaurants_per_local_authority.df$District)
      summary_data      <- left_join(summary_data,restaurants_per_local_authority.df[,c("District","TotalEstablishments")], by="District")
      summary_data$Norm <- summary_data$TotalEstablishments/norm_factor_businesses
    } else if(input$norm == "Population") {
      # set up Normalization by population if requires by the user
      mytitle <- paste("mentions per ",norm_factor_population/1000,"k people",sep="")

      population_per_local_authority.df$District <- as.character(population_per_local_authority.df$District)
      summary_data      <- left_join(summary_data,population_per_local_authority.df[,c("District","all_ages")],by="District")
      summary_data$Norm <- summary_data$all_ages/norm_factor_population
    }


    radius        <- summary_data$count/summary_data$Norm
    data.in.label <- summary_data$count/summary_data$Norm

    # color pallete for scale in the map
    colorData <- radius
    pal       <- colorNumeric("viridis", NULL)

    # points radii
    radius    <- 2500

    summary_data <- summary_data[summary_data$count > 0,]

    # plots the summary information per local authority
    leafletProxy("map", data = summary_data) %>%
      clearShapes() %>%
      clearMarkers() %>%

      addCircleMarkers(~long, ~lat,radius = 5,color = pal(colorData),stroke = TRUE, fillOpacity = 0.5,
                       label = paste(as.character(summary_data$District)," : ",as.character(round(data.in.label,4))),
                       labelOptions = labelOptions( direction = "bottom",style = list("font-size" = "14px",
                                                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                "border-color" = "rgba(0,0,0,0.5)"))) %>%

      addLegend("topright", pal=pal, values=~data.in.label, title=mytitle,layerId="colorLegend")


    # Creation of an vector with TRUE values
    selection.data <- rep(TRUE,nrow(the_data))

    # 14 Allergen selection
    selection.data <- selection.data & do_inclusive_list_selection(input$fourteen2,the_data)
    # Other Allergen selection
    selection.data <- selection.data & do_inclusive_list_selection(input$other2,the_data)
    # Sentiment Selection
    selection.data <- selection.data & do_colvalues_selection(input$sentiment2,the_data$sentiment_class)

    # source selection
    selection.data <- selection.data & do_colvalues_selection(input$source2,the_data$source)

    # Creation of a df with the selection
    ui.df <- the_data[selection.data,c("sentiment_class","source", "original_content","hashtags","District","lat","long")]
    # Renaming the columns more User friendly
    names(ui.df) <- c("Sentiment","Source","Content","Hashtags","District","Latitude","Longitude")
    output$raw_data <- renderDataTable(ui.df)

  })




}

shinyApp(ui = ui, server = server)



#
