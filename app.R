# STEP01: initialize
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(purrr)
library(dplyr)
library(plotly)


# STEP02: example data
data(iris)

# make some factors
# identifying and converting categorical variables
data(mtcars)
uvals <- sapply(mtcars, function(x) {
  length(unique(x))
})
mtcars <- map_if(mtcars, uvals < 4, as.factor) %>%
  as.data.frame()


# STEP03: UI for app

ui <- dashboardPage(
  skin = "red",
  # SeCTION01: Title
  dashboardHeader(title = "Scatter Plot"),

  # SECTION02: Dashboard pages
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
      menuItem("Explanation", tabName = "Explanation", icon = icon("chalkboard-teacher"))
    )
  ),

  # SECTION03: output
  dashboardBody(
    # h3(htmlOutput("caption")),


    # First tab content
    tabItems(
      tabItem(
        tabName = "Explanation",
        h1("Code - Explanation"),
        h3("Step - 01:"),
        h5("Loading all required packages"),
        h3("Step - 02:"),
        h5("Defining sample in-bult data"),
        h3("Step - 03:"),
        h5("Here, UI of the page has been defined. The UI of this dashboard has three sections. All these sections have been explained below."),
        h4("Section - 01:"),
        h5("Defining title of the dashboard"),
        h4("Section - 02:"),
        h5("Sidebar of the dashboard has been defined with two tabs namely, a. Dashboard and b. Explanation"),
        h4("Section - 03:"),
        h5("Defining output body for dashboard and explanation tab. Dashboard tab consists of two boxes, users 
                  can change variables and dataset in one box and other box shows the corresponding scatter plot based on the input set by the users"),
        h3("Step - 04:"),
        h5("Here, in the server part plots are generated using ggplot2 based on provided inputs and converted to advanced plot by calling plotly api"),
        h3("Step - 05:"),
        h5("Producing the dashboard to view")
      ),
      tabItem(
        tabName = "Dashboard",
        fluidRow(
          box(selectInput("dataset", "Data:",
            choices = list(iris = "iris", mtcars = "mtcars"), selected = NULL
          ),
          selectInput("variable", "Variable:", choices = NULL),
          selectInput("group", "Group:", choices = NULL),
          selectInput("plot.type", "Plot Type:", "scatterplot"),
          height = 422
          ),

          box(uiOutput("plot"))
        )
      ) # depends on input
    )
  )
)



# STEP04: shiny server side code for each call
server <- (function(input, output, session) {

  # update group(X- axis) and
  # variables(Y- axis) based on the data
  observe({
    var.opts <- colnames(get(input$dataset))
    updateSelectInput(session, "variable", choices = var.opts)
    updateSelectInput(session, "group", choices = var.opts)
  })



  output$plot <- renderUI({
    plotlyOutput("p")
  })

  # get data object
  get_data <- reactive({
    obj <- list(
      data = get(input$dataset),
      variable = input$variable,
      group = input$group
    )

    return(obj)
  })

  # plotting function using ggplot2
  output$p <- renderPlotly({

    # getting input
    plot.obj <- get_data()



    if (input$plot.type == "scatterplot") { # control for 1D or 2D graphs
      p <- ggplot(
        data = plot.obj$data,
        aes_string(
          x = plot.obj$group,
          y = plot.obj$variable
        )
      ) + geom_point(color = "red", alpha = 0.5)
    }


    p <- p + labs(
      x = input$group,
      y = input$variable
    )

    # using plotly api
    p <- ggplotly(p)

    print(p)
  })
})


# STEP05: Create Shiny dashboard ----
shinyApp(ui, server)
