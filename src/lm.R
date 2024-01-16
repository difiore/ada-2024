library(shiny)
library(DT)
library(tidyverse)
f <- "https://raw.githubusercontent.com/difiore/ADA-datasets/master/zombies.csv"
z <- read_csv(f, col_names = TRUE)
z <- select(z, height, weight, age, gender, major)
z$gender <- factor(z$gender)
z$major <- factor(z$major)
r <- c("height", "weight", "age")
p <- names(z)
# Define the UI ----
ui <- fluidPage(
  titlePanel(h1("Simple LM Visualizer")),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "response",
        label = "Choose a response variable...",
        choices = c("", r)
        # NOTE: the "" is needed to allow NO VARIABLE to be the default value
      ),
      br(),
      selectInput(
        "predictors",
        label = "Choose one or more predictor variables...",
        choices = p,
        multiple = TRUE
      ),
      br(),
      textOutput("model"),
      br(),
      tableOutput("modelresults"),
      style="text-align:center"
    ),
    mainPanel(
      dataTableOutput("datatable"),
      plotOutput("plot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {

  output$datatable <-
    renderDataTable(z, options = list(
      paging = TRUE,
      lengthMenu = list(c(5, 10, 25, -1), c('5', '10', '25', 'All')),
      pageLength = 5
    ))
  
  m <- reactive({
    mod <- NULL
    if (input$response == "" |
        length(input$predictors) == 0) {
      return(mod)
    }
    mod <- paste0(input$response, " ~ ", input$predictors[1])
    if (length(input$predictors) > 1) {
      for (i in 2:length(input$predictors)) {
        mod <- paste0(mod, " + ", input$predictors[i])
      }
    }
    return(mod)
  })
  
  output$model <- renderText({paste0("Model: ",print(m()))})
  
  output$modelresults <- renderTable({
    if (!is.null(m())) {
      res <- lm(data = z, formula = m())
      res <- as.data.frame(coefficients(res))
      names(res) <- "Beta"
      res
    }
  }, width = "100%", rownames = TRUE, striped = TRUE, spacing = "s", bordered =
    TRUE, align = "c", digits = 3)

  output$plot <- renderPlot({
    if (!is.null(m()) & length(input$predictors) == 1) {
      y <- input$response
      x <- input$predictors
      if (class(z[[x]]) != "factor") {
        p <- ggplot(data = z, aes(x = z[[x]], y = z[[y]])) +
          geom_point() +
          geom_smooth(method = lm)
      } else {
        p <- ggplot(data = z, aes(x = z[[x]], y = z[[y]])) +
          geom_boxplot()
      }
      p <- p + xlab(x) + ylab(y) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      p
    } else if (!is.null(m()) & length(input$predictors) == 2) {
      y <- input$response
      x <- input$predictors
      if (class(z[[x[1]]]) == "factor" & class(z[[x[2]]]) == "factor") {
        p <- ggplot(data = z, aes(x = z[[x[1]]], y = z[[y]])) +
          geom_boxplot() +
          facet_wrap(~ z[[x[2]]])
        p <- p + xlab(x[1]) + ylab(y)
      } else if (class(z[[x[1]]]) != "factor" & class(z[[x[2]]]) == "factor"){
        p <- ggplot(data = z, aes(x = z[[x[1]]], y = z[[y]])) +
          geom_point() +
          geom_smooth(method = lm) +
          facet_wrap(~ z[[x[2]]])
        p <- p + xlab(x[1]) + ylab(y)
      } else if (class(z[[x[1]]]) == "factor" & class(z[[x[2]]]) != "factor"){
        p <- ggplot(data = z, aes(x = z[[x[2]]], y = z[[y]])) +
          geom_point() +
          geom_smooth(method = lm) +
          facet_wrap(~ z[[x[1]]])
        p <- p + xlab(x[2]) + ylab(y)
      } else {
        p <- NULL
      }
      p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
      p
    }
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)
