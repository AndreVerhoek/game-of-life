library(shiny)
library(DT)

# Simple UI to display the grid, buttons and rules
ui <- fluidPage(
  tags$head(tags$style(".rightAlign{float:right;}")),
  fluidRow(
    column(2,
      uiOutput("Conway"),
      htmlOutput("info"),
      sliderInput("saturation",label = "Saturation of generation 0", min=0, max=0.5, value = 0.15, width = '80%'),
      sliderInput("gridsize",label = "Grid size", min=10,max = 100, value = 100, width = '80%')
    ),
    column(7, 
      htmlOutput("GameOfLife"),
      actionButton("newGen", "Get new population",width = '49%'),
      actionButton("refreshLog", "Get next generation",width = '49%'),
      htmlOutput("nr_alive"),
      imageOutput("generationImage")
    ),
    column(3,
      htmlOutput("cellularautomation"),
      uiOutput("rules")
    )
  )
)

# Creating the next generation
createNextGeneration <- function(df){
  df2 <- df
  resurrection <- 0
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)){
      # Calculating the number of neighbours (sum of the subset of all cells max 1 step away), while subtracting the cell itself
      neighbours <- sum(df[max(i-1,1):min(i+1,nrow(df)),max(j-1,1):min(j+1,ncol(df))])-1
      if(df[i,j] == 1){
        df2[i,j] <- ifelse(neighbours < 2,0,ifelse(neighbours < 4,1,0))
      } else{
        df2[i,j] <- ifelse(neighbours == 2,1,0)
        resurrection <- resurrection + ifelse(neighbours == 2,1,0)
      }
    }
  }
  return(df2)
}

getUpdatedPicture <- function(input, output,values, df){
  a <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')
    
    # Generate the PNG
    png(outfile, width = 800, height = 800)
    image(df)
    dev.off()
    temp <- image_read(outfile)
    temp <- image_crop(temp, geometry = "725x690+50+45")
    image_write(temp,outfile)
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = '100%',
         height = '100%',
         alt = paste("The Game of Life is devised by the mathematician John Horton Conway in 1970"))
  }, deleteFile = TRUE)
  output$generationImage <- a
  output$nr_alive <- renderText({paste("<p align='center'>","<br/>Generation",values$genCounter,"has a population of",sum(df),"</p>")})
}

createNewPopulation <- function(input){
  saturation <- input$saturation
  gridsize <- input$gridsize
  newPopulation <- matrix(ifelse(runif(gridsize*gridsize)<saturation,1,0),ncol=gridsize)
  return(newPopulation)
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues()
  values$saturation <- 0.3
  values$gridsize <- 100
  values$df <- matrix(ifelse(runif(100*100)<0.15,1,0),ncol=100)
  values$genCounter <- 0
  output$image_df <- getUpdatedPicture(input, output,values, values$df)
  
  ## Update generation
  observeEvent(input$refreshLog, {
    values$df <- createNextGeneration(values$df)
    values$genCounter <- values$genCounter+1
    output$image_df <- getUpdatedPicture(input, output,values, values$df)
  })
  ## Renew population 
  observeEvent(input$newGen, {
    values$df <- createNewPopulation(input)
    values$genCounter <- 0
    output$image_df <- getUpdatedPicture(input, output,values, values$df)
  })
  output$GameOfLife <- renderText({
    "<p align='center'> <b>Create a new population or evolve to the next generation</b></p>"
  })
  
  output$info <- renderText({
    "<br/> <p align='justify'> 
    Mathematician John Horton Conway devised the rules for The Game Of Life in 1970, a cellular automaton. </p><br/> <br/>"
  })
  output$Conway <- renderUI({
    tags$img(src="john-conway-696x452.jpg", height = "150px",alt="Mathematician John Conway")
  })
  output$rules <- renderUI({
    tags$img(src="rules.jpg", height = "300px")
  })
  output$cellularautomation <- renderText({
    "<br/>
    <b>Cellular automaton</b> <br/>
    <p align='justify'> 
    A cellular automaton is a discrete model studied in fields varying from mathematics, physics and theoretical biology. 
    It consists out of a grid of cells, which are either dead or alive (their state). In the picture brown is alive, yellow is dead.
    Each cell has a neighbourhood, consisting of the surrounding cells. In the first generation (G0) the state is assigned randomly, 
    each following generation follows a set of fixed rules (usually a mathematical function).
    <br/>
    Cellular automata can be used to study patterns regard artificial life, chaos, emergence, fractals, 
    nonlinear dynamics, and self-organization. This touches many scientific areas, varying from biology to mathematics:
    Plants for example regulate their intake and losses of gasses via a cellular automaton mechanism, whereas in physics cellular automata are used
    to model fluid dynamics.
    </p>"
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

