library(shiny)
library(DT)
nr_cols <- 100
df <- matrix(ifelse(runif(nr_cols*nr_cols)<0.50,1,0),ncol=nr_cols)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(8,
    actionButton("refreshLog", "Get next generation"),actionButton("newGen", "Get new population"),
    textOutput("nr_alive"), 
    imageOutput("test")),
    column(4,htmlOutput("blankspace"),uiOutput("rules"))
  )
)

updateDF <- function(df){
  df2 <- df
  resurrection <- 0
  for(i in 1:nrow(df)){
    for(j in 1:ncol(df)){
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
    png(outfile, width = 800, height = 500)
    image(df)
    dev.off()
    
    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 800,
         height = 800,
         alt = "This is alternate text")    
  }, deleteFile = TRUE)
  output$test <- a
  output$nr_alive <- renderText({paste("Generation",values$genCounter,"has a population of",sum(df))})
}


# Define server logic required to draw a histogram
server <- function(input, output) {
  values <- reactiveValues()
  values$df <- df
  values$genCounter <- 0
  
  output$image_df <- getUpdatedPicture(input, output,values, values$df)
  
  
  ## Update generation
  observeEvent(input$refreshLog, {
    values$df <- updateDF(values$df)
    values$genCounter <- values$genCounter+1
    output$image_df <- getUpdatedPicture(input, output,values, values$df)
  })
  ## Renew population 
  observeEvent(input$newGen, {
    values$df <- matrix(ifelse(runif(nr_cols*nr_cols)<0.50,1,0),ncol=nr_cols)
    values$genCounter <- 0
    output$image_df <- getUpdatedPicture(input, output,values, values$df)
  })
  output$blankspace <- renderText({
    "<br/><br/><br/><br/><br/><br/><br/><br/>"
  })
  output$rules <- renderUI({
    tags$img(src="rules.jpg", height = "300px")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

