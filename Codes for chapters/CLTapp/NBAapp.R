library(mosaic)
library(tidyverse)

x <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/NBA2016.csv')

mydata <- select_if(x, is.numeric)
nsim <- 1E4

ui <- fluidPage(
  
  titlePanel("NBA Player Salaries 2016"),
  
  sidebarPanel(width = 3,
               
               h5(a('This dataset is from ESPN.')), # Level (size) 5 heading
               
               
               helpText('The app will take 10,000 samples 
                       and produce the sampling distribution' ),
               
               br(),
               
               h5(a('Pick a variable from the list.' )),
               
               h5(a('Use the slider to choose your sample size.' )),  
               
               hr(style="border-color: black;"), # Horizontal line
               
               br(), # add blank line (line break)
               
               varSelectInput(inputId = "variable", 
                              label = "Pick a Variable:", mydata),
               
               
               sliderInput(inputId="n",
                           label = "Sample Size n",
                           value = 30,
                           min= 10,
                           max= 300, 
                           step= 10),
               
                hr(style="border-color: black;"), # Horizontal line
               
                br(), # add blank line (line break)
               
                h5(a('Summary Stats:')),
                   
                textOutput(outputId = "popmean", inline = TRUE),
                textOutput(outputId = 'popSD'),
                
                br(),
               
                textOutput(outputId = 'SamDist_center'),
                textOutput(outputId = 'SamDist_SD')
               
               
              
  ),
  
  
  
  
  
  mainPanel(
    fluidRow(plotOutput("Pop", width = '600px', height = '300px' )),
    
    fluidRow(plotOutput('SamDist',width = '600px', height = '330px'))
  )
)

server <- function(input, output) 
{
  
  # Global and reactive variables
  
  pop_mean <- reactive({ mean(mydata[[input$variable]])})
  pop_SD <- reactive({ sd(mydata[[input$variable]])})
  
  SamDist_data <- reactive({
                      mysamples <- matrix( 0, nrow = nsim, ncol = input$n)
                      mysamples <- do(nsim)*sample(mydata[[input$variable]], size = input$n)
                      mymeans <- apply(mysamples, MARGIN = 1, FUN = mean)
                      
                      return(data.frame(mymeans))
                          })
  
  
  
  # Outputs
  
  output$popmean <- renderText(paste('population mean ', 
                                     round(pop_mean(), 2), sep = '= '))
  
  output$popSD <- renderText(paste('population SD ', 
                                   round(pop_SD(),2), sep = '= '))
  
  output$SamDist_center <- renderText({
                                  temp <- SamDist_data()
                                  paste('Center of the sampling distribution ', 
                                            round(mean(temp$mymeans),2), sep = '= ')}
                                  )
    
  output$SamDist_SD <- renderText({
    temp <- SamDist_data()
    paste('Spread of the sampling distribution ', 
          round(sd(temp$mymeans),2), sep = '= ')}
  )
  
    
  output$Pop <- renderPlot({
    ggplot(mydata) +
      aes(x = !!input$variable) + 
      geom_histogram() +
      labs(title = 'Distribution of the Selected Variable',
           subtitle = 'Data Source: ESPN')
  })
  
  
  
  
  
  output$SamDist<- renderPlot({
    
    
    #mysamples <- matrix( 0, nrow = nsim, ncol = input$n)
    #mysamples <- do(nsim)*sample(mydata[[input$variable]], size = input$n)
    #mymeans <- apply(mysamples, MARGIN = 1, FUN = mean)
    
    #SampDist_center <- mean(mymeans)
    #SampDist_SD <- sd(mymeans)
    
    ggplot(SamDist_data()) +
      aes( x = mymeans) +
      geom_histogram(bins = 40) +
      labs(title = 'Sampling Distribution of the Sample Mean',
           subtitle = '',
           x = 'sample mean') +
      geom_vline(xintercept = pop_mean(), col = 'red', linetype = 'dashed')
  })
  #  }
  
  
}

# Create the Shiny app object ---------------------------------------
CLTapp <- shinyApp(ui = ui, server = server)
