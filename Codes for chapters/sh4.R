library(mosaic)
library(tidyverse)
#colleges <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/colleges2016.csv')

x <- read_csv(file = 'https://raw.githubusercontent.com/gitcnk/Data/master/NBA2016.csv')

mydata <- select_if(x, is.numeric)
nsim <- 1E4

ui <- fluidPage(
  
  titlePanel("Colleges in the US"),
  
  sidebarPanel(width = 3,
    
              helpText('This dataset is from Mike Chapple, Notre Dame University.' ),
               
               
              helpText('The app will take 10,000 samples 
                       and produce the sampling distribution' ),
              
              h6(a('Pick a variable from the list.' )),
              
              h6(a('Use the slider to choose your sample size.' )),  
              
              varSelectInput(inputId = "variable", 
                             label = "Pick a Variable:", mydata),
              
              
              sliderInput(inputId="n",
                          label = "Sample Size n",
                          value = 30,
                          min= 10,
                          max= 500, 
                          step= 50),
              

              
              
              textOutput(outputId = "test2")
              ),
  
  
  sidebarPanel(width = 3,
               helpText('Summary Stats:')),
               textOutput(outputId = "popmean"),
  
  mainPanel(
            fluidRow(plotOutput("Pop", width = '600px', height = '300px' )),
            
            fluidRow(plotOutput('SamDist',width = '600px', height = '330px'))
           )
)

server <- function(input, output) 
{
  pop_mean <- reactive({ mean(mydata[[input$variable]])})
                        
  myn <- reactive({input$n})
  
  output$test1 <- renderText(myn())
  
  output$popmean <- renderText(pop_mean())
  
    

  output$Pop <- renderPlot({
    ggplot(mydata) +
      aes(x = !!input$variable) + 
      geom_histogram() +
      labs(title = 'Distribution of the Selected Variable',
           subtitle = 'Data Source: US Colleges',
           x = 'Variable of interest')
  })
  
      output$SamDist<- renderPlot({
        

        mysamples <- matrix( 0, nrow = nsim, ncol = input$n)
        mysamples <- do(nsim)*sample(mydata[[input$variable]], size = input$n)
        mymeans <- apply(mysamples, MARGIN = 1, FUN = mean)
        
        ggplot() +
          aes( x = mymeans) +
          geom_histogram() +
          labs(title = 'Sampling Distribution of the Sample Mean',
               subtitle = '',
               x = 'sample mean')
      })
#  }
  
  
}

# Create the Shiny app object ---------------------------------------
CLTapp <- shinyApp(ui = ui, server = server)
