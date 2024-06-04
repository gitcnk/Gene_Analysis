library(mosaic)
library(tidyverse)

nsim <- 1E4

ui <- fluidPage(
  
  titlePanel("Bootstrap Method to Estimate Standard Error of xbar"),
  
  sidebarPanel(width = 3,
               
               h5(a('This App will do the following:')), # Level (size) 5 heading
               
           
               helpText('Step 1: Take a random sample of size n from 
                         the service time population.  The top left plot represent
                        the population, the top right plot represent the sample'),
               
               helpText('Step 2: Take 10,000 bootstrap samples from this sample.'),
               
               helpText('Step 3: Produce the bootstrap sampling distribution of xbar and its standard error (bottom plot).' ),
               
               br(),
               
               
               h5(a('Use the slider to choose your sample size.' )),  
               
               hr(style="border-color: black;"), # Horizontal line
               
               br(), # add blank line (line break)
               
               
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
               
               textOutput(outputId = 'BootSamDist_center'),
               textOutput(outputId = 'BootSamDist_SD')
               
               
               
  ),
  
  
  
  
  
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"),
             plotOutput("Pop", width = '400px', height = '300px' ),
             plotOutput("Sam", width = '400px', height = '300px' ))),
    
    br(), # add blank line (line break)
    br(),
    br(),
    fluidRow(plotOutput('BootSamDist',width = '600px', height = '330px'))
  )
)

server <- function(input, output) 
{
  
  # Global and reactive variables
  
  pop_mean <- 20
  pop_SD <- 20
  
  BootSamDist_data <- reactive({
    
                          mysample <- rexp(n = input$n , rate = 1/20)
    
                          my_boot_samples <- matrix( 0, nrow = nsim, ncol = input$n)
                          my_boot_samples <- do(nsim)*sample(mysample, size = input$n, replace = TRUE)
                          my_boot_means <- apply(my_boot_samples, MARGIN = 1, FUN = mean)
                          
                          return(data.frame(my_boot_means))
  })
  
  
  
  # Outputs
  
  output$popmean <- renderText(paste('population mean ', 
                                     round(pop_mean, 2), sep = '= '))
  
  output$popSD <- renderText(paste('population SD ', 
                                   round(pop_SD,2), sep = '= '))
  
  output$BootSamDist_center <- renderText({
    paste('TRUE standard error (SE) ',
          round(pop_SD/sqrt(input$n),2), sep = '= ')}
  )

  output$BootSamDist_SD <- renderText({
    temp <- BootSamDist_data()
    paste('Bootstrap Estimate of SE ',
          round(sd(temp$my_boot_means),2), sep = '= ')}
  )

  
  output$Pop <- renderPlot({
                    set.seed(1234)
                  
                    N <- 1E3
                    mypop <- rexp(N, rate = 1/20)
                    
                    p1 <- ggplot() + aes(x = mypop) + geom_density() + 
                      theme(axis.text.y = element_blank()) +
                      labs(title = 'A Typical Service Time Distribution',
                           subtitle = 'Average Service time 20 minutes',
                           x = 'Service Time',
                           y = '')
                    
                    p1
     })
  
  
  
  output$Sam <- renderPlot({
                      mysample <- rexp(input$n, rate = 1/20)
                      
                      p2 <- ggplot() + 
                                aes(x = mysample) + 
                                geom_histogram(bins = 20) + 
                                theme(axis.text.y = element_blank()) +
                                labs(title = 'A Random Sample',
                                     subtitle = paste('Sample size = ', input$n),
                                     x = 'Service Time',
                                     y = '')
                              
                      p2
  })
  
  
  
  
  output$BootSamDist<- renderPlot({
    
    ggplot(BootSamDist_data()) +
      aes( x = my_boot_means) +
      geom_histogram(bins = 40) +
      labs(title = 'Bootstrap Sampling Distribution of the Sample Mean',
           subtitle = '',
           x = 'bootstrap sample mean') 
  })
  
  
}

# Create the Shiny app object ---------------------------------------
CLT_colleges_app <- shinyApp(ui = ui, server = server)
