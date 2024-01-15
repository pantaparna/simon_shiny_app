library(shiny)
library(ggplot2)
library(dplyr)
library(plyr)
library(ggthemes)
# Read a csv file 
simon <- read.csv("/Users/aparnapant/Aparna/Documents/simon_genome_result.csv", header = T)
# Step;2 Create a ui for visualization
ui <- fluidPage(
  headerPanel(title = "Simon Genome Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sample","Select a Sample",choices = simon$name),
      
      
      selectInput(inputId = "color", label = "Choose Color", choices = c("Red" = "Red", "Blue" = "Blue", "Green" = "Green",
                                                                         selected = "Blue")),
      
      
    ),
    # Create a main pannel where all the tabs will be visualized    
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Data", tableOutput("simonData")),
                  tabPanel("Summary", verbatimTextOutput("summ")),
                  tabPanel("Plot",plotOutput(outputId = "plot"),
                           "Plot",plotOutput(outputId = "plot1")
                           
                  )
                  
      )
    )
    
  ))
# Step-3 Create a severe code to link it with ui for the visual output
server <- function(input, output) {
  output$simonData <- renderTable({
    nameFilter <- subset(simon, simon$name == input$sample)
  })
  
  output$summ <- renderPrint({
    summary(simon)
  })
  # plotting code will be written for each plot sepaately.  
  output$plot <- renderPlot({
    sim<- simon %>% filter(simon$name == input$sample) 
    g <- ggplot(sim, aes(x = number_of_calls,
                         fill = reference_name))
    
    g <- g + geom_histogram(bins=10)
    
    
    
    g <- g + theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(y="Count",x="Number Of Calls",title=paste("Number Of Calls Per Chromosome Histogram",input$sample,sep = " "))
    
    g
    
  })
  output$plot1 <- renderPlot({
    if(input$color == "Red"){
      sColor = "#ff3300"
    } else if(input$color == "Blue"){
      sColor = "#3399ff"
      
    } else if(input$color == "Green"){
      sColor = "#66ff33"
    }
    
    sim<- simon %>% filter(simon$name == input$sample) 
    g <- ggplot(sim, aes(reference_name, titv))
    
    g <- g + geom_bar(stat ="identity", width = .5, fill = sColor)
    
    
    g <- g + theme_bw()+
      theme(axis.title = element_text(size=12,color="BLACK",face="bold"),
            axis.text = element_text(size=14,color="BLACK",face="bold"))+
      labs(y="titv",x="Chromosomes",title=paste("Chromosome vs TITV Ratio",input$sample,sep = " "))
    
    g
    
  })
  
}

shinyApp(ui = ui, server = server)
