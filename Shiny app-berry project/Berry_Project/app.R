library(shiny)
library(tidyverse)
library(shinythemes)

df <- read.csv(file="data/stberry unit.csv",header=T)
line_color <- c("#33FF66","#FFFF00","#FF9933","#000099",
           "#99FFFF","#99CCCC","#0066CC","#6699CC","#9999FF",
           "#6666FF","#FFCC00","#3366CC","#FF99CC")

# Define UI for application that draws a line plot
ui <- fluidPage(
    
    theme = shinytheme("flatly"),

    # Application title
    titlePanel("Measurments for the Value of Strawberry"),

    sidebarLayout(
        sidebarPanel(
            helpText("Create line chart of Mean Value of each Measurements v.s. Year"),
            
            fluidRow(
                       selectInput("Units",
                                   "Unit: ",
                                   c("",
                                     unique(as.character(df$Unit)))),
                
            ),
        ),

        # Show the line plot 
        mainPanel(
           plotOutput("linePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$linePlot <- renderPlot({
        df_measur <- df %>% 
            filter(Unit==input$Units) %>% 
            group_by(States,Years) %>%
            summarise(Mean_Value=mean(Values))
        
        ggplot(data=df_measur,mapping=aes(x=Years,y=Mean_Value,color=States))+
            geom_line()+
            geom_point(aes(shape=States))+
            xlab("Year") + ylab("Average")+
            scale_color_manual(values = line_color)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
