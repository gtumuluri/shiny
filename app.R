library(shiny)
library(ggplot2)
library(grid)
library(dplyr)
library(scales)
library(extrafont)

options(warn=-1)

ui <- fluidPage(
  navbarPage('Shiny Application',
             tabPanel('Main',
                      titlePanel('Monthly Quantity Up To Selected Year'),
                      sidebarLayout(
                        sidebarPanel(selectInput('year', 'Select Ending Year',
                                                 choices = c('2010', '2011', '2012',
                                                             '2013', '2014', '2015'),
                                                 selected = '2010')),
                        mainPanel(plotOutput('bar'))
                        )
                      ),
             tabPanel('Documentation',
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(h3('Uage Guide')),
                          mainPanel(p('This app provides monthly volume of a quantity\
                                      cumulatively up to the year selected from the\
                                      drop down. To use the app, go to the \'Main Page\'\
                                      and then select the desired year in the drop down.\
                                      You will see a bar plot that will display the data\
                                      in monthly values for the years in the range of 2010\
                                      and the selected year.'))
                        )
                      )
                    )
             )
)

server <- function(input, output) {
  monthly_data <- read.csv('shiny_example.csv')
  monthly_data$date <- as.Date(monthly_data$date)
  
  output$bar <- renderPlot({
    selected_data <- filter(monthly_data, year <= input$year)
    ggplot(selected_data, aes(x = date, y = count)) +
      geom_bar(stat = 'identity', size = 0.01, fill = I('sienna1')) +
      theme_classic() +
      ggtitle("Some Number by Month") +
      xlab("Month and Year") +
      theme(plot.margin = unit(c(2, 2, 2, 2), 'cm'),
            title = element_text(size = 14, color = I('grey20'),
                                 family = 'Helvetica', face = 'bold', vjust = 2),
            axis.title.y = element_blank(),
            axis.title.x = element_text(vjust = -2),
            axis.line = element_line(size = 0.5, color = I('grey50'))) +
      scale_x_date(limits = c(min(monthly_data$date), max(monthly_data$date)),
                              labels = date_format('%Y-%m'), breaks = ('12 months')) +
      scale_y_continuous(labels = comma,
                         limits = c(0, 500000),
                         breaks = seq(0, 500000, 100000))
  })
}

shinyApp(ui = ui, server = server)
