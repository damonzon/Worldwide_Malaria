library(shiny)
library(shinythemes)
library(data.table)
library(plotly)
#regions<- fread("https://raw.githubusercontent.com/damonzon/Regional_TB_Incidence/master/tb_regions.csv")
regions<- fread("tb_life_malaria.csv")
all_regions <- data.frame(unique(regions$region_code))
colnames(all_regions) <- "region"
all_regions <- arrange(all_regions,region)
all_regions <- all_regions[-c(15), ] 
all_regions <- data.frame(regions=all_regions)

ui <- navbarPage(
  
  titlePanel(title=div(img(src="Mosquito.png",
          width="40px",height="40px"),"Worldwide Malaria")),
  theme = shinytheme("darkly"),
  
  navbarMenu(
    "Videos",
    tabPanel(title = "Epidemiology",
             htmlOutput("video_1a")),
    tabPanel(title = "World Malaria Day",
             htmlOutput("video_1b")),
    tabPanel(title = "Bednet Uses",
             htmlOutput("video_1c")),
    tabPanel(title = "Prevention",
             htmlOutput("video_1d"))
  ),
  
  navbarMenu(
    "Plots",
    tabPanel(
      title = "Barplots: 2016",
      uiOutput("Choose_region"),
      plotlyOutput("plot1", height = 500)),
    
    tabPanel(
      title = "Boxplots: 2016",
      plotlyOutput("plot2", height = 500)),
    
    tabPanel(
      title = "Malaria Trends: 2000-2016",
      plotOutput("plot3")),
    
    tabPanel(
      title = "Population: 2000-2016",
      plotOutput("plot4"))
    
  )
)

server = function(input, output, session) {
  output$Choose_region <- renderUI({
    selectInput("select",
                "Select a Region",
                choices = all_regions,
                selected = all_regions[1])
  })
  
  
  get_data <- reactive({
    region_selected = input$select
    region_data = subset(regions, 
                         year == 2016 &region_code == region_selected)
    region_data  = region_data[, c("Country", 
        "Life_Exp","population", "incidence","malaria")]
    return(region_data)
  })
  
  get_data2 <- reactive({
    region_selected = input$select
    region_data2 = subset(regions, 
          region_code == region_selected)
    region_data2  = region_data2[, c("Country", "year", "population","incidence","malaria","Life_Exp")]
    return(region_data2)
  })
  
  output$Choose2_region <- renderUI({
    selectInput("select",
                "Select a Region",
                choices = all_regions,
                selected = all_regions[1])
  })
  
  get_data <- reactive({
    region_selected = input$select
    region_data = subset(regions, 
       year == 2016 &region_code == region_selected)
    region_data  = region_data[, c("Country", "population", "incidence","malaria", "Life_Exp")]
    return(region_data)
  })
  
  output$video_1a <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                src = "https://www.youtube.com/embed/Y4LsJhpx7J8")
  })
  
  output$video_1b <- renderUI({
    tags$iframe(width = "1000",
                height = "500",
                src="https://www.youtube.com/embed/aIHkLRdL0Uo")
  })
  
  output$video_1c <- renderUI({
    tags$iframe(width = "1000",
      height = "500",
      src = "https://www.youtube.com/embed/dJvooI1Ec9o")
  })
  
  output$video_1d <- renderUI({
    tags$iframe(width = "1000",
    height = "500",
    src = "https://www.youtube.com/embed/INmIcOPKo_M")
  })
  
  
  output$plot1 <- renderPlotly({
    region_data = get_data()
    p <- ggplot(region_data, aes(x=reorder(Country,malaria), 
      y=malaria,country=Country, pop=population,
      Malaria=malaria, Life_Exp = Life_Exp, TB=incidence)) +
      geom_bar(stat='identity', color="black", fill="blue") +
      coord_flip() +
      ggtitle("Population in Millions: Malaria Incidence = Cases per 1000 at risk") +
      xlab("") +
      ylab("\nMalaria Incidence in 2016") +
      theme_bw() 
    ggplotly(p,tooltip = c("country","pop",
            "Malaria","Life_Exp"))
    
  })
  
  output$plot2 <- renderPlotly({
    region_data = get_data()
    p <- ggplot(region_data, aes(x="",y=malaria,
          country=Country,pop=population,
          TB_Incidence=incidence,
          Life_Exp = Life_Exp, Malaria=malaria)) + 
      geom_boxplot() +
      ggtitle("Population in Millions: Malaria Incidence = Cases per 1000") +
      xlab(paste(input$select)) +
      ylab("Malaria Incidence in 2016") +
      theme_bw() +
      geom_jitter(shape=16, position=position_jitter(0.1),color="blue")+
      theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
    ggplotly(p,tooltip = c("country","pop",
              "Malaria","Life_Exp"))
    
  })
  
  output$plot3 <- renderPlot({
    region_data2 = get_data2()
    p <- ggplot(data=region_data2, aes(x=year, y=malaria, Country
    )) +
      geom_line(aes(color = Country)) +
      geom_point(aes(shape = Country)) +
      ggtitle(paste(input$select))+
      xlab("Years 2000-2016") +
      ylab("Malaria Incidence") +
      theme_bw(base_size = 20)
    p
  }) 
  
  output$plot4 <- renderPlot({
    region_data2 = get_data2()
    p <- ggplot(data=region_data2, aes(x=year, y=population, Country
    )) +
      geom_line(aes(color = Country)) +
      geom_point(aes(shape = Country)) +
      ggtitle(paste(input$select),"Population")+
      xlab("Years 2000-2016") +
      ylab("Millions") +
      theme_bw(base_size = 20)
    p
    
  }) 
  
}

shinyApp(ui = ui, server = server)
