library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(lazyeval)

# Refereneces from shinydashboard documentation 
#https://rstudio.github.io/shinydashboard/

# References from plotly documentation 
#https://plotly.com/r/

# Reading 3 the data files 
edu_mf <- read.csv("edu_mf.csv")
force_mf <- read.csv("force_mf.csv")
emp_mf <- read.csv("emp_mf.csv")

# Tking the type of gender in the data files
gender <- c(levels(edu_mf$Sex))
# removing blanks
edu_mf <- edu_mf[!apply(is.na(edu_mf) | edu_mf$Percentage.unemployment == "..", 1, all),]

# aMaking a dummy data set from 1990
force_mf_dummy <- force_mf[force_mf$Time>1990,]

# Ui for the application
ui <- dashboardPage(skin = "blue", # using shiny dashboard
                    dashboardHeader(title = "AUS Men & Women Employment Trend",titleWidth = 410),
                    
                    #side bar contents
                    dashboardSidebar(width= 410,sidebarMenu(menuItem("Introduction"),
             "We all know that Australia as an economy is fluctuating and ",br(),
    "unpredictable from the last few years. ",br(),
"This has been a reason to build an application for knowing how" ,br(),
"Australian Men and Women work to provide themselves a healthy ",br(),
"lifestyle. The application answers the questions ",br(),
"related to Gender and Job Statistics in Australia. ",br(),br(),

"Questions: ",br(),
"  1. What is the unemployment percentage of men and women",br(),
"with various levels of education from the last 27 years? ",br(),
"2. How has labor force participation of men and women ",br(),
  "changed since 1990? ",br(),
"  3. Where do women and men prefer to be employed?",br(),br(),

"Furthermore, an analysis of Labor Force participation with ",br(),
"Unemplyment (with Education) and Employment (in various",br(),
"sectors) can also be done respectively through this application." ,br(),
    
    # Menu item of the tabs in the application
    menuItem("Menu Item"),
    menuItem("Unemployment with Education & Employment Sectors", tabName = "es", icon = icon("bar-chart-o")),
    menuItem("Labor Force Participation", tabName = "lf", icon = icon("bar-chart-o"))
  )
  ),
  
  dashboardBody(
    tabItems(
   
   #1st tab contents   
     tabItem(tabName = "es",
             "*Application will take 3-10 seconds to load the data.",br(),
             
             h2("Unemployment with Education Analysis"),
             
             #1st row of tab 1
             fluidRow(
               
               # Box for 1st 3D graph
               box( collapsible = TRUE, status = "info",
                    solidHeader = TRUE,
                    title="Unemployment within various level of education from 1995 - 2013.",
                    width=7,
                    plotlyOutput("plot1",height=600)),
               
               # Box 2 for information for the graph
               box(collapsible = TRUE, 
                   width = 4,
                   background = "black",
                   solidHeader = TRUE,
                   title="Info Box",
                   "The 3D graph reprensents change in Unemployment within various education levels.",
                   br(),"User can isolate one of the genders and focus.",
                   br(),"Change of unemployment percentage with respective to time can be seen by moving and rotating the 3D graph for a particular
                   Education level.")
             ),
             
             h2("Employment Sector Analysis"),
             
             # 2nd row of tab 1
             fluidRow(
               # Bar garph on 1st tab
               box(
                 collapsible = TRUE,
                 status = "info",
                 solidHeader = TRUE,
                 title="Employment in selected sectors from 1991-2019 for Male and Female.",
                 width=8, 
                 plotlyOutput("plot3",height = 600)),
               
               #3D graph on tab 1
               box(collapsible = TRUE, 
                   title="Info & Input Box ",
                   "The bar graph reprensents change in employment in each sector for men and women.",br(),
                   "User can isolate one of the genders and focus.",br(), 
                   "Change of employment percentage with respective to time can be seen .",
                   br(),br(),"Please select the following.",
                   background = "black",
                   width = 4,
                   selectInput("job_sector","Employment sector:",choices = c(levels(emp_mf$Type))),
                  )
               
             ),
             
             #3rd row on tab 1
             fluidRow(
              
               # 3D chart 
               box(collapsible = TRUE,
                   #background = "aqua",
                   status = "info",
                   solidHeader = TRUE,
                   title="Employment in various sectors from 1991-2019 for selected gender.",
                   width=8,
                   plotlyOutput("plot2",height = 600)),
               
             # Information box for 3D chart
               box(collapsible = TRUE,
                   title="Info & Input Box",
                   "The 3D graph reprensents change in employment for various employment sectors.",
                   br(),"User can isolate one of the employment sectors.",
                   br(),"Change of employment percentage with respective to time can be seen by moving and rotating the 3D graph for a particular
                   Gender at a setor.",br(),br(),
                   "Please select the following.",
                   background = "black",
                   width=4,
                   selectInput("gender","Gender:",choices = gender))
               

             )
            
             ),
     
     # 2nd tab contents
     tabItem(tabName = "lf",
             "*Application will take 3-10 seconds to load the data.",br(),
             
             h2("Labor Force Participation Analysis"),
             
             # 1ST row on tab 2
             fluidRow(
               
               # Animation chart 
               box(collapsible = TRUE,
                   title = "Change in rate of labor force participation from 1990-2015 for Male or Female.",
                   status = "info",
                   solidHeader = TRUE,
                   plotlyOutput("plot4")),
               
               # Info box for animation chart
               box(collapsible = TRUE,
                   title="Info Box",
                   "Please select the 'Play' button on bottom-right corner of the graph to run the animation and see the change 
                   in Labor Force participation.",br(),
                   "Variable specific change can be noticed by isolating the variables.",
                   background = "black",
                   width=4)
               
             ),
             
             h3("Labor Force Participation and Employment Sector Analysis"),
             
             # 2nd row of tab 2
             fluidRow(
               #Dual graph 1 
               box(collapsible = TRUE,
                   title = 'Change in employment at a selected employment sector and labor force participation for a selected gender.',
                   status = "info",
                   solidHeader = TRUE,
                   width=8,
                   plotlyOutput("plot7",height = 500)),
               
               # Info and input box
               box(collapsible = TRUE, 
                   background = "black",
                   width=4,
                   title="Info & Input Box",
                   "The dual graph on the left represents change in Employment for a particular sector along with the change in labor force participation for the selected gender.",br(),
                   "The user can select the interaction options on the top-right corner of the graph to explore the graph further.",br(),
                   br(),
                   "Please select the following.",
                   selectInput("gender_1","Gender:",choices = gender),
                   selectInput("job_sector_1","Employment sector:",choices = c(levels(emp_mf$Type)))
               )
             ),
             
             h3("Labor Force Participation and Unemployment with Education Analysis"),
             # row 3 tab2
             
             fluidRow(
               # Dual graph 2 tab2
               box(collapsible = TRUE,
                   width = 8, title = ' Change in unemployment with education level and labor force participation for a selected gender.',
                   status = "info",
                   solidHeader = TRUE,
                   plotlyOutput("plot8",height=500)),
               
               # info and input box dual graph 2 tab2
               box(collapsible = TRUE,
                   title="Info & Input Box",
                   "The dual graph on the left represents change in unemployment for various education levels along with the change in labor force participation for the selected gender.",br(),
                   "The user can select the interaction options on the top-right corner of the graph to explore the graph further.",br(),
                   br(),
                   width = 4,
                   background="black",
                   "Please select the following.",
                   selectInput("gender_2","Gender:",choices = gender)
               )
             )
     )
     
     
    
    
    )
  )
)


# Server code for the application

server <- function(input, output) {

  # 3D CHART 1 TAB 1
  output$plot1 <- renderPlotly({

    
    edu_mf_in <- edu_mf[edu_mf$Education=="intermediate",]
    edu_mf_ad <- edu_mf[edu_mf$Education=="advanced",]
    edu_mf_ad$Percentage.unemployment.in <- edu_mf_in$Percentage.unemployment
    
    fig <- plot_ly(edu_mf_ad, 
                   x = ~Time, 
                   y = ~Percentage.unemployment, 
                   z = ~Percentage.unemployment.in, 
                   color = ~Sex, 
                   colors = c('pink', 'cyan3'),
                   hoverinfo = "text",
                   text=~paste("Year:",Time," Adavanced Education:",Percentage.unemployment," Intermediate Education:",Percentage.unemployment.in)
                   )
   
    fig <- fig %>% add_lines()
    
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'Years '), 
                                       yaxis = list(title = 'Advanced Education'),
                                       zaxis = list(title = 'Intermediate Education')))
    
    
  })
  
  
  
  
  ## 3D CHART 2 TAB 1
  output$plot2 <- renderPlotly({
    
    fig2 <- plot_ly(emp_mf[emp_mf$Sex==input$gender,], 
                    x = ~Time, 
                    z = ~Percentage, 
                    y = ~Type, 
                    color = ~Type, 
                    sizes = 8,
                    hoverinfo="text",
                    text=~paste("Year:",Time," Sector:",Type," Employment:",Percentage,"%"))
    fig2<- fig2 %>% add_lines()
    # layout of the graph
    fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = 'Years '), 
                                         zaxis = list(title = 'Percentage Employment'),
                                         yaxis = list(title = '')))
    
  })
  
  
  
  
  
  # bar graph tab1 
  
  output$plot3 <- renderPlotly({
    
    fig3 <- plot_ly(emp_mf[emp_mf$Type==input$job_sector,], 
                    x = ~Time, 
                    y = ~Percentage,
                    color = ~Sex, 
                    sizes = 8, 
                    colors = c('pink', 'cyan3'))
    
    fig3<- fig3 %>% add_bars()
    fig3 <- fig3 %>% layout(scene = list(xaxis = list(title = 'Years '), 
                                         yaxis = list(title = 'Percentage Employment')))
    
  })
  
  
  
  
  # animation CHART 1 TAB 2
  #https://plotly.com/r/animations/
  output$plot4 <- renderPlotly({
    
    accumulatess_by <- function(datasa, var) {
      # code for animation using plotly package
      
      varis <- lazyeval::f_eval(var, datasa)
      
      lvlaas <- plotly:::getLevels(varis)
      
      dataas <- lapply(seq_along(lvlaas), function(x) {
        
        cbind(datasa[varis %in% lvlaas[seq(1, x)], ]
              , frame = lvlaas[[x]])
        
      })
      
      dplyr::bind_rows(dataas)
      
    }
    
    testing_da <- force_mf %>% accumulatess_by(~Time)
    
    fig4 <- testing_da %>% plot_ly(
      x = ~Time, 
      y = ~Labour.force.percentage, split = ~Sex, frame= ~frame, type = 'scatter',mode = 'lines', line = list(simplyfy = F)
    )
    
    fig4 <- fig4 %>% layout(xaxis = list(title = "Year"), yaxis = list( title = "Participation (in %)"))
    fig4 <- fig4 %>% animation_opts(frame =100,transition = 0, redraw = FALSE )
    fig4 <- fig4 %>% animation_slider(hide = F)
    fig4 <- fig4 %>% animation_button(x = 1, xanchor = "right", y = 0, yanchor = "bottom" )
  
    })
  
  
  
  
  
# Dual graph one tab 2
  
  output$plot7 <- renderPlotly({
    
    # data transformation needed to form dual graph
    emp_mf_dummy <- emp_mf[emp_mf$Time > 1990 & emp_mf$Time < 2017,]
    force_mf_dummy <- force_mf[force_mf$Time>1990,]
    force_in <- emp_mf_dummy[emp_mf_dummy$Type=="industry",]
    force_in$f_percentage <- force_mf_dummy$Labour.force.percentage
    force_ag <- emp_mf_dummy[emp_mf_dummy$Type=="agriculture",]
    force_ag$f_percentage <- force_mf_dummy$Labour.force.percentage
    force_se <- emp_mf_dummy[emp_mf_dummy$Type=="services",]
    force_se$f_percentage <- force_mf_dummy$Labour.force.percentage
    
    force_k <- rbind(force_ag,force_in,force_se)
    
    fig7 <- plot_ly(force_k[force_k$Sex==input$gender_1 & force_k$Type==input$job_sector_1,])
    
    fig7 <- fig7 %>% add_trace(x = ~Time, y = ~Percentage, type = 'bar', name = 'Employment',
                               marker = list(color = 'yellow'),
                               hoverinfo = "text",
                               text = ~paste(Percentage,'%, ',Time))
    
    fig7 <- fig7 %>% add_trace(x = ~Time, y = ~f_percentage, 
                               type = 'scatter', 
                               mode = 'lines', 
                               name = 'Participation', 
                               yaxis = 'y2',
                               line = list(color = '#45171D'),
                               hoverinfo = "text",
                               text = ~paste(f_percentage,'%, ',Time))
    
    fig7 <- fig7 %>% layout(xaxis = list(title = "Year" ),
                            yaxis = list(side = 'left', 
                                         title = 'Employment (%)', 
                                         showgrid = FALSE, 
                                         zeroline = FALSE ),
                            yaxis2 = list(side = 'right', 
                                          overlaying = "y", 
                                          title = 'Labor force participation (%)', 
                                          showgrid = FALSE, 
                                          zeroline = FALSE))
    
    
  })
  
  
  
  
  
  # Dual graph one tab 2
  
  output$plot8 <- renderPlotly({
    
    edu_mf_dummy <- edu_mf[edu_mf$Time>1999,]
    force_edu_dummy <- force_mf_dummy[force_mf_dummy$Time>1999 & force_mf_dummy$Time<2014,]
    
    edu_mf_dummy$Percentage <- 0
    edu_mf_dummy[edu_mf_dummy$Education=="advanced",]$Percentage <- force_edu_dummy$Labour.force.percentage
    edu_mf_dummy[edu_mf_dummy$Education=="intermediate",]$Percentage <- force_edu_dummy$Labour.force.percentage
    edu_mf_dummy$n_Percentage.unemployment <-as.numeric(as.character(edu_mf_dummy$Percentage.unemployment))
    
    edu_mk_final_a <- edu_mf_dummy[edu_mf_dummy$Education=="advanced",]
    edu_k_final_in <- edu_mf_dummy[edu_mf_dummy$Education=="intermediate",]
    edu_k_final_in$n_Percentage.unemployment_ad <- edu_mk_final_a$n_Percentage.unemployment
  
    edu_mf_dummy2 <- edu_k_final_in[edu_k_final_in$Sex==input$gender_2 ,]
    fig8 <- plot_ly(edu_mf_dummy2)
    
    fig8 <- plot_ly(edu_mf_dummy2,
                    x = ~Time, 
                    y =~n_Percentage.unemployment_ad, 
                    type = 'bar', 
                    name = 'Advanced Education',
                    hoverinfo = "text",
                    text = ~paste(n_Percentage.unemployment_ad, '%, ',Time))
    
    fig8 <- fig8 %>% add_trace( y =~n_Percentage.unemployment, 
                                marker=list(color="green"),
                                name = 'Intermediate Education',
                               hoverinfo = "text",
                               text = ~paste(n_Percentage.unemployment, '%, ',Time))

    
    fig8 <- fig8 %>% add_trace(x = ~Time, 
                               y = ~Percentage, 
                               type = 'scatter', 
                               mode = 'lines', 
                               name = 'Participation Rate', 
                               yaxis = 'y2',
                               line = list(color = '#45171D'),
                               hoverinfo = "text",
                               text = ~paste(Percentage,"%, ",Time))
    
    fig8 <- fig8 %>% layout(
                            xaxis = list(title = "Year"), 
                            barmode = 'stack',
                            yaxis = list(side = 'left', 
                                         title = 'Unemployment (%)', 
                                         showgrid = FALSE, 
                                         zeroline = FALSE),
                            yaxis2 = list(side = 'right', 
                                          overlaying = "y", 
                                          title = 'Labor force participation (%)', 
                                          showgrid = FALSE, zeroline = FALSE))
    
  })

}

# callng the shiny app function

shinyApp(ui, server)