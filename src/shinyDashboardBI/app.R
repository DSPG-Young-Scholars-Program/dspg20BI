library(shiny)
library(dplyr)
library(ggplot2)


#Profiling tab
#table of completeness, uniqueness, validity, duplicates
#drop-down menu for each year: show top publishers and companies
#paragraph talking about # of valid vs invalid
#visuals used in our midterm presentation
#mention number of articles per year





#Data Sources and Methods tab
  #data tab
    #list out data sources like fafsa (include logos/images beside each dataset)
    #include url for fda and ndc at the end of their paragraphs
  #Methods
   #Cleaning all three datasets to get corporate families
   #Fuzzy Matching
   #Network analysis

#Results tab
 #against themselves
   #ndc x ndc results
   #fda x fda results
   #dna x dna
 #across
  #across fda x ndc
  #across fda x dna
  #across dna x ndc
 #network analysis


#chart that shows matches across all three







ui <- fluidPage(

  navbarPage("Business Innovation",
             tabPanel("About",style = "margin:45px",
                      h1("SDAD/DSPG", style = "color: #E57200"),
                      p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia.
                        SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research
                        and quantitative methods to inform policy decision-making and evaluation.
                        The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology,
                        political science, policy, health IT, public health, program evaluation, and data science.
                        The SDAD office is located near our nation's capital in Arlington, VA. You can learn more about us at",
                        tags$a(href="https://biocomplexity.virginia.edu/social-decision-analytics.", "https://biocomplexity.virginia.edu/social-decision-analytics."), style = "color:#232D4B"),
                      p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at SDAD. Entering its seventh year, the program engages students from across the country
                        to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today.
                        DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information
                        generated within every community can be leveraged to improve quality of life and inform public policy. ", style = "color:#232D4B"),
                      h2("DSPG20BI Summer Project", style = "color:#E57200"),
                      p("The DSPG20BI team is one of x number of teams within the larger DSPG program tasked with looking into detecting product innovation within non-traditional data sources.
                        Our goal is to find instances of product innovation within the pharmaceutical industry thorugh niche natural-language processessing techniques in an attempt
                        to supplement the current measure of innovation", tags$a(href = "https://www.nsf.gov/statistics/srvyindustry/about/brdis/", "the Business R&D and Innovation Survey (BRDIS)"),"conducted by The National Science Foundation.")
                      ),
             #ui
             navbarMenu("Profiling",
             tabPanel("Tables",style = "margin:20px",
                      fluidRow(column(5,
                                      selectInput("selectTable", "Select", choices = c("Validity", "Profiling Table")),
                                      textOutput("aboutProfiling")

                      ),
                      column(7,
                             #h3("Validity Table", style = "color:#232D4B"),
                             tableOutput("tables")


                      )),


             ),

             tabPanel("Charts", style = "margin:20px",
                      fluidRow(column(5,
                                      selectInput("year", "Year", choices = c(2013, 2014,2015,2016,2017,2018))

                      ))

                      )
),
             #end profiling tab------------------------------------------


             navbarMenu("Data Sources and Methods",
                        tabPanel("Data",
                                 style = "margin-left: 150px;",
                                 style = "margin-top: 20px;",
                                 style = "margin-right: 150px;",
                                 ),


                        tabPanel("Methods",



                        )),#end navbar

             #end Data Sources and Methods tabs-----------------


             navbarMenu("Results",
                        tabPanel("Within Data Matching",
                                 style = "margin-left: 150px;",
                                 style = "margin-top: 20px;",
                                 style = "margin-right: 150px;",
                        ),


                        tabPanel("Across Data Matching",



                        ))

             #end Results tab

      ) #end navbarPage
  )#end fluid page





server <- function(input, output) {
  output$tables <- renderTable({
    if(input$selectTable == "Validity"){


      valid <- read.csv("validitytable.csv")
      names(valid)[names(valid) == "X"] <- "Column Name"

      valid
    }else{
      profTable <- read.csv("profilingTable.csv")

      names(profTable)[names(profTable) == "X"] <- "Column Name"

      profTable
    }

  })

  output$aboutProfiling <- renderText({
    if(input$selectTable == "Validity"){
      print("Definitions:")
    }else{
      print("Definitions:")
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)




