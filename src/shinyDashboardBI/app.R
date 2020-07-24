library(shiny)
library(dplyr)
library(ggplot2)


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
             tabPanel("Profiling",style = "margin:20px",
                      h2("Lorem Ipsum"),
                      tableOutput("profile"),
                      h3("Lorem Ipsum")

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
  output$profile <- renderTable({
    x <- read.csv("companyfrequencyandname.csv")
    head(x)
  })

}

# Run the application
shinyApp(ui = ui, server = server)




