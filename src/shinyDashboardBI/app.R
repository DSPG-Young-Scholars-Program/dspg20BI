library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)

#testing


#Notes
#dateline row in the profiling table is the only one that doesn't add up.

#Profiling tab

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


   #dna x dna
 #across


  #across dna x ndc
 #network analysis


#chart that shows matches across all three


#to do 7/26

   #double check both charts




#Finish writing as much as you can about the data sources




ui <- fluidPage(
  theme ="themes.css",

  navbarPage(title = span("Business Innovation", style = "color:#232D4B"),
             tabPanel("About",style = "margin:45px",
                      tags$h1("Business Innovation"),
                      h3("SDAD/DSPG"),
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
                      h3("DSPG20BI Summer Project"),
                      p("The DSPG20BI team is one of x number of teams within the larger DSPG program tasked with looking into detecting product innovation within non-traditional data sources.
                        Our goal is to find instances of product innovation within the pharmaceutical industry thorugh niche natural-language processessing techniques in an attempt
                        to supplement the current measure of innovation", tags$a(href = "https://www.nsf.gov/statistics/srvyindustry/about/brdis/", "the Business R&D and Innovation Survey (BRDIS)"),"conducted by The National Science Foundation."),

                      p("During the 10-week DSPG program, the Business Innovation team focused on developing functions that used NLP techniques to match strings across datasets.
                        These functions were written particularly focused on datasets mentioning innovation amongst pharmaceutical companies. However, the functions can be applied to any dataset containing strings.
                        The goal of this task it to provide future insights on the companies doing innovation as it pertains to the OLSO manual definition"),

                      h3("Our Team"),
                      p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut augue ligula, pharetra vel quam et, hendrerit pharetra turpis. Vestibulum eu faucibus neque. Proin efficitur odio at euismod pretium. Nam dui risus, porta eu dignissim ac, dapibus at dui. Vivamus tincidunt ut magna consectetur rutrum. Pellentesque ullamcorper, eros sed efficitur sagittis, nisi nisi condimentum odio, ut rutrum orci elit vitae elit. Nam pulvinar eros porta, egestas libero id, volutpat nibh. Phasellus viverra velit vitae ligula vehicula ultrices. Vestibulum in purus nec turpis consequat sollicitudin.
                        Vestibulum faucibus nulla porta neque pulvinar, convallis rhoncus tellus dictum. Sed eu lorem placerat, commodo odio nec, suscipit felis. Vivamus eu metus ullamcorper, accumsan metus eu, commodo sem.")

                      ),

             #ui
             navbarMenu("Profiling",

             tabPanel("Charts", style = "margin:20px",
                      fluidRow(column(5,
                                      selectInput("year", "Year", choices = c(2013, 2014,2015,2016,2017,2018))

                      ))

                      ),

             tabPanel("Profiles", style = "margin:20px",
                      h3("Profiling", align = "center"),
                      p(style = "margin-top:25px","One of our first tasks in the SDAD data science framework is to profile the data, which involves determining the quality of the data and its fitness for use. Here we profiled the dna data, looking
                      primarily at metrics such as completeness, uniqueness, duplication, and validity. Completeness refers to how complete the data is as a percentage. Uniqueness measures the number of unique values entered for a variable.
                        Duplicates is just the percent of duplicated values entered for a variable." ),
                      p("Further analysis using the pandas profiling package indicated that there were 35 total variables within the DNA dataset, with 29 of them being categorical variables, and 6 of them being numeric. In total, there were 1,942,855 observations."),
                      p("We were concerned primarily with a select number of variables when checking for validity. Here, we are defining the criteria for the variables of interest differently, but the overall definition of
                        what a valid entry is remains consistent-any value whose attributes make sense for the given variable and is a legitimate entry. For any variable involving company codes, we defined a code(and by extension the associated company),
                        as being a valid entry if and only if the code appeaered in both the dna dataset and the accompanying dna data dictionary. From what we gathered, we found that out of the 73,668 unique company codes found in the DNA dataset, only 64,005 codes were also found in the data dictionary, leading us to conclude that there
                        were only about 86.9% companies we would consider valid in the DNA data. This same approach was applied to the other company columns shown in the validity table below.
                        For date of publication, 100% of the articles fit our criteria of being published after 2010. For body, we wanted articles with more than 100 words (anything less would be indicative of a bot generated-article) and less than
                        10,000. Based on this criteria, we found that 78.3% of the articles were what we considered valid."),
                      sidebarLayout(
                        sidebarPanel(
                          width = 6,
                          selectInput("selectTable", "Select", choices = c("Completeness, Uniqueness, Duplicates", "Validity")),
                          h4("Definitions: ", style = "margin-top:50px"),
                          helpText("Note: All definitions are provided by Dow Jones Developer Platform"),
                          tags$ul(
                            tags$li("an - Accession Number (Unique id)"),
                            tags$li("art: Caption text and other descriptions of images and illustrations"),
                            tags$li("action: Action perfomed on a document (ex. add, rep, del)"),
                            tags$li("body: The content of the article"),
                            tags$li("byline: The author of an article"),
                            tags$li("copyright: Copyright text"),
                            tags$li("credit: Attribution text"),
                            tags$li("currency_codes: Currencies"),
                            tags$li("dateline: Place of origin and date"),
                            tags$li("document_type: Document type (ex. article, multimedia, summary"),
                            tags$li("ingestion_datetime: Data and time the artile was added to the Dow Jones Developer Platfrom"),
                            tags$li("language_code: Code for teh published language (ex. en)"),
                            tags$li("modification_datetime: Data and time that the article was modified"),
                            tags$li("modification_date: Date in which the article was last modified"),
                            tags$li("publication_date: Date in which the article was published"),
                            tags$li("publication_datetime: Date and time in which the article was published"),
                            tags$li("publisher_name: Publisher name"),
                            tags$li("region_of_origin: Publisher's region of origin"),
                            tags$li("snippet: What you see of an article outiside the paywall"),
                            tags$li("source_code: Publisher code"),
                            tags$li("source_name: Name of the source"),
                            tags$li("title: Title text"),
                            tags$li("word_count: Document word count"),
                            tags$li("subject_codes: News subjects"),
                            tags$li("region_codes: Region codes (ex. usa, namz, etc"),
                            tags$li("industry_codes: Industry codes"),
                            tags$li("person_codes: Persons"),
                            tags$li("market_index_codes: Market indices"),
                            tags$li("company_codes: Factiva IDs for companies and organizations"),
                            tags$li("company_codes_about: Companies that have high relevance to the document"),
                            tags$li("company_codes_association: Companies added to the document because of a relationship other than parent/child"),
                            tags$li("company_codes_lineage: Companies added to the document because of a parent/child relationship to another company"),
                            tags$li("company_codes_occur: Companies mentioned in the document but that do not necessarily have a high relevance to it"),
                            tags$li("company_codes_relevance: Companies added to the document because they have a certain degree of relevance to it")

                          )

                        ),
                        mainPanel(width = 3, tableOutput("tables"))
                      ))
),
             #end profiling tab------------------------------------------


             navbarMenu("Data Sources and Methods",
                        tabPanel(
                          "Data Sources",
                          h3("Data Sources", align = "center", style = "margin-bottom: 50px"),
                          style = "margin-left: 120px;",
                          style = "margin-top: 30px;",
                          style = "margin-right: 120px;",
                          fluidRow(
                            column(3, h4("Dow Jones News & Analytics (DNA)")),
                            column(6, wellPanel(p(style = "font-size:15px","The Dow Jones DNA platform collects information from Dow Jones publication with premium and licensed third party sources. This proprietary data platform contains 1.3bn articles each labeled with unique DNA taxonomies tags including word count, source name, and company code. More information on all the included data tags can be found on the DNA website. This dataset served as the primary resource for alternative text sources and will inspire the machine learning algorithms that will predict innovation. ")))
                          ),
                          hr(),
                          fluidRow(style = "margin-top:100px",
                                   column(3, h4("US Food and Drug Administration (FDA)")),
                                   column(7, wellPanel(p(style = "font-size:15px", "FDA drug approvals dataset generated and reviewed by FDA and includes information regarding. ")))
                          ),
                          hr(),
                          fluidRow(style = "margin-top:100px",
                                   column(3, h4("National Drug Code (NDC)")),
                                   column(7, wellPanel(p(style= "font-size:15px", "The National Drug Code (NDC) Directory  is a publicly available source provided by the FDA that contains a list of all current drugs manufactured, prepared, propagated, compounded, or processed for commercial distribution. The data content is manually inputted by the companies producing the drugs as required per the Drug Listing Act of 1972. The FDA assigns the NDC – a unique three-digit number, to the drug products. The administration then updates the NDC directory daily with the NDC along with the rest of the information provided. We gathered content from this dataset on [enter date here]. This data was used to cross-validate the companies that we had previously identified as producing an innovation. "))))
                        ),


                        tabPanel("Methods",
                                 h3("Methods", align = "center", style = "margin-bottom: 50px"),
                                 style = "margin-left: 120px;",
                                 style = "margin-top: 10px;",
                                 style = "margin-right: 120px;",

                                   fluidRow(
                                     column(3, h4("Cleaning")),
                                     column(6, p(style = "font-size:20px",""))
                                   ),
                                   hr(),
                                   fluidRow(style = "margin-top:100px",
                                            column(3, h4("Fuzzy Matching")),
                                            column(7, h4(""))
                                   ),
                                   hr(),
                                   fluidRow(style = "margin-top:100px",
                                            column(3, h4("Network Analysis")),
                                            column(7, h4("")))




                        )),#end navbar

             #end Data Sources and Methods tabs-----------------


             navbarMenu("Results",
                        tabPanel("Within Data Matching",
                                 selectInput("within", "Select", choice = c("NDCxNDC", "FDAxFDA", "DNAxDNA")),
                                 dataTableOutput("withinData")
                        ),


                        tabPanel("Across Data Matching",
                                 selectInput("across", "Select", choices = c("FDAxNDC", "FDAxDNA", "DNAxNDC")),
                                 dataTableOutput("AcrossData")



                        ),
                        tabPanel("Network Analysis")

                        )#end results tab


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

  #output$fda <- renderDataTable({
    #x <- read.csv("fdaxfda.csv")
    #x
  #})

  #output$ndc <- renderDataTable({
    #ndc <- read.csv("ndcxndc.csv")
    #ndc
  #})

  output$withinData <- renderDataTable({
    if(input$within == "FDAxFDA"){
      withinTable <- read.csv("fdaxfda.csv")

      withinTable$X <- NULL




      withinTable

    }else if(input$within == "NDCxNDC"){
      withinTable <- read.csv("ndcxndc.csv")

      withinTable$X <- NULL

      withinTable
    }
  })

  output$AcrossData <- renderDataTable({
    if(input$across == "FDAxNDC"){
      acrossTable <- read.csv("fdaxndc.csv")

      acrossTable$X <- NULL
      acrossTable$fda.row <- NULL
      acrossTable$clean.fda.company.name <- NULL
      acrossTable$clean.ndc.row <- NULL
      acrossTable$fuzz.ratio <- NULL
      acrossTable$clean.ndc.company <- NULL

      acrossTable
    }else if(input$across == "FDAxDNA"){
      acrossTable <- read.csv("fda_dna_matching.csv")

      acrossTable$X <- NULL
      acrossTable
    }else{
      acrossTable <- read.csv("ndc_dna_matching.csv")

      acrossTable$X <- NULL
      acrossTable
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)




