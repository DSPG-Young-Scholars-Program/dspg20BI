library(shiny)
library(dplyr)
library(ggplot2)
library(data.table)
library(rsconnect)
library(DT)

#Add hyperlinks to necessary places (i.e logos etc)

#tags$a(tags$img(height = "100%", width = "70%", src = "biilogo.png", align = "left" ), href="https://biocomplexity.virginia.edu/"

ui <- fluidPage(
  theme ="themes.css",

  navbarPage(title = span("Business Innovation", style = "color:#232D4B"),
             tabPanel("About",style = "margin:45px",
                      fluidRow(
                        column(3, tags$a(tags$img(height = "80%", width = "80%", src = "biilogo.png"),href="https://biocomplexity.virginia.edu/")),
                        column(6, h1("Business Innovation")),
                        column(3, tags$a(tags$img(height = "80%", width = "80%", src = "partnerlogos.png", align = "right"), href= "https://www.nsf.gov/statistics/"))
                      ),

                      h5("Project"),
                      p("Broadly we can define business innovation as the process of creating new or improved changes in business.
                        Following the international guidelines for surveys of business innovation in the Oslo Manual developed by the Organization for Economic Co-operation and Development (OECD) and Eurostat,
                        any business innovation can be categorized as a product innovation, a business process innovation, a marketing innovation or an organizational innovation.
                        A product innovation implies introduction of a new or improved product to the market, whereas a business process innovation indicates a new or significantly improved production or delivery method.
                        The marketing innovation includes a new marketing method and organizational innovation implies a new organizational method in business practices. Business innovation is crucial for value creation which can be achieved by
                        introduction of new or improved products in the market, on the other hand process innovation may lead to greater productivity, and thus business innovation serves as an indicator of growth in an economy. "),
                      p("Currently, the National Science for Engineering and Statistics collects data related to innovation in their annual survey called the
                         ",tags$a(href = "https://www.nsf.gov/statistics/srvyindustry/about/brdis/", "Business R&D and Innovation Survey (BRDIS)"),". Adopting the OSLO Manual 2018’s definition of innovation, which defines an innovative product as being new or improved and available on the market,
                        the survey ultimately aims to capture the state of innovation in the U.S. However, additional methods of measuring innovation can help provide further indication as to the nation’s degree of innovation.
                        Partnering with NCSES, SDAD aims to see if non-traditional data-sources can help supplement BRDIS and aid in measuring innovation."),
                      p("In the ten weeks of the DSPG program, the Business Innovation team focused on implementing the data science framework, primarily data ingestion and data wrangling, to provide the building blocks for natural language
                        processes and machine learning techniques that will detect innovation in text data.
                        We created functions that clean company names and then matched these companies across our three datasets to find overlaps. We also created matrices on the match scores of the three datasets of interest.
                        Additionally, we analyzed co-mention of companies in articles through network analysis, and consolidated our findings in this dashboard."),

                      h5("About Us"),
                      p("The Social and Decision Analytics Division (SDAD) is one of three research divisions within the Biocomplexity Institute and Initiative at the University of Virginia.
                        SDAD combines expertise in statistics and social and behavioral sciences to develop evidence-based research
                        and quantitative methods to inform policy decision-making and evaluation.
                        The researchers at SDAD span many disciplines including statistics, economics, sociology, psychology,
                        political science, policy, health IT, public health, program evaluation, and data science.
                        The SDAD office is located near our nation's capital in Arlington, VA. You can learn more about us ",
                        tags$a(href="https://biocomplexity.virginia.edu/", "here."), style = "color:#232D4B"),

                      p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at SDAD. Entering its seventh year, the program engages students from across the country
                        to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today.
                        DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information
                        generated within every community can be leveraged to improve quality of life and inform public policy. ", style = "color:#232D4B"),


                      h5("Our Team"),
                      p("SDAD: Devika Mahoney-Nair, Gizem Korkmaz, & Neil Alexander"),
                      p("DSPG: Susweta Ray (Fellow), Isabel Gomez (Intern), Ian MacLeod (Intern)"),
                      p("Sponsor: Gary Anderson, National Science Foundation (NSF), National Center for Science and Engineering (NCSES)")


                      ),


             navbarMenu("DNA Overview",

             tabPanel("Profiling the DNA Data", style = "margin:20px",
                      h5("Profiling", align = "center"),
                      p(style = "margin-top:25px","Our first task was to profile the DNA data in order to get a better understanding of both its quality and fitness for use. We looked primarily at metrics like data completeness,
                      uniqueness, and validity for select columns we felt were important. Completeness is simply a percentage of how complete the data was. Uniqueness can be defined as the number of distinct entries for each of the variables. While our
                      criteria for what counted as valid changed depending on the variable, the general definition of what it means to be valid remains consistent -- a value that is legitimate and makes sense given the context of the variable being looked at.
                      Each company-related variable (i.e company_codes, company_codes_about, etc) was a comma seperated list of codes, with each code representing a company mentioned in their respective articles.
                      When defined a code in these company columns as valid if and only if the code appeared in both the DNA articles and the accompanying DNA data dictionary. For instance, the company_codes column contains company codes for all
                      companies mentioned in a particular article, with some being listed more than once. Obtaining these company codes and getting the unique count revealed a total of 73,688 total distinct companies mentioned in these 1.9 million articles. However, only 64,005, or about 86.9% of these companies, were found
                      in the DNA code dictionary.Valid publication dates for these articles were defined as simply being past 2010,
                      as we were mainly concerned with articles published within the last decade.Finally, valid word counts for these articles needed to be greater than 100 but less than 10,000."),
                      br(),

                      sidebarLayout(
                        sidebarPanel(
                          width = 6,
                          selectInput("selectTable", "Select", choices = c("Completeness and Uniqueness", "Validity")),
                          h4("Definitions: ", style = "margin-top:30px"),
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
                        mainPanel(width = 6, dataTableOutput("tables"))
                      )),



             tabPanel("Top Publishers and Companies in DNA", style = "margin:20px",
                      h5("Top Publishers By Year"),
                      br(),
                      br(),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("year", "Year", choices = c(2013,2014,2015,2016,2017,2018)),
                          p(style = "font-size: 15px", "Bar chart showing the top publishers in DNA for each year between 2013 and 2018. NewsRx, a media and technology
                            company, as well as Dow Jones and Company, Inc. are consistenly among the top publishers between 2013 and 2018. Interestingly enough,
                            NewsRX was at one point the world's largest producer of health related news.")),
                        mainPanel(
                          imageOutput("pub"))
                      ),

                      h5("Top Company Mentions By Year"),
                      br(),
                      br(),
                      br(),

                      sidebarLayout(
                        sidebarPanel(selectInput("year2", "Year", choices = c(2013, 2014, 2015, 2016, 2017, 2018)),
                                     p(style = "font-size: 15px", "Regulatory agenices like the US Securities and Exchange Commission and the US
                                       Food and Drug Administration are over-represented in the DNA articles. Pharmaceutical giants like Pfizer and
                                       Johnson and Johnson rank consistently at the top, indicating that these corporations are some of the most talked about in the pharma space.
                                       ")
                                     ),
                        mainPanel(imageOutput("comp"))
                      )

             )
),
             #end profiling tab------------------------------------------


             tabPanel("Methods",
                    h3("Methods", align = "center", style = "margin-bottom: 20px"),
                    style = "margin-left: 120px;",
                    #style = "margin-top: 20px;",
                    style = "margin-right: 120px;",

                      fluidRow(
                        column(2, h5("Cleaning")),
                        column(7, wellPanel(p(style = "font-size:15px","In order to match company names across all three dataset we had to make all the strings similar to each other before beginning to measure string distance. To accomplish this we used regular expressions, the string package and the pandas library. The first step in the process was to lowercase all the strings. Then we removed select punctuations. Afterwards, we removed any parenthesis along with the content within the parentheses.
                                              We then removed single characters from the beginning and removed numbers, as numbers complicate the matching process. Additionally, we removed extra spaces between words, the prefix b, and any legal entities from the company name. This process was repeated for all three of our datasets.
                                              The end result was an aggressively cleaned name representing a corporate family for each company in each of the three datasets. With these collapsed names, we could then begin the matching process across the three datasets."))),
                        column(3, tags$img(height = "100%", width = "100%",src = "regexLogo.png"))
                        ),
                        hr(),
                        fluidRow(style = "margin-top:50px",
                        column(2, h5("Fuzzy Matching")),
                        column(7, wellPanel(p(style = "font-size:15px","String distancing is a way to measure how similar two strings of text are. One way to measure string distance is what is known as fuzzy matching. To complete the fuzzy matching, we used a package by SeatGeeks called FuzzyWuzzy. FuzzyWuzzy uses the Levenshtein distance to calculate the minimum number of single character edits (insertions, deletions or substitutions) needed to change one word to another. The package contains several functions that produces a similarity ratio out of 100. The fuzz.ratio function calculates the ratio by using the basic Levenshtein distance and the equation from diff.libratio: 2*M / T, where T is the total number of characters in both strings and M is the number of matches. The fuzz.partial_ratio compares the shortest string (n) against all the n-length substrings of the larger string and returns the highest fuzz partial ratio. Therefore, if the shortest string is found within the larger string then the partial ratio will return a ratio of 100. For our purposes, we focused on a fuzz.ratio that would only produce 100 or perfect matches, but future work may seek a less strict threshold."))),
                        column(3, tags$img(height = "80%", width = "100%",src = "fuzzyLogo.png"))
                        ),
                        hr(),
                        fluidRow(style = "margin-top:50px",
                            column(2, h5("Network Analysis")),
                            column(7, wellPanel(p(style = "font-size:15px", "With network analysis, we leverage the open-sourced software Gephi to visualize co-mentions between companies in DNA articles. Looking at statistical metrics like closeness-centrality, betweenness-centrality, and degree centrality, we can better understand which companies are central to the overall undirected network of co-mentions, and by extension, gain valuable insight into which pairings of companies are typically mentioned in articles about innovation."))),
                            column(3, tags$img(height = "80%", width = "100%",src = "gephyLogo.png")))

                        ),#end navbar

             #end Data Sources and Methods tabs-----------------


             navbarMenu("Results",

                        tabPanel("Across Data Matching",
                                 h5("Across Data Matching"),
                                 br(),
                                 br(),
                                 fluidRow(column(2),
                                          column(8, wellPanel(p(style = "font-size:15px", "The table below demonstrates all the matches for between the three datasets. This information helps us understand which companies show up in the other datasets and help establish corporate families along with validating innovations. The FDA dataset is known as our groundtruth, as this dataset is collected and revised by the FDA. Therefore, comparing FDA with the DNA  for instance, allows us to compare the companies mentioned in articles related to innovation and those companies producing innovation."))),
                                          column(2)
                                          ),
                                 selectInput("across", "Select", choices = c("FDAxNDC", "FDAxDNA", "DNAxNDC")),
                                 dataTableOutput("AcrossData")



                        ),
                        tabPanel("Network Analysis",
                                 h5("Network Analysis Using Gephi"),
                                 fluidRow(
                                   br(),
                                   br(),
                                   column(5,
                                          h4("Undirected Coapperance Network of Companies in Innovative Articles"),
                                          hr(),
                                          wellPanel(p("Here is an undirected network of companies who appear together in articles about innovation. The node size is defined by each nodes weighted degree (i.e the sum of the weighted edges for a particular node).
                                                      The larger the node, the larger the weighted degree and therefore the more connected the node is to the network. Johnson & Johnson has the highest weighted degree, meaning that this company occurs with other companies the most
                                                      and more often with the same companies in our subset of innovative articles.The color of each node represents the betweeness centrality, which is a metric that
                                                      measures how often a node appears on shortest paths between nodes in the network. The darker the node, the more important that node is as a sort of connector between nodes. Again, Johnson & Johnson is the highest,
                                                      meaning that when traveling from one node to another, moreoften than not the path will intersect Johnson & Johnson, making this company very central to the overall network. For illustrative purposes, only companies with degrees of 14 or more are shown, meaning that these companies are comentioned at least 14 (have 14 edges) times with other companies in our set of innovation-related articles."))
                                   ),
                                   column(7,
                                          tags$img(height = "20%", width = "100%", src = "innovationWeightBetweenDegree.png")


                                 )
                                 )


                                 ),

                        tabPanel("Scatterplots")

                        ),

            tabPanel("Data Sources",
                     h3("Data Sources", align = "center", style = "margin-bottom: 50px"),
                     style = "margin-left: 120px;",
                     style = "margin-top: 30px;",
                     style = "margin-right: 120px;",
                     fluidRow(
                       column(3, tags$img(height = "100%", width = "100%",src = "dnalogo.png")),
                       column(8, wellPanel(
                         tags$b("Dow Jones News and Analytics"),
                         p(style = "font-size:15px","The Dow Jones DNA platform collects information from Dow Jones publication with premium and licensed third party sources. This proprietary data platform contains 1.3bn articles each labeled with unique DNA taxonomies tags including word count, source name, and company code. More information on all the included data tags can be found on the DNA website. This dataset served as the primary resource for alternative text sources and will inspire the machine learning algorithms that will predict innovation. "))),
                     ),
                     fluidRow(style = "margin-top:90px",
                              column(3, tags$img(height = "100%", width = "100%", src = "fdalogo.png")),
                              column(8, wellPanel(
                                tags$b("FDA Approval Listings"),
                                p(style = "font-size:15px", "FDA drug approvals dataset generated and reviewed by FDA and includes information regarding Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vivamus dignissim vitae diam a convallis. Suspendisse nisi erat, aliquet et fringilla non, feugiat et justo. Nam felis nisl, cursus sed enim ac, posuere gravida ex. Cras maximus lacus pharetra arcu pretium pellentesque. Morbi maximus elit ut elementum auctor.",
                                  br(),
                                  br(),
                                tags$b("National Drug Code Directory"),
                                p(style = "font-size:15px", "The National Drug Code (NDC) Directory is a publicly available source provided by the FDA that contains a list of all current drugs manufactured, prepared, propagated, compounded, or processed for commercial distribution. The data content is manually inputted by the companies producing the drugs as required per the Drug Listing Act of 1972. The FDA assigns the NDC – a unique three-digit number, to the drug products. The administration then updates the NDC directory daily with the NDC along with the rest of the information provided. We gathered content from this dataset on [enter date here]. This data was used to cross-validate the companies that we had previously identified as producing an innovation. ")
                                )))
                     )


                     )




      ) #end navbarPage
  )#end fluid page





server <- function(input, output) {

  output$pub <- renderImage({

    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('www',
                                        paste(input$year, 'Publisherplot.png', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$year))



  }, deleteFile = FALSE)

  output$comp <- renderImage({

    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('www',
                                        paste(input$year2, 'Companyplot.png', sep='')))

    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image number", input$year))



  }, deleteFile = FALSE)






  output$tables <- renderDataTable({
    if(input$selectTable == "Validity"){


      valid <- read.csv("validitytable.csv")
      names(valid)[names(valid) == "X"] <- "Column Name"

      valid$Validity <- round(valid$Validity, digits = 2)

      valid
    }else{
      profTable <- read.csv("profilingTable.csv")

      names(profTable)[names(profTable) == "X"] <- "Column Name"

      profTable$Duplicates <- NULL
      profTable$Completeness <- round(profTable$Completeness, digits = 2)
      profTable$Uniqueness <- round(profTable$Uniqueness, digits = 2)



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
      withinTable$fuzz.ratio <- NULL
      withinTable$original.row.number <- NULL

      names(withinTable)[names(withinTable) == "clean.company.name"] <- "Corporate Family"
      names(withinTable)[names(withinTable) == "company.matches"] <- "Matches"
      names(withinTable)[names(withinTable) == "original.company.names"] <- "Original Company Name"




      withinTable

    }else if(input$within == "NDCxNDC"){
      withinTable <- read.csv("ndcxndc.csv")

      withinTable$X <- NULL
      withinTable$fuzz.ratio <- NULL
      withinTable$original.row.number <- NULL

      names(withinTable)[names(withinTable) == "clean.company.name"] <- "Corporate Family"
      names(withinTable)[names(withinTable) == "company.matches"] <- "Matches"
      names(withinTable)[names(withinTable) == "original.company.names"] <- "Original Company Name"

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

      names(acrossTable)[names(acrossTable) == "original.fda.company"] <- "Original FDA Company"
      names(acrossTable)[names(acrossTable) == "corporate.family"] <- "Corporate Family"
      names(acrossTable)[names(acrossTable) == "original.ndc.company"] <- "Original NDC Company"

      acrossTable
    }else if(input$across == "FDAxDNA"){
      acrossTable <- read.csv("fda_dna_matching.csv")

      acrossTable$fda.row <- NULL
      acrossTable$clean.fda.company.name <- NULL
      acrossTable$clean.dna.row <- NULL
      acrossTable$fuzz.ratio <- NULL
      acrossTable$clean.dna.company <- NULL

      acrossTable$X <- NULL

      names(acrossTable)[names(acrossTable) == "original.fda.company"] <- "Original FDA Company"
      names(acrossTable)[names(acrossTable) == "corporate.family"] <- "Corporate Family"
      names(acrossTable)[names(acrossTable) == "original.dna.company"] <- "Original DNA Company"
      acrossTable
    }else{
      acrossTable <- read.csv("ndc_dna_matching.csv")

      acrossTable$X <- NULL
      acrossTable$NDC.row <- NULL
      acrossTable$clean.NDC.company <- NULL
      acrossTable$clean.DNA.row <- NULL
      acrossTable$fuzz.ratio <- NULL
      acrossTable$clean.DNA.company <- NULL

      names(acrossTable)[names(acrossTable) == "original.NDC.company"] <- "Original NDC Company"
      names(acrossTable)[names(acrossTable) == "corporate.family"] <- "Corporate Family"
      names(acrossTable)[names(acrossTable) == "original.DNA.company"] <- "Original DNA Company"

      acrossTable
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)




