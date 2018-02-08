################################################
##
## User interface script for upside model Kobe plot app
##
## By: Tyler Clavelle
## Date: 02/24/2016
##
################################################

shinyUI(fluidPage(theme = 'bootstrap.css',
                  
                  tags$head(
                    tags$style(HTML("

body {
    font-family: 'Open Sans', Arial, sans-serif;
                                    font-size: 16px;
                                    color: #ebebeb;
                                    background-color: #293581;
                                    padding: 20px;
                                    }
                                    
                                    a {
                                    color: #7fdec7;
                                    text-decoration: underline;
                                    }
                                    
                                    .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
                                    color: #ebebeb;
                                    background-color: rgba(255, 255, 255, 0.3);
                                    border: 1px solid #4e5d6c;
                                    border-bottom-color: transparent;
                                    cursor: default;
                                    }
                                    
                                    .table-striped > tbody > tr:nth-of-type(odd) {
                                    background-color: rgba(255, 255, 255, 0.1);
                                    }
                                    
                                    .tab-content > .active {
                                    display: block;
                                    margin-top: 20px;
                                    border-top: 1px solid white;
                                    padding-top: 20px;
                                    }
                                    "))
                  ),


                  fluidRow(
                    column(width = 3,

                           h2("Overview"),
                           tags$div(list(
tags$span(tags$p("This application contains an interactive Kobe plot displaying the current status of global fisheries. Use the options on the left to customize
                             which fisheries are included in the plot. The original data and methods used in this application come from"),
tags$a(href = 'http://www.pnas.org/content/113/18/5125', 'Costello et al. (2016)')," but are updated occassionally as the underlying databases are updated.")))),
                    column(width = 6,
                           h3('Disclaimer'),
                           tags$div(list(
                             tags$span(tags$i(tags$p('Results for individual unassessed fisheries are known to be highly unreliable and should not be taken as a substitute for formal
                             stock assessments. Additionally, stock assessment data is from version 2.95 of the'),
                                       tags$a(href = 'http://ramlegacy.org/', 'Ransom A. Myers (RAM)'),
                                       ' legacy stock assessment database and may not match the most currently available data for some stocks.'))))
                    )),

                  fluidRow(
                    column(width = 2,
                           h3("Options"),
                           helpText("Select options for the desired Kobe plot"),

                           radioButtons('Level',
                                        label = 'Select Category',
                                        choices = c('Global','By Country','By ISSCAAP group','By FAO Region'),
                                        selected = 'Global'),

                           conditionalPanel(
                             condition="input.Level == 'By Country'",
                             selectizeInput("Country",
                                            label="Select Country(s)",
                                            choices= sort(c("Indonesia","USA", "China","Philippines",'Peru','Myanmar','Norway','Iceland','Morocco',
                                                            "Japan","Viet Nam",'Thailand','India','Taiwan Province of China','Spain','Canada','Argentina',
                                                            'Republic of Korea','Malaysia','Russian Federation',"Chile","Mexico",'South Africa','Denmark','United Kingdom',
                                                            'Bangladesh','Ecuador','Brazil','Namibia','New Zealand'),decreasing = F),
                                            selected="USA",
                                            multiple = T)
                           ),

                           conditionalPanel(
                             condition="input.Level == 'By ISSCAAP group'",
                             selectizeInput("isscaap",
                                            label='Select ISSCAAP\nCategory(s)',
                                            choices=c("Cods, hakes, haddocks","Herrings, sardines, anchovies","Flounders, halibuts, soles",
                                                      "Miscellaneous coastal fishes","King crabs, squat-lobsters","Miscellaneous demersal fishes",
                                                      "Crabs, sea-spiders","Lobsters, spiny-rock lobsters","Shrimps, prawns",
                                                      "Tunas, bonitos, billfishes","Scallops, pectens","Miscellaneous pelagic fishes",
                                                      "Sharks, rays, chimaeras","Clams, cockles, arkshells","Abalones, winkles, conchs",
                                                      "Squids, cuttlefishes, octopuses","Shads","Salmons, trouts, smelts",
                                                      "Miscellaneous aquatic invertebrates","Miscellaneous diadromous fishes","Mussels",
                                                      "Sea-urchins and other echinoderms","Oysters","Carps, barbels and other cyprinids",
                                                      "Miscellaneous marine crustaceans","Horseshoe crabs and other arachnoids","Sturgeons, paddlefishes" ),
                                            selected = "Cods, hakes, haddocks",
                                            multiple = T)
                           ),

                           conditionalPanel(
                             condition="input.Level == 'By FAO Region'",
                             selectizeInput("Region",
                                            label="Select Region(s)",
                                            choices= c('Arctic Sea','Northwest Atlantic','Northeast Atlantic','West Central Atlantic','Eastern Central Atlantic','Mediterranean and Black Sea',
                                                       'Southwest Atlantic','Southeast Atlantic','Western Indian Ocean','Eastern Indian Ocean','Northwest Pacific','Northeast Pacific','Western Central Pacific','Eastern Central Pacific',
                                                       'Southwest Pacific','Southeast Pacific','Atlantic Antarctic','Indian Ocean Antarctic','Pacific Antarctic'),
                                            selected='Northwest Atlantic',
                                            multiple = T)
                           ),


                           radioButtons('Dbase','Assessment\nLevel',choices = c('All','RAM','Unassessed'),selected = c('All')),

                           sliderInput('Size','Select Mininimum\nStock Size (MT)', min = 1, max = 100000, value = 1, step = 1000, round = T),

                           checkboxInput('ColorID','Identify NEI stocks?'),

                           checkboxInput("Neis","Include NEIs",TRUE),

                           h4('Citation'),

                           withTags({div(
                             p(a( href = 'http://www.pnas.org/content/113/18/5125', 'Costello, Christopher, et al. "Global fishery prospects under contrasting management regimes." Proceedings of the National Academy of Sciences (2016): 201520420.')))
                             })

                    ),

                    column(width = 8, offset = 1,
                           tabsetPanel(type = 'tabs',

                                       tabPanel('Kobe Plot',
                                                fluidRow(
                                                  column(width = 12,

                                                         plotlyOutput('kobe_plot', width = '100%', height = '100%'),

                                                         tags$div(list(
                                                           tags$span(tags$i(tags$p('Figure 1: The black triangle and square represent the median and MSY-weighted geometric
                                                                                   mean of the plotted dataset. Hovering the pointer over a point will display the B/Bmsy, F/Fmsy,
                                                                                   MSY, and stock ID code. The ID code can be used to examine the stock data in the next tab.'))))),

                                                         downloadButton(outputId = 'downloadPDF', label = 'Download PDF')
                                                  ))),

                                       tabPanel('Plot Data',
                                                fluidRow(
                                                  column(width = 12,
                                                         dataTableOutput('kobe_table'),
                                                         downloadButton(outputId = 'dl_tbl', label = 'Download CSV')))),

                                       tabPanel('Parameters',
                                                fluidRow(
                                                  column(width = 10,
                                                         h4('Histogram of Pella-Tomlinson growth parameter (g)'),
                                                         plotlyOutput('kobe_histg', width = '100%', height = '100%')),
                                                  column(width = 2, div(style = "padding-top: 40px"),
                                                         tableOutput('kobe_tableg'))),

                                                fluidRow(div(style = "padding: 20px"),
                                                         column(width = 10,
                                                                h4('Histogram of carrying capacity (k)'),
                                                                plotlyOutput('kobe_histk')),
                                                         column(width = 2, div(style = "padding-top: 40px"),
                                                                tableOutput('kobe_tablek')))),

                                       tabPanel('SFG Video',
                                                fluidRow(
                                                  column(width = 12,
                                                         embed_youtube(id = 'ryDpZ_wWEdo'))
                                                ))
                           )),

                    column(width = 1)

                  )))
