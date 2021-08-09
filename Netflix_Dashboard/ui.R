shinyUI(dashboardPage(
    dashboardHeader(title = "Netflix Dashboard"),
    
    dashboardSidebar(
        collapsed = F,
        sidebarMenu(
            menuItem(
                text = "Overview",
                tabName = "overview",
                icon = icon("eye")
            ),
            menuItem(
                text = "Content Distribution",
                tabName = "map",
                icon = icon("globe")
            ),
            menuItem(
                text = "Netflix Insights",
                tabName = "movie_insight",
                icon = icon("film")
            ),
            menuItem(
                text = "Director - Cast Networks",
                tabName = "network",
                icon = icon("project-diagram")
            ),
            menuItem(
                text = "View Code",
                tabName = "code",
                icon = icon("github")
            )
        )
    ),
    
    dashboardBody(
        tags$head(tags$style(
            HTML(
                '/* logo */
                            .skin-blue .main-header .logo {
                            background-color: #080200 ;
                            font-family: "Corbel";
                            font-weight: bold;
                            font-size: 24px;
                            }
                            /* logo when hovered */
                            .skin-blue .main-header .logo:hover {
                            background-color: #080200;
                            font-family: "Corbel";
                            font-weight: bold;
                            font-size: 24px;
                            }
                            /* navbar (rest of the header) */
                            .skin-blue .main-header .navbar {
                            background-color: #080200;
                            }
                            /* main sidebar */
                            .skin-blue .main-sidebar {
                            background-color: #A93226;
                            font-family: "Corbel";
                            }
                            /* active selected tab in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                            background-color: #CD6155;
                            color: white;
                            font-family: "Corbel";
                            }
                            /* other links in the sidebarmenu */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                            background-color: #A93226;
                            color: white;
                            font-family: "Corbel";
                            }
                            /* other links in the sidebarmenu when hovered */
                            .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                            background-color: #CD6155;
                            color: white;
                            font-family: "Corbel";
                            }
                            /* toggle button when hovered  */
                            .skin-blue .main-header .navbar .sidebar-toggle:hover{
                            background-color: #CD6155;
                            }
                            /* body */
                            .content-wrapper, .right-side {
                            background-color: #ffffff;
                            font-family: "Corbel";
                            }
                            /* icon */
                            .fa {
                            color: #ffffff;
                            opacity: 0.29;
                            }
                            /* header fonts */
                            h1,h2,h3,h4,h5,h6,.h1,.h2,.h3,.h4,.h5,.h6 {
                            font-family: "Candara";
                            }'
            )
        )),
        
        # TAB 1
        tabItems(
            tabItem(
                tabName = "overview",
                h2(tags$b("Welcome to The Netflix Dashboard!")),
                div(
                    style = "text-align:justify",
                    p(
                        "This is a dashboard I made as a part of the Capstone Project
                    assignment fufilment from the Data Visualization Specialization
                    Course from",
                        a(href = "https://algorit.ma/", "Algorit.ma"),
                        ".
                    The purpose of this dashboard is to provide exploratory analysis
                    regarding the movies and tv shows that are available in Netflix
                    platform. The data that is used within the dashboard is sourced from",
                        a(href = "https://www.kaggle.com/shivamb/netflix-shows", "Kaggle"),
                        "by the data owner, Shivam Bansal.",
                        p(
                            "The flow of this dashboard is from the overview of the insight
                                including the growth of Netflix content by year and percentage
                                of the contents compared by TV Shows and Movies. Further, the
                                distribution of Netflix Contents by groups, and several insights will
                                be provided in the next panel. Lastly, the links and networks between
                            casts and directors are shown in the last panel. For more information about
                            the code, please checkout the 'View Code' panel.",
                            tags$b("Happy
                                                                          Exploring~")
                        )
                        
                    ),
                    
                ),
                br(),
                fluidRow(
                    column(
                        width = 4,
                        fluidRow(
                            valueBox(
                                width = 12,
                                number(nrow(df_netflix), big.mark = ","),
                                "Total Movies & TV Shows",
                                icon = icon("ticket-alt"),
                                color = "red"
                            )
                        ),
                        fluidRow(
                            valueBox(
                                width = 12,
                                paste(round(nrow(movie_df) / nrow(df_netflix), 2) * 100, "%"),
                                "Percentage of Movies",
                                icon = icon("film"),
                                color = "black"
                            )
                        ),
                        fluidRow(
                            valueBox(
                                width = 12,
                                paste(round(nrow(tv_df) / nrow(df_netflix), 2) * 100, "%"),
                                "Percentage of TV Shows",
                                icon = icon("tv"),
                                color = "red"
                            )
                        ),
                        
                    ),
                    column(width = 8, box(
                        solidHeader = T,
                        width = 12,
                        plotlyOutput("mtv_growth", height = 350)
                    ))
                )
            ),
            
            #TAB 2
            tabItem(tabName = "map",
                    fluidRow(
                        box(
                            width = 12,
                            solidHeader = T,
                            background = "black",
                            column(width = 9,
                                   h3(
                                       tags$b("Netflix Content Distribution by Country")
                                   )),
                            column(
                                width = 3,
                                checkboxGroupInput(
                                    inputId = "mtv_map",
                                    label = h4(tags$b("Movie/TV Show: ")),
                                    choices = unique(df_netflix$type),
                                    selected = unique(df_netflix$type),
                                    inline = T
                                )
                            )
                        )
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            solidHeader = T,
                            plotlyOutput("map_dist", height = 500)
                        )
                    )),
            
            # TAB 3
            tabItem(
                tabName = "movie_insight",
                
                #section 1 director and casts insights
                fluidRow(
                    column(width = 6,
                           valueBoxOutput(width = 16,
                                          "pop_cast"),),
                    column(width = 6,
                           valueBoxOutput(width = 16,
                                          "pop_dir"))
                ),
                fluidRow(column(
                    width = 9,
                    box(
                        width = 16,
                        solidHeader = T,
                        h3(tags$b("Number of Casts by Countries")),
                        plotlyOutput("no_cast")
                    )
                ),
                column(
                    width = 3,
                    box(
                        solidHeader = T,
                        width = 16,
                        height = 475,
                        background = "black",
                        pickerInput(
                            inputId = "country_input",
                            label = h4(tags$b("Select Country:")),
                            choices = unique(df_netflix$main_country),
                            options = list(`actions-box` = TRUE, `live-search` = TRUE),
                            multiple = T,
                            selected = unique(df_netflix$country)
                        ),
                        checkboxGroupInput(
                            inputId = "movie_tv",
                            label = h4(tags$b("Movies/TV Shows Categories:")),
                            choices = unique(df_netflix$type),
                            selected = unique(df_netflix$type)
                        )
                    ),
                )),
                
                # section 2 genre distribution
                
                fluidRow(column(
                    width = 9,
                    box(
                        width = 16,
                        solidHeader = T,
                        h3(tags$b("Most Common Genres")),
                        plotlyOutput("genre_plot")
                    )
                ),
                column(
                    width = 3,
                    box(
                        width = 16,
                        height = 475,
                        background = "black",
                        sliderInput(
                            inputId = "year_added",
                            label = h4(tags$b("Year Range of Content Added in Platform: ")),
                            min = min(df_netflix$year_added),
                            max = max(df_netflix$year_added),
                            value = c(min(df_netflix$year_added), max(df_netflix$year_added)),
                            width = "100%",
                            sep = ""
                        )
                    )
                )),
                
                #section 3 age of contents distribution
                
                fluidRow(column(
                    width = 9,
                    box(
                        width = 16,
                        solidHeader = T,
                        h3(tags$b("Distribution of Age of Contents")),
                        h5(tags$b("By Time Difference to The Latest Content")),
                        plotlyOutput("age_dist")
                    )
                ),
                column(
                    width = 3,
                    box(
                        width = 16,
                        height = 500,
                        solidHeader = T,
                        background = "black",
                        checkboxGroupInput(
                            inputId = "viewers_cat",
                            label = h4(tags$b("Select Viewers Category: ")),
                            choices = unique(df_netflix$target_age),
                            selected = unique(df_netflix$target_age)
                        ),
                        chooseSliderSkin("Nice"),
                        sliderInput(
                            inputId = "release_year",
                            label = h4(tags$b("Year Range of Content Release:")),
                            min = min(df_netflix$release_year),
                            max = max(df_netflix$release_year),
                            value = c(min(df_netflix$release_year), max(df_netflix$release_year)),
                            width = "100%",
                            sep = ""
                        )
                    )
                ))
                
                
            ),
            
            
            #TAB 4
            tabItem(
                tabName = "network",
                box(
                    width = 12,
                    solidHeader = T,
                    background = "black",
                    column(width = 9,
                           h3(tags$b(
                               "Director and Casts Network"
                           ))),
                    column(
                        width = 3,
                        selectizeInput(
                            inputId = "dir_cast_net1",
                            label = h4(tags$b("Select Casts :")),
                            choices = unique(for_network$cast),
                            selected = c("Adam Sandler", "Drew Barrymore",
                                         "Rob Schneider"),
                            multiple = T,
                            options = list(maxItems = 3, placeholder = "Select up to 3")
                        )
                    )
                ),
                box(
                    width = 12,
                    solidHeader = T,
                    simpleNetworkOutput("dir_cast", height = 350)
                )
            )
        )
    )
))
