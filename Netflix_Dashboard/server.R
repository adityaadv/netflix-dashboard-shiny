shinyServer(function(input, output) {
    # animation
    output$mtv_growth <- renderPlotly({
        mtv_growth <- df_netflix %>%
            group_by(year_added, type) %>%
            summarise(movie_count = n()) %>%
            ungroup() %>%
            mutate(cumulative_count = ave(movie_count, type, FUN = cumsum)) %>%
            accumulate_by( ~ year_added) %>%
            ggplot(aes(
                x = year_added,
                y = cumulative_count,
                color = type,
                frame = frame
            )) +
            geom_line(size = 1.5, alpha = 0.8) +
            geom_point(
                size = 3.5,
                shape = 21,
                fill = "white",
                aes(
                    text = paste0(
                        "Year: ",
                        year_added,
                        "<br>",
                        "Content Count: ",
                        cumulative_count
                    )
                )
            ) +
            scale_color_manual(values = c("firebrick", "grey16")) +
            scale_y_continuous(labels = scales::comma) +
            labs(
                title = "Growth Numbers of Netflix Contents by Year",
                x = "",
                y = "Num. of Contents",
                color = "",
            ) +
            theme_minimal() +
            theme(title = element_text(face = "bold"),
                  text = element_text(family = "lato"))
        
        
        ggplotly(mtv_growth, tooltip = c("text", "frame")) %>%
            animation_slider(currentvalue = list(prefix = "Year: ", font = list(color = "grey16"))) %>%
            config(displayModeBar = F)
    })
    
    #numbers of casts
    output$no_cast <- renderPlotly({
        req(input$country_input, cancelOutput = T)
        req(input$movie_tv, cancelOutput = T)
        
        fig_no_cast <- df_netflix %>%
            select(title, cast, country, genre, target_age, type) %>%
            mutate(cast_count = lengths(str_split(cast, ", "))) %>%
            filter(type %in% input$movie_tv,
                   country %in% input$country_input) %>%
            arrange(desc(cast_count)) %>%
            head(10) %>%
            ggplot(aes(x = cast_count, y = fct_reorder(title, cast_count))) +
            geom_col(alpha = 0.9, fill = "firebrick", aes(
                text = paste0(
                    title,
                    "<br>",
                    "Country: ",
                    str_trunc(country, 22),
                    "<br>",
                    "Num. of Casts: ",
                    cast_count
                )
            )) +
            scale_y_discrete(
                label = function(x)
                    stringr::str_trunc(x, 12)
            ) +
            labs(
                title = NULL,
                x = "Number of Casts",
                y = "Title of The Contents",
                color = "",
            ) +
            theme_minimal() +
            theme(
                title = element_text(face = "bold"),
                text = element_text(family = "lato"),
                panel.grid = element_blank(),
                panel.grid.major.x = element_line(color = "grey88"),
                axis.text.y = element_text(angle = 45)
            )
        
        ggplotly(fig_no_cast, tooltip = "text") %>%
            config(displayModeBar = F)
    })
    
    #top cast reactive value box
    output$pop_cast <- renderValueBox({
        req(input$country_input, cancelOutput = T)
        req(input$movie_tv, cancelOutput = T)
        
        top_cast <- df_netflix %>%
            filter(
                main_cast != "Unknown",
                type %in% input$movie_tv,
                country %in% input$country_input
            ) %>%
            group_by(main_cast) %>%
            summarise(num_of_content = n()) %>%
            ungroup() %>%
            arrange(desc(num_of_content)) %>%
            head(1)
        
        
        valueBox(
            value = top_cast$main_cast,
            subtitle = "Top Main Cast by Content Counts",
            color = "red",
            icon = icon("star")
        )
    })
    
    #top directors reactive value box
    
    output$pop_dir <- renderValueBox({
        req(input$country_input, cancelOutput = T)
        req(input$movie_tv, cancelOutput = T)
        
        top_dir <- df_netflix %>%
            filter(
                director != "Unknown",
                type %in% input$movie_tv,
                country %in% input$country_input
            ) %>%
            group_by(director) %>%
            summarise(num_of_content = n()) %>%
            ungroup() %>%
            arrange(desc(num_of_content)) %>%
            head(1)
        
        valueBox(
            value = top_dir$director,
            subtitle = "Top Director by Content Counts",
            color = "red",
            icon = icon("video")
        )
    })
    
    # boxplot output for age of contents
    
    output$age_dist <- renderPlotly({
        box_plot <- df_netflix %>%
            mutate(age_dist = (max(df_netflix$date_added) - date_added) / 365) %>%
            filter(
                target_age %in% input$viewers_cat,
                release_year %>%  between(input$release_year[1], input$release_year[2])
            ) %>%
            ggplot(aes(x = type, y = age_dist)) +
            geom_boxplot(width = 0.05, aes(fill = type), alpha = 0.8) + #add stat_summary next time
            scale_fill_manual(values = c("firebrick", "grey16")) +
            labs(x = "",
                 y = "Num. of Years", ) +
            theme_minimal() +
            theme(
                legend.position = "none",
                text = element_text(family = "lato"),
                panel.grid.major.x = element_blank()
            )
        
        ggplotly(box_plot) %>%
            config(displayModeBar = F)
    })
    
    #genre plot
    
    output$genre_plot <- renderPlotly({
        genre_plot <- df_netflix %>%
            filter(year_added %>% between(input$year_added[1], input$year_added[2])) %>%
            group_by(genre) %>%
            summarise(num_of_contents = n()) %>%
            ungroup() %>%
            arrange(desc(num_of_contents)) %>%
            head(10) %>%
            ggplot(aes(x = num_of_contents, y = fct_reorder(genre, num_of_contents))) +
            geom_col(fill = "grey16", alpha = .9, aes(
                text = paste0(genre, "<br>",
                              "Num. of Contents: ",
                              num_of_contents)
            )) +
            scale_y_discrete(
                label = function(x)
                    stringr::str_trunc(x, 12)
            ) +
            labs(
                title = NULL,
                x = "Number of Contents",
                y = "",
                color = "",
            ) +
            theme_minimal() +
            theme(
                title = element_text(face = "bold"),
                text = element_text(family = "lato"),
                panel.grid = element_blank(),
                panel.grid.major.x = element_line(color = "grey88"),
                axis.text.y = element_text(angle = 45)
            )
        
        ggplotly(genre_plot, tooltip = "text") %>%
            config(displayModeBar = F)
        
    })
    
    #map output
    
    output$map_dist <- renderPlotly({
        count_country <- netflix_for_map %>%
            filter(type %in% input$mtv_map) %>%
            group_by(main_country) %>%
            summarise(content_count = n()) %>%
            ungroup() %>%
            arrange(desc(content_count))
        
        map_join <- mapdata %>%
            left_join(. , count_country, by = c("region" = "main_country")) %>%
            mutate(content_count = replace_na(content_count, 0))
        
        
        temp <- ggplot() +
            geom_polygon(
                data = map_join,
                aes(
                    fill = content_count,
                    x = long,
                    y = lat,
                    group = group,
                    text = paste0(region, "<br>",
                                  "Netflix Contents: ", content_count)
                ),
                size = 0,
                alpha = .9,
                color = "black"
            ) +
            theme_void() +
            scale_fill_gradient(
                name = "Content Count",
                trans = "pseudo_log",
                breaks = c(0, 7, 56, 403, 3000),
                labels = c(0, 7, 56, 403, 3000),
                low =  "bisque2",
                high = "#b20710"
            ) +
            theme(
                panel.grid.major = element_blank(),
                axis.line = element_blank(),
                text = element_text(family = "lato")
            )
        
        
        ggplotly(temp, tooltip = "text") %>%
            config(displayModeBar = F)
        
    })
    
    #network output
    
    output$dir_cast <- renderSimpleNetwork({
        
        req(input$dir_cast_net1, cancelOutput = T)
        
        dir_cast_network <- for_network %>% 
            filter(director != "Unknown",
                   cast != "Unknown",
                   cast %in% input$dir_cast_net1)
        
        simpleNetwork(dir_cast_network, height = "50%", width = "100%",
                      Source = 1,
                      Target = 2,
                      zoom = T,
                      linkDistance = 100,
                      fontSize = 14,
                      charge = -100,
                      fontFamily = "Candara",
                      nodeColour = "#A93226")
    })
    

})
