library(shiny)
library(tidyverse)
library(tidytext)
library(rvest)
library(data.table)
#Let's create an app that allows users to input their favorite song
#and see the lyrics for that song and its overall sentiment value

# url <- 'https://genius.com/frank-Ocean-lost-lyrics'

url %>%
    read_html() %>%
    html_nodes("p") %>%
    html_text() %>%
    .[[1]]


ui <- fluidPage(
    
    textInput(inputId = "artist",
              label = "Enter the artist"),
    textInput(inputId = "song",
              label = "enter your favorite song"),
    textOutput(outputId = "lyrics"),
    plotOutput(outputId = "sentiment")
    
)

server <- function(input, output, session) {
  
    output$lyrics <- renderText({
        
        artist.cleaned <- str_replace_all(input$artist,
                        " ",
                        "-")
        song.cleaned <- str_replace_all(input$song,
                                        " ",
                                        "-")
        
        url <- paste0('https://genius.com/',
        artist.cleaned,
        '-',
        song.cleaned,
        '-lyrics')
        
        lyrics <- url %>%
            read_html() %>%
            html_nodes("p") %>%
            html_text() %>%
            .[[1]]
        
        # lyrics.new <- unlist(lyrics) %>%
        #   view()
        # 
        # lyrics.frame <- data.frame(lyrics.new,
        #                            stringsAsFactors = FALSE)


        # lyrics.counted <- lyrics.frame %>%
        #     unnest_tokens("word",
        #                   "lyrics") %>%
        #     count(word)
        
        # lyrics.joined <- lyrics.counted %>%
        #     inner_join(get_sentiments("nrc")) %>%
        #     group_by(sentiment) %>%
        #     mutate(sum = sum(n))
        
    })
    
    # output$sentiment <- renderPlot({
    #     
    #     lyrics.joined %>%
    #         ggplot(aes(x = sentiment,
    #                    y = sum)) +
    #         geom_bar(stat = 'identity')
    #     
    #     
    #     
    #     
    # })
    
}

shinyApp(ui, server)




