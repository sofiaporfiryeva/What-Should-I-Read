library(shiny)
library(dplyr)
library(text2vec)
library(stringr)

# загрузка данных (таблица с книгами, их эмбеддинги и словарь word_vectors)
load("nussbaum_data_new.RData")

# подсчет cos similarity
cosine_similarity <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

# интерфейс
ui <- fluidPage(
  titlePanel("📚 Ask Martha Nussbaum: What Should I Read?"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("query", 
                "Enter a theme or question:", 
                placeholder = "e.g. joy, love, fear of death"),
      sliderInput("n", 
                  "Number of recommendations:", 
                  min = 1, max = 15, value = 5),
      actionButton("go", "Get Recommendations"),
      textOutput("status")
    ),
    
    mainPanel(
      tableOutput("results")
    )
  )
)

# сервер: токенизация ввода, усреднение word_vectors,cos similarity
server <- function(input, output, session) {
  
  results <- eventReactive(input$go, {
    req(input$query)
    output$status <- renderText("🔎 Searching, please wait...")
    
    # приведение запроса к нижнему регистру
    query_tokens <- word_tokenizer(tolower(input$query))[[1]]
    
    # ответ, на случай, если для слова нет вектора
    valid_tokens <- query_tokens[query_tokens %in% rownames(word_vectors)]
    
    if (length(valid_tokens) == 0) {
      output$status <- renderText("Sorry! We have to think a bit more on this topic.")
      return(NULL)
    }
    
    #  эмбеддинг запроса как среднее по словам
    query_vector <- colMeans(word_vectors[valid_tokens, , drop = FALSE])
    
    # сравнение эмбеддинга с title
    sims <- apply(title_embeddings, 1, cosine_similarity, y = query_vector)
    
    # top-n
    top_indices <- order(sims, decreasing = TRUE)[1:input$n]
    
    
    results <- combined_df_clean[top_indices, ]
    results$similarity <- round(sims[top_indices], 3)
    
    output$status <- renderText("✅ Found some readings!")
    return(results)
  })
  
  # вывод в таблицу
  output$results <- renderTable({
    req(results())
    results() %>% select(type, title, author, year, similarity)
  })
}

# poehali!
shinyApp(ui, server)
