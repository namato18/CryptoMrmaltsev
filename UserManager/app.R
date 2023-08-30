library(shiny)
library(DT)
library(aws.s3)
library(shinyalert)
library(shinybusy)
library(bslib)

Sys.setenv(
  "AWS_ACCESS_KEY_ID" = "AKIAZI3NHYNJ2L5YMIHV",
  "AWS_SECRET_ACCESS_KEY" = "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",
  "AWS_DEFAULT_REGION" = "us-east-1"
)

# Define UI for application
ui <- fluidPage(
  
  add_busy_spinner(spin = "circle", color = "blue", height = "100px", width="100px", position = "bottom-right"),
  # theme = bs_theme(version = 4, bootswatch = "minty"),
  titlePanel("Username/Password Manager"),
  sidebarLayout(
    sidebarPanel(textInput("username", "Enter a Username", placeholder = "type username here..."),
                 textInput("password","Enter a Password Here",placeholder = "type password here..."),
                 actionButton("add",label = "Add User", class = "btn-success"),),
    mainPanel(
      
      br(),
      br(),
      dataTableOutput("userTable"),
      actionButton("remove",label = "Remove User", class = "btn-danger"),
      actionButton("submit",label = "Submit Changes", class = "btn-warning")
    )
  )
  
  
)

# Define server logic
server <- function(input, output) {
  
  userpass.df = reactiveValues()
  userpass.df$data =  s3read_using(FUN = readRDS, bucket = "cryptomlbucket/mlprophet_users", object = "userpass.df.rds")
  
  
  output$userTable = renderDataTable({
    datatable(userpass.df$data, rownames = FALSE)
  })
  
  observeEvent(input$remove, {
    userpass.df$data = userpass.df$data[-input$userTable_rows_selected,]
  })
  
  observeEvent(input$add, {
    x = data.frame(user = input$username,
                   password = input$password)
    
    userpass.df$data = rbind(userpass.df$data, x)
  })
  
  observeEvent(input$submit, {
    saveRDS(userpass.df$data, "updated_users/userpass.df.rds")
    
    put_object(
      file = file.path("updated_users", "userpass.df.rds"),
      object = "userpass.df.rds",
      bucket = "cryptomlbucket/mlprophet_users"
    )
    
    shinyalert("Success",
               "Users/Passwords Updated!",
               type = 'success')
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
