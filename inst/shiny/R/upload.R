#Load database
ui_upload_db <- function(){
  sidebarLayout(
    sidebarPanel(
      #actionButton("load_btn1", "Load example database"),
      fileInput("database", paste("Upload a database file"),accept = c(".csv", ".tsv",".txt"))
    ),
    mainPanel(
      h3("Database"),
         DT::DTOutput("preview_database")
    )
  )
}

#Load mixture or reference
ui_upload <- function(type){
  sidebarLayout(
    sidebarPanel(
      fileInput(type, paste("Upload a", type, "file"),accept = c(".csv", ".tsv",".txt"))
      #actionButton("load_btn2", "Load example data")
    ),
    mainPanel(
      h3(paste(paste(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), sep=""), "profile"),
         #DT::DTOutput(paste0("preview_",type))
         div(DT::DTOutput(paste0("preview_",type)), style = "font-size:60%")
      )
    )
  )
}

#Load mixture/reference example data
ui_upload_ex <- function(type){
  sidebarLayout(
    sidebarPanel(
      if(type=="database") actionButton("load_btn1", "Load example database")
      else actionButton("load_btn2", "Load example data"),
      style = "margin-top: 20px"),
    mainPanel(
      )
    )
}