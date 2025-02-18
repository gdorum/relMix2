#Load mixture, reference or database
ui_upload <- function(type){
  sidebarLayout(
    sidebarPanel(
      fileInput(type, paste("Upload a", type, "file"),accept = c(".csv", ".tsv",".txt")),
    ),
    mainPanel(
      h3(paste(paste(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), sep=""), "profile"),
         DT::DTOutput(paste0("preview_",type))
      )
    )
  )
}