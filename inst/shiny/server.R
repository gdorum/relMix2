

# Define server logic
function(input, output, session) {

  # Upload ---------------------------------------------------------

    #Example data
    observeEvent(input$load_btn, {
      # Load the specific data set
      mixEx <- read.table(here::here("inst","extdata","mixture.txt"), header = TRUE, sep = "\t")
      refEx <- read.table(here::here("inst","extdata","references.txt"), header = TRUE, sep = "\t")
      databaseEx <- read.table(here::here("inst","extdata","frequencies22Markers.txt"), header = TRUE, sep = "\t")

      # Render the data table
      output$preview_mixture <- DT::renderDT({
        mix = req(mixEx)
        datatable(mix, rownames = FALSE,
                  options = list(scrollY = "300px", paging = FALSE, dom = "t"))
      })
      output$preview_reference <- DT::renderDT({
        refs = req(refEx) |> prettyRef()
        datatable(refs, rownames = FALSE,
                  options = list(scrollY = "300px", paging = FALSE, dom = "t"))
      })
      output$preview_database <- DT::renderDT({
        db = req(databaseEx)
        datatable(db, rownames = FALSE,
                  options = list(scrollY = "300px", paging = FALSE, dom = "t"))
      })
  })

  #Read mixture file
  raw <- reactive({ print("input$mixture")
    req(input$mixture)
    read.table(input$mixture$datapath, sep = "\t", header=TRUE)
  })

   output$preview_mixture <- DT::renderDT({
     mix = req(raw())
     datatable(mix, rownames = FALSE,
               options = list(scrollY = "300px", paging = FALSE, dom = "t"))
     })

  #Read reference file
   raw2 <- reactive({ print("input$reference")
     req(input$reference)

     df = read.table(input$reference$datapath, sep = "\t", header=TRUE)
     if(ncol(df) != 4)
       stop("Reference file must have 4 columns")
     names(df) = c("SampleName", "Marker", "Allele1", "Allele2")
     df
   })

   output$preview_reference <- DT::renderDT({
     refs = req(raw2()) |> prettyRef()
     datatable(refs, rownames = FALSE,
               options = list(scrollY = "300px", paging = FALSE, dom = "t"))
   })

   #Read database file
   raw3 <- reactive({
     req(input$database)
     read.table(input$database$datapath, sep = "\t", header=TRUE)
   })
   output$preview_database <- DT::renderDT({
     db = req(raw3())
     datatable(db, rownames = FALSE,
               options = list(scrollY = "300px", paging = FALSE, dom = "t"))
   })



   #Mutations
   observeEvent(input$mutModel, {
     if (input$mutModel == "Stepwise") {
       updateNumericInput(session, "range", value=0.5, min=0, max=1, step=0.01)
      }  else {
        updateNumericInput(session, "range", value=NULL)
      }
   })

   create_pedigree <- function(pedigree){
     if(pedigree=="Paternity"){
       persons <- c("Mo", "Fa", "Ch")
       sex <- c("female", "male", "male")
       ped <- Familias::FamiliasPedigree(id=persons, dadid=c(NA,NA,"Fa"), momid=c(NA,NA,"Mo"), sex=c("female", "male", "male"))
     #} else if(pedigree=="Non-paternity"){
     } else {
       #Define the persons involved in the case
       persons <- c("Mother", "Father", "Child")
       sex <- c("female", "male", "male")
       ped <- Familias::FamiliasPedigree(id=persons, dadid=c(NA,NA,NA), momid=c(NA,NA,"Mother"), sex=c("female", "male", "male"))
     }
     ped
   }


   #Contributors
   observeEvent(input$pedigree1, {
    req(input$pedigree1)
     ped1 <- create_pedigree(input$pedigree1)
     #Choices should be updated according to the names of the individuals in the pedigree
     updateCheckboxGroupInput(session, "cont1", choices=ped1$id)
      #The pedtools arguments create warnings
     output$plot1 <- renderPlot(plot(ped1))#, hatched=ped1$id, title="Pedigree under H1"))

     })
   observeEvent(input$pedigree2, {
   req(input$pedigree2)
     ped2 <- create_pedigree(input$pedigree2)
     #Choices should be updated according to the names of the individuals in the pedigree
     updateCheckboxGroupInput(session, "cont2", choices=ped2$id)
     #The pedtools arguments create warnings
     output$plot2 <- renderPlot(plot(ped2))#, hatched=ped2$id,  title="Pedigree under H2"))
   })

   #Plot pedigrees
   # Code by Magnus Dehli Vigeland
   # Function for plotting a list of pedigrees with typed members specified for each
   plotPeds = function(peds, typed) {
     npeds <- length(peds)

     # Make sure each pedigree is a list of components
     pedlist <- lapply(peds, function(p) if(pedtools::is.ped(p)) list(p) else p)

     # Component-wise plot data
     plotdat <- lapply(1:npeds, function(i) {
       pedi <- pedlist[[i]]
       ty <- typed[[i]]
       lapply(pedi, function(cmp) list(cmp, carrier = ty))
     })

     # Remove outer list layer
     plotdat <- unlist(plotdat, recursive = FALSE)

     # Group comps according to original pedigrees
     ncomps <- lengths(pedlist)
     groups <- split(seq_along(plotdat), rep(seq_along(ncomps), ncomps))

     # Titles
     titles <- paste0("H", 1:npeds)

     # Plot!
     pedtools::plotPedList(plotdat, frames = TRUE, groups = groups, titles = titles,
                           ### Further args to consider/tweak:
                           hatched = pedtools::typedMembers,
                           cex = 1.2,
                           cex.main = 1.2,
                           fmar = 0.02
     )
   }



}
