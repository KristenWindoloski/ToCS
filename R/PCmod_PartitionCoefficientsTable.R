
#######################################
# UI - PC PARTITION COEFFICIENTS TABLE
#######################################

PC_PCTable_ui <- function(id){

  htmltools::tagList(shiny::uiOutput(shiny::NS(id,"downloadPCtable_cond")),
                     DT::DTOutput(shiny::NS(id,"PCtable")),
                     shiny::textOutput(shiny::NS(id,"PCtableCaption"))
  )
}

############################################
# SERVER - PC PARTITION COEFFICIENTS TABLE
############################################

PC_PCTable_server <- function(id,pc_args){

  shiny::moduleServer(id, function(input, output, session) {

    #--- Reactive set to be used
    sol <- shiny::reactive({pc_args()[[1]]})
    pars <- shiny::reactive({pc_args()[[2]]})
    runsim <- shiny::reactive({pars()[["runsim"]]})

    #--- Outputs table of partition coefficients
    # output$PCtable<- shiny::renderTable({
    #   sol()[[2]]}, digits = 4, display = rep("fg", 16), rownames = TRUE)

    output$PCtable<- DT::renderDT({
      sol()[[2]]}, options = list(scrollX = TRUE))

    #--- Outputs partition coefficient table caption
    output$PCtableCaption <- shiny::renderText({
      shiny::req(sol(),runsim())
      "Table 2: Table of partition coefficients for all selected compounds in
      each of the available tissues (adipose, bone, brain, gut, heart, kidney,
      liver, lung, muscle, skin, spleen, red blood cells (rbc), rest - collective
      term for remaining tissues). The compounds are listed in ascending order
      based on the median partition coefficient of each compound across all
      tissues. The median partition coefficient for each compound is shown in
      the last column of the table."})

    #--- Creates table download button
    output$downloadPCtable_cond <- shiny::renderUI({
      shiny::req(sol(),runsim())
      shiny::downloadButton(session$ns("downloadPCtable"), "Download Table 2")})

    #--- Downloads partition coefficient table
    output$downloadPCtable <- shiny::downloadHandler(
      filename = function(){paste("PCdata-",Sys.Date(),".csv", sep = "")},
      content = function(file){write.csv(sol()[[2]], file)})
  })
}
