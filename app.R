

source('app_functions.R')

cred <- read.csv('credentials.csv')
credentials <- reactiveValues(DF = data.frame(
  user = cred$user,
  password = cred$password,
  stringsAsFactors = FALSE
))

ui = secure_app(head_auth = tags$script(inactivity),
                  dashboardPagePlus(md = FALSE,
                                    dashboardHeaderPlus(
                                      fixed = TRUE,
                                      title = tagList(
                                        span(class = "logo-lg", "Activities App")),
                                      enable_rightsidebar = FALSE),
                                    dashboardSidebar(
                                      sidebarMenu(
                                        menuItem(text = "Tables", tabName = "tables", icon = icon("table"),
                                                 menuSubItem('Activities', tabName = 'activities',icon = icon('hiking')),
                                                 menuSubItem('Participants',tabName = 'participants',icon = icon('portrait')),
                                                 menuSubItem('Locations',tabName = 'locations',icon = icon('home')),
                                                 menuSubItem('Organizers',tabName = 'organizers',icon = icon('users')),
                                                 menuSubItem('Activity types',tabName = 'types',icon = icon('football-ball')),
                                                 menuSubItem('Targets',tabName = 'targets',icon = icon('bullseye'))
                                                 ),
                                        menuItem(text = "Tools", tabName = "tools", icon = icon("hammer"),
                                                 menuSubItem('Generate Word', tabName = 'word1',icon = icon('print')),
                                                 menuSubItem('Download database',tabName = 'database',icon = icon('database'))
                                                 ),
                                        menuItem(text = "Settings", tabName = "settings", icon = icon("gears"),
                                                 menuSubItem('Reset password',tabName = 'login', icon = icon('key'))),
                                        actionButton("button", "Update", icon = icon("spinner"),class = "btn-success")#,
                                        # menuItem(text = "Help", tabName = "helpbar",icon = icon("question"))
                                      )
                                     ),
                                    dashboardBody(
                                      h4('Sangria'), # Sangria
                                       # use a bit of shinyEffects
                                      setShadow(class = "dropdown-menu"),
                                      setShadow(class = "box"),
                                       
                                      shiny::tags$head(
                                        shiny::tags$style(
                                          rel = "stylesheet",
                                          type = "text/css",
                                          href = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/styles/qtcreator_dark.min.css"
                                          ),
                                        shiny::tags$script(
                                          src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.12.0/highlight.min.js")
                                        ),
                                      tabItems(tabItem(tabName = "activities", fluidRow(box(uiOutput('activities'), width = 12))),
                                               tabItem(tabName = "participants", fluidRow(box(uiOutput('participants'), width = 12))),
                                               tabItem(tabName = "targets", fluidRow(box(uiOutput('targets'), width = 12))),
                                               tabItem(tabName = "organizers", fluidRow(box(uiOutput('organizers'), width = 12))),
                                               tabItem(tabName = "types", fluidRow(box(uiOutput('types'), width = 12))),
                                               tabItem(tabName = "locations", fluidRow(box(uiOutput('locations'), width = 12))),
                                               tabItem(tabName = "word1", fluidRow(box(
                                                 title = 'Download a Word file',
                                                 selectInput("activity", "Select Activity by index", choices = getBooks()$id),
                                                 downloadButton("downloadTemplate", "Download Word", icon = icon("file-word")),
                                                 
                                                 textOutput("message2"),
                                                 width = 12))),
                                               tabItem(tabName = 'database', fluidRow(box(
                                                 h5("Click the button below to download the database in .sqlite format."),
                                                 h5("SQLite is a public-domain, single-user, very light-weight database engine that implements a decent subset of the SQL 92 standard."),
                                                 h5("You can open the file dowloaded using this webpage: https://sqliteonline.com/"),
                                                 h5("From this website you can view the tables of the database and download them in .csv format"),
                                                 downloadButton('downloadData', 'Download .sqlite database'),
                                                 width = 12))),
                                               tabItem(tabName = "login", fluidRow(box(
                                                 title = 'Reset password:',
                                                 passwordInput("passwordOld", "Current Password:"),
                                                 passwordInput("passwordNew1", "New Password:"),
                                                 passwordInput("passwordNew2", "Repeat New Password:"),
                                                 actionButton("reset", "Reset", icon = icon("fingerprint")),
                                                 textOutput("message"),
                                                 width = 8)))
                                               )
                                      ),
                                    title = "shinyDashboardPlus",
                                    footer = dashboardFooter(
                                      left_text = "By Jaime Ugarte",
                                      right_text = "Montreal, 2020"
                                      )
                                    ))

server <- function(input, output, session) {
  result_auth <- secure_server(check_credentials = check_credentials(credentials$DF))
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })
  # classic app
  
  eventReactive(Activities$thedata, {
    reactiveBooks(Activities$thedata)
  })
  participants <- getParticipants()
  places <- getPlaces()
  organizers <- getOrganizers()
  types <- getTypes()
  targets <- getTargets()
  reactiveBooks <- reactiveVal({getBooks()})
  
  typesR <- reactiveVal(getTypes()$Type)
  targetsR <- reactiveVal(getTargets()$Target)
  placesR <- reactiveVal(getPlaces()$Place)
  organizersR <- reactiveVal(getOrganizers()$Organizer)
  participantsR <- reactiveVal(getParticipants()$participant)
  instructorR <- reactiveVal(getParticipants()$participant[getParticipants()$instructor == 'yes'])
  
  Activities <- dtedit(input, output,
                       name = 'activities',
                       thedata = reactiveBooks,
                       edit.cols = c('Type','For','Location','Organizer','Start', 'End', 'Director','Assistant1','Assistant2','Secretary','Instructor','Participants'),
                       edit.label.cols = c('Activity type','For','Location','Organizer', 'Starting Date','Ending date','Director','Assistant director 1','Assistant director 2','Secretary','Priest','Participants'),
                       input.types = c(Type = 'selectInputReactive',
                                       For = 'selectInputReactive',
                                       Location = 'selectInputReactive',
                                       Organizer  = 'selectInputReactive',
                                       Director = 'selectInputReactive',
                                       Assistant1 = 'selectInputReactive',
                                       Assistant2 = 'selectInputReactive',
                                       Secretary = 'selectInputReactive',
                                       Priest = 'selectInputReactive',
                                       Participants= 'selectInputMultipleReactive'),
                       input.choices = list(Type = 'types.list',
                                            For = 'targets.list',
                                            Location = 'places.list',
                                            Organizer = 'organizers.list',
                                            Director = 'participants.list',
                                            Assistant1 = 'participants.list',
                                            Assistant2 = 'participants.list',
                                            Secretary = 'participants.list',
                                            Priest = 'priests.list',
                                            Participants = 'participants.list'),
                       input.choices.reactive = list(organizers.list = organizersR,
                                                     types.list = typesR,
                                                     places.list = placesR,
                                                     participants.list = participantsR,
                                                     priests.list = priestsR,
                                                     targets.list = targetsR),
                       callback.update = books.update.callback,
                       callback.insert = books.insert.callback,
                       callback.delete = books.delete.callback,
                       defaultPageLength = 15)
  
  observeEvent(input$button, {
    typesR(getTypes()$Type)
    targetsR(getTargets()$Target)
    placesR(getPlaces()$Place)
    organizersR(getOrganizers()$Organizer)
    participantsR(getParticipants()$participant)
    priestsR(getParticipants()$participant[getParticipants()$priest == 'yes'])
  })
  
  
  dtedit(input, output,
         name = 'participants',
         thedata = participants,
         edit.cols = c('participant', 'priest', 'phone', 'email', 'diet', 'notes'),
         edit.label.cols = c('Participant', 'Is he a priest?', 'Phone number', 'Email','Need special diet?','Other comments'),
         input.types = c(participant = 'textInput',
                         phone = 'textInput',
                         email = 'textInput',
                         diet = 'textInput',
                         priest = 'selectInput',
                         notes = 'textAreaInput'),
         input.choices = list(priest = c('no','yes')),
         view.cols = names(participants)[names(participants) != 'id'],
         callback.update = participants.update.callback,
         callback.insert = participants.insert.callback,
         callback.delete = participants.delete.callback,
         defaultPageLength = 15)
  
  dtedit(input, output,
         name = 'locations',
         thedata = places,
         edit.cols = c('Place'),
         edit.label.cols = c('Place'),
         input.types = c(Place = 'textInput'),
         callback.update = places.update.callback,
         callback.insert = places.insert.callback,
         callback.delete = places.delete.callback,
         defaultPageLength = 15)
  
  dtedit(input, output,
         name = 'organizers',
         thedata = organizers,
         edit.cols = c('Organizer'),
         edit.label.cols = c('Organizer'),
         input.types = c(Organizer = 'textInput'),
         callback.update = organizers.update.callback,
         callback.insert = organizers.insert.callback,
         callback.delete = organizers.delete.callback,
         defaultPageLength = 15)
  
  dtedit(input, output,
         name = 'targets',
         thedata = targets,
         edit.cols = c('Target'),
         edit.label.cols = c('Target'),
         input.types = c(Target = 'textInput'),
         callback.update = targets.update.callback,
         callback.insert = targets.insert.callback,
         callback.delete = targets.delete.callback,
         defaultPageLength = 15)
  
  dtedit(input, output,
         name = 'types',
         thedata = types,
         edit.cols = c('Type'),
         edit.label.cols = c('Type'),
         input.types = c(Type = 'textInput'),
         callback.update = types.update.callback,
         callback.insert = types.insert.callback,
         callback.delete = types.delete.callback,
         defaultPageLength = 15)
  
  output$downloadData <- downloadHandler(
    
    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = paste0(Sys.Date(),'_database','.sqlite'),
    
    # This function should write data to a file given to it by
    # the argument 'file'.
    content <- function(file) {
      file.copy("books.sqlite", file)
    },
    contentType = "application/x-sqlite3"
  )
  mensaje <- reactiveVal('')
  observeEvent(input$reset,{
    if(input$passwordOld != credentials$DF$password){
      mensaje('Wrong current password')
    } else if(input$passwordNew1 != input$passwordNew2){
      mensaje('You have written different passwords')
    } else if(input$passwordNew1 == input$passwordOld) {
      mensaje('The New password is the same as the old one.')
    } else if(gsub("[[:space:]]", "", input$passwordNew1)  %in% c('',NA)) {
      mensaje('The password cannot be empty')
    } else {
      credentials$DF <- data.frame(user = 'admin',password = input$passwordNew1)
      NewCreds <- data.frame(user = 'admin', password = credentials$DF$password)
      write.csv(NewCreds,'credentials.csv', row.names = F)
      mensaje('Password succesfully reset!')
    }
    
  })
  output$message <- renderText({
    mensaje()       
    
  })
  
  mensaje2 <- reactiveVal('')
  observeEvent(input$activity,{
    if(is.null(input$activity)){
      mensaje2('Select an activity')
    } else {
      id <- input$activity
      mensaje2(id)
      activityInfo <- getBooks()[getBooks()$id == id,]
      info <- data.frame(location = activityInfo$Location,
                         target = activityInfo$For,
                         type = activityInfo$Type,
                         organizer = activityInfo$Organizer,
                         startYear = format(activityInfo$Start,"%Y"),
                         startMonth = format(activityInfo$Start,"%m"),
                         startDay = format(activityInfo$Start,"%d"),
                         endYear = format(activityInfo$End,"%Y"),
                         endMonth = format(activityInfo$End,"%m"),
                         endDay = format(activityInfo$End,"%d"),
                         director = activityInfo$Director,
                         as1 = activityInfo$Assistant1,
                         as2 = activityInfo$Assistant2,
                         sc = activityInfo$Secretary,
                         pr = activityInfo$Priest,
                         participants = activityInfo$Participants)
      saveRDS(info,'params.RDS')
    }
  })
  
  output$downloadTemplate <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      params2 <- readRDS('params.RDS')
      part <- params2[,ncol(params2)]
      part2 <- ''
      for(i in part[1:nrow(params2)]){
        if(i != part[nrow(params2)]) {
          part2 <- paste0(part2,i,', ')
        } else {
          part2 <- paste0(part2,i,'.  ')
        }
        
      }
      rmarkdown::render(tempReport, output_file = file,
                        params = list(location = params2$location[1],
                                      target = params2$target[1],
                                      type = params2$type[1],
                                      organizer = params2$organizer[1],
                                      startYear = params2$startYear[1],
                                      startMonth = params2$startMonth[1],
                                      startDay = params2$startDay[1],
                                      endYear = params2$endYear[1],
                                      endMonth = params2$endMonth[1],
                                      endDay = params2$endDay[1],
                                      director = params2$director[1],
                                      as1 = params2$as1[1],
                                      as2 = params2$as2[1],
                                      sc = params2$sc[1],
                                      pr = params2$pr[1],
                                      participants = part2),
                        envir = new.env(parent = globalenv())
      )
    },
  )
  
  
  output$message2 <- renderText({
    mensaje2()       
    
  })
  observeEvent(input$button,{
    vars <- getBooks()$id
    updateSelectInput(session = session,inputId = "activity", choices = vars)
  })
  
  
  
}
  
shinyApp(ui = ui, server = server)
