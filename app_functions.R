library(shinymanager)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(RSQLite)
library(DTedit)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"
# data.frame with credentials info



##### Load tables data.frames as a SQLite database
conn <- dbConnect(RSQLite::SQLite(), "books.sqlite")

##### Add books table from books.csv if it's not already in the DB
if (!'books' %in% dbListTables(conn)) {
  books <- read.csv('books.csv', stringsAsFactors = FALSE)
  books$Participants <- strsplit(books$Participants, ';')
  books$Participants <-
    lapply(books$Participants, trimws) # Strip white space
  books$Participants <-
    unlist(lapply(books$Participants, paste0, collapse = ';'))
  dbWriteTable(conn, "books", books, overwrite = TRUE)
}

##### Add participants table from participants.csv if it's not already in the DB
if (!'participants' %in% dbListTables(conn)) {
  participants <-
    read.csv('participants.csv', stringsAsFactors = FALSE)
  participants$id <- 1:nrow(participants)
  dbWriteTable(conn, "participants", participants, overwrite = TRUE)
}

##### Add places table from places.csv if it's not already in the DB
if (!'places' %in% dbListTables(conn)) {
  places <- read.csv('places.csv', stringsAsFactors = FALSE)
  dbWriteTable(conn, "places", places, overwrite = TRUE)
}

if (!'organizers' %in% dbListTables(conn)) {
  organizers <- read.csv('organizers.csv', stringsAsFactors = FALSE)
  dbWriteTable(conn, "organizers", organizers, overwrite = TRUE)
}

if (!'types' %in% dbListTables(conn)) {
  types <- read.csv('types.csv', stringsAsFactors = FALSE)
  dbWriteTable(conn, "types", types, overwrite = TRUE)
}

if (!'targets' %in% dbListTables(conn)) {
  targets <- read.csv('targets.csv', stringsAsFactors = FALSE)
  dbWriteTable(conn, "targets", targets, overwrite = TRUE)
}

getParticipants <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM participants")
  participants <- dbFetch(res)
  dbClearResult(res)
  participants$instructor <- as.character(participants$instructor)
  participants$phone <- as.character(participants$phone)
  return(participants)
}

getPlaces <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM places")
  places <- dbFetch(res)
  dbClearResult(res)
  return(places)
}

getOrganizers <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM organizers")
  organizers <- dbFetch(res)
  dbClearResult(res)
  return(organizers)
}

getTargets <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM targets")
  targets <- dbFetch(res)
  dbClearResult(res)
  return(targets)
}

getTypes <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM types")
  types <- dbFetch(res)
  dbClearResult(res)
  return(types)
}

getBooks <- function() {
  res <- dbSendQuery(conn, "SELECT * FROM books")
  books <- dbFetch(res)
  dbClearResult(res)
  books$Location <- as.character(books$Location)
  books$Start <- as.Date(books$Start)
  books$End <- as.Date(books$End)
  books$Director <-
    as.character(books$Director)#factor(books$Director, levels = participants$participant)
  books$Assistant1 <-
    as.character(books$Assistant1) # factor(books$Assistant1, levels = participants$participant)
  books$Assistant2 <-
    as.character(books$Assistant2) # factor(books$Assistant2, levels = participants$participant)
  books$Secretary <-
    as.character(books$Secretary) # factor(books$Secretary, levels = participants$participant)
  books$instructor <-
    as.character(books$instructor) # factor(books$instructor, levels = instructors$participant)
  books$Participants <- strsplit(books$Participants, ';')
  count_par <- function(i) {
    part <- unlist(books$Participants[i])
    dir <- as.character(books$Director[i])
    as1 <- as.character(books$Assistant1[i])
    as2 <- as.character(books$Assistant2[i])
    sc <- as.character(books$Secretary[i])
    pr <- as.character(books$instructor[i])
    num1 <- unique(c(part, dir, as1, as2, sc, pr))
    num2 <- length(num1[!num1 %in%  c('', NA, NULL)])
    return(num2)
  }
  books$Total <- unlist(sapply(1:nrow(books), count_par))
  return(books)
}

##### Callback functions.
books.insert.callback <- function(data, row) {
  query <-
    paste0(
      "INSERT INTO books (id, Type, For, Location, Organizer, Start, End, Director, Assistant1, Assistant2, Secretary,instructor, Participants) VALUES (",
      "",
      max(getBooks()$id) + 1,
      ", ",
      "'",
      as.character(data[row, ]$Type),
      "', ",
      "'",
      as.character(data[row, ]$For),
      "', ",
      "'",
      as.character(data[row, ]$Location),
      "', ",
      "'",
      as.character(data[row, ]$Organizer),
      "', ",
      "'",
      as.character(data[row, ]$Start),
      "', ",
      "'",
      as.character(data[row, ]$End),
      "', ",
      "'",
      as.character(data[row, ]$Director),
      "', ",
      "'",
      as.character(data[row, ]$Assistant1),
      "', ",
      "'",
      as.character(data[row, ]$Assistant2),
      "', ",
      "'",
      as.character(data[row, ]$Secretary),
      "', ",
      "'",
      as.character(data[row, ]$instructor),
      "', ",
      "'",
      paste0(data[row, ]$Participants[[1]], collapse = ';'),
      "')"
    )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getBooks())
}
participants.insert.callback <- function(data, row) {
  query <-
    paste0(
      "INSERT INTO participants (id, participant, instructor, phone, email, diet, notes) VALUES (",
      "",
      max(getParticipants()$id) + 1,
      ", ",
      "'",
      as.character(data[row, ]$participant),
      "', ",
      "'",
      as.character(data[row, ]$instructor),
      "', ",
      "'",
      as.character(data[row, ]$phone),
      "', ",
      "'",
      as.character(data[row, ]$email),
      "', ",
      "'",
      as.character(data[row, ]$diet),
      "', ",
      "'",
      as.character(data[row, ]$notes),
      "')"
    )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getParticipants())
}

places.insert.callback <- function(data, row) {
  query <- paste0(
    "INSERT INTO places (id,Place) VALUES (",
    "",
    max(getPlaces()$id) + 1,
    ", ",
    "'",
    as.character(data[row, ]$Place),
    "')"
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getPlaces())
}

organizers.insert.callback <- function(data, row) {
  query <- paste0(
    "INSERT INTO organizers (id,Organizer) VALUES (",
    "",
    max(getOrganizers()$id) + 1,
    ", ",
    "'",
    as.character(data[row, ]$Organizer),
    "')"
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getOrganizers())
}

targets.insert.callback <- function(data, row) {
  query <- paste0(
    "INSERT INTO targets (id,Target) VALUES (",
    "",
    max(getTargets()$id) + 1,
    ", ",
    "'",
    as.character(data[row, ]$Target),
    "')"
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getTargets())
}

types.insert.callback <- function(data, row) {
  query <- paste0(
    "INSERT INTO types (id,Type) VALUES (",
    "",
    max(getTypes()$id) + 1,
    ", ",
    "'",
    as.character(data[row, ]$Type),
    "')"
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getTypes())
}

books.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "update books SET ",
    "Type = '",
    as.character(data[row, ]$Type),
    "', ",
    "For = '",
    as.character(data[row, ]$For),
    "', ",
    "Location = '",
    as.character(data[row, ]$Location),
    "', ",
    "Organizer = '",
    as.character(data[row, ]$Organizer),
    "', ",
    "Start = '",
    as.character(data[row, ]$Start),
    "', ",
    "End = '",
    as.character(data[row, ]$End),
    "', ",
    "Start = '",
    as.character(data[row, ]$Start),
    "', ",
    "End = '",
    as.character(data[row, ]$End),
    "', ",
    "Director = '",
    as.character(data[row, ]$Director),
    "', ",
    "Assistant1 = '",
    as.character(data[row, ]$Assistant1),
    "', ",
    "Assistant2 = '",
    as.character(data[row, ]$Assistant2),
    "', ",
    "Secretary = '",
    as.character(data[row, ]$Secretary),
    "', ",
    "instructor = '",
    as.character(data[row, ]$instructor),
    "', ",
    "Participants = '",
    paste0(data[row, ]$Participants[[1]], collapse = ';'),
    "' ",
    "WHERE id = ",
    data[row, ]$id
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getBooks())
}

participants.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "update participants SET ",
    "participant = '",
    as.character(data[row, ]$participant),
    "', ",
    "instructor = '",
    as.character(data[row, ]$instructor),
    "', ",
    "phone = '",
    as.character(data[row, ]$phone),
    "', ",
    "email = '",
    as.character(data[row, ]$email),
    "', ",
    "diet = '",
    as.character(data[row, ]$diet),
    "', ",
    "notes = '",
    data[row, ]$notes,
    "' ",
    "WHERE id = ",
    data[row, ]$id
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getParticipants())
}

places.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "update places SET ",
    "Place  = '",
    as.character(data[row, ]$Place),
    "' ",
    "WHERE id = ",
    data[row, ]$id
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getPlaces())
}

organizers.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "update organizers SET ",
    "Organizer  = '",
    as.character(data[row, ]$Organizer),
    "' ",
    "WHERE id = ",
    data[row, ]$id
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getOrganizers())
}

targets.update.callback <- function(data, olddata, row) {
  query <- paste0(
    "update targets SET ",
    "Target  = '",
    as.character(data[row, ]$Target),
    "' ",
    "WHERE id = ",
    data[row, ]$id
  )
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getTargets())
}

types.update.callback <- function(data, olddata, row) {
  query <- paste0("update types SET ",
                  "Type  = '",
                  as.character(data[row, ]$Type),
                  "' ",
                  "WHERE id = ",
                  data[row, ]$id)
  print(query) # For debugging
  dbSendQuery(conn, query)
  return(getTypes())
}

books.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM books WHERE id = ', data[row, ]$id)
  dbSendQuery(conn, query)
  return(getBooks())
}

participants.delete.callback <- function(data, row) {
  query <-
    paste0('DELETE FROM participants WHERE id = ', data[row, ]$id)
  dbSendQuery(conn, query)
  return(getParticipants())
}

places.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM places WHERE id = ', data[row, ]$id)
  dbSendQuery(conn, query)
  return(getPlaces())
}

organizers.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM organizers WHERE id = ', data[row, ]$id)
  dbSendQuery(conn, query)
  return(getOrganizers())
}

targets.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM targets WHERE id = ', data[row, ]$id)
  dbSendQuery(conn, query)
  return(getTargets())
}

types.delete.callback <- function(data, row) {
  query <- paste0('DELETE FROM types WHERE id = ', data[row, ]$id)
  dbSendQuery(conn, query)
  return(getTypes())
}