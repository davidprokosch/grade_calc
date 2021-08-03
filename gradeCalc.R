library(R6)
library(checkmate)

Modul <- R6Class("Modul",
                 public = list(
                   # is the Subject a compulsory modul?
                   compModul <- NULL,
                   # "Kurzbezeichnung des Moduls"
                   shortNameModul <- NULL,
                   nameModul <- NULL,
                   # parts/subjects of the Modul
                   subjects <- NULL,
                   ects <- NULL,
                   createModul <- 
                     function(compModul, shortNameModul, nameModul, subjects, ects) {
                       self$compModul <- assertLogical(compModul)
                       self$shortNameModul <- assertString(shortNameModul)
                       self$nameModul <- assertString(nameModul)
                       self$subjects <- assertCharacter(subjects)
                       self$ects <- assertNumber(ects)
                     }
                 )
)

list(
  P1 <- list()
)