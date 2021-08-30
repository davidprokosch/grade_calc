library(R6)
library(checkmate)
library(data.table)

Modul <- R6Class("Modul",
                 public = list(
                   # is the Subject a compulsory modul?
                   compulsoryModul = NULL,
                   # Is the Modul composed
                   composedModul = NULL,
                   # "Kurzbezeichnung des Moduls"
                   shortNameModul = NULL,
                   nameModul = NULL,
                   # parts/subjects of the Modul
                   # "Bezeichnung dere Lehrveranstallting" if Module != subject
                   subjects = NULL,
                   ects = NULL,
                   grade = NA,
                   initialize =
                     function(compulsoryModul, composedModul, shortNameModul, nameModul, subjects, ects) {
                       self$compulsoryModul <- assertFlag(compulsoryModul)
                       self$composedModul <- assertFlag(composedModul)
                       self$shortNameModul <- assertString(shortNameModul)
                       self$nameModul <- assertString(nameModul)
                       self$subjects <- assertCharacter(subjects)
                       self$ects <- assertNumeric(ects, lower = 3)
                     },
                   getGrade = function(x) {
                     if (!missing(x)) {
                       self$grade <- assertNumeric(x, lower = 1.0, upper = 5.0)
                     }
                     self$grade
                   }
                 )
)


schnittGrade <- function(moduls) {
  assertList(moduls, types = "Modul")
  modulsDone <- rbindlist(list(list(modul = NULL, ects = NULL, grade = NULL)))
  for (i in seq_along(moduls)) {
    if (!is.na(moduls[[i]]$grade) && moduls[[i]]$grade != 5) {
      modulsDone <- rbindlist(list(modulsDone,
                                   list(moduls[[i]]$shortNameModul,
                                        sum(moduls[[i]]$ects),
                                        weighted.mean(moduls[[i]]$grade, moduls[[i]]$ects))))
    }
  }
  weighted.mean(modulsDone[, 3], modulsDone[, 2])
}
