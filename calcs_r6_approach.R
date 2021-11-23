subjects <- list(
  list(TRUE, FALSE, "P1", "Einfuehrung in die deskriptive Statistik", "Einfuehrung in die deskriptive Statistik", 6),
  list(TRUE, FALSE, "P2", "Analysis fuer Informatiker und Statistiker", "Analysis fuer Informatiker und Statistiker", 9),
  list(TRUE, FALSE, "P3", "Einfuehrung in die Matrizenrechnung", "Einfuehrung in die Matrizenrechnung", 9),
  list(TRUE, FALSE, "P4", "Einfuehrung in die Wahrscheinlichkeitsrechnung und in die induktive Statistik",
       "Einfuehrung in die Wahrscheinlichkeitsrechnung und in die induktive Statistik", 12),
  list(TRUE, FALSE, "P5", "Einfuehrung in die Analysis", "Einfuehrung in die Analysis", 9),
  list(TRUE, TRUE, "P6", "Einfuehrung in die praktische Statistik",
       c("P6.1" = "Statistische Software", "P6.2" = "Anfaengerpraktikum"), c(3, 3)),
  list(TRUE, FALSE, "P7", "Grundlagen der Statistik 1", "Wahrscheinlichkeitstheorie und Inferenz 1", 9),
  list(TRUE, TRUE, "P8", "Grundlagen der Statistik 2", 
       c("P 8.1" = "Wahrscheinlichkeitstheorie und Inferenz 2", "P 8.2" = "Bachelor-Seminar"), c(9, 6)),
  list(TRUE, FALSE, "P9", "Einfuehrung in Lineare Modelle", "Einfuehrung in Lineare Modelle", 9),
  list(TRUE, TRUE, "P10", "Fortgeschrittene praktische Statistik", 
       c("P10.1" = "Programieren mit statistischer Software", "P10.2" = "Statistisches Praktikum"), c(6, 9)),
  list(TRUE, FALSE, "P11", "Generalisierte Regression", "Generalisierte Regression", 9),
  list(TRUE, FALSE, "P12", "Einfuehrung in die multivariaten Verfahren", "Einfuehrung in die multivariaten Verfahren", 9),
  # Strictly speaking its just one subject, but we maintain it otherwise
  list(TRUE, TRUE, "P12", "ABschlussmodul",
       c("P13.1" = "Bachelorarbeit", "P13.2" = "Disputation"), c(12, 3)),
  list(FALSE, FALSE, "WP1", "Einfuehrung in die Informatik: Programmierung und Software-Entwicklung",
       "Einfuehrung in die Informatik: Programmierung und Software-Entwicklung", 6),
  list(FALSE, FALSE, "WP2", "Einfuehrung in die angewandte Statistik", "Einfuehrung in die angewandte Statistik", 6),
  list(FALSE, FALSE, "WP3", "Stichprobentheorie", "Stichprobentheorie", 6),
  list(FALSE, FALSE, "WP4", "Wirtschafts- und Sozialstatistik", "Wirtschafts- und Sozialstatistik", 6),
  list(FALSE, FALSE, "WP5", "Versuchsplanung", "Versuchsplanung", 6),
  list(FALSE, TRUE, "WP6", "Praktische Statistik und verteilungsfreie Verfahren",
       c("WP6.0.1" = "Verteilungsfreie Verfahren", "WP6.0.3" = "Ausgewaehlte Gebiete der angewandten Statistik B",
         "WP6.0.5" = "Praxisprojekt"), c(3, 3)),
  list(FALSE, FALSE, "WP7", "Zeitreihen", "Zeitreihen", 6),
  list(FALSE, FALSE, "WP8", "Ausgewaehlte Gebiete der angewandten Statistik A",
       "Ausgewaehlte Gebiete der angewandten Statistik A", 6)
)
moduls <- lapply(subjects, function(x)  Modul$new(x[[1]], x[[2]], x[[3]], x[[4]], x[[5]], x[[6]]))

                 
calc_subject_grades <- function(list_subjects) {
  lapply(list_subjects, assert_r6, "Subject")
  assert_list(list_subjects)
  # logical vector indicating, if a subject is absolved
  is_absolved <- vapply(list_subjects, function(x) !is.na(x$grade), logical(1))
  # asserts if there are any grades to calculate with
  assert_true(any(is_absolved))
  subjects_absolved <- list_subjects[is_absolved]
  moduls_absolved <- lapply(subjects_absolved, function(x) {
    if (x$composedModul) {
      
    }
  })
}


calculator <- function(list_subjects) {
  assert_list(list_subjects)
  # logical vector indicating, if a subject is absolved
  subj_absolved <- vapply(list_subjects, function(x) !is.na(x$grade), logical(1))
  # asserts if there are any grades to calculate with
  assert_true(any(subj_absolved))
  subjects_absolved <- list_subjects[subj_absolved]
}
