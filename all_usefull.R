library(data.table)
library(checkmate)

subjects <- list(
  list(TRUE, FALSE, "P1", "Einfuehrung in die deskriptive Statistik", "Einfuehrung in die deskriptive Statistik", 6),
  list(TRUE, FALSE, "P2", "Analysis fuer Informatiker und Statistiker", "Analysis fuer Informatiker und Statistiker", 9),
  list(TRUE, FALSE, "P3", "Einfuehrung in die Matrizenrechnung", "Einfuehrung in die Matrizenrechnung", 9),
  list(TRUE, FALSE, "P4", "Einfuehrung in die Wahrscheinlichkeitsrechnung und in die induktive Statistik",
       "Einfuehrung in die Wahrscheinlichkeitsrechnung und in die induktive Statistik", 12),
  list(TRUE, FALSE, "P5", "Einfuehrung in die Analysis", "Einfuehrung in die Analysis", 9),
  list(TRUE, TRUE, "P6", "Einfuehrung in die praktische Statistik",  "Statistische Software", 3),
  list(TRUE, TRUE, "P6", "Einfuehrung in die praktische Statistik", "Anfaengerpraktikum", 3),
  list(TRUE, FALSE, "P7", "Grundlagen der Statistik 1", "Wahrscheinlichkeitstheorie und Inferenz 1", 9),
  list(TRUE, TRUE, "P8", "Grundlagen der Statistik 2", "Wahrscheinlichkeitstheorie und Inferenz 2", 9),
  list(TRUE, TRUE, "P8", "Grundlagen der Statistik 2", "Bachelor-Seminar", 6),
  list(TRUE, FALSE, "P9", "Einfuehrung in Lineare Modelle", "Einfuehrung in Lineare Modelle", 9),
  list(TRUE, TRUE, "P10", "Fortgeschrittene praktische Statistik", "Programieren mit statistischer Software", 6),
  list(TRUE, TRUE, "P10", "Fortgeschrittene praktische Statistik", "Statistisches Praktikum", 9),
  list(TRUE, FALSE, "P11", "Generalisierte Regression", "Generalisierte Regression", 9),
  list(TRUE, FALSE, "P12", "Einfuehrung in die multivariaten Verfahren", "Einfuehrung in die multivariaten Verfahren", 9),
  # Strictly speaking its just one subject, but we maintain it otherwise
  list(TRUE, TRUE, "P13", "Abschlussmodul", "Bachelorarbeit", 12),
  list(TRUE, TRUE, "P13", "Abschlussmodul", "Disputation", 3),
  list(FALSE, FALSE, "WP1", "Einfuehrung in die Informatik: Programmierung und Software-Entwicklung",
       "Einfuehrung in die Informatik: Programmierung und Software-Entwicklung", 6),
  list(FALSE, FALSE, "WP2", "Einfuehrung in die angewandte Statistik", "Einfuehrung in die angewandte Statistik", 6),
  list(FALSE, FALSE, "WP3", "Stichprobentheorie", "Stichprobentheorie", 6),
  list(FALSE, FALSE, "WP4", "Wirtschafts- und Sozialstatistik", "Wirtschafts- und Sozialstatistik", 6),
  list(FALSE, FALSE, "WP5", "Versuchsplanung", "Versuchsplanung", 6),
  list(FALSE, TRUE, "WP6", "Praktische Statistik und verteilungsfreie Verfahren",
       "Ausgewaehlte Gebiete der angewandten Statistik B", 3),
  list(FALSE, TRUE, "WP6", "Praktische Statistik und verteilungsfreie Verfahren",
       "Praxisprojekt", 3),
  list(FALSE, TRUE, "WP6", "Praktische Statistik und verteilungsfreie Verfahren",
       "Verteilungsfreie Verfahren", 3),
  list(FALSE, FALSE, "WP7", "Zeitreihen", "Zeitreihen", 6),
  list(FALSE, FALSE, "WP8", "Ausgewaehlte Gebiete der angewandten Statistik A",
       "Ausgewaehlte Gebiete der angewandten Statistik A", 6)
)
subj_dt <- rbindlist(subjects)[, grade := NA_integer_]
colnames(subj_dt) <- c("compulsoryModul", "composedModul", "shortNameModul",
                       "nameModul", "subjects", "ects", "grade")

#  for test-purposes: subj_dt$grade <- runif(27, 1, 4)

calc_moduls_grade <- function(subj_dt) {
  assert_data_table(subj_dt, ncols = 7)
  subj_dt[, .(compulsoryModul = mean(compulsoryModul), ects = sum(ects),
              modul_grade = weighted.mean(grade, ects)),
          by = shortNameModul][!is.na(modul_grade)]
}

calc_grade <- function(passed_moduls) {
  list_moduls <- copy(passed_moduls)
  assert_data_table(list_moduls, ncols = 4)
  enough_passed_moduls <- sum(list_moduls$compulsoryModul) == 13 &&
    sum(list_moduls$ects) >= 18
  # calculates grade, if all necessary moduls are passed
  if (enough_passed_moduls) {
    # First we need to "delete" unnecessary not-compulsory-moduls
    setorder(list_moduls, compulsoryModul, -modul_grade, -ects)
    while (sum(list_moduls[compulsoryModul == 0, ects]) > 18) {
      # if new_weight is positive or zero -> subject unnecessary
      # if new_weight is negative -> new_weight gets new ects entry
      new_weight <- sum(list_moduls[!(compulsoryModul == 1), ects]) -
        list_moduls[1, ects] - 18
      if(new_weight > 0) {
        list_moduls <- list_moduls[-1]
      } else {
        list_moduls[1, ects := abs(new_weight)]
      }
    }
    
    # Second we need to "delete" modulswith the worst grades
    setorder(list_moduls, -modul_grade, -ects)
    while (sum(list_moduls[, ects]) > 123) {
        list_moduls <- list_moduls[-1]
    }
    # grade of bachelor_main_subject_degree
    weighted.mean(list_moduls$modul_grade, list_moduls$ects)
    
  }
}


