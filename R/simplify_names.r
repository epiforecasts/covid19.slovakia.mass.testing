##' Sipmlify county names
##'
##' Internal function to match county names for incidence data wiht county names for esting data
##' @param x data frame with a "county" column that contains the original county name
##' @return data frame with a "simple_name" column that contains the simplified name
##' @author Sebastian Funk
##' @importFrom dplyr mutate case_when
##' @keywords internal
simplify_names <- function(x) {
  x %>%
    mutate(simple_name = sub(" (- .+|[IV]+)$", "", county),
           simple_name = gsub("[ú]", "u", simple_name),
           simple_name = gsub("[á]", "a", simple_name),
           simple_name = gsub("[Ľ]", "L", simple_name),
           simple_name = gsub("[ľ]", "l", simple_name),
           simple_name = gsub("[č]", "c", simple_name),
           simple_name = gsub("[Č]", "C", simple_name),
           simple_name = gsub("[ž]", "z", simple_name),
           simple_name = gsub("[Ž]", "Z", simple_name),
           simple_name = gsub("[é]", "e", simple_name),
           simple_name = gsub("[ň]", "n", simple_name),
           simple_name = gsub("[ý]", "y", simple_name),
           simple_name = gsub("[í]", "i", simple_name),
           simple_name = gsub("[Š]", "S", simple_name),
           simple_name = gsub("[š]", "s", simple_name),
           simple_name = gsub("[ť]", "t", simple_name),
           simple_name = case_when(
             simple_name == "Kysucke Nove Mesto" ~ "Kysocke Nove Mesto",
             simple_name == "Banovce nad Bebravou" ~ "Banovce",
             simple_name == "Spisska Nova Ves" ~ "Spiska Nv",
             simple_name == "Vranov nad Toplou" ~ "Vranov",
             simple_name == "Nove Mesto nad Vahom" ~ "Nove Mesto Nv",
             simple_name == "Rimavska Sobota" ~ "Rimavska",
             simple_name == "Ziar nad Hronom" ~ "Ziar Nh",
             TRUE ~ simple_name
           )) %>%
    mutate(simple_name = gsub(" ", "_", tolower(simple_name)))
}
