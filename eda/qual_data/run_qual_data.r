run_qual_data <- function(a_data) {
    source("eda/qual_data/qual_data.r")

    datalist <- course_add_notes(a_data)
    a_data_temp <- datalist$a_data_temp
    a_data <- datalist$a_data

    return(environment())
}