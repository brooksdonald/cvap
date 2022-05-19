
helper_tr_add_suffix_to_list <- function(l, suffix) {
    return(sprintf(paste0("%s", suffix), l))
}

helper_replace_values_with_map <- function(data, values, map,
                                           drop_rest = TRUE, na_fill = "") {
    if (!drop_rest) {
        unique_values <- unique(data)
        for (uv in unique_values) {
            if (!(uv %in% values)) {
                values <- append(values, uv)
                map <- append(map, uv)
            }
        }
    }

    dict <- data.frame(
        val = values,
        map = map
    )

    data <- dict$map[match(data, dict$val)]

    if (na_fill != "") {
        data[is.na(data)] <- na_fill
    }

    return(data)
}

helper_add_char_to_list <- function(l, char = "Y") {
    return(sprintf(paste0(char, "%s"), l))
}