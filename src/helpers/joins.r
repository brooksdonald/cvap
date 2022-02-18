

helper_join_dataframe_list <- function(l, join_by, allx = TRUE, ally = FALSE) {
    join <- Reduce(
        function(x, y) merge(x, y, by = join_by, all.x = allx, all.y = ally),
        l
    )

    return(join)
}

