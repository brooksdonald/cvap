

helper_join_dataframe_list <- function(l, join_by) {
    join <- Reduce(
        function(x, y) merge(x, y, by = join_by, all.x = TRUE),
        l
    )

    return(join)
}