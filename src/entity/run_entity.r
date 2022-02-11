# 40 - 117 lines of original code


source("src/entity/entity_characteristics.r")

run_entity <- function(local = new.env()) {
    print(" > Loading entity characteristics data...")
    ec <- load_entity_chars()
    print(" > Done.")
    print(" > Transforming entity characteristics data...")
    local$entity_characteristis <- transform_entity_chars(ec)
    print(" > Done.")

    .GlobalEnv$entity_characteristics <- local$entity_characteristics

    return(local)
}

run_entity()