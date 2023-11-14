a <- system("sensors", intern = TRUE) |>
    stringr::str_subset("temp") |>
    stringr::str_replace_all("temp1:|[:space:]+|\\+|\\((.*?)\\)|°C", "") |>
    as.numeric() |>
    as.matrix() |>
    t() |>
    as.data.frame() |>
    dplyr::mutate(tm = Sys.time())

suppressWarnings(
    write.table(
        a,
        "./temperature.csv",
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = !file.exists("./temperature.csv"))
)
