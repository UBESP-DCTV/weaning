get_problematic_dupes <- function(db) {
  db |>
    janitor::get_dupes(
      .data[["file"]], .data[["id_pat"]], .data[["date"]],
      .data[["ora"]]
    )
}
