#' Read an ATE .rcp recipe file
#'
#' Reads an XML-based ATE `.rcp` recipe file and extracts deposition-relevant
#' parameters into a data frame. The returned table contains one row per
#' `"Deposit Rate"` action in the recipe, preserving recipe order.
#'
#' For each deposition action, the function reports the source name, the first
#' chamber pressure set point encountered before or within the recipe, the most
#' recent substrate temperature ramp set point before deposition, the target
#' deposition rate, timeout, and target thickness.
#'
#' XML namespaces are ignored when matching element names.
#'
#' @param filename Character string. Path to the `.rcp` XML recipe file.
#'
#' @return A `data.frame` with columns:
#' \describe{
#'   \item{source_name}{Name of the recipe source used for the deposition action.}
#'   \item{pre_vacuum_pressure}{First chamber pressure set point from a
#'   `"Wait for Chamber Pressure"` action.}
#'   \item{last_substrate_temperature}{Most recent substrate temperature
#'   ramp target or set point before the deposition action.}
#'   \item{deposition_rate}{Target deposition rate from the `"Deposit Rate"`
#'   action.}
#'   \item{deposition_time}{Timeout value from the `"Deposit Rate"` action.}
#'   \item{thickness}{Target thickness from the `"Deposit Rate"` action.}
#' }
#'
#' If no deposition actions are found, a one-row data frame is returned with
#' `NA` values for deposition-specific fields.
#'
#' @details
#' Source names are resolved by matching each deposition action's
#' `RecipeSourceID` to the corresponding `RecipeSource` entry.
#'
#' The substrate temperature is inferred from the last preceding
#' `"Ramp Temperature"` action. The function tries the following XML field names,
#' in order, and returns the first numeric value found:
#' `TemperatureSetpoint`, `TargetTemperature`, `Temperature`, and `Setpoint`.
#'
#' @examples
#' \dontrun{
#' recipe <- ATE.readRecipe("my_recipe.rcp")
#' recipe
#' }
#'
#' @importFrom xml2 read_xml xml_find_first xml_find_all xml_text
#' @export
ATE.readRecipe <- function(filename) {

  doc <- xml2::read_xml(filename)

  # Helper: get text from a child node by local XML name, ignoring namespaces
  child_text <- function(node, name) {
    out <- xml2::xml_find_first(node, paste0("./*[local-name()='", name, "']"))
    if (inherits(out, "xml_missing")) return(NA_character_)
    xml2::xml_text(out)
  }

  child_num <- function(node, name) {
    as.numeric(child_text(node, name))
  }

  # Build source ID -> source name lookup table
  source_nodes <- xml2::xml_find_all(doc, "//*[local-name()='RecipeSource']")

  sources <- data.frame(
    source_id = vapply(source_nodes, child_text, character(1), name = "Id"),
    source_name = vapply(source_nodes, child_text, character(1), name = "Name"),
    stringsAsFactors = FALSE
  )

  source_name_from_id <- function(id) {
    matched <- sources$source_name[sources$source_id == id]
    if (length(matched) == 0) return(NA_character_)
    matched[[1]]
  }

  # All recipe actions, in recipe order
  action_nodes <- xml2::xml_find_all(doc, "//*[local-name()='RecipeActionBaseClass']")

  action_name <- vapply(action_nodes, child_text, character(1), name = "ActionName")

  # Pre-vacuum pressure: take the first chamber pressure set point
  pressure_idx <- which(action_name == "Wait for Chamber Pressure")

  pre_vacuum_pressure <- if (length(pressure_idx) > 0) {
    child_num(action_nodes[[pressure_idx[[1]]]], "PressureSetpoint")
  } else {
    NA_real_
  }

  # Indices of deposition actions
  dep_idx <- which(action_name == "Deposit Rate")

  if (length(dep_idx) == 0) {
    return(data.frame(
      source_name = NA_character_,
      pre_vacuum_pressure = pre_vacuum_pressure,
      last_substrate_temperature = NA_real_,
      deposition_rate = NA_real_,
      deposition_time = NA_real_,
      thickness = NA_real_
    ))
  }

  get_last_substrate_temperature <- function(i) {
    prior_idx <- seq_len(i - 1)
    ramp_idx <- prior_idx[action_name[prior_idx] == "Ramp Temperature"]

    if (length(ramp_idx) == 0) {
      return(NA_real_)
    }

    last_ramp <- action_nodes[[ramp_idx[[length(ramp_idx)]]]]

    # Most useful value is usually the ramp target/set point temperature.
    # Try common field names in order.
    possible_fields <- c(
      "TemperatureSetpoint",
      "TargetTemperature",
      "Temperature",
      "Setpoint"
    )

    for (field in possible_fields) {
      value <- child_num(last_ramp, field)
      if (!is.na(value)) return(value)
    }

    NA_real_
  }

  result <- data.frame(
    source_name = vapply(
      dep_idx,
      function(i) {
        id <- child_text(action_nodes[[i]], "RecipeSourceID")
        source_name_from_id(id)
      },
      character(1)
    ),
    pre_vacuum_pressure = rep(pre_vacuum_pressure, length(dep_idx)),
    last_substrate_temperature = vapply(
      dep_idx,
      get_last_substrate_temperature,
      numeric(1)
    ),
    deposition_rate = vapply(
      dep_idx,
      function(i) child_num(action_nodes[[i]], "TargetRate"),
      numeric(1)
    ),
    deposition_time = vapply(
      dep_idx,
      function(i) child_num(action_nodes[[i]], "Timeout"),
      numeric(1)
    ),
    thickness = vapply(
      dep_idx,
      function(i) child_num(action_nodes[[i]], "TargetThickness"),
      numeric(1)
    ),
    stringsAsFactors = FALSE
  )

  rownames(result) <- NULL
  result
}
