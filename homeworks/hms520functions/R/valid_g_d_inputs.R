#' Valid Group Data inputs
#'
#' @description Used in regress_group_data and summarise_group_data
#' to determine whether function inputs have the below structure.
#' @param group_id character, list of characters, or null
#' @param other_vars character or list of characters
#'
#' @return boolean. if TRUE, inputs are valid
#' @export
#'
#' @examples
#' Not for end user.
valid_g_d_inputs <- function(group_id, other_vars) {
  if((is.character(group_id) | is.null(group_id)) & is.character(other_vars)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
