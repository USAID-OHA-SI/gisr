#' Get OU Identifiers
#'
#' @param country Country name
#' @param username Datim Account username
#' @param label Keychain item name
#' @export
#' @examples
#' \dontrun{
#' get_ouuid("Kenya", "my_username")
#' get_ouuid("Kenya", username = "my_username", label = "my_keychain_label")
#' }
#'
get_ouuid <- function(country, username, password) {

    baseurl = "https://final.datim.org/"

    cntry = toupper({{country}})
    user <- {{username}}
    key <- {{password}}

    ous <- Wavelength::identify_ouuids(username = user, password = key)

    if (is.data.frame(ous) & "displayName" %in% names(ous) & cntry %in% toupper(ous$displayName)) {
        ous <- ous %>%
            dplyr::filter(toupper(displayName) == cntry)
    }
    else {
        cat("\nInvalid PEPFAR OU or Country Name:\n",
            Wavelength::paint_red(cntry),
            "\n")

        return(NULL)
    }

    return(ous$id[1])
}


