## PROJECT: Geospatial Analytics Utility functions
## PURPOSE: Extract OperatingUnir boundaries from PEPFAR VcPolygons
## AUTHOR:  B.Kagniniwa | USAID
## DATE:    2021-02-04


# Libraries ----

library(tidyverse)
library(glamr)
library(gisr)
library(sf)

# Variables ----

  cntry <- "Nigeria"

  file_shp <- return_latest(
      folderpath = glamr::si_path("path_vector"),
      pattern = "VcPepfarPolygons.shp$",
      recursive = TRUE
    )

  # Ou folder
  dir_ou_bndries <- paste0(si_path("path_vector"), "/OU-Boundaries")

  if (!dir.exists(dir_ou_bndries))
    dir.create(dir_ou_bndries)

  # psnu folder
  dir_psnu_bndries <- paste0(si_path("path_vector"), "/OU-Prioritizations")

  if (!dir.exists(dir_psnu_bndries))
    dir.create(dir_psnu_bndries)

  # communities folder
  dir_comm_bndries <- paste0(si_path("path_vector"), "/OU-Communities")

  if (!dir.exists(dir_comm_bndries))
    dir.create(dir_comm_bndries)

# Functions ----

  #' Extract boundaries
  extract_boundaries <-
    function(spdf, country,
             level = 3,
             username = NULL,
             password = NULL,
             export = FALSE,
             name = NULL) {

    # Params
    cntry <- {{country}}

    lvl <- {{level}}

    user <- base::ifelse(base::is.null(username),
                         glamr::datim_user(),
                         {{username}})

    pass <- base::ifelse(base::is.null(password),
                         glamr::datim_pwd(),
                         {{password}})

    #ou uid
    uid <- get_ouuid(operatingunit = cntry,
                     username = user, password = pass)

    # list of orgs at the specified level
    orgs <- get_ouorgs(ouuid = uid, level = lvl,
                       username = user, password = pass) %>%
      mutate(org_level = lvl)

    # filter sp df
    spdf <- spdf %>%
      left_join(orgs, by = "uid") %>%
      filter(!is.na(orgunit))

    # Export
    if (export == TRUE & !is.null(name)) {

      sf::st_write(spdf,
                   delete_dsn = TRUE,
                   dsn = paste0(name, ".shp"))

    }

    return(spdf)
  }

# Data ----

  # Get OU UID
  ouuid <- get_ouuid(operatingunit = cntry,
                   username = glamr::datim_user(),
                   password = glamr::datim_pwd())


  #Levels
  levels <- get_levels(username = datim_user(), password = datim_pwd()) %>%
    filter(operatingunit == cntry)

  levels$country
  levels$prioritization
  levels$community
  levels$facility

  # Geodata
  pepfar_polygons <- file_shp %>% read_sf()

  pepfar_polygons %>% glimpse()

  # Operatingunit boundaries
  spdf_ou <- pepfar_polygons %>%
    filter(uid == ouuid)

  spdf_ou %>% gview()

  # psnu boundaries: uid lookup
  psnu_uids <- get_ouorguids(ouuid = ouuid, level = levels$prioritization)

  spdf_psnu <- pepfar_polygons %>%
    filter(uid %in% psnu_uids)

  # psnu boundaries: join & filter
  psnus <- get_ouorgs(
      ouuid = ouuid,
      level = levels$prioritization
    ) %>%
    mutate(org_type = "psnu", org_level = levels$prioritization)

  spdf_psnu <- pepfar_polygons %>%
    left_join(psnus, by = "uid") %>%
    filter(!is.na(orgunit))

  spdf_psnu %>% gview()


  # communities boundaries: uid lookup
  comm_uids <- get_ouorguids(ouuid = ouuid, level = levels$community)

  spdf_comm <- pepfar_polygons %>%
    filter(uid %in% comm_uids)

  spdf_comm %>% gview()

  # communities boundaries: join & filter
  comms <- get_ouorgs(
      ouuid = ouuid,
      level = levels$community
    ) %>%
    mutate(org_type = "community", org_level = levels$community)

  spdf_comm <- pepfar_polygons %>%
    left_join(comms, by = "uid") %>%
    filter(!is.na(orgunit))

  spdf_comm %>% gview()

  # Extract boundaries for all ou levels
  # OU all
  extract_boundaries(
      spdf = pepfar_polygons,
      country = cntry,
      level = 3
    ) %>%
    gview()

  # all
  levels %>%
    pivot_longer(cols = country:last_col(),
                 names_to = "level",
                 values_to = "val") %>%
    filter(level != "facility") %>%
    pull(val) %>%
    sort() %>%
    map(.x, .f = ~ extract_boundaries(spdf = pepfar_polygons,
                                      country = cntry,
                                      level = .x))


  # Export all OU Boundaries

  ous <- get_levels(
      username = glamr::datim_user(),
      password = glamr::datim_pwd()) %>%
    distinct(operatingunit) %>%
    pull(operatingunit)

  ous %>%
    map(.x, .f = ~ extract_boundaries(spdf = pepfar_polygons,
                                      country = .x,
                                      level = 3,
                                      export = TRUE,
                                      name = file.path(
                                        si_path("path_vector"),
                                        dir_ou_bndries,
                                        str_replace_all(.x, " ", "_") %>%
                                          str_to_lower())))

  # Export all psnu Boundaries - exclude Regional OUs
  ous2 <- ous %>%
    str_subset(pattern = " Region$",
               negate = TRUE)

  psnu_levels <- ous2 %>%
    map(.x, .f = ~get_ouorglevel(
      operatingunit = .x,
      org_type = "prioritization")) %>%
    unlist()

    map2(ous2, psnu_levels, .x, .y,
         .f = ~ extract_boundaries(spdf = pepfar_polygons,
                                    country = .x,
                                    level = .y,
                                    export = TRUE,
                                    name = file.path(
                                      si_path("path_vector"),
                                      dir_psnu_bndries,
                                      paste0(str_replace_all(.x, " ", "_") %>%
                                             str_to_lower(), "_psnu"))))



