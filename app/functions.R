#' Parse JORE route files, WIP
#' 
#' A Jore export file forms a tree of elements.
#' Column indexes significant for us are marked.
#' 
#' - (`route`: route ids and names)
#' - `rvariant`: route _variants_ (in most cases, _directions_) thereof
#'   - 1 label, 2 hastus variant, 7 line_id
#' - `rvpoint`: stops used by the route variants thereof
#'   - 1 label, 2 active_place, 7 stop_id, 8 variant_id
#' - (`place`: unique Hastus places used by rvpoints)
#' - `stop`: unique stops used by rvpoints
#'   - 1 label, 2 stop_id, 4 stop_name, 8 stop_place, 9 stop_lon, 10 stop_lat, 11 stop_code
#' - (`stpdist`: distances between pairs of stops used by rvpoints)

library(stringr)
library(dplyr)
library(tidyr)
library(sf)

#' Validate Digiroad link dataset
#'
#' @param x dr_linkki layer, returned by `st_read()`
#'
#' @return A data.frame of validation results
validate_dr_data <- function(x) {
  d <- data.frame(
    condition = 'Is not empty',
    result = (nrow(x) > 0),
    stringsAsFactors = FALSE
  )
  d <- rbind(d, list(condition = 'Has fields LINK_ID, AJOSUUNTA, TIENIMI_SU', 
                     result = all(c('LINK_ID', 'AJOSUUNTA', 'TIENIMI_SU') %in% colnames(x))))
  d <- rbind(d, list(condition = 'Has LINESTRING geometry',
                     result = all(st_geometry_type(x) == 'LINESTRING')))
  d <- rbind(d, list(condition = 'AJOSUUNTA has values 2, 3, 4 only',
                     result = 'AJOSUUNTA' %in% colnames(x) & all(sort(unique(x$AJOSUUNTA)) == c(2, 3, 4))))
  d <- rbind(d, list(condition = 'LINK_ID has unique values',
                     result = 'LINK_ID' %in% colnames(x) & !any(duplicated(x$LINK_ID))))
  return(d)
}

#' Transform from Digiroad links to sujuiko links and nodes
#'
#' @param x dr_linkki layer, returned by `st_read()`
#'
#' @return A list with `link` and `node` data.frames
transform_dr_data <- function(x) {
  link <- x %>%
    mutate(link_id = as.integer(LINK_ID),
           oneway = case_when(
             AJOSUUNTA == 2 ~ FALSE,
             AJOSUUNTA == 3 ~ TRUE,
             AJOSUUNTA == 4 ~ TRUE,
             TRUE ~ NA
           ),
           link_modes = '{"bus"}',
           link_label = TIENIMI_SU,
           data_source = 'Digiroad',
           source_date = format(Sys.Date(), '%Y-%m-%d'),
           geom = if_else(AJOSUUNTA == 3, st_cast(st_reverse(geom), 'GEOMETRY'), st_cast(geom, 'GEOMETRY'))) %>%
    select(link_id, oneway, link_modes, link_label, data_source, source_date, geom) %>%
    mutate(geom = st_cast(geom, 'LINESTRING'))
  
  link_inode <- link %>%
    select(link_id, geom) %>%
    mutate(geom = st_startpoint(geom))
  
  link_jnode <- link %>%
    select(link_id, geom) %>%
    mutate(geom = st_endpoint(geom))
  
  node <- rbind(link_inode, link_jnode) %>%
    select(geom) %>%
    distinct() %>%
    # Number the nodes by X and Y order
    mutate(nd_x = st_coordinates(geom)[, 'X'], nd_y = st_coordinates(geom)[, 'Y']) %>%
    arrange(nd_x, nd_y) %>%
    mutate(node_id = row_number()) %>%
    select(node_id, geom)
  
  link_inode_noded <- st_join(x = link_inode, y = node, join = st_intersects) %>%
    st_drop_geometry() %>%
    select(link_id, i_node = node_id)
  
  link_jnode_noded <- st_join(x = link_jnode, y = node, join = st_intersects) %>%
    st_drop_geometry() %>%
    select(link_id, j_node = node_id)
  
  link <- link %>%
    left_join(y = link_inode_noded, by = 'link_id') %>%
    left_join(y = link_jnode_noded, by = 'link_id') %>%
    select(link_id, i_node, j_node, oneway, link_modes, link_label, data_source,
           source_date, geom) %>%
    mutate(geom_text = st_as_text(geom)) %>%
    st_drop_geometry()
  
  node <- node %>%
    mutate(geom_text = st_as_text(geom)) %>%
    st_drop_geometry()
  
  return(list(link = link, node = node))
}

#' Parse single Jore route file
#' 
#' The file name must be of format `<route_id>_<valid_from_yyyymmdd>_<valid_to_yyyymmdd>.txt`.
#'
#' @param file Path of the `.txt` file
#' @param name (Original) file name, if different from `file`
#'
#' @return List of stop, route_version and stop_on_route tibbles
parse_jore_file <- function(file, name = file) {
  fname_els <- str_split_fixed(string = name, pattern = '_', n = 3)
  if (ncol(fname_els) < 3) {
    stop('File name must be like <route_id>_<valid_from_yyyymmdd>_<valid_to_yyyymmdd>.txt')
  }
  tryCatch(
    {
      valid_from <- as.Date(fname_els[2], format = '%Y%m%d')
      valid_to <- as.Date(fname_els[3], format = '%Y%m%d')
      stopifnot(valid_from <= valid_to)
    }, 
    error = function(e) {
      msg <- paste(e,
                   sprintf('File %s:', file),
                   'From and to dates must be of format yyyymmdd, and from <= to')
      stop(msg)
    })
  
  raw_lines <- readLines(con = file, encoding = 'latin1')
  
  stopifnot(any(str_detect(raw_lines, 'rvariant;')))
  stopifnot(any(str_detect(raw_lines, 'rvpoint;')))
  stopifnot(any(str_detect(raw_lines, 'stop;')))
  
  stop <- tibble(x = str_subset(raw_lines, 'stop;')) %>%
    separate(col = 'x',
             into = c(NA, 'stop_id', NA, 'stop_name', NA, NA, NA, 'stop_place',
                      'stop_lat', 'stop_lon', 'stop_code'),
             sep = ';') %>%
    mutate(stop_id = as.integer(stop_id),
           stop_place = if_else(nchar(stop_place) == 0, NA_character_, stop_place),
           stop_lat = as.numeric(stop_lat),
           stop_lon = as.numeric(stop_lon),
           # Fixed default values:
           stop_radius_m = 20.0,
           stop_mode = 'bus',
           source_date = Sys.Date(),
           parent_stop_id = NA_integer_) %>%
    st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326) %>%
    st_transform(crs = 3067) %>%
    mutate(geom_text = st_as_text(geometry)) %>%
    st_drop_geometry() %>%
    select(stop_id, stop_radius_m, stop_mode, stop_code, stop_name, stop_place,
           parent_stop_id, source_date, geom_text)
  
  route_version <- tibble(x = str_subset(raw_lines, 'rvariant;')) %>%
    separate(col = 'x',
             into = c(NA, 'hastus_variant', NA, NA, NA, 'variant_id', 'line_id'),
             sep = ';') %>%
    mutate(dir = as.integer(str_sub(hastus_variant, start = -1L, end = -1L)),
           variant = str_extract(hastus_variant, '[[:alpha:]]'),
           route = str_c(line_id, str_replace_na(variant, '')),
           route_mode = 'bus',
           valid_during = sprintf('[%s,%s]', format(valid_from, '%Y-%m-%d'), format(valid_to, '%Y-%m-%d')),
           route_ver_id = sprintf('%s_%d_%s_%s', route, dir, format(valid_from, '%Y%m%d'), format(valid_to, '%Y%m%d')))
  
  stop_on_route <- tibble(x = str_subset(raw_lines, 'rvpoint;')) %>%
    separate(col = 'x',
             into = c(NA, 'active_place', NA, NA, NA, NA, 'stop_id', 'variant_id'),
             sep = ';') %>%
    mutate(stop_id = as.integer(stop_id),
           active_place = if_else(nchar(active_place) == 0, NA_character_, active_place)) %>%
    group_by(variant_id) %>%
    mutate(stop_seq = row_number()) %>%
    ungroup() %>%
    left_join(route_version %>% select(variant_id, route_ver_id),
              by = 'variant_id') %>%
    select(route_ver_id, stop_seq, stop_id, active_place)
  
  route_version <- route_version %>% 
    select(route_ver_id, route, dir, valid_during, route_mode)
  
  return(list(stop = stop, route_version = route_version, stop_on_route = stop_on_route))
}

#' Parse a set of Jore route files
#'
#' @param files Paths of the `.txt` file(s)
#' @param names (Original) file names, if different from `files`
#'
#' @return List of stop, route_version and stop_on_route tibbles
parse_setof_jore_files <- function(files, names = files) {
  stopifnot(all(file.exists(files)))
  res <- mapply(parse_jore_file, files, names, SIMPLIFY = FALSE)
  
  all_stops <- do.call(rbind, lapply(res, function(x) x$stop))
  # NOTE: Possible duplicate stop_ids with different attribute values are simply
  #       ignored here, only distinct rows by stop_id are kept.
  stops_uniq_by_id <- distinct(all_stops, stop_id, .keep_all = TRUE)
  
  route_version <- do.call(rbind, lapply(res, function(x) x$route_version))
  
  stop_on_route <- do.call(rbind, lapply(res, function(x) x$stop_on_route))
  
  return(list(stop = stops_uniq_by_id, 
              route_version = route_version, 
              stop_on_route = stop_on_route))
}

#' Write csv with empty NAs
#' 
#' Calls `write_csv` with `na = ''`.
#'
#' @param x A tibble to write
#' @param file File to write to
custom_write_csv <- function(x, file) {
  readr::write_csv(x = x, file = file, na = '')
}