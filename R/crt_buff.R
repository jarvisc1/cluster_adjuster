#' Reassign points based on distance to cluster boundaries
#'
#' This function takes simple features points and polygon file.
#' It reassigns the points file to the intervention and id number of the cluster.
#'
#' @param points simple feature points locations of data
#' @param poly simple feature polygons of cluster boundaries
#' @param distance distance of buffer for cluster in metres
#' @param trt names of the intervention of treatment variable
#' @param trt_value value of the treatment to to buffer for
#' @param id name of id variable
#' @return spatial points file with new ids and treatment variables
#' @export


crt_buff <- function(points,
                     poly,
                     distance = 50,
                     trt,
                     trt_value,
                     id
                     ){

  # Filter to just get relevant clusters
  poly <- poly[, c(trt, id)]
  poly_trt <- poly[poly[[trt]]  == trt_value, ]

  # Identify where the names are
  poly_trt_loc    <- which(names(poly_trt) == trt)
  poly_id_loc     <- which(names(poly_trt) == id)

  # Define new names based on distance
  new_trt_name <- paste0(trt, "_", distance)
  new_id_name  <- paste0(id,  "_", distance)

  names(poly_trt)[poly_trt_loc] <- new_trt_name
  names(poly_trt)[poly_id_loc]  <- new_id_name


  new_points <- sf::st_join(points, poly_trt, join = st_is_within_distance, dist = distance)
  new_points[[new_trt_name]] <- ifelse(is.na(new_points[[new_trt_name]]),
                                           new_points[[trt]],
                                           new_points[[new_trt_name]]
                                           )
  new_points[[new_id_name]] <- ifelse(is.na(new_points[[new_id_name]]),
                                           new_points[[id]],
                                           new_points[[new_id_name]]
                                           )
  new_points

}



