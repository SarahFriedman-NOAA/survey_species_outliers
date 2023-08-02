# convert degree decimal minutes to decimal degrees
ddm_to_dd <- function(x, type){
  if(type == "lat"){
  out <- tibble(ddm = x) %>%
    separate(ddm, into = c("deg", "min"), sep = "(?<=[0-9]{2})", extra = "merge") %>%
    mutate(deg = as.numeric(deg),
           min = as.numeric(min)) %>%
    mutate(dd = deg + (min/60))
  } 
  if(type == "long"){
    out <- tibble(ddm = x) %>%
      separate(ddm, into = c("deg", "min"), sep = "(?<=[0-9]{3})", extra = "merge") %>%
      mutate(deg = as.numeric(deg),
             min = as.numeric(min)) %>%
      mutate(dd = deg + (min/60))
  }
  out$dd
  }
  

# function to identify outliers in speices IDs for each year
check_outlier <- function(data, check_year, catch_data, plot = FALSE,
                          eps = 7, minPts = 2) {
  sp_catch <- catch_data %>%
    filter(species_code == data$species_code[1]) 

  if(!any(sp_catch$year %in% check_year)){
    sp_catch <- bind_rows(sp_catch, data) %>%
      fill(species_name) %>%
      filter(!is.na(start_latitude))
  }
  
  if ( nrow(sp_catch) > 0 ) {
    # clustering <- dbscan(sp_catch[,c("start_longitude", "start_latitude")],
    #                      eps = 3, minPts = 2, borderPoints = FALSE)
    clustering <- dbscan(sp_catch[, c("start_longitude", "start_latitude")],
      eps = eps, minPts = minPts, borderPoints = FALSE
    )

    # flag anything that is only observed from check year
    if ( length(check_year) == 1 & all(sp_catch$year %in% check_year) ) {
      tmp <- sp_catch %>%
        mutate(
          cluster = 0,
          outlier = "flag"
        )
    } else {
      tmp <- sp_catch %>%
        mutate(
          cluster = clustering$cluster,
          outlier = ifelse(cluster == 0, "flag", "")) %>%
        filter(year %in% check_year)
    }

    # vector of outliers to plot as red
    o <- tmp[tmp$outlier != "", ]

    # data frame to return
    out <- tmp %>%
      dplyr::select(-cluster) %>%
      filter(outlier != "") %>%
      select(-species_name, -outlier)

    # if( nrow(o) == nrow(sp_catch) ) plot <- F
    
    if ( plot & length(o) > 0 ) {
      world <- map_data("world2", wrap = c(40, 400)) %>%
        filter(region %in% c("Russia", "USA", "Canada"))
      sp <- paste0(sp_catch$species_name[1], " (", sp_catch$species_code[1], ")")

      p <- ggplot() +
        geom_polygon(
          data = world, aes(x = long, y = lat, group = group),
          col = "grey60", fill = "grey90", lwd = 0
        ) +
        coord_map(ylim = c(45, 70), xlim = c(150, 250)) +
        theme_bw() +
        labs(x = "Longitude", y = "Latitude") +
        geom_point(
          data = sp_catch,
          aes(x = start_longitude, y = start_latitude), cex = 1
        ) +
        geom_point(
          data = o, aes(x = start_longitude, y = start_latitude),
          col = "red", cex = 1.5
        ) +
        # geom_text_repel(data = o, aes(x = start_longitude, y = start_latitude,
        #                               label = year)) +
        # ggtitle(label = sp, subtitle = sp_catch$common_name[1])
        ggtitle(label = sp,
             subtitle = ifelse(!is.na(out$voucher), "vouchered", "")
             )
      print(p)
    }
  } else {
    out <- NA
  }

  return(out)
}
