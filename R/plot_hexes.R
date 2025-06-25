#' Create a plot map for helicopter decisions
#'
#' @param plot_id string plot
#' @param plot_sf_df
#' @param sites_sf_sf
#' @param site_col_label
#' @param near_water_course
#' @param water_body
#' @param crs_to_use
#' @param ... For future expansions. Currently only 'text_size' included.
#'
#' @returns
#' @export
#'
#' @examples
plot_hex <- function(plot_id,
                     plot_col_label,
                     site_col_label,
                     plot_sf_df,
                     sites_sf_sf,
                     near_water_course,
                     water_body,
                     crs_to_use= 3161, show_tz=FALSE, ...){
  args <- list(...)
  if(!'label_text_size' %in% names(args)) args$label_text_size <- 10
  if(!'site_tz_size' %in% names(args)) args$site_tz_size <- 6
  if(!'site_text_size' %in% names(args)) args$site_text_size <- 6

  # ex <- plots$PSU_ID[[i]]
  ARUtools:::check_cols(plot_sf_df, {{plot_col_label}})
  ARUtools:::check_cols(sites_sf_sf, {{plot_col_label}})
  ARUtools:::check_cols(sites_sf_sf, {{site_col_label}})
  suppressWarnings({
  PSU <- plot_sf_df |> dplyr::filter({{plot_col_label}}==plot_id) |>
    sf::st_transform(crs_to_use)
  loc_sites <- sites_sf_sf |> dplyr::filter({{plot_col_label}}==plot_id) |>
    # dplyr::select({{site_col_label}}, tz)|>
    sf::st_transform(crs_to_use)


  wc_loc <-  sf::st_zm(sf::st_crop(sf::st_transform(near_water_course, crs_to_use),
                           PSU))
  wb_loc <- sf::st_crop(water_body,
                    sf::st_transform(PSU, sf::st_crs(water_body))) |>
    sf::st_transform(crs_to_use)
  })

  # site_geom <- ifelse(show_tz,
  #                     )
  suppressWarnings({lab_loc <- st_centroid(PSU) |>
    st_coordinates() |>
    as_tibble() |>
    mutate(X = X + 1700,
           Y = Y + 2200,
           label = plot_id) |>
    st_as_sf(coords = c("X", "Y"), crs = crs_to_use)
  })




  plt <- ggplot2::ggplot() +
    gen_geoms(wc_loc, wb_loc, PSU, loc_sites, show_tz, {{site_col_label}}, args) +
    ggplot2::theme_void() +
    # ggplot2::labs(title = plot_id) +
    geom_sf_text(data = lab_loc, aes(label = label),
                 size = args$label_text_size)+
    ggplot2::theme(legend.position = 'none',
          plot.title = element_text(hjust = .8),
    ) +
    ggspatial::annotation_north_arrow(
      location='br',which_north = 'true',
      pad_y = unit(0.3, "in"),
      style =
        ggspatial::north_arrow_minimal(line_width = 2,
                                       text_col = 'white'))

  if(isTRUE(args$add_table)){
    if(rlang::is_installed('cowplot') &&
       rlang::is_installed('gridExtra')){
    full_site_tab <- args$aru_n[args$aru_n$PlotID==plot_id,c("SiteNumber", "ARU_ID")]
    tab_plt <- gridExtra::tableGrob(full_site_tab,
                                                       rows= NULL, cols = NULL,
                                                       theme =gridExtra::ttheme_minimal())

    return(cowplot::ggdraw(plt) + cowplot::draw_grob(tab_plt,
                                                 x = .12, y = .68,
                                                 width = 0.2, height = 0.2 * 10/6 ) )
    } else{rlang::abort("Packages 'cowplot'  and 'gridExtra' are required to add tables")}

  } else{plt}

    tab_plt
}



gen_geoms <- function(wc_loc, wb_loc, PSU, loc_sites, show_tz, site_col_label, args){
  list(
    ggplot2::geom_sf(data =wc_loc,
                     colour = 'skyblue'),
      ggplot2::geom_sf(data = wb_loc ,
                       fill = 'skyblue', size = 3),
      ggplot2::geom_sf(data = PSU,fill = NA ) ,
       if(show_tz){
         geom_sf(data = loc_sites,aes(shape = tz),
            size=args$site_tz_size
    )},
    if(show_tz)geom_sf_text(data = loc_sites,aes(label = {{site_col_label}}),
                   fontface = 'bold',size = args$site_text_size),
    if(show_tz)  scale_shape_manual(values = c("America/Toronto"=0,"America/Winnipeg"=1)) ,

    if(!show_tz) ggrepel::geom_text_repel(
                       data = loc_sites,
                       aes(label = {{site_col_label}}, geometry = geometry),
                       stat = "sf_coordinates",
                       min.segment.length = 0, size = args$site_text_size
                     )

  )




}




#' Pair plots
#'
#' @param plot_list list of ggplot plots
#'
#' @returns
#' @export
#'
#' @examples
pair_plots <- function(plot_list, legend_plot = NULL){
  ll <- length(plot_list)
  if(!is.null(legend_plot)) {
    plot_list[[ll+1]] <-  ggpubr::get_legend(legend_plot)
    ll <- ll +1 }

  purrr::map(seq(1,ll, by=2),~{
  withr::with_package("patchwork",{
    .x_2 <- (min(.x+1, ll))
    o <- switch(as.character(((ll%%2)!=0) & (.x==ll)),
                "TRUE"=plot_spacer(),
                "FALSE" =plot_list[[.x_2]] )
  (plot_spacer() + plot_list[[.x]] + plot_spacer() + o  +
      plot_spacer() +
     plot_layout(widths = c(0.02, 0.45, 0.07, 0.45, 0.005)))

  })
  } )
}



#' gen_legend
#'
#' @param sites_df_sf
#'
#' @returns
#' @export
#'
#' @examples
gen_legend <- function(sites_df_sf){
  ARUtools:::check_cols(sites_df_sf, tz)
  pf <-  ggplot2::ggplot(data= sites_df_sf, aes(shape = tz)) +
    ggplot2::geom_sf() +
    ggplot2::scale_shape_manual(values = c("America/Toronto"=0,
                                           "America/Winnipeg"=1))
  ggpubr::get_legend(pf) |>
    ggpubr::as_ggplot()

}
