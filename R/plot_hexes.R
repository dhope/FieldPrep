#' Create a plot map for helicopter decisions
#'
#' @param plot_id string plot
#' @param plot_sf_df
#' @param sites_sf_sf
#' @param site_col_label
#' @param near_water_course
#' @param water_body
#' @param crs_to_use
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
                     crs_to_use= 3161, show_tz=FALSE){

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
# browser()
  ggplot2::ggplot() +
    gen_geoms(wc_loc, wb_loc, PSU, loc_sites, show_tz, {{site_col_label}}) +
    ggplot2::theme_void() +
    ggplot2::labs(title = plot_id) +
    ggplot2::theme(legend.position = 'none',
          plot.title = element_text(hjust = .8),
    ) +
    ggspatial::annotation_north_arrow(
      location='br',which_north = 'true',
      pad_y = unit(0.3, "in"),
      style =
        ggspatial::north_arrow_minimal(line_width = 2,
                                       text_col = 'white'))
}



gen_geoms <- function(wc_loc, wb_loc, PSU, loc_sites, show_tz, site_col_label){
  list(
    ggplot2::geom_sf(data =wc_loc,
                     colour = 'skyblue'),
      ggplot2::geom_sf(data = wb_loc ,
                       fill = 'skyblue', size = 3),
      ggplot2::geom_sf(data = PSU,fill = NA ) ,
       if(show_tz){
         geom_sf(data = loc_sites,aes(shape = tz),
            size=6
    )},
    if(show_tz)geom_sf_text(data = loc_sites,aes(label = {{site_col_label}}),
                   fontface = 'bold'),
    if(show_tz)  scale_shape_manual(values = c("America/Toronto"=0,"America/Winnipeg"=1)) ,

    if(!show_tz) ggrepel::geom_text_repel(
                       data = loc_sites,
                       aes(label = {{site_col_label}}, geometry = geometry),
                       stat = "sf_coordinates",
                       min.segment.length = 0
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
     plot_layout(widths = c(0.01, 0.45, 0.07, 0.45)))
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
