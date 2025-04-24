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
                     crs_to_use= 3161){

  # ex <- plots$PSU_ID[[i]]
  ARUtools:::check_cols(plot_sf_df, {{plot_col_label}})
  ARUtools:::check_cols(sites_sf_sf, {{plot_col_label}})
  ARUtools:::check_cols(sites_sf_sf, {{site_col_label}})

  PSU <- plot_sf_df |> dplyr::filter({{plot_col_label}}==plot_id) |>
    sf::st_transform(crs_to_use)
  loc_sites <- sites_sf_sf |> dplyr::filter({{plot_col_label}}==plot_id) |>
    dplyr::select({{site_col_label}})|>
    sf::st_transform(crs_to_use)

  wc_loc <-  sf::st_zm(sf::st_crop(sf::st_transform(near_water_course, crs_to_use),
                           PSU))
  wb_loc <- sf::st_crop(water_body,
                    sf::st_transform(PSU, sf::st_crs(water_body))) |>
    sf::st_transform(crs_to_use)


  ggplot2::ggplot() +
    ggplot2::geom_sf(data =wc_loc,
            colour = 'skyblue')+
    ggplot2::geom_sf(data = wb_loc ,
            fill = 'skyblue', size = 3)+
    ggplot2::geom_sf(data = PSU,fill = NA ) +
    # geom_sf(data = loc_sites,aes(shape = tz),
    #         size=6
    #              ) +
    # geom_sf_text(data = loc_sites,aes(label = SiteN),
    #              fontface = 'bold') +

    ggrepel::geom_text_repel(
      data = loc_sites,
      aes(label = {{site_col_label}}, geometry = geometry),
      stat = "sf_coordinates",
      min.segment.length = 0
    )+


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


#' Pair plots
#'
#' @param plot_list list of ggplot plots
#'
#' @returns
#' @export
#'
#' @examples
pair_plots <- function(plot_list){
  purrr::map(seq(1,length(plot_list), by=2),~{
  withr::with_package("patchwork",{
  (plot_spacer() + plot_list[[.x]] + plot_spacer() +plot_list[[(.x+1)]] +
     plot_layout(widths = c(0.01, 0.45, 0.07, 0.45)))
  })
  } )
}
