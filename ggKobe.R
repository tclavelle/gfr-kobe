#' ggKobe
#'
#' \code{ggKobe} produces a kobe plot in the manner of Costello
#' et al 2016, using data frmo
#' @param dat dataframe containing required stuff
#' @param xvar thing you want plotted on the x axis
#' @param yvar thing you want plotted on the y axis
#'
#' @return a kobe plot ggplot
#' @export
#'
#' @examples
#' ggKobe(filter(ProjectionData, Year == 2012), xvar = 'BvBmsy', yvar = 'FvFmsy')
ggKobe <- function(dat, xvar = 'BvBmsy', yvar = 'FvFmsy', dotcolor = 'Dbase', density = F) {

  # dat <- data.frame(dat)
  # browser()
  
  dat <- ungroup(dat)

  orig_names = colnames(dat)

  xvar_name = xvar
  
  # xvar_name = expression('B/B'['MSY'])
  xvar_name = 'B/Bmsy'

  # yvar_name = expression('F/F'['MSY'])
  yvar_name = 'F/Fmsy'

  dat = dat %>%
    rename_(xvar = xvar, yvar = yvar, dotcols = dotcolor) %>%
    mutate(yvar = pmin(4, yvar))

  if(dotcolor=='Dbase') { dat$is_ram <- dat$dotcols == 'RAM' }

  dots <-
    eval(parse(
      text = paste(
        'list(~median(',
        xvar,
        ', na.rm = T),~median(',
        yvar,
        ', na.rm = T))',
        sep = ''
      )
    ))
  summary_dat <- dat %>%
    ungroup() %>%
    mutate(has_all = is.na(MSY) == F & is.na(xvar) == F & is.na(yvar) == F,
  has_both_y = is.na(MSY) == F & is.na(yvar) == F) %>% 
    filter(has_all == T) %>% 
    summarise(median_x = median(xvar, na.rm = T),
              median_y = median(yvar, na.rm = T),
              geom_mean_msy_weight_x = exp(sum(MSY * log(xvar), na.rm = T) / sum(MSY, na.rm = T)),
              geom_mean_msy_weight_y = exp(sum(MSY * log(yvar + 1e-3), na.rm = T) / sum(MSY, na.rm = T))) %>%
    mutate(is_ram = NA, MSY = NA)

## Kobe with density ##
if(density == T) {
  kobe <- dat %>%
    ggplot(aes(xvar, yvar)) + #general aesthetic
    stat_density_2d(
      aes(fill = ..density..),
      geom = 'tile',
      n = 100,
      alpha = 0.8,
      contour = F
    ) + #eggplot
    scale_fill_gradient2(
      guide = F,
      low = 'skyblue1',
      mid = 'white',
      high = 'khaki1',
      midpoint = 0.3
    ) + #set eggplot colors
    geom_hline(aes(yintercept = 1), linetype = 'longdash') +
    geom_vline(aes(xintercept = 1), linetype = 'longdash') +
    # plot points
      geom_point(aes(
      xvar,
      yvar,
      color = dotcols,
      size = MSY,
      alpha = (MSY),
      key   = IdOrig
    )) + 
    geom_point(
      data = summary_dat,
      aes(median_x, median_y),
      shape = 17,
      size = 4,
      color = 'Median'
    ) +
    geom_point(
      data = summary_dat,
      aes(geom_mean_msy_weight_x, geom_mean_msy_weight_y),
      shape = 15,
      size = 4,
      color = 'MSY-weighted\ngeometric mean'
    ) +
    scale_size_continuous(guide = F) + #turn off legends
    scale_alpha_continuous(guide = F, range = c(0.5, 0.9)) +
    xlab(xvar_name) +
    ylab(yvar_name) +
    theme_classic() +
    theme(text = element_text(size = 16)) + #Extend the boundaries
    scale_x_continuous(
      limits = c(-1, 4),
      breaks = seq(-1, 4, by = 0.5),
      labels = c(
        seq(-1, 2, by = 0.5),
        expression(phantom(x) >= 2.5),
        seq(3, 4, by = 0.5)
      )
    ) +
    scale_y_continuous(
      limits = c(-1, 6),
      breaks = seq(-1, 6, by = 0.5),
      labels = c(
        seq(-1, 3.5, by = 0.5),
        expression(phantom(x) >= 4),
        seq(4.5, 6, by = 0.5)
      )
    ) +
    coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 4)) #Trim the boundaries
}
  
## Kobe without density ##
if(density == F) {
    kobe <- dat %>%
      ggplot(aes(xvar, yvar)) + #general aesthetic
      geom_hline(aes(yintercept = 1), linetype = 'longdash') +
      geom_vline(aes(xintercept = 1), linetype = 'longdash') +
      # plot points
      geom_point(aes(
        xvar,
        yvar,
        color = dotcols,
        size = MSY,
        alpha = (MSY),
        key   = IdOrig
      )) + 
      geom_point(
        data = summary_dat,
        aes(median_x, 
            median_y),
        shape = 17,
        size = 4
      ) +
      geom_point(
        data = summary_dat,
        aes(geom_mean_msy_weight_x,
            geom_mean_msy_weight_y),
        shape = 15,
        size = 4
      ) +
      scale_size_continuous(guide = F) + #turn off legends
      scale_alpha_continuous(guide = F, range = c(0.5, 0.9)) +
      xlab(xvar_name) +
      ylab(yvar_name) +
      theme_classic() +
      theme(text = element_text(size = 16)) + #Extend the boundaries
      scale_x_continuous(
        limits = c(-1, 4),
        breaks = seq(-1, 4, by = 0.5),
        labels = c(
          seq(-1, 2, by = 0.5),
          expression(phantom(x) >= 2.5),
          seq(3, 4, by = 0.5)
        )
      ) +
      scale_y_continuous(
        limits = c(-1, 6),
        breaks = seq(-1, 6, by = 0.5),
        labels = c(
          seq(-1, 3.5, by = 0.5),
          expression(phantom(x) >= 4),
          seq(4.5, 6, by = 0.5)
        )
      ) +
      coord_cartesian(xlim = c(0, 2.5), ylim = c(0, 4)) #Trim the boundaries
  }  

  return(kobe)
}