# F1. plot CDF of attributes of an feature
plot_feature_CDF <- function(DT){
  p1 <- ggplot(DT,aes(x = maxoU,color = class,linetype = class)) + stat_ecdf(size = 1.5) +
    xlab('Util') + ylab('') +
    coord_cartesian(xlim = c(-3,103),ylim = c(-0.05,1.05),expand = F) + 
    scale_y_continuous(breaks = seq(0,1,0.2)) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    guides(color = guide_legend(title=NULL),linetype = guide_legend(title = NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_text(size = 26,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 22),
          # axis.text.x = element_text(angle = 40,margin = margin(15)),
          axis.title = element_text(size = 24),
          
          legend.key.width = unit(4,units = 'line'),
          legend.key.height = unit(2,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(0,1),legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  return(p1)
}