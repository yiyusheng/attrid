ggplot(cm,aes(x = item,y = AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  xlab('Disk Age (years)') + ylab('Annual Failure Rate (%)') + 
  guides(fill = guide_legend(title=NULL)) + 
  scale_x_continuous(breaks = floor(min(cm$item)):ceiling(max(cm$item))) +
  scale_y_continuous(breaks = floor(min(cm$AFR)):ceiling(max(cm$AFR))) +
  theme_bw() +
  theme(panel.background = element_rect(color = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
        panel.grid.major.x = element_blank(),
        
        plot.title = element_text(size = 26, face = 'bold'),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        legend.position = 'top',
        legend.background = element_rect(fill = alpha('grey',0.5)),
  )