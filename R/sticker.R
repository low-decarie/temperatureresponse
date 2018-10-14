# library(hexSticker)
# require(ggplot2)
# output <- with(Emiliania_huxleyi, fitmodellist(temp=temp, rate=rate, augment=T))
# p <- qplot(data=output,x=temp, y=.fitted,colour=model, geom="line", alpha=I(0.5))
# p <- p+theme(legend.position = "none", axis.title.y = element_blank(),
#              panel.grid.major = element_blank(),
#              panel.grid.minor = element_blank(),
#              rect = element_rect(fill = "transparent", size = 0))
#
# sticker(expression(print(p)),
#         package="temperatureresponse",
#         p_size=4, s_x=1, s_y=.8, s_width=1.5, s_height=1,
#         filename="baseplot.png",
#         h_color="#FF62BC", h_fill="#A3A500")
