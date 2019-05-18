library(ggplot2)
library(hexSticker)

# blank plot
p <- ggplot(aes(x = mpg, y = wt), data = mtcars)
p <- p + theme_void() + theme_transparent()

# laziest thing I could think of
sticker(p, package = "tidylsr", filename = here::here("other", "tidylsr.png"),
        p_color = "white", p_size = 20, h_color = "white",
        h_fill = "black", p_y = 1)
