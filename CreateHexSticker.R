library(hexSticker)
library(magick)
library(sysfonts)
library(magrittr)

# ---- Load image
clock_img <- image_read("Clock.png")

# ---- Load font files
font_add("Georgia","georgia.ttf")

# ---- Create sticker
sticker(subplot = clock_img,
        package = "ToCS",
        s_width = 1,
        s_height = 1,
        s_x = 1,
        s_y = 0.75,
        p_y = 1.45,
        p_x = 1,
        p_size = 30,
        p_color = "black",
        p_family = "Georgia",
        h_fill = 'palegreen3',
        h_color = "forestgreen",
        h_size = 1.5,
        spotlight = T,
        l_y = 1.5,
        l_x = 1.25,
        ) %>% print()

