# chunks:
# hide code and messages
# cache everything
knitr::opts_chunk$set(
  eval = TRUE, 
  echo = FALSE, collapse = FALSE,
  include = FALSE, warning = FALSE, message = FALSE,
  cache = TRUE, 
  # cache.path = here::here("slides", "facebook", "facebook_cache/"),
  # fig.path = here::here("slides", "MPMC-2019", "MPMC_figs/"),
  dpi = 100, fig.retina = 3, 
  fig.align = "center"
)

# Xaringan settings
library("xaringan")
library("xaringanthemer")


# primary <- "#cb4b16"
# secondary <- "#268bd2"

mono_light(
  base_color = primary,
  # header_font_google = google_font("Roboto"),
  header_font_google = google_font("Source Sans Pro"),
  text_font_google = google_font("Source Serif Pro"), 
  code_font_google = google_font("Fira Code"), 
  text_bold_color = secondary,
  # code_inline_background_color    = "#F5F5F5", 
  # table_row_even_background_color = "white", 
  extra_css = list(
    "h1, h2, h3" = list(
      # "font-style" = "italic", 
      "font-weight" = "bold"
    ),
    ".title-slide h1, .title-slide h3" = list(
      "font-style" = "normal"
    ),
    ".title-slide h2" = list(
      "font-style" = "normal"
    ),
    ".title-slide h3" = list(
      "font-style" = "normal", "font-weight" = "normal"
    ),
    ".remark-slide-content" = list(
      "font-size" = "26px"
    ),
    ".remark-slide-number" = list(
      "display" = "none"
    ),
    ".remark-inline-code" = list(
      "background" = "#F5F5F5", 
      # "background" = "#e7e8e2", # /* darker */
      "border-radius" = "3px", 
      "padding" = "4px"
    ),
    # ".inverse h1, .inverse h2, .inverse h3" = list(
    #   "color" = "#FFFFFF"
    # ),
    ".left-code" = list(
      "width" = "38%", "height" = "92%", "float" = "left"
    ),
    ".right-plot" = list(
      "width" = "60%", "float" = "right", "padding-left" = "1%"
    )
  )
)

img <- "slides/facebook/img"




# pagedown::chrome_print(here::here("slides", "facebook", "facebook-slides.html"))





