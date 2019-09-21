# Place plot on a PowerPoint slide 
nextSlide <<- function (title, PLOT,footer=FOOTER)
{
  suppressWarnings(
    print(PLOT)
  )
  PPTX <<- add_slide(PPTX, layout = "Title and Content", master = MASTER)
  PPTX <<- ph_with_text(PPTX, type = "title", str = title)
  suppressWarnings(
    PPTX <<- ph_with_vg(PPTX, code = print(PLOT), type = "body")
  )
  WriteFooter(footer)
}

