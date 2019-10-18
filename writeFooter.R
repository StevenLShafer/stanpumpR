# Footer Function: place a footer on a PowerPoint slide
writeFooter <<- function(X)
{
  PPTX <<- ph_with_text(PPTX, type = "ftr", str = X)
  PPTX <<- ph_with_text(PPTX, type = "dt", str = DATE)
  PPTX <<- ph_with_text(PPTX, type = "sldNum", str = SLIDE)
  SLIDE <<- SLIDE + 1
}
