setup <- function(slides) {
  paste0(
  'layout: true

  <script>
    feather.replace()
  </script>
  
  <div class="slides-footer">
  <span>
  
  <a class = "footer-icon-link" href = "https://github.com/datalorax/psych-seminar21/raw/main/anderson-psych-seminar21.pdf">
    <i class = "footer-icon" data-feather="download"></i>
  </a>
  
  <a class = "footer-icon-link" href = "https://datalorax.github.io/psych-seminar21/">
    <i class = "footer-icon" data-feather="link"></i>
  </a>
  
  <a class = "footer-icon-link" href = "https://github.com/datalorax/psych-seminar21">
    <i class = "footer-icon" data-feather="github"></i>
  </a>
  
  </span>
  </div>
  '
  )
}