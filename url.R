
url_fun <- function(fn,title=fn) {
    cat(sprintf('<li><a href="notes/%s.html">%s</a> (<a href="notes/%s.rmd">Rmarkdown</a>)</li>\n',fn,title,fn))
}
lapply(c("intro","likelihood","optimization","linearity","challenges"),
       url_fun)
