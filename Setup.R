## Default repo
local({r <- getOption("repos")
       r["CRAN"] <- "https://cran.r-project.org" 
       options(repos=r)
})

install.packages('IRkernel')
install.packages('jsonlite')
install.packages('rlang')
install.packages("languageserver")
IRkernel::installspec(user = FALSE)
