TrialistNof1
============

A JAGS based DPU

## Use in R

A DPU is simply an R function that is designed to be called either locally or remotely. To install:

    #install
    library(devtools)
    install_github("TrialistNof1", "jservadio")
    
To call the DPU locally in R using the example dataset included in the repo:

    library(jsonlite)
    library(TrialistNof1)

    payload <- fromJSON("example_data.json")
    res <- do.call(wrap, payload)
    output <- toJSON(res)

To call this DPU remotely the client needs to perform a HTTP POST request as exemplified with curl:

    curl -H "Content-Type: application/json" --data @example_data.json \ 
    https://pilots.ohmage.org/ocpu/github/jservadio/TrialistNof1/R/wrap/json
    
