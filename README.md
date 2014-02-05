TrialistNof1
============

A JAGS based DPU

## Use in R

A DPU is simply an R function that is designed to be called either locally or remotely.

    #install
    library(devtools)
    install_github("TrialistNof1", "openmhealth")

    #use
    library("trialist.nof1.dpu")
    mydata <- data.frame(
      Pain = c(22, 18, 21,16, 22, 15, 23, 14), 
      Fatigue = c(7,4,9,3,7,4,8,3), 
      Drowsy = c(5,5,5,4,5,5,4,5), 
      Sleep = c(4,2,4,1,4,1,4,1), 
      Thinking = c(5,2,6,1,8,4,7,6), 
      Constipation = c(10,7,10,6,9,5,10,3),
      Treat = c(0,1,0,1,0,1,0,1)
    )

    out <- wrap(data, metadata)
    
       
## Call remotely as DPU through OpenCPU

To call this DPU remotely the client needs to perform a HTTP POST request as exemplified with curl:

    curl -H "Content-Type: application/json" --data @example_post.json \             https://pilots.ohmage.org/ocpu/github/jservadio/TrialistNof1/R/wrap/json


    library(jsonlite)
    library(TrialistNof1)

    payload <- fromJSON("example_post.json")
    res <- do.call(wrap, payload)
    output <- toJSON(res)

