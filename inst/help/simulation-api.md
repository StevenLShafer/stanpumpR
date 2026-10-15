## What it is

The Simulation API lets another program run a stanpumpR simulation without using this app. It is a web service: a program sends it a dose table, the patient's covariates and how long to simulate, and gets back the predicted plasma and effect-site concentrations of every drug, as JSON, CSV, Excel or XML. The numbers are the ones this app plots, from the same models.

It is meant for programs written by other people: an electronic health record, a research pipeline, a teaching tool, a spreadsheet. A request contains no patient identifier, only weight, height, age, sex and any optional laboratory or pharmacogenetic covariates, and the server keeps nothing between requests.

## The documentation

The full documentation, written for whoever will call the API, is a web page:

**[stanpumpr.io/api](https://stanpumpr.io/api)**

It covers the endpoints, the request and response formats, the limits, the error messages and how to run the service. Send that address to anyone who wants to integrate with stanpumpR.

## From R

The same simulation is available from R without the web service: `apiSimulate()` takes the request as a list and returns the result as data frames, and `apiWriteResult()` saves it as a CSV, Excel, JSON or XML file. See [Driving the engine from R](help:scripting).

## Related

The [TCI API](help:tci-api) is a separate service for pump controllers: it returns the next infusion rate for a target-controlled infusion, rather than a whole simulation.
