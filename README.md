# CDISCQuery

a Shiny web application to search CDISC terminologies using SPARQL.

## Function

This Shiny web application can be used to search keywords from CDISC standards and CDISC terminologies, and validation rules of OpenCDISC's config.xml.

## Requirement

You have to set up a SPARQL Endpoint that distributes RDF representation of CDISC standards and terminologies, and a LifeScienceDictionary RDF to translate Japanese search terms into English.

## Install

1. Edit endpoints.R to point appropriate SPARQL endpoint. Currently there is no public endpoint of CDISC standards and terminologies, but you can set up your own endpoint with Apache Jena, or other software.
You can download RDF files from their original location and import them into your endpoint.

2. Place server.R, ui.R and endpoints.R into your shiny web-app directory, and run it.

## License

This software is released under the MIT License, see LICENSE.txt.
