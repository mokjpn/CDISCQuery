
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(SPARQL)

shinyServer(function(input, output) {
  interm <- reactive({
    words <- strsplit(input$term, " +")[[1]]
    if(length(words)>1)
      interm <- paste(words,collapse=".*")
    else
      interm <- paste(".*",input$term,".*",sep="")
  })
  output$searchResult1 <- renderTable({
    query <- paste("
PREFIX mms: <http://rdf.cdisc.org/mms#>
PREFIX cts:<http://rdf.cdisc.org/ct/schema#>
SELECT  ?id ?SubmissionValue ?Definition ?domainsubv
WHERE
{
  ?id cts:cdiscDefinition ?Definition FILTER regex(?Definition,'", interm(),"','i') .
  ?id cts:cdiscSubmissionValue ?SubmissionValue .
  ?id mms:inValueDomain ?valuedomain .
  ?valuedomain cts:cdiscSubmissionValue ?domainsubv .
                   } LIMIT 300",sep="")
  ns <- c(
    'cdash','<http://rdf.cdisc.org/cdash-terminology#>',
    'sdtm', '<http://rdf.cdisc.org/sdtm-terminology#>',
    'glossary', '<http://rdf.cdisc.org/glossary-terminology#>',
    'coa','<http://rdf.cdisc.org/coa-terminology#>',
    'mms','<http://rdf.cdisc.org/mms#>',
    'cts','<http://rdf.cdisc.org/ct/schema#>')
    d1 <- SPARQL(url="http://axis.md.tsukuba.ac.jp/ql/term/query",
                query=query,  ns=ns)
    ids <- unlist(sapply(1:nrow(d1$results), function(x) {
      id <- d1$results[x, "id"] 
      subv <- d1$results[x,"domainsubv"]
      idsplit <- strsplit(id, '(\\.|:)')[[1]]
      code1 <- idsplit[2]
      category <- idsplit[1]
      if(category == "sdtm")
        link <- paste('<a href="http://evs.nci.nih.gov/ftp1/CDISC/SDTM/SDTM%20Terminology.html#CL.', code1, ".", subv, '">sdtm:', code1, ".", subv, "</a>", sep="")
      if(category == "cdash")
        link <- paste('<a href="http://evs.nci.nih.gov/ftp1/CDISC/SDTM/CDASH%20Terminology.html#CL.', code1, ".", subv, '">cdash:', code1, ".", subv, "</a>", sep="")
      if(category == "glossary")
        link <- paste('<a href="http://evs.nci.nih.gov/ftp1/CDISC/Clinical_Data_Element_Glossary/Clinical%20Data%20Element%20Glossary.html#CL.', code1, ".", subv, '">glossary:', code1, ".", subv, "</a>", sep="")
      if(category == "coa")
        link <- paste('<a href="http://evs.nci.nih.gov/ftp1/CDISC/COA/COA%20Terminology.html#CL.', code1, ".", subv, '">coa:', code1, ".", subv, "</a>", sep="")
      return(link)
    }))
    d1$results$id <- ids
    d1$results
  }, sanitize.text.function = function(x) x)

  output$searchResult2 <- renderTable({
    query <- paste("
PREFIX mms: <http://rdf.cdisc.org/mms#>
PREFIX cts:<http://rdf.cdisc.org/ct/schema#>
SELECT ?id ?DataElementName ?DataElementDescription
WHERE
{
  ?id  mms:dataElementDescription ?DataElementDescription FILTER regex(?DataElementDescription,'", interm(),"','i') .
  ?id  mms:dataElementName ?DataElementName
} LIMIT 300",sep="")
ns <- c(
  'sdtmig-3-1-3', '<http://rdf.cdisc.org/std/sdtmig-3-1-3#>',
  'cdash-1-1','<http://rdf.cdisc.org/std/cdash-1-1#>',
  'sdtm-1-3', '<http://rdf.cdisc.org/std/sdtm-1-3#>'
)

    d2 <- SPARQL(url="http://axis.md.tsukuba.ac.jp/ql/phuse/query",
                 query=query,  ns=ns)
    d2$results
  })

output$searchResult3 <- renderTable({
  query <- paste("
PREFIX my: <http://www.okada.jp.org/schema/Myconfig2rdf#>
SELECT ?ruleid ?Variable ?RuleDescription
WHERE
{
  ?ruleid my:description ?RuleDescription FILTER regex(?RuleDescription,'", interm(),"','i') .
  ?ruleid my:target ?Variable .
} LIMIT 300",sep="")
  ns <- c(
    'config-sdtm-3.2','<http://www.okada.jp.org/schema/config2rdf#>')
  d3 <- SPARQL(url="http://axis.md.tsukuba.ac.jp/ql/config/query",
               query=query,  ns=ns)
  d3$results
})


})
