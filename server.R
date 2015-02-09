
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
  
  intermMatch <- reactive({
    words <- strsplit(input$term, " +")[[1]]
    if(length(words)>1)
      interm <- paste("(",paste(words,collapse="|"),")",sep="")
    else
      interm <- paste("(",input$term,")",sep="")
  })
  
  dto <- list(createdRow=I("function(nRow, aData,index) {
                $('td:eq(0)',nRow).html(aData[0].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(1)',nRow).html(aData[1].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(2)',nRow).html(aData[2].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(3)',nRow).html(aData[3].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
              }"), 
              autoWidth=FALSE,
              columns = list(list(sWidth="10%"), list(sWidth="10%"), list(sWidth="70%"), list(sWidth="10%")))
                                       
  
  
  output$searchResult1 <- renderDataTable({
    
    query <- paste("
PREFIX mms: <http://rdf.cdisc.org/mms#>
PREFIX cts:<http://rdf.cdisc.org/ct/schema#>
SELECT  ?id ?SubmissionValue ?Definition ?Synonyms ?domainsubv ?nciCode 
WHERE
{
  ?id cts:cdiscDefinition ?Definition .
  ?id cts:cdiscSubmissionValue ?SubmissionValue .
  ?id cts:cdiscSynonyms ?Synonyms .
  ?id cts:nciCode ?nciCode .
  FILTER ( regex(?nciCode,'", interm(),"','i') ||regex(?Definition,'", interm(),"','i') || regex(?Synonyms, '" , interm(), "','i') || regex(?SubmissionValue, '", interm(),"','i')) .
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
    if(nrow(d1$results)>0) {
      ids <- unlist(sapply(1:nrow(d1$results), function(x) {
        id <- d1$results[x, "id"] 
        subv <- d1$results[x,"domainsubv"]
        idsplit <- strsplit(id, '(\\.|:)')[[1]]
        category <- idsplit[1]
        code1 <- idsplit[2]
        code2 <- idsplit[3]
        linkcode <- paste(code1, ".", subv, sep="")
        dispcode <- paste(category, ":", subv, ".", code2, sep="")
        dispcode <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", dispcode,ignore.case=TRUE)
        link <- ""        
        if(category == "sdtm")
          link <- "http://evs.nci.nih.gov/ftp1/CDISC/SDTM/SDTM%20Terminology.html#CL."
        if(category == "cdash")
          link <- "http://evs.nci.nih.gov/ftp1/CDISC/SDTM/CDASH%20Terminology.html#CL."
        if(category == "glossary")
          link <- "http://evs.nci.nih.gov/ftp1/CDISC/Clinical_Data_Element_Glossary/Clinical%20Data%20Element%20Glossary.html#CL."
        if(category == "coa")
          link <- "http://evs.nci.nih.gov/ftp1/CDISC/COA/COA%20Terminology.html#CL."
        cell <- paste('<a href="', link, linkcode, '">', dispcode, '</a>', sep="")
        return(cell)
      }))
      d1$results$id <- ids
    }
    d1$results$domainsubv <- NULL
    d1$results$nciCode <- NULL
    if(intermMatch() != "()") {
      d1$results$SubmissionValue <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", d1$results$SubmissionValue,ignore.case=TRUE)
      d1$results$Definition <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", d1$results$Definition,ignore.case=TRUE)
      d1$results$Synonyms <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", d1$results$Synonyms,ignore.case=TRUE)
    }
    d1$results
  },options=dto)
  #, sanitize.text.function = function(x) x)
  
  output$searchResult2 <- renderDataTable({
    query <- paste("
PREFIX mms: <http://rdf.cdisc.org/mms#>
PREFIX cts:<http://rdf.cdisc.org/ct/schema#>
SELECT ?id ?DataElementName ?DataElementDescription
WHERE
{
  ?id  mms:dataElementDescription ?DataElementDescription .
  ?id  mms:dataElementName ?DataElementName .
  FILTER ( regex(?DataElementDescription,'", interm(),"','i') || regex(?DataElementName, '", interm(),"','i')) .
} LIMIT 300",sep="")
    ns <- c(
      'sdtmig-3-1-3', '<http://rdf.cdisc.org/std/sdtmig-3-1-3#>',
      'cdash-1-1','<http://rdf.cdisc.org/std/cdash-1-1#>',
      'sdtm-1-3', '<http://rdf.cdisc.org/std/sdtm-1-3#>'
    )
    
    d2 <- SPARQL(url="http://axis.md.tsukuba.ac.jp/ql/phuse/query",
                 query=query,  ns=ns)
    if(intermMatch() != "()") {
      write(intermMatch(),stderr())
      d2$results$DataElementName <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", d2$results$DataElementName,ignore.case=TRUE)
      d2$results$DataElementDescription <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", d2$results$DataElementDescription,ignore.case=TRUE)
    }
    
    d2$results
  },options=dto)
  
  output$searchResult3 <- renderDataTable({
    query <- paste("
PREFIX my: <http://www.okada.jp.org/schema/Myconfig2rdf#>
SELECT ?ruleid ?Variable ?RuleDescription
WHERE
{
  ?ruleid my:description ?RuleDescription .
  ?ruleid my:target ?Variable .
  FILTER ( regex(?RuleDescription,'", interm(),"','i') || regex(?Variable, '", interm(),"','i')) .
} LIMIT 300",sep="")
    ns <- c(
      'config-sdtm-3.2','<http://www.okada.jp.org/schema/config2rdf#>')
    d3 <- SPARQL(url="http://axis.md.tsukuba.ac.jp/ql/config/query",
                 query=query,  ns=ns)
    if(intermMatch() != "()") {
      write(intermMatch(),stderr())
      d3$results$Variable <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", d3$results$Variable,ignore.case=TRUE)
      d3$results$RuleDescription <- gsub(intermMatch(),"<span style='background-color: #FFFF00'>\\1</span>", d3$results$RuleDescription,ignore.case=TRUE)
    }
    
    d3$results
  },options=dto)
  
  
})
