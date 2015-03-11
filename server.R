
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(SPARQL)
source("endpoints.R")

shinyServer(function(input, output) {
  interm <- reactive({
    words <- translateJ2E(strsplit(input$term, " +")[[1]])
    interm <- paste("(",paste(words,collapse="|"),")",sep="")
  })
    
  translateJ2E <- function(terms){
    trterms <- vector()
    for(term in terms) {
      if(grepl("[:alnum:]+",term))
        trterms <- append(trterms, term)
      else {
        query <- paste("
PREFIX lsd: <http://purl.jp/bio/10/lsd/ontology/201209#>
PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT DISTINCT ?synonymLabelJa ?synonymLabelEn WHERE {
  ?code1 rdfs:label \"",term, "\"@ja ;
    skos:closeMatch ?synonymJa .
  ?synonymJa rdfs:label ?synonymLabelJa ;
    lsd:hasEntry [lsd:hasEnglishTranslationOf ?synonymEn].
  ?synonymEn rdfs:label ?synonymLabelEn .
  FILTER(lang(?synonymLabelJa) = \"ja\")
  FILTER(lang(?synonymLabelEn) = \"en\")
} ORDER BY ?synonymLabelJa ?synonymLabelEn", sep="")
        ns <- c(
        'lsd','<http://purl.jp/bio/10/lsd/ontology/201209#>')
        dtr <- SPARQL(url=endpoint_lsd,
                     query=query,  ns=ns)
        if(nrow(dtr$results)>0) 
          trterms <- append(trterms, gsub("(\"|@en)","",dtr$results$synonymLabelEn))
      }
    }
    return(trterms)
  }
  
  dto_terminology <- list(createdRow=I("function(nRow, aData,index) {
                $('td:eq(0)',nRow).html(aData[0].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(1)',nRow).html(aData[1].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(2)',nRow).html(aData[2].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(3)',nRow).html(aData[3].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(4)',nRow).html(aData[4].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
              }"), 
              autoWidth=FALSE,
              columns = list(list(sWidth="10%"), list(sWidth="10%"), list(sWidth="10%"), list(sWidth="60%"), list(sWidth="10%")))
                                       
  dto_standard <- list(createdRow=I("function(nRow, aData,index) {
                $('td:eq(0)',nRow).html(aData[0].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(1)',nRow).html(aData[1].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(2)',nRow).html(aData[2].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(3)',nRow).html(aData[3].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
              }"), 
              autoWidth=FALSE,
              columns = list(list(sWidth="10%"), list(sWidth="10%"), list(sWidth="40%"), list(sWidth="40%")))

  dto_validation <- list(createdRow=I("function(nRow, aData,index) {
                $('td:eq(0)',nRow).html(aData[0].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(1)',nRow).html(aData[1].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
                $('td:eq(2)',nRow).html(aData[2].replace(/&lt;/g,'<').replace(/&gt;/g,'>')); 
              }"), 
                       autoWidth=FALSE,
                       columns = list(list(sWidth="10%"), list(sWidth="10%"), list(sWidth="70%")))
  
  
  output$searchResult1 <- renderDataTable({
    
    query <- paste("
PREFIX mms: <http://rdf.cdisc.org/mms#>
PREFIX cts:<http://rdf.cdisc.org/ct/schema#>
SELECT ?id ?valuedomain ?SubmissionValue ?Definition ?Synonyms ?domainsubv ?nciCode 
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
    d1 <- SPARQL(url=endpoint_term,
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
        dispcode <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", dispcode,ignore.case=TRUE)
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
    if(interm() != "()") {
      d1$results$SubmissionValue <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d1$results$SubmissionValue,ignore.case=TRUE)
      d1$results$Definition <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d1$results$Definition,ignore.case=TRUE)
      d1$results$Synonyms <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d1$results$Synonyms,ignore.case=TRUE)
    }
    d1$results
  },options=dto_terminology)
  #, sanitize.text.function = function(x) x)
  
  output$searchResult2 <- renderDataTable({
    query <- paste("
PREFIX mms: <http://rdf.cdisc.org/mms#>
PREFIX cts:<http://rdf.cdisc.org/ct/schema#>
PREFIX cdiscs:<http://rdf.cdisc.org/std/schema#>
SELECT ?id ?DataElementName ?DataElementDescription ?QuestionOrAssumption
WHERE
{
  ?id  mms:dataElementDescription ?DataElementDescription .
  ?id  mms:dataElementName ?DataElementName .
  OPTIONAL 
  { ?id cdiscs:questionText ?QuestionOrAssumption .}
  OPTIONAL
  { ?aid  cdiscs:about ?id .
  ?aid  cdiscs:assumptionText ?QuestionOrAssumption . } 
  FILTER ( regex(?DataElementDescription,'", interm(),"','i') || regex(?DataElementName, '", interm(),"','i') || regex(?QuestionOrAssumption, '", interm(), "','i')) .
} LIMIT 300",sep="")
    ns <- c(
      'sdtmig-3-1-3', '<http://rdf.cdisc.org/std/sdtmig-3-1-3#>',
      'cdash-1-1','<http://rdf.cdisc.org/std/cdash-1-1#>',
      'sdtm-1-3', '<http://rdf.cdisc.org/std/sdtm-1-3#>'
    )
    
    d2 <- SPARQL(url=endpoint_std,
                 query=query,  ns=ns)
    if(interm() != "()") {
      #write(interm(),stderr())
      d2$results$DataElementName <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d2$results$DataElementName,ignore.case=TRUE)
      d2$results$DataElementDescription <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d2$results$DataElementDescription,ignore.case=TRUE)
      d2$results$QuestionOrAssumption <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d2$results$QuestionOrAssumption,ignore.case=TRUE)
    }
    
    qst <- d2$results$QuestionOrAssumption
    qst[is.na(qst)] <- ""
    if(nrow(d2$results)>0) 
      results <- data.frame(Std=sapply(strsplit(d2$results$id, ":"), function(x){return(x[1])}),
                             Name=d2$results$DataElementName, 
                             Description=d2$results$DataElementDescription,
                             "QuestionOrAssumptionText"=qst,
                          stringsAsFactors=FALSE)
    else
      results <- data.frame(Std=character(0),  Name=character(0), Description=character(0),QuestionOrAssumptionText=character(0))
    #cat(str(results))
    results
    },options=dto_standard)
  
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
    d3 <- SPARQL(url=endpoint_config,
                 query=query,  ns=ns)
    if(interm() != "()") {
      #write(interm(),stderr())
      d3$results$Variable <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d3$results$Variable,ignore.case=TRUE)
      d3$results$RuleDescription <- gsub(interm(),"<span style='background-color: #FFFF00'>\\1</span>", d3$results$RuleDescription,ignore.case=TRUE)
    }
    d3$results
  },options=dto_validation)
  
  
})
