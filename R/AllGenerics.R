if(!isGeneric("summaryStats"))
  setGeneric("summaryStats",
             function(object, ...) standardGeneric("summaryStats"))

if(!isGeneric("ci"))
  setGeneric("ci", function(object, ...) standardGeneric("ci"))

if(!isGeneric("turnover"))
  setGeneric("turnover", function(object, ...) standardGeneric("turnover"))

if(!isGeneric("means"))
  setGeneric("means", function(object, ...) standardGeneric("means"))

if(!isGeneric("counts"))
  setGeneric("counts", function(object, ...) standardGeneric("counts"))

if(!isGeneric("totalCounts"))
  setGeneric("totalCounts", function(object, ...) standardGeneric("totalCounts"))

if(!isGeneric("marginals"))
  setGeneric("marginals", function(object, ...) standardGeneric("marginals"))

if(!isGeneric("naCounts"))
  setGeneric("naCounts", function(object, ...) standardGeneric("naCounts"))
