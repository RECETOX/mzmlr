# Utility functions for mzmlr package

#' Get XML content as string
#'
#' Internal helper to extract XML content as a string, handling both
#' xml2 documents and base R parsed XML.
#'
#' @param xml_doc XML document object
#' @return Character string of XML content
#' @keywords internal
.get_xml_content <- function(xml_doc) {
  if (inherits(xml_doc, "xml_document")) {
    # xml2 package - convert to string
    return(as.character(xml_doc))
  } else if (inherits(xml_doc, "mzml_xml_base")) {
    # Base R fallback
    return(xml_doc$content)
  }
  ""
}
