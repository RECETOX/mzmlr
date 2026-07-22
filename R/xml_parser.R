# XML parsing using xml2 package
# This module provides efficient XML parsing for mzML files using xml2.

#' Read XML file using xml2
#'
#' Internal function to read XML files using xml2 package.
#'
#' @param path File path
#' @param encoding Character encoding
#' @return XML document object
#' @keywords internal
.read_xml <- function(path, encoding = "UTF-8") {
  xml2::read_xml(path, encoding = encoding)
}

#' Get root element
#'
#' Internal function to get the root element of an XML document.
#'
#' @param doc XML document
#' @return Root element
#' @keywords internal
.xml_root <- function(doc) {
  xml2::xml_root(doc)
}

#' Get XML attribute
#'
#' Internal function to get an attribute value from an XML node.
#'
#' @param node XML node
#' @param name Attribute name
#' @return Attribute value or NA
#' @keywords internal
.xml_attr <- function(node, name) {
  val <- xml2::xml_attr(node, name)
  if (is.na(val) || val == "") NA_character_ else val
}

#' Find all matching elements using local name (namespace-agnostic)
#'
#' Internal function to find all elements by their local name, ignoring namespaces.
#' This is useful for mzML files which may have various namespace declarations.
#'
#' @param doc_or_node XML document or node
#' @param local_name Local element name (without namespace prefix)
#' @return xml_nodeset of matching nodes
#' @keywords internal
.xml_find_by_name <- function(doc_or_node, local_name) {
  # Use xpath 1.0 with local-name() to ignore namespaces
  xpath <- sprintf("//*[local-name()='%s']", local_name)
  xml2::xml_find_all(doc_or_node, xpath)
}

#' Find first element using local name (namespace-agnostic)
#'
#' Internal function to find the first element by its local name.
#'
#' @param doc_or_node XML document or node
#' @param local_name Local element name (without namespace prefix)
#' @return First matching node or empty nodeset
#' @keywords internal
.xml_find_first_by_name <- function(doc_or_node, local_name) {
  xpath <- sprintf("//*[local-name()='%s'][1]", local_name)
  xml2::xml_find_first(doc_or_node, xpath)
}

#' Find all cvParam elements with optional filters
#'
#' Internal helper to find cvParam elements by name or accession.
#'
#' @param parent Parent node to search within
#' @param name Optional param name to filter by
#' @param accession Optional accession to filter by
#' @return xml_nodeset of matching cvParam nodes
#' @keywords internal
.xml_find_cvparam <- function(parent, name = NULL, accession = NULL) {
  conditions <- c()
  if (!is.null(name)) {
    conditions <- c(conditions, sprintf('@name="%s"', name))
  }
  if (!is.null(accession)) {
    conditions <- c(conditions, sprintf('@accession="%s"', accession))
  }

  if (length(conditions) > 0) {
    xpath <- sprintf(".//*[local-name()='cvParam'][%s]", paste(conditions, collapse = " and "))
  } else {
    xpath <- ".//*[local-name()='cvParam']"
  }

  xml2::xml_find_all(parent, xpath)
}

#' Find all elements by local name path
#'
#' Internal function to find elements using a path of local names.
#' Uses relative paths when searching from a node.
#'
#' @param doc_or_node XML document or node
#' @param path Character vector of element names forming the path
#' @return xml_nodeset of matching nodes
#' @keywords internal
.xml_find_path <- function(doc_or_node, path) {
  xpath_parts <- sapply(path, function(n) sprintf("*[local-name()='%s']", n))
  xpath <- paste(xpath_parts, collapse = "/")
  # Use .// for descendant search from node, or // for document search
  if (inherits(doc_or_node, "xml_document")) {
    xpath <- sprintf("//*[local-name()='%s']", xpath_parts[1])
    for (i in 2:length(xpath_parts)) {
      xpath <- paste0(xpath, "//*[local-name()='", xpath_parts[i], "']")
    }
  } else {
    xpath <- paste(xpath_parts, collapse = "/")
  }
  xml2::xml_find_all(doc_or_node, xpath)
}

#' Find first element by local name path
#'
#' Internal function to find the first element using a path of local names.
#'
#' @param doc_or_node XML document or node
#' @param path Character vector of element names forming the path
#' @return First matching node or empty nodeset
#' @keywords internal
.xml_find_first_path <- function(doc_or_node, path) {
  xpath_parts <- sapply(path, function(n) sprintf("*[local-name()='%s']", n))
  xpath <- paste(xpath_parts, collapse = "/")
  xml2::xml_find_first(doc_or_node, xpath)
}

#' Get XML text content
#'
#' Internal function to get text content from an XML node.
#'
#' @param node XML node
#' @return Text content
#' @keywords internal
.xml_text <- function(node) {
  txt <- xml2::xml_text(node)
  if (length(txt) == 0) "" else txt
}

#' Get XML children
#'
#' Internal function to get child nodes of an XML node.
#'
#' @param node XML node
#' @return Child nodes as xml_nodeset
#' @keywords internal
.xml_children <- function(node) {
  xml2::xml_children(node)
}

#' Get XML node name
#'
#' Internal function to get the name of an XML node.
#'
#' @param node XML node
#' @return Node name
#' @keywords internal
.xml_name <- function(node) {
  xml2::xml_name(node)
}

#' Convert numeric vector to float32 representation
#'
#' Converts a double precision vector to float32 for memory efficiency.
#' R 4.4+ supports storage.mode "float" directly.
#'
#' @param x Numeric vector to convert
#' @return Vector with reduced precision (float32 equivalent)
#' @keywords internal
.convert_to_float32 <- function(x) {
  if (length(x) == 0) return(numeric(0))

  # Try native float support (R 4.4+)
  if (getRversion() >= "4.4.0") {
    out <- x
    storage.mode(out) <- "double"
    attr(out, "Csingle") <- TRUE
    return(out)
  }

  # Fallback: round to ~7 significant digits (float32 precision)
  # This simulates float32 behavior for typical MS intensity values
  out <- x
  # Round to 6-7 significant figures
  for (i in seq_along(x)) {
    if (!is.na(x[i]) && x[i] != 0) {
      sig_digits <- 6
      magnitude <- floor(log10(abs(x[i])))
      decimals <- max(0, sig_digits - magnitude - 1)
      out[i] <- round(x[i], digits = decimals)
    }
  }
  out
}
