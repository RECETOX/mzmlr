# Internal XML handling - tries xml2 first, falls back to base R

#' Read XML file
#'
#' Internal function to read XML files using xml2 or pure R fallback.
#'
#' @param path File path
#' @param encoding Character encoding
#' @return XML document object or parsed structure
#' @keywords internal
.read_xml <- function(path, encoding = "UTF-8") {
  # Try xml2 first
  if (requireNamespace("xml2", quietly = TRUE)) {
    return(xml2::read_xml(path, encoding = encoding))
  }

  # Fall back to base R XML parsing
  .parse_xml_base(path)
}

#' Parse XML using base R
#'
#' Internal function for parsing XML without external dependencies.
#'
#' @param path File path
#' @return Parsed XML structure with class "mzml_xml_base"
#' @keywords internal
.parse_xml_base <- function(path) {
  # Read file content
  content <- readLines(path, warn = FALSE, encoding = "UTF-8")
  content <- paste(content, collapse = "\n")

  # Create simple XML tree structure
  xml_tree <- list(
    content = content,
    path = path,
    root = NULL
  )
  class(xml_tree) <- "mzml_xml_base"

  # Extract root element info
  root_match <- regmatches(content, regexpr('<mzML[^>]*>', content, ignore.case = TRUE))
  if (length(root_match) > 0 && nchar(root_match[1]) > 0) {
    xml_tree$root <- .parse_attributes(root_match[1])
  }

  xml_tree
}

#' Parse attributes from an XML tag
#'
#' Internal helper to extract attributes from an XML tag string.
#'
#' @param tag XML tag string
#' @return Named list of attributes
#' @keywords internal
.parse_attributes <- function(tag) {
  attrs <- list()

  # Match name="value" patterns
  attr_pattern <- '([a-zA-Z_][a-zA-Z0-9_]*)\\s*=\\s*"([^"]*)"'
  matches <- gregexpr(attr_pattern, tag, ignore.case = TRUE, perl = TRUE)

  if (matches[[1]][1] != -1) {
    # Use regmatches with capture groups
    all_matches <- regmatches(tag, matches)[[1]]

    for (attr_str in all_matches) {
      # Extract name and value using sub
      name <- sub('^"|"$', '', sub('.*?([a-zA-Z_][a-zA-Z0-9_]*)\\s*=.*', '\\1', attr_str))
      value <- sub('.*=\\s*"([^"]*)".*', '\\1', attr_str)

      if (nchar(name) > 0) {
        attrs[[name]] <- value
      }
    }
  }

  # Also try single quotes
  attr_pattern_sq <- "([a-zA-Z_][a-zA-Z0-9_]*)\\s*=\\s*'([^']*)'"
  matches_sq <- gregexpr(attr_pattern_sq, tag, ignore.case = TRUE, perl = TRUE)

  if (matches_sq[[1]][1] != -1) {
    all_matches <- regmatches(tag, matches_sq)[[1]]

    for (attr_str in all_matches) {
      name <- sub('.*=\\s*\'([^\']*)\'.*', '\\1', attr_str)
      name <- sub('^([a-zA-Z_][a-zA-Z0-9_]*)\\s*=.*', '\\1', name)
      value <- sub('.*=\\s*\'([^\']*)\'.*', '\\1', attr_str)

      if (nchar(name) > 0 && !exists(name, where = attrs)) {
        attrs[[name]] <- value
      }
    }
  }

  attrs
}

#' Get root element
#'
#' Internal function to get the root element of an XML document.
#'
#' @param doc XML document
#' @return Root element
#' @keywords internal
.xml_root <- function(doc) {
  if (inherits(doc, "xml_document")) {
    return(xml2::xml_root(doc))
  }

  # Base R fallback
  if (inherits(doc, "mzml_xml_base")) {
    return(doc)
  }

  doc
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
  if (inherits(node, "xml_node")) {
    return(xml2::xml_attr(node, name))
  }

  if (inherits(node, "mzml_xml_base") && !is.null(node$root)) {
    val <- node$root[[name]]
    return(if (is.null(val)) NA_character_ else val)
  }

  NA_character_
}

#' Find all matching elements
#'
#' Internal function to find all elements matching an XPath expression.
#'
#' @param doc_or_node XML document or node
#' @param xpath XPath expression (simplified support)
#' @param ns Namespace prefix mapping
#' @return List of matching nodes
#' @keywords internal
.xml_find_all <- function(doc_or_node, xpath, ns = NULL) {
  if (inherits(doc_or_node, "xml_nodeset")) {
    return(xml2::xml_find_all(doc_or_node, xpath, ns = ns))
  }

  if (inherits(doc_or_node, "xml_node")) {
    return(xml2::xml_find_all(doc_or_node, xpath, ns = ns))
  }

  # Base R fallback - simplified xpath support
  .find_elements_base(doc_or_node$content, xpath)
}

#' Find first matching element
#'
#' Internal function to find the first element matching an XPath expression.
#'
#' @param doc_or_node XML document or node
#' @param xpath XPath expression
#' @param ns Namespace prefix mapping
#' @return First matching node or empty result
#' @keywords internal
.xml_find_first <- function(doc_or_node, xpath, ns = NULL) {
  if (inherits(doc_or_node, "xml_nodeset")) {
    result <- xml2::xml_find_first(doc_or_node, xpath, ns = ns)
    return(result)
  }

  if (inherits(doc_or_node, "xml_node")) {
    result <- xml2::xml_find_first(doc_or_node, xpath, ns = ns)
    return(result)
  }

  # Base R fallback
  all_matches <- .find_elements_base(doc_or_node$content, xpath)
  if (length(all_matches) == 0) {
    # Return empty nodeset-like object
    return(structure(list(), class = "xml_nodeset"))
  }
  all_matches[[1]]
}

#' Base R element finder (simplified xpath support)
#'
#' Internal function for basic element finding without full XPath support.
#'
#' @param content XML content
#' @param xpath XPath expression
#' @return List of matched element strings
#' @keywords internal
.find_elements_base <- function(content, xpath) {
  # Simplified xpath parser for common patterns like ".//ns:element"
  # Extract element name from xpath
  elem_pattern <- "(?:\\w+:)?(\\w+)(?:\\[|>|/|$|\\s)"
  elem_match <- regexec(elem_pattern, xpath, ignore.case = TRUE)

  if (elem_match[[1]][1] == -1) {
    return(list())
  }

  elem_name <- regmatches(xpath, elem_match)[[1]][2]
  if (is.na(elem_name) || elem_name == "") {
    return(list())
  }

  # Find all occurrences of this element
  tag_pattern <- sprintf('<%s[^>]*/?>', elem_name)
  matches <- gregexpr(tag_pattern, content, ignore.case = TRUE)

  if (matches[[1]][1] == -1) {
    return(list())
  }

  # Return match positions (simplified - just return that we found them)
  matches_list <- regmatches(content, matches)[[1]]
  lapply(matches_list, function(x) list(tag = x))
}

#' Get XML text content
#'
#' Internal function to get text content from an XML node.
#'
#' @param node XML node
#' @return Text content
#' @keywords internal
.xml_text <- function(node) {
  if (inherits(node, "xml_node") || inherits(node, "xml_nodeset")) {
    return(xml2::xml_text(node))
  }

  if (is.list(node) && !is.null(node$tag)) {
    return("")
  }

  ""
}

#' Get XML children
#'
#' Internal function to get child nodes of an XML node.
#'
#' @param node XML node
#' @return Child nodes
#' @keywords internal
.xml_children <- function(node) {
  if (inherits(node, "xml_node")) {
    return(xml2::xml_children(node))
  }

  list()
}

#' Get XML node name
#'
#' Internal function to get the name of an XML node.
#'
#' @param node XML node
#' @return Node name
#' @keywords internal
.xml_name <- function(node) {
  if (inherits(node, "xml_node")) {
    return(xml2::xml_name(node))
  }

  if (is.list(node) && !is.null(node$tag)) {
    # Extract name from tag
    match <- regexec('<(\\w+)', node$tag)
    if (match[[1]][1] != -1) {
      return(regmatches(node$tag, match)[[1]][2])
    }
  }

  ""
}
