##' Add a new paragraph with a Normal style
##'
##' @name body_add_normal
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_normal(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_normal <- function(x, value, style = "Normal", pos = "after") {
    body_add_par(x, value, style = style)
}

##' Add a new paragraph with a Comment style
##'
##' @name body_add_comment
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_comment(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_comment <- function(x, value, style = "Comment", pos = "after") {
    body_add_par(x, value, style = style)
}

##' Add a new paragraph with a Alert style
##'
##' @name body_add_alert
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_alert(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_alert <- function(x, value, style = "Alert", pos = "after") {
    body_add_par(x, value, style = style)
}

##' Add a new paragraph with a Verbatim style
##'
##' @name body_add_verbatim
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_verbatim(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_verbatim <- function(x, value, style = "Verbatim", pos = "after") {
    body_add_par(x, value, style = style)
}

##' Add a new paragraph with a figurereference style
##'
##' @name body_add_plotlegend
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_plotlegend(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_plotlegend <- function(x, value, style = "figurereference", pos = "after") {
    body_add_par(x, value, style = style)
}

##' Add a new paragraph with a figurereference2 style 
##'
##' @name body_add_plotref
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param depth integer, what is the depth of the current section to print
##' @param style a character string
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_plotref(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_plotref <- function (x, value, depth = 1, style = "figurereference2") {
    x <- body_add_par(x, value, style = style)    
    x <- slip_in_text(x, str = ". ", style = NULL, pos = "before")
    x <- slip_in_seqfield(x, str = "SEQ graph \\* Arabic \\s 1 \\* MERGEFORMAT", style = NULL, pos = "before")
    x <- slip_in_text(x, str = "-", style = NULL, pos = "before")
    x <- slip_in_seqfield(x, str = sprintf("STYLEREF %.0f \\s", depth), style = NULL, pos = "before")
    x <- slip_in_text(x, str = "Figure ", style = NULL, pos = "before")
    x
}

##' Add a new paragraph with a tablereference style
##'
##' @name body_add_tablegend
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_tablegend(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_tablegend <- function(x, value, style = "tablereference", pos = "after") {
    body_add_par(x, value, style = style)
}

##' Add a new paragraph with a tablereference2 style
##'
##' @name body_add_tableref
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param depth integer, what is the depth of the current section to print
##' @param style a character string
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_plotlegend2(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_tableref <- function (x, value, depth = 1, style = "tablereference2") {
    x <- body_add_par(x, value, style = style)    
    x <- slip_in_text(x, str = ". ", style = NULL, pos = "before")
    x <- slip_in_seqfield(x, str = "SEQ table \\* Arabic \\s 1 \\* MERGEFORMAT", style = NULL, pos = "before")
    x <- slip_in_text(x, str = "-", style = NULL, pos = "before")
    x <- slip_in_seqfield(x, str = sprintf("STYLEREF %.0f \\s", depth), style = NULL, pos = "before")
    x <- slip_in_text(x, str = "Tableau ", style = NULL, pos = "before")
    x
}

##' Add a title
##'
##' @name body_add_title
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param level a integer (1 to 9)
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_itemize(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_title <- function(x, value, level = 1, style = "heading", pos = "after") {
    style <- paste(style, level)
    body_add_par(x, value, style = style)
}

##' Add a list of items
##'
##' @name body_add_itemize
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param level a integer (1 to 4)
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_itemize(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_itemize <- function(x, value, level = 1, style = "Itemize", pos = "after") {
    style <- paste0(style, level)
    body_add_par(x, value, style = style)
}

##' Add a list of numerated items
##'
##' @name body_add_enumerate
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param value a character string
##' @param level a integer (1 to 4)
##' @param style a character string
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- body_add_enumerate(doc, "Coucou")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_enumerate <- function(x, value, level = 1, style = "Enumerate", pos = "after") {
    style <- paste0(style, level)
    body_add_par(x, value, style = style)
}


##' add an image into an rdocx object, with autofiting of width and height
##'
##' @name body_add_img_autofit
##' @param x the rdocx object (created with the read_docx function of officer package)
##' @param src image filename, the basename of the file must not contain any blank
##' @param style paragraph style
##' @param pos where to add the new element relative to the cursor, one of "after", "before", "on"
##' @param landscape is the image inserted in a landscape section? (default: FALSE)
##' @return
##'   a new rdocx object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' 
##' jpeg("carre.jpg", width = 300, height = 300)
##' plot(1)
##' dev.off()
##' jpeg("long.jpg", width = 300, height = 600)
##' plot(1)
##' dev.off()
##' jpeg("large.jpg", width = 600, height = 300)
##' plot(1)
##' dev.off()
##' 
##' doc <- read_docx()
##' doc <- body_add_img_autofit(doc, "carre.jpg")
##' doc <- body_add_img_autofit(doc, "long.jpg") 
##' doc <- body_add_img_autofit(doc, "large.jpg") 
##' doc <- body_end_section_portrait(doc)
##' 
##' doc <- body_add_img_autofit(doc, "carre.jpg", landscape = TRUE) 
##' doc <- body_add_img_autofit(doc, "long.jpg", landscape = TRUE) 
##' doc <- body_add_img_autofit(doc, "large.jpg", landscape = TRUE) 
##' doc <- body_end_section_landscape(doc)
##' 
##' print(doc, target = "toto.docx")
##' }
##' @keywords univar
##' @export
##' @import officer
body_add_img_autofit <- function(x, src, style = NULL, pos = "after", landscape = FALSE) {
    if (!requireNamespace("magick", quietly = TRUE)) {
        stop("Package \"magick\" needed for this function to work. Please install it.",
             call. = FALSE)
    }
    
    tmp <- docx_dim(x)
    w <- tmp$page["width"] - sum(tmp$margins[c("left", "right")])
    h <- tmp$page["height"] - sum(tmp$margins[c("top", "bottom")])

    require(magick)
    img_info <- image_info(image_read(src))
    ratio_wh <- img_info$width/img_info$height
    
    if (!landscape) {
        if (ratio_wh < 1) { # image plus longue que large => on redimensionne a la auteur de la page
            height <- h
            width <- h*ratio_wh
        } else { # image plus large que longue
            width <- w
            height <- w/ratio_wh
        }
    } else {
        if (ratio_wh <= 1) { # image plus longue que large => on redimensionne a la auteur de la page
            height <- w
            width <- w*ratio_wh
        } else { # image plus large que longue
            width <- h
            height <- h/ratio_wh
        }
    }
    
    x <- body_add_img(x = x, src = src, style = style, width = width, height = height, pos = pos)
    return(x)
}
# doc <- read_docx()
# doc <- body_add_img_autofit(doc, "../rapports/graph/AF.jpg") # image carr�e
# doc <- body_add_img_autofit(doc, "../rapports/graph/os.jpg") # image longue
# doc <- body_add_img_autofit(doc, "../rapports/graph/os2.jpg") # image large
# doc <- body_add_crosstable(doc, cross(cbind(...) ~ ., data = esoph))
# doc <- body_end_section_portrait(doc)
# 
# doc <- body_add_img_autofit(doc, "../rapports/graph/AF.jpg", landscape = TRUE) # image carr�e
# doc <- body_add_img_autofit(doc, "../rapports/graph/os.jpg", landscape = TRUE) # image longue
# doc <- body_add_img_autofit(doc, "../rapports/graph/os2.jpg", landscape = TRUE) # image large
# doc <- body_add_crosstable(doc, cross(cbind(...) ~ ., data = esoph))
# doc <- body_end_section_landscape(doc)
# 
# print(doc, target = "toto.docx")


##' add a table into an rdocx object, with autofiting of width
##'
##' @param x an rdocx object
##' @param value \code{flextable} object
##' @param align left, center (default) or right
##' @param pos where to add the flextable relative to the cursor, one of "after", "before", "on" (end of line).
##' @param split set to TRUE if you want to activate Word option 'Allow row to break across pages'.
##' @param landscape is the table inserted in a landscape section? (default: FALSE)
##' @return
##'   A new \code{rdocx} object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' library(flextable)
##' mytable <- flextable(head(iris))
##' doc <- read_docx()
##' doc <- body_add_flextable(doc, mytable)
##' doc <- body_add_break(doc)
##' doc <- body_add_flextable_autofit(doc, mytable)
##' doc <- body_end_section_portrait(doc)
##' doc <- body_add_flextable(doc, mytable)
##' doc <- body_add_break(doc)
##' doc <- body_add_flextable_autofit(doc, mytable, landscape = TRUE)
##' doc <- body_end_section_landscape(doc)
##' 
##' print(doc, target = "toto.docx")
##' }
##' @keywords univar
##' @export
##' @importFrom flextable flextable
##' @import officer
body_add_flextable_autofit <- function(x, value, align = "center", pos = "after", split = FALSE, landscape = FALSE) {
    if (!landscape) {
        tmp <- docx_dim(doc)
        w <- tmp$page["width"] - sum(tmp$margins[c("left", "right")])
    } else {
        tmp <- docx_dim(doc)
        w <- tmp$page["height"] - sum(tmp$margins[c("top", "bottom")])
    }
    
    ft <- width(value, width = w*dim(value)$widths/sum(dim(value)$widths))
    
    doc <- body_add_flextable(x, value = ft, align = align, pos = pos, split = split)
    return(doc)
}


##' Create a docx object with a specific template and informations already inserted
##'
##' @name create.officer
##' @param template either 'gerc', 'urc', or 'cephepi'
##' @param title Study title (character string)
##' @param watermark Watermark ('draft')?
##' @param acronym Study acronym (character string)
##' @param version Version number (character string)
##' @param npromo Promotion number (character string, not used with gerc template)
##' @param nct Clinical trial number (character string, not used with gerc template)
##' @param invest Name of the principal investigator (character string)
##' @param biostat Name of the biostatistician (character string)
##' @param methodo Name of the methodologist (character string, not used with gerc template)
##' @param date_lastmodif Date of last modification of the document (character string)
##' @param date_freez Date of data freezing (character string)
##' @param date_update Date of last history description update  (character string)
##' @param history History description. A list of length 1 (min) to 7 (max), each element being a list of four elements: list(version = "Version number", author = "Author", description = c("Description 1", "Description 2"), date = "Date")
##' @return
##'   a FlexTable object
##' @author David Hajage
##' @examples
##' \dontrun{
##' library(biostat2)
##' library(officer)
##' doc <- create.officer(template = 'urc',
##'                      title = 'A great study',
##'                      watermark = FALSE,
##'                      acronym = 'GREAT',
##'                      version = '1.3',
##'                      npromo = 'AOR17123',
##'                      nct = 'NCT123456',
##'                      invest = 'Casimir',
##'                      biostat = 'Goldorak',
##'                      methodo = 'Actarus',
##'                      date_lastmodif = '23/03/1979',
##'                      date_freez = '22/03/1979',
##'                      date_update = '24/03/1979',
##'                      history = list(
##'                                     list(version = '1',
##'                                          author = 'Goldorak',
##'                                          description = c('First description', 
##'                                          'Primary outcome analysis'),
##'                                          date = '28/03/1979'),
##'                                     list(version = '1.3',
##'                                          author = 'Goldorak',
##'                                          description = 'Add forgotten things',
##'                                          date = '29/03/1979')
##'                                    )
##'                     )
##' }
##' @keywords univar
##' @export
##' @import officer
create.officer <- function(template = c("gerc", "urc", "cephepi"), watermark = FALSE, title = "", acronym = "", version = "", npromo = "", nct = "", invest = "", biostat = "", methodo = "", date_lastmodif = "", date_freez = "", date_update = "", history = NULL) {

    URC <- FALSE
    if (template[1] == "urc") {
        if (watermark) {
            template.file <- system.file("templates/template_urc_draft_officer.docx", package = "biostat2")
        } else {
            template.file <- system.file("templates/template_urc_officer.docx", package = "biostat2")
        }
        URC <- TRUE
    } else if (template[1] == "cephepi") {
        if (watermark) {
            template.file <- system.file("templates/template_cephepi_draft_officer.docx", package = "biostat2")
        } else {
            template.file <- system.file("templates/template_cephepi_officer.docx", package = "biostat2")
        }
        URC <- TRUE
    } else if (template[1] == "gerc") {
        if (watermark) {
            template.file <- system.file("templates/template_gerc_draft_officer.docx", package = "biostat2")
        } else {
            template.file <- system.file("templates/template_gerc_officer.docx", package = "biostat2")
        }
    }
    doc <- read_docx(template.file)

    doc <- cursor_reach(doc, keyword = "VERSION_DATE")
    doc <- body_add_par(doc, value = paste("VERSION ", version, " du ", date_lastmodif, sep = ""), style = "Textpage1", pos = "on")

    doc <- cursor_reach(doc, keyword = "ACRONYME")
    doc <- body_add_par(doc, value = acronym, style = "Bigpage1", pos = "on")
    
    doc <- cursor_reach(doc, keyword = "TITRE")
    doc <- body_add_par(doc, value = title, style = "Smallpage1", pos = "on")
    
    if (URC) {
        doc <- cursor_reach(doc, keyword = "NPROMO")
        doc <- body_add_par(doc, value = npromo, style = "Subtextpage1", pos = "on")

        doc <- cursor_reach(doc, keyword = "NCT")
        doc <- body_add_par(doc, value = nct, style = "Subtextpage1", pos = "on")
    }

    doc <- cursor_reach(doc, keyword = "NOM_INVESTIGATEUR")
    doc <- body_add_par(doc, value = invest, style = "Subtextpage1", pos = "on")

    if (URC) {
        stat_resp <- flextable(data.frame(biostat = c(biostat, "DATE ET SIGNATURE", ""), methodo = c(methodo, "DATE ET SIGNATURE", "")), cwidth = 3.24)
        stat_resp <- height(stat_resp, i = 3, height = 1)
        stat_resp <- set_header_labels(stat_resp, biostat = "BIOSTATISTICIEN", methodo = "RESPONSABLE URC")
        stat_resp <- border(x = stat_resp, i = 2, border.bottom = fp_border(width = 0))
        stat_resp <- border(x = stat_resp, border.left = fp_border(width = 2))
        stat_resp <- border(x = stat_resp, border.right = fp_border(width = 2))
        stat_resp <- border(x = stat_resp, border.left = fp_border(width = 2), part = "header")
        stat_resp <- border(x = stat_resp, border.right = fp_border(width = 2), part = "header")
        stat_resp <- border(x = stat_resp, border.bottom = fp_border(width = 0), part = "header")
        
        stat_resp <- align(stat_resp, align = "center", part = "all" )
        doc <- cursor_reach(doc, keyword = "NOM_BIOSTAT_RESP")
        doc <- body_add_flextable(doc, value = stat_resp, align = "center", pos = "on")
    } else {
        doc <- cursor_reach(doc, keyword = "NOM_BIOSTAT_RESP")
        doc <- body_add_par(doc, value = biostat, style = "Subtexttabpage1", pos = "on")
    }

    doc <- headers_replace_text_at_bkm(x = doc, bookmark = "ENTETE_ACRONYME",value = acronym)
    doc <- headers_replace_text_at_bkm(x = doc, bookmark = "ENTETE_DATE",value = date_lastmodif)
    doc <- headers_replace_text_at_bkm(x = doc, bookmark = "ENTETE_BIOSTAT",value = biostat)

    doc <- cursor_reach(doc, keyword = "DATE_GEL")
    doc <- body_add_par(doc, value = date_freez, style = "Normal", pos = "on")

    doc <- cursor_reach(doc, keyword = "DATE_MAJ")
    doc <- body_add_par(doc, value = date_update, style = "Normal", pos = "on")
    

    if (!is.null(history)) {
        df_history <- data.frame(do.call("rbind", history))
        df_history$description <- lapply(df_history$description, paste, collapse = "\n")
        df_history <- data.frame(lapply(df_history, unlist), stringsAsFactors = FALSE)
        
        ft_history <- flextable(df_history)
        ft_history <- set_header_labels(ft_history, version = "VERSION", author = "AUTEUR", description = "DESCRIPTION DES MODIFICATIONS", date = "DATE")
        ft_history <- autofit(ft_history, add_w = 0.4)
        ft_history <- theme_booktabs(ft_history)
        ft_history <- hline(ft_history, border = fp_border(width = .75, color = "#365F91"), part = "body" )
        ft_history <- hline_top(ft_history, border = fp_border(width = 2, color = "#365F91"), part = "header")
        ft_history <- hline(ft_history, border = fp_border(width = 2, color = "#365F91"), part = "header" )
        
        ft_history <- bg(ft_history, bg = "#365F91", part = "header")
        ft_history <- color(ft_history, color = "#FFFFFF", part = "header")
        ft_history <- align(ft_history, align = "left", part = "all" )
        
        doc <- cursor_reach(doc, keyword = "HISTORY")
        tmp <- docx_dim(doc)
        w <- tmp$page["width"] - sum(tmp$margins[c("left", "right")])
        ft_history <- width(ft_history, width = w*dim(ft_history)$widths/sum(dim(ft_history)$widths))
        doc <- body_add_flextable(doc, value = ft_history, align = "center", pos = "on")
    } else {
        doc <- cursor_reach(doc, keyword = "HISTORY")
        doc <- body_remove(doc)
    }
    return(doc)
}
