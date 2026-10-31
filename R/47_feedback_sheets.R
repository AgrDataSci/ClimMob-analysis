# ................................................................
# Feedback to participants
#
# Follows the design work of Schumann et al. (2024) and its field
# validation by Mueller et al. (2025): a single-page feedback sheet and
# a separate certificate of participation, simple option letters next
# to the item names, the participant's own ranking next to the ranking
# of everyone, a best and worst mark per trait, a note that the items
# may not be released, few words, local languages, and a design that
# prints well in black and white and can be sent by phone (PNG).
#
# Everything is drawn with ggplot2 on a fixed A4 canvas, so the layout
# does not depend on Word or on the fonts of the computer.
# ................................................................

#' Find the columns with the participant id, name and location
participant_columns = function(cmdata) {

  pick = function(candidates) {
    hit = candidates[candidates %in% names(cmdata)]
    if (length(hit) > 0) hit[1] else NA_character_
  }

  loc = grep("^(REG_|registration_)(village|community|location|ward|district|county|kommune|municipality)$",
             names(cmdata), value = TRUE, ignore.case = TRUE)

  list(id = pick(c("REG_qst162", "package_id", "id")),
       name = pick(c("REG_farmername", "package_farmername", "registration_participant_name",
                     "package_participant_name", "REG_participant_name")),
       location = loc)

}

#' Everything needed for the feedback to participants
#'
#' @param cmdata the cleaned trial data (rank_dat$cmdata)
#' @param rank_dat output of organize_ranking_data()
#' @param models the Plackett-Luce models, one per trait
#' @return a list with the participants table, the long table of their
#'  answers by trait and the results of the whole trial
get_feedback_data = function(cmdata, rank_dat, models) {

  cols = participant_columns(cmdata)
  letters3 = LETTERS[seq_along(rank_dat$technologies_index)]
  ref_i = rank_dat$reference_trait_index

  ids = if (!is.na(cols$id)) as.character(cmdata[[cols$id]]) else as.character(seq_len(nrow(cmdata)))
  nm = if (!is.na(cols$name)) as.character(cmdata[[cols$name]]) else rep("", nrow(cmdata))
  nm[is.na(nm)] = ""

  loc = if (length(cols$location) > 0) {
    apply(cmdata[cols$location], 1, function(x) paste(unique(x[!is.na(x) & nzchar(x)]), collapse = ", "))
  } else rep("", nrow(cmdata))

  items = as.matrix(cmdata[, rank_dat$technologies_index])
  colnames(items) = letters3

  # worth of each item in each trait (share of 100 within the trial)
  worth = lapply(models, function(m) {
    w = stats::coef(m, log = FALSE)
    w = w[!grepl("^tie", names(w))]
    w / sum(w)
  })

  # answers of every participant, trait by trait
  answers = do.call("rbind", lapply(seq_along(rank_dat$trait_list), function(t) {

    tr_t = rank_dat$trait_list[[t]]
    best = as.character(cmdata[[tr_t$strings[1]]])
    worst = as.character(cmdata[[tr_t$strings[2]]])
    ok = tr_t$keep

    item_of = function(letter) {
      out = rep(NA_character_, length(letter))
      i = which(letter %in% letters3)
      out[i] = items[cbind(i, match(letter[i], letters3))]
      out
    }

    data.frame(participant_id = ids,
               trait = rank_dat$trait_names[t],
               is_reference_trait = t == ref_i,
               answered = ok,
               best_option = ifelse(ok, best, NA),
               best_item = ifelse(ok, item_of(best), NA),
               worst_option = ifelse(ok, worst, NA),
               worst_item = ifelse(ok, item_of(worst), NA),
               stringsAsFactors = FALSE)
  }))

  # own ranking of the reference trait and the ranking of everyone
  ref = answers[answers$is_reference_trait, ]
  w_ref = worth[[ref_i]]

  middle = vapply(seq_len(nrow(ref)), function(i) {
    if (!isTRUE(ref$answered[i])) return(NA_character_)
    setdiff(letters3, c(ref$best_option[i], ref$worst_option[i]))[1]
  }, character(1))

  points = t(apply(items, 1, function(x) {
    w = w_ref[x]
    if (any(is.na(w))) return(rep(NA_real_, length(x)))
    round(100 * w / sum(w))
  }))
  colnames(points) = paste0("points_", letters3)

  participants = data.frame(participant_id = ids,
                            name = nm,
                            location = loc,
                            item_A = items[, 1], item_B = items[, 2], item_C = items[, 3],
                            answered_reference_trait = ref$answered,
                            first = ref$best_option,
                            second = middle,
                            third = ref$worst_option,
                            points,
                            stringsAsFactors = FALSE)

  # results of the whole trial
  results = do.call("rbind", lapply(seq_along(worth), function(t) {
    w = worth[[t]]
    data.frame(trait = rank_dat$trait_names[t],
               is_reference_trait = t == ref_i,
               item = names(w),
               worth_percent = round(100 * as.numeric(w), 1),
               rank = rank(-as.numeric(w), ties.method = "min"),
               n_answers = sum(rank_dat$trait_list[[t]]$keep),
               stringsAsFactors = FALSE)
  }))

  results = results[order(results$trait != rank_dat$trait_names[ref_i], results$trait, results$rank), ]
  rownames(results) = NULL

  list(participants = participants,
       answers = answers,
       results = results,
       reference_trait = rank_dat$trait_names[ref_i],
       n_participants = sum(rank_dat$trait_list[[ref_i]]$keep),
       crop = if ("project_technology" %in% names(cmdata)) as.character(stats::na.omit(cmdata$project_technology))[1] else NA,
       project = rank_dat$projname,
       option = rank_dat$option)

}

# ................................................................
# Drawing
# ................................................................

#' Colours of the sheets, colour or black and white
sheet_palette = function(colour = TRUE) {
  if (isTRUE(colour)) {
    list(first = "#E2B33C", second = "#C9CED3", third = "#C98A4B",
         bar = "#2E4A3A", bar_text = "white", accent = "#2E4A3A", dot = "#2E4A3A")
  } else {
    list(first = "grey55", second = "grey80", third = "grey92",
         bar = "black", bar_text = "white", accent = "black", dot = "black")
  }
}

#' An empty A4 canvas, 100 wide and 141 high
sheet_canvas = function() {
  ggplot2::ggplot() +
    ggplot2::coord_fixed(xlim = c(0, 100), ylim = c(0, 141), expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                   plot.margin = ggplot2::margin(0, 0, 0, 0))
}

#' A heading bar across the page
sheet_bar = function(y, label, pal) {
  list(ggplot2::annotate("rect", xmin = 4, xmax = 96, ymin = y - 1.6, ymax = y + 1.6, fill = pal$bar),
       ggplot2::annotate("text", x = 50, y = y, label = label, colour = pal$bar_text,
                         fontface = "bold", size = 3.6))
}

#' A podium with three places
#'
#' @param labels the labels of the 1st, 2nd and 3rd places (two lines each)
sheet_podium = function(y0, labels, pal) {

  blocks = data.frame(place = c(2, 1, 3),
                      xmin = c(23, 41, 59), xmax = c(41, 59, 77),
                      ymax = y0 + c(7, 11, 4.5),
                      fill = c(pal$second, pal$first, pal$third),
                      stringsAsFactors = FALSE)

  out = list(ggplot2::annotate("rect", xmin = blocks$xmin, xmax = blocks$xmax,
                               ymin = y0, ymax = blocks$ymax,
                               fill = blocks$fill, colour = "black", linewidth = 0.3),
             ggplot2::annotate("text", x = (blocks$xmin + blocks$xmax) / 2,
                               y = y0 + (blocks$ymax - y0) / 2,
                               label = blocks$place, fontface = "bold", size = 6))

  for (k in 1:3) {
    b = blocks[blocks$place == k, ]
    out = c(out, list(ggplot2::annotate("text", x = (b$xmin + b$xmax) / 2, y = b$ymax + 2.2,
                                        label = labels[k], size = 3, lineheight = 0.9,
                                        vjust = 0)))
  }

  out

}

#' Wrap and shorten a text for the sheets
short_text = function(x, width = 22, max_lines = 2) {
  x = ifelse(is.na(x), "", as.character(x))
  vapply(x, function(s) {
    l = strwrap(s, width = width)
    if (length(l) > max_lines) {
      l = l[seq_len(max_lines)]
      l[max_lines] = paste0(substr(l[max_lines], 1, width - 3), "...")
    }
    paste(l, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

#' Ten dots, filled in proportion to the points
sheet_dots = function(x0, y, points, pal) {
  filled = round(points / 10)
  ggplot2::annotate("point", x = x0 + (0:9) * 3, y = y, shape = 21, size = 2.6,
                    fill = ifelse(0:9 < filled, pal$dot, "white"), colour = pal$dot)
}

#' The feedback sheet of one participant
#'
#' @param p one row of feedback$participants
#' @param answers the rows of feedback$answers of this participant
#' @param feedback output of get_feedback_data()
#' @param settings the report settings (date, colour, recommendations, contact)
draw_feedback_sheet = function(p, answers, feedback, settings = list()) {

  pal = sheet_palette(settings$feedback_colour %||% TRUE)
  letters3 = c("A", "B", "C")
  items = c(A = p$item_A, B = p$item_B, C = p$item_C)
  opt = function(l) paste(tr("fb_option"), l)

  g = sheet_canvas()

  # header
  g = g +
    ggplot2::annotate("text", x = 5, y = 137.5, label = format_date(settings$date %||% Sys.Date()),
                      hjust = 0, size = 3) +
    ggplot2::annotate("text", x = 95, y = 137.5, label = short_text(p$location, 35, 1),
                      hjust = 1, size = 3) +
    ggplot2::annotate("text", x = 50, y = 131, label = short_text(p$name, 34, 1),
                      fontface = "bold", size = 7.5) +
    ggplot2::annotate("text", x = 50, y = 125.5,
                      label = tr("fb_thanks"), size = 3.6) +
    sheet_bar(120, tr("fb_your_ranking"), pal)

  # own ranking
  if (isTRUE(p$answered_reference_trait)) {
    places = c(p$first, p$second, p$third)
    labels = paste0(opt(places), "\n", short_text(items[places], 18, 1))
    g = g + sheet_podium(97, labels, pal)
  } else {
    g = g + ggplot2::annotate("text", x = 50, y = 105, label = short_text(tr("fb_no_answer"), 70, 3),
                              size = 3.4, lineheight = 0.95)
  }

  # ranking of everyone, for the same three options
  g = g + sheet_bar(89, tr("fb_everyone_ranking", n = feedback$n_participants), pal)

  pts = c(A = p$points_A, B = p$points_B, C = p$points_C)

  if (all(!is.na(pts))) {
    ord = names(sort(pts, decreasing = TRUE))
    fills = c(pal$first, pal$second, pal$third)
    for (k in 1:3) {
      y = 83 - (k - 1) * 6
      l = ord[k]
      g = g +
        ggplot2::annotate("point", x = 7, y = y, shape = 21, size = 6, fill = fills[k], colour = "black") +
        ggplot2::annotate("text", x = 7, y = y, label = k, fontface = "bold", size = 3.2) +
        ggplot2::annotate("text", x = 11, y = y, label = paste0(opt(l), "  ", short_text(items[l], 22, 1)),
                          hjust = 0, size = 3.2) +
        sheet_dots(55, y, pts[[l]], pal) +
        ggplot2::annotate("text", x = 95, y = y, label = tr("fb_points", n = pts[[l]]),
                          hjust = 1, size = 3.2)
    }
    g = g + ggplot2::annotate("text", x = 50, y = 66.5, label = short_text(tr("fb_points_explained"), 95, 2),
                              size = 2.6, colour = "grey30", lineheight = 0.9)
  }

  # best and worst by trait
  recs = settings$feedback_recommendations %||% ""
  has_recs = any(nzchar(recs))
  footer_top = if (has_recs) 30 else 15

  g = g + sheet_bar(61, tr("fb_by_trait"), pal) +
    ggplot2::annotate("point", x = 55, y = 57, shape = 24, size = 2.6, fill = pal$accent) +
    ggplot2::annotate("text", x = 57.5, y = 57, label = tr("fb_best"), hjust = 0, size = 3, fontface = "bold") +
    ggplot2::annotate("point", x = 77, y = 57, shape = 25, size = 2.6, fill = "white") +
    ggplot2::annotate("text", x = 79.5, y = 57, label = tr("fb_worst"), hjust = 0, size = 3, fontface = "bold")

  a = answers[order(!answers$is_reference_trait), ]
  # rows shrink to fit every trait, down to a readable minimum
  step = min(4.1, (53 - footer_top - 2) / max(nrow(a) - 1, 1))
  step = max(step, 2.6)
  max_rows = floor((53 - footer_top - 2) / step) + 1
  a = utils::head(a, max_rows)
  txt_size = if (step < 3.5) 2.6 else 3

  for (k in seq_len(nrow(a))) {
    y = 53 - (k - 1) * step
    g = g + ggplot2::annotate("text", x = 6, y = y, label = short_text(a$trait[k], 45, 1),
                              hjust = 0, size = txt_size)
    if (isTRUE(a$answered[k])) {
      g = g +
        ggplot2::annotate("point", x = 55, y = y, shape = 24, size = if (step < 3.5) 1.9 else 2.3, fill = pal$accent) +
        ggplot2::annotate("text", x = 57.5, y = y, label = opt(a$best_option[k]), hjust = 0, size = txt_size) +
        ggplot2::annotate("point", x = 77, y = y, shape = 25, size = if (step < 3.5) 1.9 else 2.3, fill = "white") +
        ggplot2::annotate("text", x = 79.5, y = y, label = opt(a$worst_option[k]), hjust = 0, size = txt_size)
    } else {
      g = g + ggplot2::annotate("text", x = 70, y = y, label = "-", size = 3, colour = "grey50")
    }
    g = g + ggplot2::annotate("segment", x = 5, xend = 95, y = y - step / 2, yend = y - step / 2,
                              colour = "grey85", linewidth = 0.2)
  }

  # recommendations from the implementer (optional)
  if (has_recs) {
    g = g + sheet_bar(27, tr("fb_recommendations"), pal) +
      ggplot2::annotate("text", x = 6, y = 23.5, label = paste(short_text(recs, 95, 3), collapse = "\n"),
                        hjust = 0, vjust = 1, size = 2.8, lineheight = 0.95)
  }

  # footer
  contact = settings$feedback_contact %||% ""
  footer = c(feedback$project, contact)
  footer = footer[!is.na(footer) & nzchar(footer)]
  g = g +
    ggplot2::annotate("segment", x = 4, xend = 96, y = 12.5, yend = 12.5, linewidth = 0.3) +
    ggplot2::annotate("text", x = 50, y = 9, label = short_text(tr("fb_disclaimer"), 100, 2),
                      size = 2.7, lineheight = 0.95) +
    ggplot2::annotate("text", x = 50, y = 4.5,
                      label = short_text(paste(footer, collapse = "  |  "), 110, 1),
                      size = 2.5, colour = "grey30")

  g

}

#' Certificate of participation
draw_certificate = function(name, feedback, settings = list()) {

  pal = sheet_palette(settings$feedback_colour %||% TRUE)
  crop = feedback$crop
  what = if (!is.na(crop) && nzchar(crop)) tr("cert_for_crop", crop = crop) else
    tr("cert_for_items", options = plural_label(feedback$option))

  sheet_canvas() +
    ggplot2::annotate("rect", xmin = 3, xmax = 97, ymin = 3, ymax = 138, fill = NA,
                      colour = pal$accent, linewidth = 2) +
    ggplot2::annotate("rect", xmin = 5.5, xmax = 94.5, ymin = 5.5, ymax = 135.5, fill = NA,
                      colour = pal$accent, linewidth = 0.4) +
    ggplot2::annotate("point", x = 50, y = 117, shape = 21, size = 30, fill = pal$first,
                      colour = "black", stroke = 0.6) +
    ggplot2::annotate("text", x = 50, y = 119, label = "tricot", fontface = "bold", size = 6) +
    ggplot2::annotate("text", x = 50, y = 114, label = format(as.Date(settings$date %||% Sys.Date()), "%Y"),
                      size = 4) +
    ggplot2::annotate("text", x = 50, y = 97, label = toupper(tr("cert_title")),
                      fontface = "bold", size = 6.5) +
    ggplot2::annotate("text", x = 50, y = 88, label = tr("cert_awarded_to"), size = 4) +
    ggplot2::annotate("text", x = 50, y = 79, label = short_text(name, 30, 1),
                      fontface = "bold", size = 9) +
    ggplot2::annotate("segment", x = 20, xend = 80, y = 74, yend = 74, linewidth = 0.3) +
    ggplot2::annotate("text", x = 50, y = 67, label = short_text(what, 60, 3), size = 4,
                      lineheight = 0.95) +
    ggplot2::annotate("text", x = 50, y = 56, label = tr("cert_thanks"), fontface = "bold", size = 5) +
    ggplot2::annotate("text", x = 50, y = 48, label = short_text(feedback$project, 60, 2),
                      size = 3.4, colour = "grey30") +
    ggplot2::annotate("segment", x = c(12, 58), xend = c(42, 88), y = 26, yend = 26, linewidth = 0.4) +
    ggplot2::annotate("text", x = c(27, 73), y = 22, label = c(tr("cert_breeder"), tr("cert_field_agent")),
                      size = 3.2) +
    ggplot2::annotate("text", x = 50, y = 11, label = format_date(settings$date %||% Sys.Date()), size = 3.2)

}

#' One page with the results of the whole trial, for group sessions
#' and for participants who did not send their answers
draw_community_sheet = function(feedback, settings = list(), max_items = 12) {

  pal = sheet_palette(settings$feedback_colour %||% TRUE)
  res = feedback$results
  ref = res[res$is_reference_trait, ]
  ref = utils::head(ref[order(ref$rank), ], max_items)

  # points relative to the best item, so the best has ten dots
  pts = round(100 * ref$worth_percent / max(ref$worth_percent))

  g = sheet_canvas() +
    ggplot2::annotate("text", x = 5, y = 137.5, label = format_date(settings$date %||% Sys.Date()),
                      hjust = 0, size = 3) +
    ggplot2::annotate("text", x = 50, y = 130.5, label = short_text(feedback$project, 40, 1),
                      fontface = "bold", size = 6.5) +
    ggplot2::annotate("text", x = 50, y = 124.5,
                      label = tr("fb_community_intro", n = feedback$n_participants), size = 3.4) +
    sheet_bar(119, tr("fb_community_ranking", trait = feedback$reference_trait), pal)

  fills = c(pal$first, pal$second, pal$third)
  step = 5

  for (k in seq_len(nrow(ref))) {
    y = 113 - (k - 1) * step
    g = g +
      ggplot2::annotate("point", x = 7, y = y, shape = 21, size = 5.5,
                        fill = if (k <= 3) fills[k] else "white", colour = "black") +
      ggplot2::annotate("text", x = 7, y = y, label = ref$rank[k], size = 3, fontface = "bold") +
      ggplot2::annotate("text", x = 11, y = y, label = short_text(ref$item[k], 28, 1), hjust = 0, size = 3.3) +
      sheet_dots(55, y, pts[k], pal)
  }

  y_traits = 113 - nrow(ref) * step - 4
  g = g + sheet_bar(y_traits, tr("fb_best_by_trait"), pal)

  best = do.call("rbind", lapply(split(res, res$trait), function(x) x[which.min(x$rank), ]))
  best = best[order(!best$is_reference_trait, best$trait), ]
  best = utils::head(best, floor((y_traits - 18) / 4.2))

  for (k in seq_len(nrow(best))) {
    y = y_traits - 5 - (k - 1) * 4.2
    g = g +
      ggplot2::annotate("text", x = 6, y = y, label = short_text(best$trait[k], 45, 1), hjust = 0, size = 3) +
      ggplot2::annotate("point", x = 55, y = y, shape = 24, size = 2.3, fill = pal$accent) +
      ggplot2::annotate("text", x = 58, y = y, label = short_text(best$item[k], 28, 1), hjust = 0, size = 3) +
      ggplot2::annotate("segment", x = 5, xend = 95, y = y - 2.1, yend = y - 2.1, colour = "grey85",
                        linewidth = 0.2)
  }

  g +
    ggplot2::annotate("segment", x = 4, xend = 96, y = 12.5, yend = 12.5, linewidth = 0.3) +
    ggplot2::annotate("text", x = 50, y = 8, label = short_text(tr("fb_disclaimer"), 100, 2),
                      size = 2.7, lineheight = 0.95) +
    ggplot2::annotate("text", x = 50, y = 4, label = short_text(settings$feedback_contact %||% "", 110, 1),
                      size = 2.5, colour = "grey30")

}

# ................................................................
# Writing the files
# ................................................................

#' Write the feedback sheets, certificates and the community sheet
#'
#' Files in <output_path>/participant-report/
#'   feedback-sheets.pdf      one page per participant, for printing
#'   certificates.pdf         one page per participant
#'   community-results.pdf    and .png, results of the whole trial
#'   png/                     sheet and certificate of each participant, for phones
#' @return the paths of the PNG images of each participant
write_feedback_files = function(feedback, settings) {

  dir = file.path(settings$output_path, "participant-report")
  png_dir = file.path(dir, "png")
  dir.create(png_dir, recursive = TRUE, showWarnings = FALSE)

  pdf_device = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf
  a4 = c(8.27, 11.69)

  parts = feedback$participants
  sheets = vector("list", nrow(parts))
  certs = vector("list", nrow(parts))

  for (i in seq_len(nrow(parts))) {
    p = parts[i, ]
    a = feedback$answers[feedback$answers$participant_id == p$participant_id, ]
    sheets[[i]] = draw_feedback_sheet(p, a, feedback, settings)
    certs[[i]] = draw_certificate(p$name, feedback, settings)
  }

  write_pages = function(plots, file) {
    if (capabilities("cairo")) {
      grDevices::cairo_pdf(file, width = a4[1], height = a4[2], onefile = TRUE)
    } else {
      grDevices::pdf(file, width = a4[1], height = a4[2], onefile = TRUE)
    }
    on.exit(grDevices::dev.off())
    for (g in plots) print(g)
  }

  write_pages(sheets, file.path(dir, "feedback-sheets.pdf"))
  write_pages(certs, file.path(dir, "certificates.pdf"))

  community = draw_community_sheet(feedback, settings)
  write_pages(list(community), file.path(dir, "community-results.pdf"))
  ggplot2::ggsave(file.path(dir, "community-results.png"), community,
                  width = 21, height = 29.7, units = "cm", dpi = 200, bg = "white")

  files = data.frame(participant_id = parts$participant_id,
                     sheet = file.path(png_dir, paste0("feedback_", safe_file_name(parts$participant_id), ".png")),
                     certificate = file.path(png_dir, paste0("certificate_", safe_file_name(parts$participant_id), ".png")),
                     stringsAsFactors = FALSE)

  for (i in seq_len(nrow(parts))) {
    ggplot2::ggsave(files$sheet[i], sheets[[i]], width = 21, height = 29.7, units = "cm", dpi = 200, bg = "white")
    ggplot2::ggsave(files$certificate[i], certs[[i]], width = 21, height = 29.7, units = "cm", dpi = 200, bg = "white")
  }

  invisible(files)

}

#' Make a value safe to use in a file name
#' @examples safe_file_name(c("12", "P:3 John"))
safe_file_name = function(x) {
  gsub("[^A-Za-z0-9_-]+", "-", as.character(x))
}

#' Placeholder used when the feedback data fails
error_data_feedback = list(participants = data.frame(), answers = data.frame(),
                           results = data.frame())
