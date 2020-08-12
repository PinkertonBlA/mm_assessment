# MM Assessment Reporting Package
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(jsonlite)
library(tidyverse)
library(kableExtra)

#' Read in JSON file based on path to file
#'
#' @param path the filepath to the JSON file
#'
#' @return
#' @export
#'
#' @examples dataframe <- readin("home/Ricky/tempfile.json")
readin <- function(path="/home/kent/Contracts/missionmet/mm-assessor/survey_monkey/results-example/MM Checkup for Chambers.json") {
  jsonlite::fromJSON(path)
}

# df <- readin()
#
# df_report_format <- df[1]
#
# df_transformations <- df[[3]]
#
# df_definitions <- df[[4]]
#
# df_responses <- df[[6]]

# Functions -----

## Color Palettes -----

mm_colors <-  c(b_connect = "#58B7C1",
                b_execute = "#01476B",
                b_vision = "#657072",
                r_adrift = "#BC3C35",
                y_caution = "#F5D50C",
                g_pace = "#98BB60",
                gr_pending = "#C4CED4")

mm_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return(mm_colors)

  mm_colors[cols]
}

mm_palettes = list(analogous = mm_cols("b_connect", "b_execute", "b_vision"),
                   scale = mm_cols("b_pending", 'b_execute'))

mm_pal <- function(palette = "analogous", reverse = FALSE, ...) {
  pal <- mm_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

scale_fill_mm <- function(palette = "analogous", discrete = TRUE, reverse = FALSE, ...) {
  pal <- mm_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("mm_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

## Plot Themes -----

#' Theme for ggplot graphical outputs. Styles are overridden by calls from individual functions.
#'
mm_theme <- function() {theme_minimal(base_size = 12) +
    theme(text = element_text(family = "Raleway"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank())
}

## Tables -----

# Swot by Role
# *****SWOT ORDER SHOULD END UP GETTING DEFINED IN THE DEFINITIONS SECTION, RIGHT*****
swot_order <- c("strengths", "weaknesses", "opportunities", "threats", "priorities") # This should come from definitions

#' SWOT Tables
#'
#' @param pivot_value the naming convention that will be searched for in varibale names for conversion from wide to long format
#' @param value_name the name of the specfic value for the table to be generated
#' @param roles_id a definitions id value that indexes the order of the organization roles i.e. "chamber_roles"
#'
#' @example pivot_table_by("swot", "strengths", "chamber_roles")
pivot_table_by <- function(pivot_value, value_name, roles_id) {

  vars_ <- names(df_responses)[which(str_ends(names(df_responses), "\\d") & str_starts(names(df_responses), pivot_value))]

  roles_order <- unlist(df_definitions$options[which(df_definitions$id==roles_id)][[1]][1])

  swot <-
    df_responses %>%
    dplyr::select(participant_role, participant_name, vars_) %>%
    dplyr::mutate(participant_role = ordered(participant_role, levels = roles_order)) %>%
    pivot_longer(cols = starts_with(pivot_value), names_to = pivot_value, values_to = value_name) %>%
    separate(col = pivot_value, into = c("pivot", "name_", "rank"), sep = "_")

    swot %>%
      dplyr::filter(name_ == value_name) %>%
      dplyr::select(-pivot, -name_) %>%
      dplyr::arrange(participant_role) %>%
      kable(col.names = c("Role", "Name", "Rank", "Response")) %>%
      kable_styling() %>%
      collapse_rows(1:2, valign = "top")
}

#' Write tables of text
#'
#' @param variables variables from 'df_responses' data to include in the table single or list
#' @param names human readable names for the table headers single or list
#' @param collapse_columns numeric vector of columns to collapse ex: 1, 1:2, c(1, 2, 4)
#'
#' @return outputs kable table
#' @examples mm_textTable(c('participant_role', 'participant_name'), c("Role", "Name"), 1)
mm_textTable <- function(variables, names_=variables, collapse_columns=1) {
  df_responses %>%
    select(variables) %>%
    kable(col.names = names_) %>%
    kable_styling() %>%
    collapse_rows(collapse_columns, valign = "top")
}

## Summary Graphs -----

#' Create Summary Graphs of mean & sd for variables
#'
#' @param variables variable names from df_responses dataframe ex: c("sp_plan", "sp_process")
#' @param mean_or_sd Is the summary displaying mean or standard deviation of scores? "mean" or "sd"
#' @param scale_ What scale is to be used as defined in 'df_definitions' dataframe
#' @param wrap_ length of characters before wrapping the axis labels
#'
#' @examples mm_summary_graphs(c("sp_plan", "sp_process"), "sd", "mm_likert_scale", wrap_ = 30)
#' mm_summary_graphs(c("sp_plan", "sp_process"))
mm_summary_graphs <- function(variables, mean_or_sd = "mean", scale_ = "mm_likert_scale", wrap_ = 30) {

  matrix <- variables
  names(matrix) <- df_definitions$subtext[which(df_definitions$id %in% variables)] %>%
    str_wrap(width = wrap_)

  scale_lab <- df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[1]]
  names(scale_lab) <- df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[2]]

  matrix_summary <-
    df_responses %>%
    select(matrix) %>% # select all "matrix" style variables
    pivot_longer(cols = everything(), names_to = "facet", values_to = "value") %>% # take the variable names and pass them as observations to variable "facet". # corresponding values to value
    group_by(facet) %>% # group the df by facet
    summarise(m_ = mean(value, na.rm = TRUE), # summarise facet groups by mean and sd
              sd_ = sd(value, na.rm = TRUE)) %>%
    mutate(min_ = m_-sd_, # impute new variables the mean +- the standard deviation.
           max_ = m_+sd_)

  if(mean_or_sd == "mean") {
    matrix_summary %>%
      ggplot(aes(x = reorder(facet, m_))) + # reorder by mean
      geom_linerange(aes(ymin = min_,
                         ymax = max_), color = "gray") +
      geom_point(aes(y = m_)) +
      scale_y_continuous(breaks = scale_lab, labels = replace_na(names(scale_lab), "")) +
      mm_theme() +
      theme(panel.grid.major.x = element_line('gray', linetype = 3),
            panel.grid.major.y = element_line(linetype = 3)) +
      labs(title = "Average Scores",
           subtitle = "by average") +
      coord_flip(ylim = range(scale_lab))

  } else {

    matrix_summary %>%
      ggplot(aes(x = reorder(facet, -sd_))) +
      geom_linerange(aes(ymin = min_,
                         ymax = max_), color = "black") +
      geom_point(aes(y = m_), color = "gray") +
      scale_y_continuous(breaks = scale_lab, labels = replace_na(names(scale_lab), "")) +
      mm_theme() +
      theme(panel.grid.major.x = element_line('gray', linetype = 3),
            panel.grid.major.y = element_line(linetype = 3)) +
      labs(title = "Deviation in Scores",
           subtitle = "by standard deviation") +
      coord_flip(ylim = range(scale_lab))
  }
}



## Appendices Bars and Tiles -----

#' Create bar charts for MM items
#'
#' @param variable variable to chart
#' @param scale_ scale used to chart variable defined in df_definitions
#' @examples bars("sp_plan")
bars <- function(variable, scale_ = "mm_likert_scale") {

  # Scale labels

  scale_lab <- df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[1]]
  names(scale_lab) <- replace_na(df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[2]], " ")

  # Plots

  df_responses %>%
    group_by(.data[[variable]]) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count)) %>%
    ggplot(aes(x = .data[[variable]],
               y = perc)) +
    geom_col(fill = mm_colors[1], width = 0.9) +
    mm_theme() +
    scale_x_continuous(breaks = seq(min(scale_lab), max(scale_lab), 1), labels = paste(scale_lab, names(scale_lab))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    coord_cartesian(xlim = c(min(scale_lab), max(scale_lab)), ylim = c(0,1))
}

#' Create heatmap 'tile' charts for MM items
#'
#' @param variable variable to chart
#' @param scale_ scale used to chart variable defined in df_definitions
#' @examples tiles("sp_plan")
tiles <- function(variable, scale_ = "mm_likert_scale") {

  # Scale labels

  scale_lab <- df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[1]]
  names(scale_lab) <- replace_na(df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[2]], " ")

  df_responses[[variable]] <- ordered(df_responses[[variable]], levels = scale_lab, labels = scale_lab)

  # Plots

  ggplot(data = df_responses,
         aes(x = participant_name,
             y = str_wrap(participant_role, 5),
             fill = .data[[variable]])) +
    geom_tile() +
    mm_theme() +
    scale_fill_mm(discrete = TRUE,
                  reverse = TRUE,
                  guide = guide_legend(reverse = TRUE),
                  breaks = scale_lab,
                  labels = names(scale_lab),
                  drop = FALSE) +
    coord_flip()
}


#' Plots bars and tiles side by side with title centered above both plots
#'
#' @param variable variable to be applied to both bar and tile plots
#'
#' @examples dbl_output("sp_plan")
dbl_output <- function(variable) {
  p1 <- bars(variable)
  p2 <- tiles(variable)

  print(
    ggpubr::annotate_figure(top = ggpubr::text_grob(df_definitions$subtext[which(variable == df_definitions$id)],
                            family = 'Raleway', color = mm_colors['b_vision']),
                            ggpubr::ggarrange(p1, p2, ncol = 2, common.legend = FALSE)
    )
  )
}

#' Appendix B
#'
#' @param variables variables included in the heatmap
#' @param scale_ What scale should be used. Default is "mm_likert_scale"
#'
#' @examples appendix_b("sp_plan")
appendix_b <- function(variables, scale_ = "mm_likert_scale") {

  scale_lab <- df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[1]]
  names(scale_lab) <- df_definitions$options[which(df_definitions$type == "scale" & df_definitions$id == scale_)][[1]][[2]]


  basicdf <-
    df_responses %>%
    select(participant_name, variables) %>%
    mutate_at(vars(variables), ~as.numeric(.)) %>%
    pivot_longer(cols =variables, names_to = "question", values_to = 'score') %>%
    left_join(., df_definitions[c('id','text')], by = c('question' = 'id')) %>%
    mutate(question = sub(":", "\n", text)) %>%
    select(-text)

  basic_part <-
    basicdf %>%
    group_by(participant_name) %>%
    summarise(score = round(mean(score, na.rm = TRUE), digits = 1)) %>%
    mutate(question = "Participant Average")

  basic_question <-
    basicdf %>%
    group_by(question) %>%
    summarise(score = round(mean(score, na.rm = TRUE), digits = 1)) %>%
    mutate(participant_name = "Question Average")

  rbind(basicdf, basic_part, basic_question) %>%
    mutate(question = ordered(question,
                              levels = append("Participant Average", unique(basicdf$question))),
           participant_name = ordered(participant_name,
                                      levels = append(unique(df_responses$participant_name), "Question Average"))) %>%
    ggplot() +
    geom_tile(aes(x = question, y = participant_name, fill = score), color = "black") +
    geom_text(aes(x = question, y = participant_name, label = score)) +
    scale_x_discrete(breaks = append("Participant Average", unique(basicdf$question))) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 5)) +
    mm_theme() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(hjust = 1),
          panel.grid = element_blank(),
          legend.position = "bottom") +
    scale_fill_mm(discrete = F, palette = "scale", breaks = scale_lab, labels = replace_na(names(scale_lab), "")) +
    coord_flip()
}
