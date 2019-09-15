
# Code file 2 of 2 for 
# "Setting the Target: Precise Estimands and the Gap Between Theory and Empirics"
# by Ian Lundberg, Rebecca Johnson, and Brandon Stewart

# Code by Rebecca Johnson,
# with very minor edits by Ian Lundberg.

# Produces map showing weights implied by OLS regression

library(tidyverse)
library(haven)
library(fiftystater)
library(data.table)

setwd("/Users/iandl/Downloads/paglayan_replication/paglayan_replication")

data("fifty_states")

theme_new <- function(base_size = 16, base_family = "Helvetica"){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      panel.grid = element_blank(),   
      panel.border = element_rect(fill = NA, colour = "black", size=1),
      panel.background = element_rect(fill = "white", colour = "black"), 
      strip.background = element_rect(fill = NA),
      axis.text.x = element_text(color = "black"),
      axis.text.y = element_text(color = "black")
    )
}


union_df = read_dta("data/Paglayan Dataset.dta") %>%
  mutate(union_required = ifelse(CBstatusby1990 == 2,
                                 1, 0)) 

## find most recent year of observed data for controls (while
## restricting it to years before first tx adoption year)
control_vars = c("pnwht", "purban", "South", "ESWI", "perinc")
year_first_tx = min(union_df$YearCBrequired, na.rm = TRUE)
union_controlvars_mostrecent_pretx = union_df %>%
  filter(year < year_first_tx) %>%
  group_by(State) %>%
  arrange(desc(year)) %>%
  mutate_at(control_vars,  ~dplyr::first(na.omit(.))) %>%
  ungroup() %>%
  dplyr::select(State, one_of(control_vars)) %>%
  filter(!duplicated(State))

# Ian addition: Note when control variables are measured
year_measured <- union_df %>%
  filter(year < year_first_tx) %>%
  select(State, year, one_of(control_vars)) %>%
  melt(id = c("State","year")) %>%
  filter(!is.na(value) & year < year_first_tx) %>%
  group_by(State,variable) %>%
  summarize(year_measured = max(year))

year_measured %>%
  group_by(variable) %>%
  summarize(min_year_measured = min(year_measured),
            max_year_measured = max(year_measured))

summary(year_measured$year_measured)

  group_by(State) %>%
  arrange(desc(year)) %>%
  mutate_at(control_vars,  ~dplyr::first(na.omit(.))) %>%
  ungroup() %>%
  dplyr::select(State, one_of(control_vars)) %>%
  filter(!duplicated(State))

## create union analytic by binding with treatment status
## in 1990
union_analytic = merge(union_df %>% filter(year == 1990) %>% dplyr::select(State, 
                                                                           union_required),
                       union_controlvars_mostrecent_pretx,
                       by = "State")

sprintf("%s states missing data on unionization attributes", 51 - nrow(union_analytic %>%
                                                                         filter(!is.na(ESWI))))

## load achievement data
naep_data =  read.csv("data/naep_1996.csv")

## add state abbreviations to naep and clean
naep_data = naep_data %>% 
  mutate(state_abbrev = state.abb[match(naep_data$state,state.name)],
         state_lower = tolower(state)) %>%
  filter(group %in% c("White", "Black")) %>%
  rename(year_naep = year) %>%
  dcast(year_naep + state + state_abbrev + state_lower ~ group, value.var = "test_score") %>%
  mutate(white_black_gap = as.numeric(White)/as.numeric(Black)) %>%
  arrange(desc(white_black_gap)) %>%
  mutate(state_abbrev = ifelse(state == "District of Columbia", "DC",
                               state_abbrev)) %>%
  right_join(union_analytic %>% dplyr::select(State),
             by = c("state_abbrev" = "State")) %>%
  mutate(white_black_gap_complete = ifelse(is.na(white_black_gap),
                                           mean(white_black_gap, na.rm = TRUE),
                                           white_black_gap)) 

## n missing
sprintf("%s states missing data on black white gap", 51 - nrow(naep_data %>% filter(!is.na(white_black_gap))))

## merge test data with union data
union_df_wtest = merge(union_analytic, 
                       naep_data,
                       by.x = "State",
                       by.y = "state_abbrev",
                       all.x = TRUE) 

## complete data
control_vars_andlateroutcome = c(control_vars, "white_black_gap")
union_df_complete = union_df_wtest %>%
  filter_at(vars(control_vars_andlateroutcome), all_vars(!is.na(.))) 


sprintf("Estimating model with %s states", length(unique(union_df_complete$State)))

fit_treatment = lm(as.formula(paste("union_required ~ ", paste(control_vars, collapse = "+"))),
                   data = union_df_complete)

## load map data
us.data <- map_data("state")

## create residuals data for maps
union_resid = data.frame(state = union_df_complete$State,
                         state_lower = union_df_complete$state_lower,
                         resid = as.numeric(residuals(fit_treatment))) %>%
  mutate(weight = resid^2) %>%
  # Normalize weights: This is new from Ian
  group_by() %>%
  mutate(weight = weight / sum(weight)) %>%
  # Now back to Rebecca
  arrange(desc(weight)) %>%
  mutate(cumul_weight = cumsum(weight)) %>%
  mutate(state_char = as.character(state),
         state_lower_char = as.character(state_lower),
         state_final = ifelse(state_char == "OK", "oklahoma",
                              ifelse(state_char == "SD", "south Dakota",
                                     ifelse(state_char == "ID", "idaho",
                                            ifelse(state_char == "KS", "kansas",
                                                   ifelse(state_char == "OH", "ohio",
                                                          ifelse(state_char == "NH", "new hampshire",
                                                                 ifelse(state_char == "IL", 
                                                                        "illinois", state_lower_char)))))))) %>%
  # This is edited by Ian to include the state that pushes us over the 50 % mark
  group_by() %>%
  mutate(cumul_perctotal = cumul_weight) %>%
  mutate(over_50 = cumul_perctotal > .5,
         top_50 = cumsum(over_50) <= 1)

state_withresid = dplyr::left_join(fifty_states,
                                   union_resid,
                                   by = c('id' =  "state_final")) %>%
  mutate(nom_sample = ifelse(!is.na(weight), 1, 0),
         weight_final = ifelse(!is.na(weight), weight, 0))

### all states
ggplot(data = state_withresid, aes(x = long, y = lat, 
                                   group = group,
                                   fill = factor(nom_sample,
                                                 levels = c(0, 1),
                                                 labels = c("Not in sample", "In sample")))) + 
  geom_polygon(color = "black", alpha = 0.8)  + xlab("") + 
  scale_fill_manual(values = c("white", "darkgreen")) +
  ylab("") + 
  theme_new(base_size = 24) + theme(axis.text = element_blank(), 
                                    axis.title=element_blank(),
                                    legend.position = c(0.99, 0.3),
                                    panel.border = element_blank(),
                                    legend.background = element_blank(),
                                    axis.ticks = element_blank(),
                                    panel.background = element_blank(),
                                    panel.grid = element_blank(),
                                    plot.margin = unit(c(1,4.5,1,1), "cm"),
                                    aspect.ratio = .6) +
  fifty_states_inset_boxes() +
  labs(fill = "") +
  ggsave("figures/initial_states.pdf",
         device = "pdf",
         width = 12,
         height = 8,
         plot = last_plot()) 

## weights on states
ggplot(data = state_withresid, aes(x = long, y = lat, 
                                   group = group,
                                   fill = weight_final)) + 
  geom_polygon(color = "black", alpha = 0.8)  + xlab("") + 
  scale_fill_gradient(low = "white", high = "darkgreen") +
  ylab("") + 
  theme_new(base_size = 24) + theme(axis.text = element_blank(), 
                                    axis.title=element_blank(),
                                    #legend.position = c(1.1, 0.3),
                                    panel.border = element_blank(),
                                    legend.background = element_blank(),
                                    axis.ticks = element_blank(),
                                    panel.background = element_blank(),
                                    panel.grid = element_blank(),
                                    #plot.margin = unit(c(1,4.5,1,1), "cm"),
                                    aspect.ratio = .6) +
  fifty_states_inset_boxes() +
  labs(fill = "Weight") +
  ggsave("figures/weight_states_crossx.pdf",
         device = "pdf",
         width = 12,
         height = 8,
         plot = last_plot()) 

