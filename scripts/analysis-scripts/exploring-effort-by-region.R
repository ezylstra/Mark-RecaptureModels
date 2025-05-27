# Run wintering-grounds-mexico.R through line 68

# Load effort data
effort.raw <- read.csv('output/banding-effort-data/banding-effort-all-sites-RMNP.csv')
# Total banding days per year

# Edit effort data to include location
effort.dat <- effort.raw %>% 
  # Sites not included in capture data for analysis:
  filter(!site %in% c('CLP', 'BGMD', 'WB2','WB1', 'WPK1', 'NFPC', 'POLC', 'SHIP')) %>%
  mutate(location = ifelse(site %in% c('GNMTN', 'HOLZ', 'KV1'), 'west', 'east'))

# Standardize effort
effort.z <- effort.dat %>% 
  group_by(location, year) %>%  # grouping by location seems the right thing to do, but 
  # I can't add the effort to the ddl
  summarize(total_days = sum(total_banding_days, na.rm = TRUE), 
            .groups = 'drop') %>% 
  rename(time = year,
         effort_raw = total_days) %>% 
  mutate(effort_z = z.stand(effort_raw)) %>%
  as.data.frame()

# Summarize effort
effort.z <- effort.z %>%
  mutate(n_sites = ifelse(location == "east", 9, 3),
         effort_per_site = round(effort_raw / n_sites, 2)) 

plot(effort_per_site ~ time, 
     data = effort.z[effort.z$location == "east", ],
     ylim = c(0, 20), col = "blue", type = "b")
points(effort_per_site ~ time, 
       data = effort.z[effort.z$location == "west", ],
       col = "red", type = "b")

# Number of birds banded in each region
count(ch.adults, location)
sum(ch.adults$location == "east") / sum(ch.adults$location == "west")

# Convert effort data to wide form to use in CJS models
effw <- effort.z %>%
  pivot_wider(id_cols = location, 
              names_from = time, 
              names_prefix = "eff",
              values_from = effort_z)

ch.adults <- ch.adults %>%
  left_join(effw, by = "location")

ahy.process <- process.data(ch.adults,
                            model = 'CJS',
                            begin.time = 2003,
                            groups = c('sex', 'location'))

ahy.ddl <- make.design.data(ahy.process)

# Run a few models
test.run <- function()
{
  Phi.sexloc <- list(formula = ~sex + location + time)
  
  p.dot <- list(formula = ~1)
  p.sex <- list(formula = ~sex)
  p.eff <- list(formula = ~eff)
  p.loc <- list(formula = ~location)
  p.sexeff <- list(formula = ~sex + eff)
  p.sexloc <- list(formula = ~sex + location)
  p.effloc <- list(formula = ~eff + location)
  p.sexeffloc <- list(formula = ~sex + eff + location)
  
  # Create a data frame of all combinations of parameter specifications for each 
  # parameter
  cml <- create.model.list('CJS')  
  
  # Construct and run a set of MARK models from the cml data frame
  results <- mark.wrapper(cml, 
                          data = ahy.process,
                          ddl = ahy.ddl,
                          adjust = FALSE) # Accepts the parameter counts from MARK
  return(results)
}

# Run the function
test.results <- test.run()
test.results

# Look at estimates and standard errors 
results.1 <- test.results[[4]]
results.1$results$beta
results.1$results$real

invisible(file.remove(list.files(pattern = 'mark.*\\.(inp|out|res|vcv|tmp)$')))
