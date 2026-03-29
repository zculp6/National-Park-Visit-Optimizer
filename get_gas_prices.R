library(chromote)
library(dplyr)

# 1. SETUP REGIONS (50 States + DC)
regions <- data.frame(
  State = c(state.name, "District of Columbia"),
  stringsAsFactors = FALSE
) %>%
  mutate(url_name = tolower(gsub(" ", "-", State)))

update_gas_cache <- function() {
  cat("Starting Weekly Update...\n")
  
  # Start one browser session for the whole loop
  b <- ChromoteSession$new()
  # Mask browser as a real user to avoid "Found but empty" errors
  b$Network$setUserAgentOverride(userAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
  
  results_list <- list()
  
  for (i in 1:nrow(regions)) {
    state_name <- regions$State[i]
    url_name <- regions$url_name[i]
    url <- paste0("https://fuelinsights.gasbuddy.com/Home/US/", url_name)
    
    cat(sprintf("[%2d/51] %s... ", i, state_name))
    
    tryCatch({
      b$Page$navigate(url)
      
      # Wait for the JS 'data-bind' to fire (Max 12 seconds)
      price_found <- FALSE
      start_time <- Sys.time()
      while(as.numeric(difftime(Sys.time(), start_time, units="secs")) < 12) {
        js_eval <- b$Runtime$evaluate('document.querySelector("#tickingAvgPriceText") ? document.querySelector("#tickingAvgPriceText").innerText : ""')
        val <- js_eval$result$value
        
        if (!is.null(val) && grepl("[0-9]", val)) {
          results_list[[i]] <- tibble(State = state_name, Regular = as.numeric(val))
          price_found <- TRUE
          cat(sprintf("$%.3f ✓\n", as.numeric(val)))
          break
        }
        Sys.sleep(0.5)
      }
      
      if(!price_found) {
        cat("FAILED (Timeout)\n")
        results_list[[i]] <- tibble(State = state_name, Regular = NA)
      }
      
    }, error = function(e) {
      cat(sprintf("FAILED (%s)\n", e$message))
      results_list[[i]] <- tibble(State = state_name, Regular = NA)
    })
    
    # Small jitter to prevent rate-limiting
    Sys.sleep(runif(1, 1, 2))
  }
  
  b$close()
  
  # Combine and save
  final_df <- bind_rows(results_list) %>%
    mutate(last_updated = Sys.time())
  
  saveRDS(final_df, "gas_prices.rds")
  cat("\nUpdate Complete. Saved to gas_prices.rds\n")
}

update_gas_cache()

