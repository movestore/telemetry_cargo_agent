library(move)
library(ctmm)
# mv <- readRDS("input4_goat.rds") #input2_geese.rds
# rds <- as.telemetry(mv)

# rds is a telemetry.list 
analyze <- function(rds) {
  root <- NA
  
  tryCatch(
    {
      if (file.exists(myResultFileName)) {
        # clean up
        log_debug("removing last result-file of cargo-agent-r")
        file.remove(myResultFileName)
      }
      
      if (length(rds)==0)
      {
        # fallback for N=0
        log_debug("Analyzing for N=0")
        root <- mapOutput(
          n = "empty-result",
          animal_names = NA,
          timezone = NA,
          projection = NA
        )
      }
      else
      {
        
        if (is(rds,'telemetry'))
        {
          #N=1
          animal.name <-  summary(rds)$identity
          tz <- summary(rds)$timezone
          prj <- summary(rds)$projection
          }
        else if (is(rds,'list'))
        {
          #N>1
          animal.name <- unlist(lapply(rds, function(x) summary(x)$identity))
          tz <-  unique(unlist(lapply(rds, function(x) summary(x)$timezone)))
          prj <-  unique(unlist(lapply(rds, function(x) summary(x)$projection)))
        }
        root <- mapOutput(
          animal_names = animal.name,
          timezone = tz,
          projection = prj
        )

      }
    },
    error = function(cond) {
      log_error(cond)
    },
    warning = function(cond) {
      log_warn(cond)
    }
  )
  
  # provide in any case the json file (in case of an exception the empty one)
  writeResult(root)
}