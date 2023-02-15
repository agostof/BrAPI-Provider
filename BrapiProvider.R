require(R6)
require(QBMS)

DEBUG <- FALSE

saveEnv <- function(privateEnv, qbmsEnv){
  #print("SAVING"), qbmsEnv$config)
  if (DEBUG){
    cat("\n****SAVING CONFIG & STATE****\n")
    cat("  qbmsEnv$config <-- "); cat(str( qbmsEnv$config))
    cat("  qbmsEnv$state  <-- "); cat(str( qbmsEnv$state))
  }

  privateEnv$config <- qbmsEnv$config
  privateEnv$state  <- qbmsEnv$state

  if (DEBUG){
    cat("  privateEnv$config <-- "); cat(str( privateEnv$config))
    cat("  privateEnv$state  <-- "); cat(str( privateEnv$state))
    cat("\n***SAVING DONE***\n\n")
  }
  
}

debugPrint <- function(privateEnv=NULL, qbmsEnv=NULL){
  if (DEBUG){
    cat("\n****DEBUG PRINT****\n")
    cat("  qbmsEnv$config <-- "); cat(str( qbmsEnv$config))
    cat("  qbmsEnv$state  <-- "); cat(str( qbmsEnv$state))
    cat("  privateEnv$config <-- "); cat(str( privateEnv$config))
    cat("  privateEnv$state  <-- "); cat(str( privateEnv$state))
    cat("\n***DEBUG PRINT DONE***\n\n")
  }
}

restoreEnv <- function(privateEnv, qbmsEnv){
   if (!is.null(privateEnv$config)){
      qbmsEnv$config <- privateEnv$config
      if (DEBUG){ cat("restoring LOCAL DATA CONFIG"); cat(str(privateEnv$config), "\n")}
   }
   if (!is.null(privateEnv$state)){
     qbmsEnv$state <- privateEnv$state
     if (DEBUG){ cat("restoring LOCAL DATA STATE: ");cat(str(privateEnv$state), "\n")}
         # print("LOCAL DATA", PROVIDER.data$state)
   }
   debugPrint(privateEnv, qbmsEnv)

}

get_clean_qbms_env <- function(){
    if (DEBUG) cat("\n***Creating new QBMS env**\n")
    qbmsEnv <- new.env()
    qbmsEnv$config <- list(crop = NULL)
    qbmsEnv$state  <- list(token = NULL)
    if (DEBUG) cat("\n***DONE new QBMS env**\n")
    qbmsEnv
}


# BrapiProviderPlatform = c("GIGWA", "BMS", "BREEDBASE", "PHG")
# names(BrapiProviderPlatform) <- c("GIGWA", "BMS", "BREEDBASE", "PHG")

BrapiProviderPlatform = R6::R6Class("BrapiProviderPlatform")
BrapiProviderPlatform$GIGWA <- "GIGWA"
BrapiProviderPlatform$BMS <- "BMS"
BrapiProviderPlatform$BREEDBASE <- "BREEDBASE"
BrapiProviderPlatform$PHG <- "PHG"


BrapiProvider <- R6Class("BrapiProvider",
  lock_objects = FALSE,
  public = list(
    url = "",
    username = "",
    password = "",
    # token = "",
    engine = NULL,
    initialized = FALSE,
    PROVIDER.data = NULL,
    initialize = function(url, username=NULL, password=NULL, token=NULL) {
      self$url <- url
      self$username <- username
      self$password <- password
      self$token <- token
      # message("Creating BrapiProvider instance.")
      # initialize session info
      #self$token <- token
      self$url <- url
      self$PROVIDER.data <- get_clean_qbms_env()
      self$engine <- self$platform
      message("Creating BrapiProvider instance.")
      #self$engine = engine
      userAppEnv <- QBMS::debug_qbms()
      saveEnv(userAppEnv, self$PROVIDER.data)

      QBMS::set_qbms_config(url, 
                time_out = 300, no_auth = TRUE, engine = self$engine)

      saveEnv(self$PROVIDER.data, userAppEnv)
      #saveEnv(self$PROVIDER.data, userAppEnv)
      if (!is.null(token)) {
        #cat("Setting token\n")
        self$set_token(token)
      }
      self$register_all_QBMS()
      self$initialized <- TRUE


    },
    wrapper = function(target_fn){
        target = target_fn
        #cat("\nWRAPPER REGISTERING: \n")
        #
        #on.exit(function(){cat("\nDECORATOR EXIT \n")})
        function(...){
          if (DEBUG) {
            cat("CALLING DECORATED FUNC\n")
            cat( as.character(substitute(target_fn)))
          }
          userAppEnv <- QBMS::debug_qbms()
          if (DEBUG){
            cat("\n Incomming Settings \n")
            cat("   qbms@config: -->"); cat(str(userAppEnv$config), "\n" )
            cat("   qbms@state: -->"); cat(str(userAppEnv$config), "\n" )
          }
          #urAppEnv <- QBMS::debug_qbms()
          restoreEnv(self$PROVIDER.data, userAppEnv)
          on.exit(saveEnv(self$PROVIDER.data, userAppEnv))
          # cat("Calling farget_fn\n")
          target_fn(...)
        }
    },
    register_fn = function(target_fn=NULL, fn_name=NULL) {
      if (DEBUG) cat("Registration\n")
      #fn_name <- as.character(substitute(target_fn))
      if (DEBUG) cat("Registering FUNC <-- ", fn_name, "\n")
      self[[fn_name]] <- self$wrapper(get(fn_name))
      # environment(self[[fn_name]]) <- environment(self$register_fn)
      environment(self[[fn_name]])$self <- self
    },
    register_all_QBMS = function(){
        fn_names <- ls("package:QBMS", all.names = TRUE)
        for (fn_name in fn_names) {
            # skip debug_qbms, 
            # should we skip 'login_bms' too?
            if (fn_name %in% c( "debug_qbms")) {
                if (DEBUG) print(paste("SKIPING", str(fn_name)))
                next;
            }
            if (DEBUG) print(fn_name)
            self$register_fn(fn_name=fn_name)
        }
    },
    login = function(...) {
      # Method must be implemented in subclasses
      stop("login must be implemented in subclass")
    },
    activate_connection = function() {
      # Method must be implemented in subclasses
      stop("activate_connection must be implemented in subclass")
    },
    set_token = function(token) {
      self$token <- token
      # cat("Calling set_token\n")
      # cat( str(self$PROVIDER.data$state))
      self$PROVIDER.data$state$token <<- token
      # cat("CUURENT DATA", str(self$PROVIDER.data$state))
    },
    get_token = function() {
      self$token
      self$PROVIDER.data$state$token
    },
    platform = NULL
  ),
  active = list(
    # login = function() {
    #   # Method must be implemented in subclasses
    #   stop("login must be implemented in subclass")
    # },
    # qbms_activate = function() {
    #   # Method must be implemented in subclasses
    #   stop("qbms_activate must be implemented in subclass")
    # }
  ),
  private = list(
    #  platform = NULL
    token = ""
  )
)


PhenotypeProvider <- R6Class("PhenotypeProvider", inherit = BrapiProvider,
  public = list(),
  private = list()
)

GenotypeProvider <- R6Class("GenotypeProvider", inherit = BrapiProvider,
  public = list(),
  private = list()
)


BrapiGigwa <- R6Class("BrapiGigwa", inherit = GenotypeProvider,
  lock_objects = FALSE,
  public = list(
    activate_connection = function() {
      require(QBMS)

      message("Activating QBMS for GIGWA params-->", self$url, self$username, self$token, self$platform )
      message("Setting URL", self$url)
      QBMS::set_qbms_config(self$url, time_out = 300, engine = self$platform, no_auth = FALSE)
      userAppEnv$state$token <- self$token
      userAppEnv$state$user <- self$username
      #userAppEnv$state$expires_in <- 'Token_expires_in'
    },
    login = function(...) {
      message("Logging into GIGWA")
      self$login_gigwa(...)
    },
    platform = "gigwa"
  ),
  private = list()
)

BrapiPhg <- R6Class("BrapiPhg", inherit = GenotypeProvider,
  public = list(),
  private = list()
)

BrapiBreedBase <- R6Class("BrapiBreedBase", inherit = PhenotypeProvider,
  public = list(),
  private = list()
)

BrapiBms <- R6Class("BrapiBms", inherit = PhenotypeProvider,
  lock_objects = FALSE,
  public = list(
    activate_connection = function() {
      require(QBMS)
      message("Activating QBMS for BMS params-->", self$url, self$username, self$token, self$platform )
      message("Setting URL", self$url, self$platform)
      QBMS::set_qbms_config(self$url, time_out = 300, engine = self$platform)
      userAppEnv <- QBMS::debug_qbms()
      userAppEnv$state$token <- self$token
      userAppEnv$state$user <- self$username
      userAppEnv$state$expires_in <- 'Token_expires_in'
      
    },
    login = function(...) {
      message("Logging into BMS")
      self$login_bms(...)
    },
    platform = "bms"
  ),
  private = list()
)


# provider_factory <- function(provider_type, url, username, password) {
#   if (provider_type == "GIGWA") {
#     return BrapiGigwa$new(url, username, password)
#   } else if (provider_type == "PHG") {
#     return BrapiPhg$new(url, username, password)
#   } else if (provider_type == "BREEDBASE") {
#     return BrapiBreedBase$new(url, username, password)
#   } else if (provider_type == "BMS") {
#     return BrapiBms$new(url, username, password)
#   } else {
#     stop(paste("Unknown provider type:", provider_type))
#   }
# }


BrapiProviderFactory <- R6Class("BrapiProviderFactory",
  public = list(
    url = NULL,
    username = NULL,
    password = NULL,
    initialize = NULL,
    create_provider = function(provider_type, url, username, password=NULL, token) {
      switch(provider_type,
        # "BrapiGigwa" = CompanyApple$new(self$url, self$username, self$password),
        "GIGWA" = BrapiGigwa$new(url, username, password, token),
        "PHG" = BrapiPhg$new(url, username, password, token),
        "BREEDBASE" = BrapiBreedBase$new(url, username, password, token),
        "BMS" = BrapiBms$new(url, username, password, token),
        stop(paste("Unknown provider type:", provider_type))
        #stop("Invalid provider type")
      )
    }
  )
)