check_string_contains <- function(x,
                                  y,
                                  arg = rlang::caller_arg(x),
                                  call = rlang::caller_env()) {
  if(!x %in% y) {
    cli::cli_abort(c("The object provided to {.arg {arg}} must be a {.cls character} string with a value that is one of {.or {.val {y}}}.",
                     "x" = "You've supplied a {.cls {class(x)}} with a value {.val {x}}"),
                   call = call)
  }

}


check_logical <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!rlang::is_logical(x)) {
    cli::cli_abort(c("The object provided to {.arg {arg}} must be a {.cls {class(TRUE)}}",
                     "x" = "You've supplied a {.cls {class(x)}}"),
                   call = call)
  }
}

check_named_list <- function(x,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!rlang::is_named(x)) {
    cli::cli_abort(c("The object provided to {.arg {arg}} must be a named {.cls list}",
                     "x" = "You've supplied an unnamed {.cls {class(x)}}"),
                   call = call)
  }
  if (!rlang::is_list(x)) {
    cli::cli_abort(c("The object provided to {.arg {arg}} must be a named {.cls list}",
                     "x" = "You've supplied an named {.cls {class(x)}}"),
                   call = call)
  }
}

check_numeric <- function(x,
                          arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  if (!rlang::is_double(x)) {
    cli::cli_abort(c("The object provided to {.arg {arg}} must be a {.cls {class(1)}}.",
                     "x" = "You've supplied a {.cls {class(x)}}"),
                   call = call)
  }

}

check_string <- function(x,
                         arg = rlang::caller_arg(x),
                         call = rlang::caller_env()) {
  if (!rlang::is_string(x)) {
    cli::cli_abort(c("The object provided for {.arg {arg}} must be a {.cls {class('string')}}.",
                     "x" = "You've supplied a {.cls {class(x)}}"),
                   call = call)
  }
}

check_file_exists <- function(x,
                              arg = rlang::caller_arg(x),
                              call = rlang::caller_env()) {
  if (!fs::file_exists(x)) {
    cli::cli_abort(c("The filepath provided to {.arg {arg}}: {.file {x}} does not point to an existing file.",
                     "Check your filepath with {.code fs::file_exists({arg})}"),
                   call = call)
  }
}


check_dir_exists <- function(x,
                             arg = rlang::caller_arg(x),
                             call = rlang::caller_env()) {
  if (!fs::is_dir(x)) {
    cli::cli_abort(c("The directory provided to {.arg {arg}}: {.file {x}} does not point to an existing folder.",
                     "Check your filepath with {.code fs::dir_exists({arg})}"),
                   call = call)
  }
}

check_spat_ras <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if(!inherits(x, c("SpatRaster", "SpatRasterCollection"))) {
    cli::cli_abort("The object supplied to {.arg {arg}} must be a {.cls SpatRaster} or {.cls SpatRasterCollection} object. Try loading the raster with {.code terra::rast()}.",
                   call = call)
  }
}

check_spat_vect <- function(x,
                           arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if(!inherits(x, c("SpatVector"))) {
    cli::cli_abort("The object supplied to {.arg {arg}} must be a {.cls SpatVector} object. Try loading the raster with {.code terra::vect()}.",
                   call = call)
  }
}



return_object <- function(type, x, i = 1,
                          arg_type = rlang::caller_arg(type),
                          call = rlang::caller_env()) {
  if(type == "wbt") {
    return(x)
  }
  if(type == "terra") {
    x <- whitebox::wbt_result(x, i = i, attribute = "output")
    return(x)
  }
  else {
    cli::cli_abort("Something went wrong here, sorry this isn't helpful yet! {.arg {arg_type}} is probably incorrect.",
                   call = call)
  }
}


check_whitebox_wd <- function(x) {
  if(is.null(x)) {
    ## this prevents temp {whitebox} files being written to the project directory by default
    whitebox::wbt_wd(wd = tempdir())
  }
  if(!is.null(x)) {
    ## else we can point to whatever directory the user wants {whitebox} generated files to be written
    whitebox::wbt_wd(wd = x)
  }

}

reset_whitebox_wd <- function(x) {
  ## reset whitebox wd
  if(is.null(x)) {
    whitebox::wbt_wd("")
  }
}


whitebox_bin <- function() {

  ## need to check whitebox tools is installed
  if(!whitebox::check_whitebox_binary()) {
    cli::cli_abort("Please install whitebox using {.code `whitebox::install_whitebox()`}")
  }

}

