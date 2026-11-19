# The flood-shock reader rejects bad files and combines countries in one event.
#   Rscript tests/test_flood_shocks.R

source("main/read_flood_shocks.R")

failures <- character()
check <- function(cond, label) {
    if (!isTRUE(cond)) {
        failures <<- c(failures, label)
        message("FAIL: ", label)
    } else {
        message("OK: ", label)
    }
}

iso <- c("AAA", "BBB", "CCC")
fao <- data.frame(fao_code = c(1, 2), iso3 = c("AAA", "BBB"), stringsAsFactors = FALSE)
dir.create("tests/tmp", showWarnings = FALSE, recursive = TRUE)

write_and_read <- function(text, name) {
    path <- file.path("tests/tmp", name)
    writeLines(text, path)
    tryCatch(read_flood_shocks(path, iso, fao), error = function(e) e)
}

good <- write_and_read(
    paste(
        "event_id,crop,iso3,fao_code,year,production_loss_fraction,production_loss_mt,reactive_ban_fraction,shock_source,units",
        "flood_a,wheat,AAA,,2010,0.10,,0.5,illustrative_placeholder,fraction_of_baseline_production",
        "flood_a,wheat,BBB,,2010,0.20,,0.5,illustrative_placeholder,fraction_of_baseline_production",
        "flood_b,rice_milled,,2,2008,,40,0,observed_loss,metric_tons",
        sep = "\n"
    ),
    "good.csv"
)
check(!inherits(good, "error"), "a valid two-event file is accepted")
check(length(good) == 2, "two event ids become two scenarios")
check(identical(sort(good[[1]]$iso3), c("AAA", "BBB")), "one event keeps both countries")
check(good[[2]]$iso3 == "BBB", "a blank ISO3 is filled from the FAO code")
check(is.finite(good[[2]]$loss_mt) && good[[2]]$loss_mt == 40, "tonnes are kept when no fraction is given")

both <- write_and_read(
    paste(
        "event_id,crop,iso3,year,production_loss_fraction,production_loss_mt",
        "flood_a,wheat,AAA,2010,0.1,5",
        sep = "\n"
    ),
    "both.csv"
)
check(inherits(both, "error") && grepl("not both", both$message), "fraction and tonnes together are rejected")

unknown <- write_and_read(
    paste(
        "event_id,crop,iso3,year,production_loss_fraction",
        "flood_a,wheat,ZZZ,2010,0.1",
        sep = "\n"
    ),
    "unknown.csv"
)
check(inherits(unknown, "error") && grepl("not in the 195", unknown$message), "an unknown ISO3 is rejected")

dup <- write_and_read(
    paste(
        "event_id,crop,iso3,year,production_loss_fraction",
        "flood_a,wheat,AAA,2010,0.1",
        "flood_a,wheat,AAA,2010,0.2",
        sep = "\n"
    ),
    "dup.csv"
)
check(inherits(dup, "error") && grepl("duplicate", dup$message), "a repeated country in one event is rejected")

net <- list(nc = 3, iso = iso, P = c(AAA = 100, BBB = 50, CCC = 10))
dP <- shock_production_change(net, good[[1]])
check(abs(dP["AAA"] + 10) < 1e-8 && abs(dP["BBB"] + 10) < 1e-8, "fractions become losses on each country")
dP_mt <- shock_production_change(net, good[[2]])
check(abs(dP_mt["BBB"] + 40) < 1e-8, "a tonne loss is applied to the named country")

too_big <- good[[2]]
too_big$loss_mt <- 9999
over <- tryCatch(shock_production_change(net, too_big), error = function(e) e)
check(inherits(over, "error") && grepl("exceeds", over$message), "a tonne loss above baseline production is rejected")

unlink(list.files("tests/tmp", full.names = TRUE))

real <- read_flood_shocks(
    "inputs/yawei_placeholder_flood_shocks.csv",
    iso_codes = c("THA", "PAK", "MMR", "USA"),
    fao_lookup = read.csv("outputs/export_for_RIKEN/country_lookup_ISO3_FAO.csv", stringsAsFactors = FALSE)
)
check(length(real) == 6, "the illustrative shock file has six events")
check(all(vapply(real, function(e) e$shock_source, "") == "illustrative_placeholder"),
      "the illustrative file is labeled as a placeholder")

if (length(failures)) stop(paste(failures, collapse = "\n"))
message("All flood-shock checks passed")
