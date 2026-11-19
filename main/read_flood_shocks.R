# Read a flood-production-loss file for the 2018-2021 benchmark.
# The year column is metadata. These runs do not switch to that year's network.
#
# Required columns: event_id, crop, year, and either iso3 or fao_code.
# A loss is production_loss_fraction (a positive share of baseline production)
# or production_loss_mt (positive metric tons). Provide one of those, not both.
# Several rows with the same event_id are one scenario. They must share a crop,
# a shock_source, and a reactive_ban_fraction.
#
#   shocks <- read_flood_shocks("inputs/yawei_placeholder_flood_shocks.csv", iso, fao)

read_flood_shocks <- function(path, iso_codes, fao_lookup = NULL) {
    raw <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
    required <- c("event_id", "crop", "year")
    missing <- setdiff(required, names(raw))
    if (length(missing)) stop("Shock file is missing columns: ", paste(missing, collapse = ", "))
    if (!any(c("iso3", "fao_code") %in% names(raw))) {
        stop("Shock file needs iso3 or fao_code")
    }
    if (!any(c("production_loss_fraction", "production_loss_mt") %in% names(raw))) {
        stop("Shock file needs production_loss_fraction or production_loss_mt")
    }
    if (!nrow(raw)) stop("Shock file has no rows")

    blank <- function(x) {
        if (is.null(x)) return(rep(TRUE, nrow(raw)))
        !nzchar(trimws(as.character(x))) | is.na(x)
    }
    num <- function(name) {
        if (!name %in% names(raw)) return(rep(NA_real_, nrow(raw)))
        out <- suppressWarnings(as.numeric(raw[[name]]))
        out
    }

    iso <- toupper(trimws(if ("iso3" %in% names(raw)) as.character(raw$iso3) else rep("", nrow(raw))))
    iso[is.na(raw$iso3) | blank(if ("iso3" %in% names(raw)) raw$iso3 else NULL)] <- ""
    fao <- num("fao_code")
    fraction <- num("production_loss_fraction")
    loss_mt <- num("production_loss_mt")
    ban <- num("reactive_ban_fraction")
    ban[is.na(ban)] <- 0
    source <- if ("shock_source" %in% names(raw)) as.character(raw$shock_source) else rep("unspecified", nrow(raw))
    source[is.na(source) | !nzchar(source)] <- "unspecified"
    problems <- character()

    if (!is.null(fao_lookup) && any(iso == "" & is.finite(fao))) {
        fao_key <- as.integer(fao_lookup$fao_code)
        fao_iso <- as.character(fao_lookup$iso3)
        for (i in which(iso == "" & is.finite(fao))) {
            hit <- fao_iso[match(as.integer(fao[i]), fao_key)]
            if (is.na(hit) || !nzchar(hit)) {
                problems <- c(problems, sprintf("row %d: FAO code %s is not in the country lookup", i, fao[i]))
            } else {
                iso[i] <- hit
            }
        }
    }

    for (i in seq_len(nrow(raw))) {
        if (!nzchar(as.character(raw$event_id[i]))) problems <- c(problems, sprintf("row %d: event_id is blank", i))
        if (!nzchar(iso[i])) problems <- c(problems, sprintf("row %d: no ISO3 code", i))
        else if (!iso[i] %in% iso_codes) problems <- c(problems, sprintf("row %d: %s is not in the 195-country network", i, iso[i]))
        if (!is.finite(raw$year[i])) problems <- c(problems, sprintf("row %d: year is not numeric", i))
        has_frac <- is.finite(fraction[i])
        has_mt <- is.finite(loss_mt[i])
        if (has_frac && has_mt) problems <- c(problems, sprintf("row %d: give a fraction or tonnes, not both", i))
        if (!has_frac && !has_mt) problems <- c(problems, sprintf("row %d: production loss is missing", i))
        if (has_frac && (fraction[i] <= 0 || fraction[i] > 1)) {
            problems <- c(problems, sprintf("row %d: production_loss_fraction must be in (0, 1]", i))
        }
        if (has_mt && loss_mt[i] <= 0) problems <- c(problems, sprintf("row %d: production_loss_mt must be positive", i))
        if (!is.finite(ban[i]) || ban[i] < 0 || ban[i] > 1) {
            problems <- c(problems, sprintf("row %d: reactive_ban_fraction must be in [0, 1]", i))
        }
    }

    key <- paste(raw$event_id, iso)
    dup <- key[iso != "" & duplicated(key)]
    if (length(dup)) problems <- c(problems, paste("duplicate country in an event:", unique(dup)))

    if (length(problems)) stop(paste(problems, collapse = "\n"))

    raw$iso3 <- iso
    raw$production_loss_fraction <- fraction
    raw$production_loss_mt <- loss_mt
    raw$reactive_ban_fraction <- ban
    raw$shock_source <- source
    events <- lapply(split(raw, raw$event_id), function(block) {
        if (length(unique(block$crop)) != 1) stop("event ", block$event_id[1], " mixes crops")
        if (length(unique(block$reactive_ban_fraction)) != 1) stop("event ", block$event_id[1], " mixes ban fractions")
        if (length(unique(block$shock_source)) != 1) stop("event ", block$event_id[1], " mixes shock_source")
        list(
            event_id = block$event_id[1],
            crop = block$crop[1],
            years = paste(sort(unique(block$year)), collapse = ","),
            shock_source = block$shock_source[1],
            iso3 = block$iso3,
            fraction = block$production_loss_fraction,
            loss_mt = block$production_loss_mt,
            reactive_ban_fraction = block$reactive_ban_fraction[1]
        )
    })
    unname(events)
}

shock_production_change <- function(net, event) {
    dP <- setNames(rep(0, net$nc), net$iso)
    for (i in seq_along(event$iso3)) {
        who <- event$iso3[i]
        if (is.finite(event$fraction[i])) {
            dP[who] <- dP[who] - event$fraction[i] * net$P[who]
        } else {
            if (event$loss_mt[i] > net$P[who] + 1) {
                stop(sprintf(
                    "%s loss of %.0f t exceeds %s baseline production of %.0f t",
                    event$event_id, event$loss_mt[i], who, net$P[who]
                ))
            }
            dP[who] <- dP[who] - event$loss_mt[i]
        }
    }
    dP
}
