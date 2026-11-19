## Helpers for building FSC production, trade, and reserve baselines.
## Sourced by main/ProcessInputs.R. The functions do not read the bulk FAO files
## themselves, so they can be tested on small tables.

fsc_root <- function() {
    if (file.exists("ancillary/country_list195_2012to2016.csv")) {
        return(normalizePath("."))
    }
    stop("Run ProcessInputs.R from the repository root (the directory that contains ancillary/).")
}

parse_year_arg <- function(x) {
    x <- trimws(x)
    if (grepl(":", x, fixed = TRUE)) {
        parts <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
        if (length(parts) != 2 || any(!is.finite(parts))) {
            stop("Could not parse --years. Use 2018:2021 or 2018,2019,2020.")
        }
        parts[1]:parts[2]
    } else {
        years <- as.integer(strsplit(x, ",", fixed = TRUE)[[1]])
        if (any(!is.finite(years))) stop("Could not parse --years.")
        years
    }
}

dot_names <- function(df) {
    names(df) <- gsub(" ", ".", names(df), fixed = TRUE)
    df
}

pick_col <- function(df, candidates, label) {
    hit <- candidates[candidates %in% names(df)]
    if (!length(hit)) {
        stop("Missing ", label, " column. Looked for: ", paste(candidates, collapse = ", "))
    }
    hit[[1]]
}

read_fsc_table <- function(path) {
    df <- read.csv(path, stringsAsFactors = FALSE, fileEncoding = "Latin-1",
                   check.names = FALSE)
    dot_names(df)
}

# FAO area codes that have to be split before they can join the 195-country list.
default_predecessor_splits <- function() {
    list(
        list(old = 206L, new = c(276L, 277L), last_year = 2011L,
             share_years = 2012:2014, label = "Sudan (former)"),
        list(old = 186L, new = c(272L, 273L), last_year = 2005L,
             share_years = 2006:2008, label = "Serbia and Montenegro")
    )
}

apply_predecessor_splits <- function(prod, splits = default_predecessor_splits()) {
    log <- character()
    shares <- list()
    for (sp in splits) {
        old_rows <- prod$FAOST_CODE == sp$old & prod$Year <= sp$last_year
        if (!any(old_rows)) next
        share_rows <- prod$FAOST_CODE %in% sp$new & prod$Year %in% sp$share_years
        if (!any(share_rows)) {
            stop(sprintf(
                "Cannot split %s (FAO code %s): no successor production in %s-%s.",
                sp$label, sp$old, min(sp$share_years), max(sp$share_years)
            ))
        }
        totals <- tapply(prod$value[share_rows], prod$FAOST_CODE[share_rows], sum)
        totals <- totals[as.character(sp$new)]
        totals[is.na(totals)] <- 0
        if (sum(totals) <= 0) {
            stop(sprintf("Cannot split %s: successor production sums to zero.", sp$label))
        }
        share_vec <- totals / sum(totals)
        names(share_vec) <- as.character(sp$new)
        shares[[sp$label]] <- share_vec
        log <- c(log, sprintf(
            "Split FAO %s (%s) through %s using %s-%s production shares: %s",
            sp$old, sp$label, sp$last_year, min(sp$share_years), max(sp$share_years),
            paste(sprintf("%s=%.4f", names(share_vec), share_vec), collapse = ", ")
        ))
        pred <- prod[old_rows, , drop = FALSE]
        pieces <- lapply(sp$new, function(code) {
            out <- pred
            out$FAOST_CODE <- as.integer(code)
            out$value <- pred$value * unname(share_vec[as.character(code)])
            out
        })
        prod <- rbind(prod[!old_rows, , drop = FALSE], do.call(rbind, pieces))
    }
    list(prod = prod, log = log, shares = shares)
}

psd_name_map <- function() {
    c(
        "Bolivia" = "Bolivia (Plurinational State of)",
        "Brunei" = "Brunei Darussalam",
        "Burkina" = "Burkina Faso",
        "Burma" = "Myanmar",
        "China" = "China (mainland)",
        "Eswatini" = "Swaziland",
        "Former Czechoslovakia" = "Czechoslovakia",
        "Former Yugoslavia" = "Yugoslav SFR",
        "Gambia, The" = "Gambia",
        "Hong Kong" = "Hong Kong SAR, China",
        "Iran" = "Iran (Islamic Republic of)",
        "Korea, North" = "Democratic People's Republic of Korea",
        "Korea, South" = "Republic of Korea",
        "Laos" = "Lao People's Democratic Republic",
        "Macedonia" = "The former Yugoslav Republic of Macedonia",
        "North Macedonia" = "The former Yugoslav Republic of Macedonia",
        "Moldova" = "Republic of Moldova",
        "Russia" = "Russian Federation",
        "Syria" = "Syrian Arab Republic",
        "Taiwan" = "Taiwan",
        "Tanzania" = "United Republic of Tanzania",
        "Union of Soviet Socialist Repu" = "USSR",
        "United States" = "United States of America",
        "Venezuela" = "Venezuela (Bolivarian Republic of)",
        "Vietnam" = "Viet Nam"
    )
}

harmonize_psd_names <- function(x, name_map = psd_name_map()) {
    out <- x
    hit <- match(out, names(name_map))
    out[!is.na(hit)] <- unname(name_map[hit[!is.na(hit)]])
    out <- gsub("Congo (Brazzaville)", "Congo", out, fixed = TRUE)
    out <- gsub("Congo (Kinshasa)", "Democratic Republic of the Congo", out, fixed = TRUE)
    out <- gsub("Yemen (Aden)", "Yemen Dem", out, fixed = TRUE)
    out <- gsub("Yemen (Sanaa)", "Yemen Ar Rp", out, fixed = TRUE)
    out
}

eu_member_names <- function() {
    c("Austria", "Belgium", "Denmark", "Finland", "France", "Germany", "Greece",
      "Ireland", "Italy", "Luxembourg", "Netherlands", "Portugal", "Spain", "Sweden",
      "United Kingdom", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", "Estonia",
      "Hungary", "Latvia", "Lithuania", "Poland", "Romania", "Slovakia", "Slovenia")
}

# prod: FAOST_CODE, Year, itemCode, value (tonnes)
# trade: reporter, partner, Year, itemCode, value (tonnes)
# stocks: Country, Year, Product, value (1000 metric tons of ending stocks)
build_fsc_baseline <- function(prod, trade, stocks, country_list, years,
                               unit = c("kcal", "tonnes"), spec,
                               splits = default_predecessor_splits(),
                               eu_members = eu_member_names()) {
    unit <- match.arg(unit)
    log <- character()
    country_list <- country_list[order(country_list$iso3), ]
    country_list$FAOST_CODE <- as.integer(country_list$FAOST_CODE)
    years <- as.integer(years)

    prod$FAOST_CODE <- as.integer(prod$FAOST_CODE)
    prod$Year <- as.integer(prod$Year)
    prod$itemCode <- as.integer(prod$itemCode)
    split_res <- apply_predecessor_splits(prod, splits)
    prod <- split_res$prod
    log <- c(log, split_res$log)

    keep_prod <- prod$itemCode %in% spec$prod_ids & prod$Year %in% years &
        prod$FAOST_CODE %in% country_list$FAOST_CODE
    prod <- prod[keep_prod, , drop = FALSE]
    prod$factor <- conversion_factor(prod$itemCode, unit, spec)
    dropped_prod <- !is.finite(prod$factor) | prod$factor == 0
    if (any(dropped_prod)) {
        log <- c(log, sprintf(
            "Dropped production item(s) with no %s factor: %s",
            unit, paste(unique(prod$itemCode[dropped_prod]), collapse = ", ")
        ))
        prod <- prod[!dropped_prod, , drop = FALSE]
    }
    prod$converted <- prod$value * prod$factor
    prod_sum <- aggregate(converted ~ FAOST_CODE + Year, prod, sum)
    P_mat <- pivot_country_year(prod_sum$FAOST_CODE, prod_sum$Year, prod_sum$converted,
                                country_list$FAOST_CODE, years)
    P0 <- rowMeans(P_mat)

    trade$reporter <- as.integer(trade$reporter)
    trade$partner <- as.integer(trade$partner)
    trade$Year <- as.integer(trade$Year)
    trade$itemCode <- as.integer(trade$itemCode)
    self <- trade$reporter == trade$partner
    if (any(self, na.rm = TRUE)) {
        log <- c(log, sprintf("Dropped %d self-trade rows.", sum(self, na.rm = TRUE)))
        trade <- trade[!self, , drop = FALSE]
    }
    keep_trade <- trade$itemCode %in% spec$trade_ids & trade$Year %in% years &
        trade$reporter %in% country_list$FAOST_CODE &
        trade$partner %in% country_list$FAOST_CODE
    outside <- !(trade$reporter %in% country_list$FAOST_CODE &
                     trade$partner %in% country_list$FAOST_CODE) &
        trade$itemCode %in% spec$trade_ids & trade$Year %in% years
    if (any(outside, na.rm = TRUE)) {
        log <- c(log, sprintf(
            "Ignored %d trade rows whose reporter or partner is outside the 195-country list.",
            sum(outside, na.rm = TRUE)
        ))
    }
    trade <- trade[keep_trade, , drop = FALSE]
    trade$factor <- conversion_factor(trade$itemCode, unit, spec)
    dropped_trade <- !is.finite(trade$factor) | trade$factor == 0
    if (any(dropped_trade)) {
        log <- c(log, sprintf(
            "Dropped trade item(s) with no %s factor: %s",
            unit, paste(unique(trade$itemCode[dropped_trade]), collapse = ", ")
        ))
        trade <- trade[!dropped_trade, , drop = FALSE]
    }
    trade$converted <- trade$value * trade$factor
    if (nrow(trade)) {
        trade_sum <- aggregate(converted ~ reporter + partner + Year, trade, sum)
        trade_mean <- aggregate(converted ~ reporter + partner, trade_sum, mean)
    } else {
        trade_mean <- data.frame(reporter = integer(), partner = integer(), converted = numeric())
    }
    code_to_i <- match(country_list$FAOST_CODE, country_list$FAOST_CODE)
    names(code_to_i) <- as.character(country_list$FAOST_CODE)
    E <- matrix(0, nrow = nrow(country_list), ncol = nrow(country_list),
                dimnames = list(country_list$iso3, country_list$iso3))
    if (nrow(trade_mean)) {
        ri <- match(as.character(trade_mean$reporter), names(code_to_i))
        ci <- match(as.character(trade_mean$partner), names(code_to_i))
        ok <- !is.na(ri) & !is.na(ci)
        E[cbind(ri[ok], ci[ok])] <- trade_mean$converted[ok]
    }
    diag_sum <- sum(diag(E))
    if (diag_sum != 0) {
        log <- c(log, sprintf("Zeroed a diagonal sum of %g.", diag_sum))
        diag(E) <- 0
    }

    stock_res <- build_reserves(
        stocks = stocks, country_list = country_list, years = years, unit = unit,
        spec = spec, P_mat = P_mat, eu_members = eu_members,
        scg_shares = split_res$shares[["Serbia and Montenegro"]]
    )
    log <- c(log, stock_res$log)

    imports <- colSums(E)
    exports <- rowSums(E)
    domestic <- P0 + imports - exports
    reexport <- data.frame(
        iso3 = country_list$iso3,
        Country = country_list$Country,
        production = P0,
        imports = imports,
        exports = exports,
        domestic_use = domestic,
        exports_exceed_supply = exports > (P0 + imports + 1e-8),
        stringsAsFactors = FALSE
    )
    n_reexport <- sum(reexport$exports_exceed_supply)
    if (n_reexport) {
        log <- c(log, sprintf(
            "%d countries have exports greater than production plus imports: %s",
            n_reexport,
            paste(reexport$iso3[reexport$exports_exceed_supply], collapse = ", ")
        ))
    }

    Pkbyc <- data.frame(
        FAOST_CODE = country_list$FAOST_CODE,
        iso3 = country_list$iso3,
        Country = country_list$Country,
        P0 = as.numeric(P0),
        stringsAsFactors = FALSE
    )
    list(
        E0_avg = E,
        Pkbyc = Pkbyc,
        R0 = stock_res$R0,
        stocks_report = stock_res$report,
        reexport_report = reexport,
        log = log,
        unit = unit
    )
}

conversion_factor <- function(item_code, unit, spec) {
    key <- as.character(item_code)
    if (unit == "kcal") {
        out <- spec$kcal[key]
    } else {
        out <- spec$tonne_factor[key]
    }
    out[is.na(out)] <- NA_real_
    as.numeric(out)
}

pivot_country_year <- function(codes, years, values, all_codes, year_levels) {
    mat <- matrix(0, nrow = length(all_codes), ncol = length(year_levels),
                  dimnames = list(as.character(all_codes), as.character(year_levels)))
    if (!length(values)) return(mat)
    ri <- match(as.character(codes), rownames(mat))
    ci <- match(as.character(years), colnames(mat))
    ok <- !is.na(ri) & !is.na(ci) & is.finite(values)
    # Sum in case the caller did not pre-aggregate.
    split_id <- paste(ri[ok], ci[ok], sep = ":")
    summed <- tapply(values[ok], split_id, sum)
    idx <- do.call(rbind, strsplit(names(summed), ":", fixed = TRUE))
    mat[cbind(as.integer(idx[, 1]), as.integer(idx[, 2]))] <- as.numeric(summed)
    mat
}

build_reserves <- function(stocks, country_list, years, unit, spec, P_mat,
                           eu_members, scg_shares = NULL) {
    log <- character()
    stocks$Country <- harmonize_psd_names(stocks$Country)
    stocks$Year <- as.integer(stocks$Year)
    stocks <- stocks[stocks$Product %in% spec$reserve_products & stocks$Year %in% years, , drop = FALSE]
    if (!nrow(stocks)) {
        log <- c(log, sprintf(
            "No PSD ending-stock rows for %s in %s-%s.",
            paste(spec$reserve_products, collapse = "/"), min(years), max(years)
        ))
    }
    scg <- stocks$Country == "Serbia and Montenegro"
    if (any(scg)) {
        if (is.null(scg_shares) || !all(c("272", "273") %in% names(scg_shares))) {
            stop("PSD still has Serbia and Montenegro, but production shares for FAO 272/273 were not estimated. Include 2006-2008 production in the extract.")
        }
        name_272 <- country_list$Country[match(272L, country_list$FAOST_CODE)]
        name_273 <- country_list$Country[match(273L, country_list$FAOST_CODE)]
        base <- stocks[scg, , drop = FALSE]
        srb <- base
        srb$Country <- name_272
        srb$value <- base$value * unname(scg_shares["272"])
        mne <- base
        mne$Country <- name_273
        mne$value <- base$value * unname(scg_shares["273"])
        stocks <- rbind(stocks[!scg, , drop = FALSE], srb, mne)
        log <- c(log, sprintf(
            "Split PSD 'Serbia and Montenegro' to %s (%.4f) and %s (%.4f).",
            name_272, scg_shares["272"], name_273, scg_shares["273"]
        ))
    }
    factor <- if (unit == "kcal") spec$reserve_kcal else spec$reserve_tonne_factor
    # PSD ending stocks are in 1000 metric tons.
    stocks$converted <- stocks$value * 1000 * factor
    if (nrow(stocks)) {
        stock_sum <- aggregate(converted ~ Country + Year, stocks, sum)
    } else {
        stock_sum <- data.frame(Country = character(), Year = integer(), converted = numeric())
    }
    countries <- unique(c(country_list$Country, eu_members, "European Union"))
    R_mat <- matrix(NA_real_, nrow = length(countries), ncol = length(years),
                    dimnames = list(countries, as.character(years)))
    if (nrow(stock_sum)) {
        ri <- match(stock_sum$Country, rownames(R_mat))
        ci <- match(as.character(stock_sum$Year), colnames(R_mat))
        ok <- !is.na(ri) & !is.na(ci)
        R_mat[cbind(ri[ok], ci[ok])] <- stock_sum$converted[ok]
    }
    present_names <- unique(stock_sum$Country)

    if ("European Union" %in% rownames(R_mat)) {
        members <- intersect(eu_members, country_list$Country)
        prod_idx <- match(members, country_list$Country)
        if (any(!is.na(prod_idx)) && any(is.finite(R_mat["European Union", ]))) {
            for (j in seq_along(years)) {
                eu_stock <- R_mat["European Union", j]
                if (!is.finite(eu_stock)) next
                weights <- P_mat[prod_idx, j]
                weights[!is.finite(weights)] <- 0
                if (sum(weights) <= 0) {
                    log <- c(log, sprintf(
                        "EU stock in %s was not apportioned because member production sums to zero.",
                        years[j]
                    ))
                    next
                }
                R_mat[members, j] <- eu_stock * weights / sum(weights)
            }
            log <- c(log, "Apportioned European Union ending stocks to member countries by production.")
        }
    }

    report_status <- rep("missing_psd_row", nrow(country_list))
    direct <- country_list$Country %in% present_names
    report_status[direct] <- "psd_reported"
    # True zeros are reported rows whose converted stock is 0 in every requested year.
    if (any(direct)) {
        zero_direct <- vapply(which(direct), function(i) {
            vals <- R_mat[country_list$Country[i], ]
            all(is.finite(vals) & vals == 0)
        }, logical(1))
        report_status[which(direct)[zero_direct]] <- "psd_true_zero"
    }
    eu_hit <- country_list$Country %in% eu_members
    # Members receive an apportioned value even when USDA does not list them.
    got_eu <- eu_hit & apply(is.finite(R_mat[country_list$Country, , drop = FALSE]), 1, any)
    report_status[got_eu] <- "eu_apportioned"

    R_use <- R_mat[country_list$Country, , drop = FALSE]
    missing_cells <- !is.finite(R_use)
    R_use[missing_cells] <- 0
    R0 <- rowMeans(R_use)
    names(R0) <- country_list$iso3
    report <- data.frame(
        iso3 = country_list$iso3,
        Country = country_list$Country,
        status = report_status,
        R0 = as.numeric(R0),
        stringsAsFactors = FALSE
    )
    log <- c(log, sprintf(
        "Stock status counts: %s",
        paste(sprintf("%s=%d", names(table(report_status)), table(report_status)), collapse = ", ")
    ))
    unmatched <- setdiff(present_names, c(country_list$Country, eu_members, "European Union"))
    if (length(unmatched)) {
        log <- c(log, paste("PSD names with stocks that did not match the country list:",
                            paste(unmatched, collapse = ", ")))
    }
    list(R0 = R0, report = report, log = log)
}

load_mass_factors <- function(path = "ancillary/mass_equivalent_factors.csv") {
    read.csv(path, stringsAsFactors = FALSE)
}

crop_spec_from_lists <- function(crop, unit = c("kcal", "tonnes"),
                                  milling_rate = 0.67,
                                  factor_path = "ancillary/mass_equivalent_factors.csv") {
    unit <- match.arg(unit)
    crop <- tolower(crop)
    factors <- load_mass_factors(factor_path)
    factors <- factors[factors$crop == crop, ]
    if (!nrow(factors)) stop("No mass-equivalent rows for crop: ", crop)

    stem <- switch(crop,
                   wheat = "Wheat", maize = "Maize", rice = "Rice", soybean = "Soybean",
                   stop("crop must be wheat, maize, rice, or soybean"))
    reserve_products <- switch(crop,
                               wheat = "Wheat",
                               maize = "Corn",
                               rice = "Rice, Milled",
                               soybean = "Soybeans")
    if (crop == "soybean" && unit == "kcal") {
        stop("Soybean has no kilocalorie commodity list in this repository. Use --unit tonnes.")
    }
    if (unit == "tonnes") {
        if (crop == "rice" && 27L %in% factors$cropid) {
            factors$tonne_factor[factors$cropid == 27L] <- milling_rate
            # Husked rice is about 80% of paddy, so its milled factor is milling_rate/0.80.
            if (any(factors$cropid %in% c(28L, 29L))) {
                factors$tonne_factor[factors$cropid %in% c(28L, 29L)] <- milling_rate / 0.80
            }
        }
        use <- factors$tonne_factor > 0
        spec <- list(
            crop = crop,
            prod_ids = factors$cropid[factors$in_production == 1 & factors$tonne_factor > 0],
            trade_ids = factors$cropid[use],
            kcal = setNames(rep(NA_real_, sum(use)), factors$cropid[use]),
            tonne_factor = setNames(factors$tonne_factor[use], factors$cropid[use]),
            reserve_products = reserve_products,
            reserve_kcal = NA_real_,
            reserve_tonne_factor = 1,
            excluded_items = factors$item[factors$tonne_factor <= 0]
        )
        spec
    } else {
        prod_list <- read.csv(sprintf("ancillary/%s_Avg20152017cropcommodity_prodlist.csv", stem),
                              stringsAsFactors = FALSE)
        trade_list <- read.csv(sprintf("ancillary/%s_Avg20152017cropcommodity_tradelist.csv", stem),
                               stringsAsFactors = FALSE)
        reserve_list <- read.csv(sprintf("ancillary/%s_Avg20152017cropcommodity_reserveslist.csv", stem),
                                 stringsAsFactors = FALSE)
        kcal_col <- if ("kcal.ton" %in% names(trade_list)) "kcal.ton" else "kcal/ton"
        kcal_trade <- setNames(as.numeric(trade_list[[kcal_col]]), trade_list$cropid)
        kcal_prod <- setNames(as.numeric(prod_list[[kcal_col]]), prod_list$cropid)
        kcal <- kcal_trade
        kcal[names(kcal_prod)] <- kcal_prod
        reserve_kcal <- as.numeric(reserve_list[[kcal_col]][match(reserve_products, reserve_list$cropname)])
        if (crop == "maize") {
            # USDA calls it Corn. The kcal factor is the maize grain factor.
            reserve_kcal <- as.numeric(reserve_list[[kcal_col]][reserve_list$cropid == 56][1])
        }
        if (crop == "rice") {
            milled <- reserve_list$cropid == 31
            reserve_kcal <- as.numeric(reserve_list[[kcal_col]][milled][1])
        }
        list(
            crop = crop,
            prod_ids = as.integer(prod_list$cropid),
            trade_ids = as.integer(trade_list$cropid),
            kcal = kcal,
            tonne_factor = setNames(rep(NA_real_, length(kcal)), names(kcal)),
            reserve_products = reserve_products,
            reserve_kcal = reserve_kcal,
            reserve_tonne_factor = NA_real_,
            excluded_items = character()
        )
    }
}

normalize_production_table <- function(df) {
    df <- dot_names(df)
    area <- pick_col(df, c("Area.Code", "FAOST_CODE", "AreaCode"), "production area")
    item <- pick_col(df, c("Item.Code", "itemCode", "ItemCode"), "production item")
    year <- pick_col(df, c("Year", "Year.Code"), "production year")
    value <- pick_col(df, c("Value", "value"), "production value")
    out <- data.frame(
        FAOST_CODE = as.integer(df[[area]]),
        Year = as.integer(df[[year]]),
        itemCode = as.integer(df[[item]]),
        value = as.numeric(df[[value]]),
        stringsAsFactors = FALSE
    )
    if ("Element" %in% names(df)) {
        out <- out[df$Element == "Production", , drop = FALSE]
    }
    out <- out[is.finite(out$value) & is.finite(out$FAOST_CODE) & is.finite(out$Year), , drop = FALSE]
    out
}

normalize_trade_table <- function(df) {
    df <- dot_names(df)
    reporter <- pick_col(df, c("Reporter.Country.Code", "reporter", "ReporterCountryCode"), "trade reporter")
    partner <- pick_col(df, c("Partner.Country.Code", "partner", "PartnerCountryCode"), "trade partner")
    item <- pick_col(df, c("Item.Code", "cropid", "ItemCode"), "trade item")
    year <- pick_col(df, c("Year", "year"), "trade year")
    value <- pick_col(df, c("Value", "value"), "trade value")
    out <- data.frame(
        reporter = suppressWarnings(as.integer(df[[reporter]])),
        partner = suppressWarnings(as.integer(df[[partner]])),
        Year = suppressWarnings(as.integer(df[[year]])),
        itemCode = suppressWarnings(as.integer(df[[item]])),
        value = as.numeric(df[[value]]),
        stringsAsFactors = FALSE
    )
    if ("Element" %in% names(df)) {
        out <- out[df$Element == "Export Quantity", , drop = FALSE]
    }
    out <- out[is.finite(out$value) & is.finite(out$reporter) & is.finite(out$partner), , drop = FALSE]
    out
}

normalize_psd_table <- function(df) {
    df <- dot_names(df)
    if (all(c("Country", "Year", "Product", "value") %in% names(df))) {
        return(data.frame(
            Country = df$Country, Year = as.integer(df$Year), Product = df$Product,
            value = as.numeric(df$value), stringsAsFactors = FALSE
        ))
    }
    country <- pick_col(df, c("Country_Name", "Country"), "PSD country")
    year <- pick_col(df, c("Market_Year", "Year"), "PSD year")
    product <- pick_col(df, c("Commodity_Description", "Product"), "PSD commodity")
    value <- pick_col(df, c("Value", "value"), "PSD value")
    out <- data.frame(
        Country = df[[country]],
        Year = as.integer(df[[year]]),
        Product = df[[product]],
        value = as.numeric(df[[value]]),
        stringsAsFactors = FALSE
    )
    if ("Attribute_Description" %in% names(df)) {
        out <- out[df$Attribute_Description == "Ending Stocks", , drop = FALSE]
    }
    out <- out[is.finite(out$value), , drop = FALSE]
    out
}
