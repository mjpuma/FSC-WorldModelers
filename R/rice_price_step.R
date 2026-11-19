# Annual rice price index from exporter surplus shocks and stock-to-use.
# This is a reduced form. It is not the Agrimate agent update
# P * (demand / expected sales)^beta.
#
# Exportable surplus is max(production - domestic use, 0).
# The surplus shock is surplus / mean(surplus over baseline years) - 1.
# The exporter price factor is
#   exp(beta_surplus * surplus_shock + beta_stu * log(stu / stu_ref))
# with stu_ref the exporter's mean stock-to-use over the baseline years.
# The world index is the baseline-export-weighted mean of those factors.
# An exporter with zero baseline surplus stays out of the index.

rice_price_step <- function(df, baseline_years, beta_surplus, beta_stu,
                            use_imputed = FALSE) {
    if (!is.data.frame(df)) stop("df must be a data frame")
    if (!is.numeric(beta_surplus) || length(beta_surplus) != 1 || !is.finite(beta_surplus)) {
        stop("beta_surplus must be a finite number")
    }
    if (!is.numeric(beta_stu) || length(beta_stu) != 1 || !is.finite(beta_stu)) {
        stop("beta_stu must be a finite number")
    }
    baseline_years <- as.integer(baseline_years)
    need <- c("iso3", "year", "production_mt", "domestic_use_mt",
              "beginning_stocks_mt", "exports_mt")
    if (isTRUE(use_imputed)) {
        need <- c(need, "reserves_imputed_mt", "PSD_Missing_Flag")
    }
    missing <- setdiff(need, names(df))
    if (length(missing)) {
        stop("Missing columns: ", paste(missing, collapse = ", "))
    }

    out <- df
    out$iso3 <- as.character(out$iso3)
    out$year <- as.integer(out$year)
    out$production_mt <- suppress_nonfinite(out$production_mt)
    out$domestic_use_mt <- suppress_nonfinite(out$domestic_use_mt)
    out$exports_mt <- pmax(suppress_nonfinite(out$exports_mt), 0)
    stocks <- suppress_nonfinite(out$beginning_stocks_mt)
    if (isTRUE(use_imputed)) {
        flag <- out$PSD_Missing_Flag == 1
        imputed <- suppress_nonfinite(out$reserves_imputed_mt)
        replace <- !is.na(flag) & flag & !is.na(imputed)
        stocks[replace] <- imputed[replace]
    }
    out$stocks_mt <- pmax(stocks, 0)
    out$stocks_mt[is.na(stocks)] <- NA_real_
    out$exportable_surplus_mt <- pmax(out$production_mt - out$domestic_use_mt, 0)
    out$stock_to_use <- safe_stu(out$stocks_mt, out$domestic_use_mt)

    base <- out[out$year %in% baseline_years, , drop = FALSE]
    if (!nrow(base)) stop("No rows fall in baseline_years")
    ref <- aggregate_reference(base)
    out <- merge(out, ref, by = "iso3", all.x = TRUE, sort = FALSE)

    shock <- rep(NA_real_, nrow(out))
    has_base <- !is.na(out$baseline_surplus_mt) & out$baseline_surplus_mt > 0
    shock[has_base] <- out$exportable_surplus_mt[has_base] / out$baseline_surplus_mt[has_base] - 1
    out$surplus_shock <- shock

    factor <- rep(NA_real_, nrow(out))
    usable <- has_base &
        !is.na(out$surplus_shock) &
        !is.na(out$stock_to_use) & out$stock_to_use > 0 &
        !is.na(out$stu_ref) & out$stu_ref > 0
    factor[usable] <- exp(
        beta_surplus * out$surplus_shock[usable] +
            beta_stu * log(out$stock_to_use[usable] / out$stu_ref[usable])
    )
    out$price_factor <- factor
    out$in_index <- usable &
        !is.na(out$baseline_export_mt) & out$baseline_export_mt > 0 &
        is.finite(out$price_factor)

    world <- world_index(out)
    exporters <- out[, c(
        "iso3", "year", "exportable_surplus_mt", "surplus_shock",
        "stock_to_use", "stu_ref", "price_factor", "baseline_export_mt", "in_index"
    )]
    exporters <- exporters[order(exporters$iso3, exporters$year), , drop = FALSE]
    rownames(exporters) <- NULL
    list(exporters = exporters, world = world)
}

suppress_nonfinite <- function(x) {
    x <- as.numeric(x)
    x[!is.finite(x)] <- NA_real_
    x
}

safe_stu <- function(stocks, use) {
    out <- rep(NA_real_, length(stocks))
    ok <- !is.na(stocks) & !is.na(use) & use > 0 & stocks >= 0
    out[ok] <- stocks[ok] / use[ok]
    out
}

aggregate_reference <- function(base) {
    iso <- unique(base$iso3)
    baseline_surplus_mt <- vapply(iso, function(code) {
        mean(base$exportable_surplus_mt[base$iso3 == code], na.rm = TRUE)
    }, numeric(1))
    baseline_export_mt <- vapply(iso, function(code) {
        mean(base$exports_mt[base$iso3 == code], na.rm = TRUE)
    }, numeric(1))
    stu_ref <- vapply(iso, function(code) {
        vals <- base$stock_to_use[base$iso3 == code]
        vals <- vals[is.finite(vals)]
        if (!length(vals)) NA_real_ else mean(vals)
    }, numeric(1))
    baseline_surplus_mt[!is.finite(baseline_surplus_mt)] <- NA_real_
    baseline_export_mt[!is.finite(baseline_export_mt)] <- NA_real_
    data.frame(
        iso3 = iso,
        baseline_surplus_mt = baseline_surplus_mt,
        baseline_export_mt = baseline_export_mt,
        stu_ref = stu_ref,
        stringsAsFactors = FALSE
    )
}

world_index <- function(out) {
    years <- sort(unique(out$year))
    rows <- lapply(years, function(yr) {
        sub <- out[out$year == yr & out$in_index, , drop = FALSE]
        if (!nrow(sub)) {
            return(data.frame(
                year = yr, rice_price_index = NA_real_, n_exporters = 0L,
                stringsAsFactors = FALSE
            ))
        }
        w <- sub$baseline_export_mt
        data.frame(
            year = yr,
            rice_price_index = sum(w * sub$price_factor) / sum(w),
            n_exporters = nrow(sub),
            stringsAsFactors = FALSE
        )
    })
    do.call(rbind, rows)
}
