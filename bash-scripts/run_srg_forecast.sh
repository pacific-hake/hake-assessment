Rscript -e "source(here::here('R/all.R')); \
            catch_levels <- list(rep(525000, 4), '535,000 t', '16-525000'); \
            run_forecasts(here::here('models/srg_request'), catch_levels)"