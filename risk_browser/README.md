This is the code for the NHANES risk browser.

If you want to run it locally, you'll need to change the `path` variable in `server.R` to something that matches your system. You might need to change the `filePath` variable in `ui.R` as well.

At the moment, the code is not particularly clean. There are a few different variable-naming conventions, minimal comments at times, and out-of-date commands that have been commented-out instead of removed.

The `analytic` subdirectory contains all of the NHANES datasets used for the browser. The dataset structure was designed to be easily usable in the browser, so there's a lot of redundant variables in them.
