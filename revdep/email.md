Hi,

This is an automated email to let you know about the upcoming release of {{{my_package}}} version {{{my_version}}}, which will be submitted to CRAN on {{{date}}}.

To check for potential problems, I ran `R CMD check` on your package {{{your_package}}} (version {{{your_version}}}) with the development version of ggplot2 and plotly. I found {{{your_summary}}}.

{{#you_have_problems}}
{{{your_results}}}

{{#you_cant_install}}Looks like I couldn't install your package {{{your_package}}}, I'd recommend you check it yourself. Unfortunately I don't have the resources to manually fix installation failures.

{{/you_cant_install}}
{{^you_cant_install}}Please submit an update of your package {{{your_package}}} to fix any ERRORs or WARNINGs. They may not be caused by the update to {{{my_package}}}, but it really makes life easier if you also fix any other problems that may have accumulated over time. Please also try to minimize the NOTEs. It's not essential you do this, but the fewer the false positives the more likely I am to detect a real problem with your package.

{{/you_cant_install}}

To get the development version of {{{my_package}}} so you can run the checks yourself, you can run:

    # install.packages("devtools")
    ## installs development of ggplot2
    devtools::install_github("{{my_github}}")
    ## if you need plotly to work with ggplot2
    devtools::install_github("ropensci/plotly")

To see what's changed visit <https://github.com/{{{my_github}}}/blob/master/NEWS.md>.  To see a list of all reverse dependency checks for {{{my_package}}} visit <https://github.com/{{{my_github}}}/blob/master/revdep/README.md>.

{{/you_have_problems}}
{{^you_have_problems}}It looks like everything is ok, so you don't need to take any action, but you might want to read the NEWS, <https://github.com/{{{my_github}}}/blob/master/NEWS.md>, to see what's changed.  To see a list of all reverse dependency checks for {{{my_package}}} visit <https://github.com/{{{my_github}}}/blob/master/revdep/README.md>.

{{/you_have_problems}}

If you have any questions about this email, please feel free to respond directly.

Best,
{{{ me }}}
