
# Introduction

This is a "how to" guide for writing and improving functions for finance-related R packages such as `PerformanceAnalytics`, with many of the principles carried into other packages as well.  

`PerformanceAnalytics` is a collection of functionality for doing returns-based analysis in finance.  Most of what it does is focused on performance and risk evaluation.  Companion projects under development on GitHub include:

- `PortfolioAnalytics`: focuses on portfolio construction and ex-ante analysis of portfolios;
- `PortfolioAttribution`: calculates ex-post performance attribution of portfolios given returns and weights; and
- `FactorAnalytics`: provides factor-based analysis of returns.

The specifics and examples in this document, however, will focus on `PerformanceAnalytics`.

If you are reading this document, you are likely already using `PerformanceAnalytics` or another of our packages.  Perhaps you have some code that you think would be relevant to include, or would like to collaborate on a project to develop ideas from your own or other research.  Maybe you've identified ways that the package can be improved or identified a bug.  If so, we appreciate you taking the time to help -- your participation is critical to us continuing to improve these packages. 

This document is written with two types of contributors in mind.  The first kind is one whose contribution is limited in scope - say, a bug fix to an existing function or a new function thought to be useful.  This document should be helpful for this type of contributor, but it may seem like overkill.  Be assured that we welcome contributions of all kinds -- not just 'finished' ones.  If you think you are in this camp, this document will hopefully answer some questions but please do not hesitate to contact us with ideas and snippets.

The second type of contributor we have in mind is more the focus of this document.  This kind of contributor is working on a defined project around a body of research, porting existing code from another language, or developing an idea in collaboration with us.  We encourage this type of contribution and for many years have collaborated with students through the [Google Summer of Code](https://summerofcode.withgoogle.com/) program.  This type of project-oriented collaboration requires more organization and structure -- sometimes there's a plan and a schedule, a mentor or co-author or three, maybe a book, a dissertation, or a few journal articles.

Either way, this guide assembles the things that we've said in answer to questions over the years to those we've collaborated with.  We hope it provides a good foundation for you to get started, but please do not hesitate to ask for more detail or help where there isn't enough.  And finally, please don't let our strident list of requirements below prevent you from contributing -- we value hastily constructed, first-draft, proof-of-concept code, too. Nevertheless, hopefully the following subsections regarding how to submit feature requests, bug reports, pull requests and how to write good commit messages give you some confidence as you take the first steps toward contributing to the package in some way.

## Submitting a bug report or a feature request

If you have a bug report or feature request, below are some contributing pointers you can follow:

1. Determine which repository the bug/feature should be reported in. The process
   of creating a [*minimal*, reproducible example](http://stackoverflow.com/q/5963269/271616)
   should identify the package that contains the bug or should contain the
   feature. Please email the maintainer if you're unsure where to create an
   issue.
2. Search current open GitHub [issues](https://github.com/braverock/quantstrat/issues)
   to check if the bug/feature has already been reported/requested.
3. Ensure your fork and local copy are up-to-date, and verify the bug still
   exists in the HEAD of the master branch.
4. If the bug exists in the HEAD of master, and you can't find an open issue,
   then [open a new issue](https://github.com/braverock/quantstrat/issues).
   Please be sure to:
    * Use an informative and descriptive title,
    * Describe the expected behavior and why you think the current behavior is
      a bug.
    * Include as much relevant information as possible; at minimum:
        * a [*minimal*, reproducible example](http://stackoverflow.com/q/5963269/271616)
        * the output from `sessionInfo()`

## Submitting a Pull Request

1. Changes that are purely cosmetic in nature (e.g. whitespace changes, code
   formatting, etc) will generally not be accepted because they do not add to
   the stability, functionality, or testability of the project.
2. Unless the change is extremely trivial (e.g. typos), please
   [create an issue](#have-a-bug-report-or-feature-request?) and wait for
   feedback *before* you start work on a pull request. That will avoid the
   possibility you spend time on a patch that won't be merged.
3. Create a branch for the feature/bug fix reported in the issue. Please use a
   short and descriptive branch name that starts with the issue number (e.g.
   123_custom_function). Use that branch as the base for your pull request.
   Pull requests on your version of `master` will not be accepted, because
   they can make it difficult for you to update your fork if your pull request
   isn't incorporated verbatim.
4. A pull request should only be for one issue, so please `git rebase -i` and
   squash the commits on your feature branch into one commit before creating
   the pull request. Please use `git commit --amend` to amend your commit if
   you are asked to make changes. It's okay to force update your pull request
   with `git push --force`.
5. Please write a great [commit message](#commit-messages).
6. It would be much appreciated if you also add tests that cover your changes.

## Stack Overflow & R-SIG-Finance

Other useful community sources for asking questions are [Stack Overflow](http://stackoverflow.com/questions/tagged/r)
or the [R-SIG-Finance](https://stat.ethz.ch/mailman/listinfo/r-sig-finance)
mailing list (you must subscribe to post)

## Good Commit Messages

Follow the [The Seven Rules](http://chris.beams.io/posts/git-commit/#seven-rules)
of [How to Write a Git Commit Message](http://chris.beams.io/posts/git-commit/).
Pay particular attention to [rule 7: Use the body to explain what and why
versus how](http://chris.beams.io/posts/git-commit/#why-not-how). The body
should also include the motivation for the change and how it compares to prior
behavior.

If the commit is to fix a bug or add a feature, the commit message should
contain enough information to understand the bug/feature without having to
reference an external tracker (e.g. GitHub issues). But please *do reference
the GitHub issue* on the last line of your commit message body. For example,
here is a great [xts commit message](https://github.com/joshuaulrich/xts/commit/ce1b667ab7c38cb2633fca0075652a69e5d2a343) from Joshua Ulrich's powerful xts package:

```text
Correct endpoints when index is before the epoch

The endpoints C code casts the double index to long, which truncates
it toward zero. This behavior is desired when the index is positive,
because it moves the endpoint *back* in time. But when the index is
negative, truncating toward zero moves the endpoint *forward* in time.

This is also an issue if the index value is stored as integer, since the
C99 specification states that integer division truncates toward zero.

If the first index value is less than zero, branch into a special case
to handle pre-epoch index values. This avoids performance degradation
if all index values are after the epoch.

If the index value is less than zero, simply add 1 to offset the
truncation toward zero. We also need to furthre adjust the potential
endpoint value if the index is exactly equal to zero.

Fixes #144.
```

## References

1. The [data.table Contributing Guide](https://github.com/Rdatatable/data.table/blob/master/Contributing.md).
2. The [Atom Contributing Guide](https://github.com/atom/atom/blob/master/CONTRIBUTING.md).
3. How to create a [minimal, reproducible example](http://stackoverflow.com/q/5963269/271616).
4. [How to Write a Git Commit Message](http://chris.beams.io/posts/git-commit/).
5. The [Mercurial Contributing Guide](https://www.mercurial-scm.org/wiki/ContributingChanges).
6. The [Hugo Contributing Guide](https://github.com/spf13/hugo/blob/master/CONTRIBUTING.md).

## How to become a contributor

As mentioned above, the easiest way to become a contributor is to email us, or submit an idea or bug within the appropriate project on GitHub.  We like to hear about what you are using the package for, answer any questions you might have, chase down bugs you encounter, or consider changes and additions that would improve the package.  

We ask more of those seeking longer-term developer membership in our projects. Packages like those listed above are used by professionals and educators around the world.  As such, we are careful when vetting new members of the project.

Typically, the process starts by you becoming a member of the community - perhaps asking good questions, identifying issues with the code or proposing new functionality.  The next step is usually for you to propose patches or pull requests for existing code, but new additions, bug fixes, and adjustments to documentation are all welcome.  All patches and pull requests are reviewed by one of the core team members.  After several accepted patches or pull requests, and once we get to know you and your needs, then the core team will discuss adding another member to the team.

We look forward to hearing more from you as you use and enhance these packages.  Hopefully you'll be able to join us as a contributor in the near future. 

<!-- [ one paragraph on Copyrights and licensing? ] -->

The remainder of this paper is in three sections.  The first provides a brief introduction to the tools we use, and might be generally helpful if you are newer to R.  The second section describes the principles we use to guide our development, and the third section discusses in more detail and with examples how to use these principles as you develop functions.  By the end of this document, you should have a good understanding about not only *how* but *why* we do things the way that we do. 

# Getting Established

This section introduces the tools we use for development.  While these aren't all required to be successful, we find that it is easier to help people through issues when we are all working with the same tools.

## Find the project on GitHub

We moved our packages to [GitHub](http://github.com/braverock) for development a few years ago.  GitHub provides a set of tools for source code management (`git`) supported by some web-based functionality.  You'll need to register as a site user.    

## Get proficient at using `git`

GitHub uses `git` for version control, and it is very important for you to be adept at using it.  For more specifics about how to use `git`, take a look at the book, [Pro Git](https://git-scm.com/book/en/v2).  The book will certainly be overkill for your initial use, but the first few chapters are worth reviewing.  We also find that new users may benefit from the [introductory course about `git` on DataCamp](https://learn.datacamp.com/courses/introduction-to-git) or [Codecademy](https://www.codecademy.com/learn/learn-git).

Please do not hesitate to ask for help if you get stuck - this is a critical component of our workflow and will be important for keeping everyone up to date with current code.  

If you want to commit to GitHub, the code needs to be checked out using the instructions for developer checkout, via ssh, not the normal anonymous checkout.  If you previously checked out the code anonymously, you will need to check it out again using your GitHub login (and ssh key) before you will be able to commit your changes.  

When making modifications or adding new functions, make frequent, small, tested commits.  Those are easier for us to stay on top of.  Please try to make commits to `git` at least daily while coding.  If you make an improvement and it works - check it in.  This way we will be able to test code more frequently and we will know quickly if something is broken.  

We believe that there are thousands of financial professionals around the world who regularly use and rely on `PerformanceAnalytics`.  When things break we hear about it, and often only minutes after a source commit.  All code has bugs, of course, and we work hard to fix bugs as we are made aware of them.  We also try as hard as we can to not introduce new bugs when we touch code.

Make sure to test other behaviors beyond what you are working on to make sure that other functionality is unaffected by the changes you introduced.

We suggest an iterative approach to development: first make it work, then make it work *correctly*, and finally make it work fast (if needed).  Do not try to make it perfect or even pretty before checking something in.  

Make sure you always provide a useful message for each commit.  Look at the log of the repository you will be working with to get a feel for the commit message style. 

## Create a branch

Functions that are candidates for inclusion in `PerformanceAnalytics` should be checked into a well-named (e.g., `username-taskname`) branch using `git`.  When a function is complete, documented, and tested, you should submit a pull request that we can review along with a demonstration of the functionality you added or modified.

This structure provides an easy way for you to test whether the functions will "build" correctly. The next section will describe how you should be able to use `R CMD check` on your "package", so that the check process runs all your examples and demonstrates that everything is working the way you expect.

## Install build tools and use `R CMD check`

Everyone should know how to build packages from source. 

A *nix machine should have everything needed (see Appendix A of ['R Installation and Administration'](http://cran.r-project.org/doc/manuals/R-admin.pdf)), but a regular Windows machine will not.  Windows users will need to install [RTools](
http://cran.r-project.org/bin/windows/Rtools/), a collection of resources for building packages for R under Microsoft Windows (see Appendix D of ['R Installation and Administration'](http://cran.r-project.org/doc/manuals/R-admin.pdf)).  

Once all tools are in place, you should be able to build a package by opening a shell, moving to the directory of the package, and typing `R CMD INSTALL packagename`.  

## Use `roxygen2` for documentation

We either have or are currently converting all of our packages' documentation to [`roxygen2`](http://cran.r-project.org/web/packages/roxygen2/index.html), an in-source ['literate programming'](http://en.wikipedia.org/wiki/Literate_programming) documentation system for generating Rd, collation, and NAMESPACE files.  What that means is that the documentation will be in the same file as the functions (as comments before each function) which will make writing and synchronizing the documentation easier for everyone.  Every function file will have the documentation and roxygen tags in the file, and `roxygenize` will be run before the package build process to generate the Rd documentation files required by R.

For more information on documentation and R package development in general, read ['Writing R Extensions'](http://cran.r-project.org/doc/manuals/R-exts.pdf).

## Use `xts` and `zoo` internally

`PerformanceAnalytics` uses the xts package for managing time series data for several reasons. Besides being fast and efficient, `xts` includes functions that test the data for periodicity and draw attractive and readable time-based axes on charts. Another benefit is that `xts` provides compatibility with Rmetrics' `timeSeries`, `zoo` and other time series classes, such that `PerformanceAnalytics` functions that return a time series will return the results in the same format as the object that was passed in. Jeff Ryan and Josh Ulrich, the authors of `xts`, have been extraordinarily helpful to the development of `PerformanceAnalytics` and we are very grateful for their contributions to the community. 

The `xts` package extends the excellent `zoo` package written by Achim Zeileis and Gabor Grothendieck. `zoo` provides more general time series support, whereas `xts` provides functionality that is specifically aimed at users in finance.

## Learn how to ask good questions

This might be your most important tool. If you're having trouble, try to provide a small, reproducible example that someone can actually run on easily available data. If more complex data structures are required, `dump("x", file=stdout())` will print an expression that will recreate your data object, `x`.

A good place to ask questions is the [r-sig-finance](https://stat.ethz.ch/mailman/listinfo/r-sig-finance) mailing list, where the authors and several contributors to our packages are known to hang out. Before asking a question, make sure to read the [posting guide](http://www.r-project.org/posting-guide.html).  And then, for good measure, read [Eric Raymond's essay](http://www.catb.org/~esr/faqs/smart-questions.html) about how to ask a good question in forums like these.   

## Make your life easier with RStudio

Although [RStudio](http://rstudio.com) is not required for contributing to `PerformanceAnalytics`, it is a very capable IDE for R and has a number of tools for automating some of the workflow described above.  Highly recommended.

## How to create patches

Or, you can just send us patches.  Patches are a text description of changes made to a function that will add a new feature, fix a bug, or add documentation.  To create a patch for a single file, use `diff` or a Windows-equivalent (either UnxUtils or WinMerge) to create a unified diff patch file.  For example, in a shell you might run something like:

```
diff -u original.R revised.R > original.patch
```
In a Linux shell, you can learn more by typing `man diff` or `man patch`.

## Or just email us

Emailing us directly might work too, but is notoriously slow and somewhat less dependable.  [Submitting an issue or bug report](https://github.com/braverock/PerformanceAnalytics/issues) is probably better.

# Writing Calculation Functions

This section discusses some of the key principles that we've tried to adhere to when writing functions for `PerformanceAnalytics` in the belief that they help make the package easier for users to adopt into their work.  With the principles in mind and tools in hand, we'll take a closer look at some specific examples.  These examples won't be exhaustive or complete, so take a look at similar functions in the package for alternative methods as well.

## Provide useful analysis of returns

`PerformanceAnalytics` is carefully scoped to focus on the analysis of returns rather than prices.  If you are concerned that a function may not fit the package for some reason, please reach out to us and we can discuss what to do.  We are authors of several other packages such as `quantstrat` and `blotter` that analyze prices and positions rather than returns and weights.

It is important to note that `PerformanceAnalytics` does not aim to be exhaustive.  Instead, we see it as a well-curated package of useful functions.  We do not aspire to include every method ever conceived, but rather focus on functions that can affect investment decisions made by practitioners in real-world circumstances.

`PerformanceAnalytics` is also not a reporting tool.  Other packages and tools should be expected to use functions in this package for creating reports, but that functionality is beyond the scope of this package.  For example, users wanting to export output to Excel have several packages to choose from, including `xlsx` or `XLConnect`.  Those wanting a more interactive browser-based experience should look at `shiny`.

## Write readable and maintainable code

Although preferences for code style do vary, when there are a number of contributors to the package it can be important for readability and future maintainability of the code.  You should strive (as much as is practical) to match the style in the existing code.  When in doubt, rely on [Google's R Style Guide](https://google.github.io/styleguide/Rguide.html) or ask us.

## Provide consistent and standard interfaces

Given the number of users that `PerformanceAnalytics` has right now and the institutions in which they work, we try to be as 'stable' as possible for our users. That stability comes from allowing users to expect two things: that similar functions will work similarly to one another, and that interfaces to functions won't change without backward compatibility and warning.

For the first aspect, `PerformanceAnalytics` uses standardized argument names and calling conventions throughout the package.  For example, in every function where a time series of returns is used, the argument for the data is named `R`.  If two such series are used, such as an asset and a benchmark time series, they are referred to as `Ra` and `Rb`, respectively.  Several other similar conventions exist, such as using `p` for specifying the quantile parameter in `VaR`, `ES`, `CDaR`, and other similar functions.  When writing or modifying a function, please use argument names that match similar functions.

Secondly, the public interfaces of functions must not change in ways that break the users' existing code.  In particular, do not change the names, the order of the arguments or the format of the results delivered if at all possible.  Exceptions that are made for good reason require warnings about deprecation and handling for backward compatibility.

We prefer that new arguments be added after dots (`...`).  When arguments appear after dots, the user is required to pass the argument in explicitly, using the argument name.  That prevents users from being confused about the order of the inputs. 

Parameter inputs should be the same periodicity as the data provided.  For example, if a user is providing monthly returns data and specifying a "minimum acceptable return" (such as `MAR` in `DownsideDeviation`), the `MAR` parameter is assumed to also be a monthly return - the same periodicity as the input data.

## Provide convenient analysis

This largely means hiding complexity from the end user. For example, given how we expect the input data to be organized functions will handle multi-column input and provide a calculated result (or NA) for each column, with the outputs labeled completely and delivered in a rational data structure.

Functions will use `xts` internally where possible, mainly because `xts` is fast, efficient, and makes it easy to merge and subset data.

Where the output is a time series derived from a time series input, the function will provide the output in the same format as the input series.  See `?xts::reclass` for more information.

## Write documentation first

Document as you write.  It is really important to write the documentation as you write functions, perhaps even *before* you write the function, at least to describe what it should do.  

When documenting a function:

- make sure function and argument naming is consistent;
- make sure equations are correct and cited;
- make sure all user-facing functions have examples;
- make sure you know the expected results of the examples (these will become tests);
- make sure relevant literature is cited everywhere; and
- apply a standard mathematical notation.  In most cases, follow the notation from the original paper.  

Ideally, the documentation will go a step further to briefly describe how to interpret results.

Make sure you use the author tag in roxygen with your name behind it.  Credit is yours, and we want to remember who wrote it so we can ask you questions later!  Also in the documentation, refer to any related code that is included, where it is, and how you matched it against known data to ensure it gives the same result.

And think about see also tags in roxygen, linking charts and underlying functions in the documentation (such as `benchmarkSR` and its plot).

Equations in documentation should have both full LaTeX code for printing in the PDF and a text representation that will be used in the console help. Use:
```
\eqn{\LaTeX}{ascii}
```
or
```
\deqn{\LaTeX}{ascii}
```
Greek letters will also be rendered in the HTML help.  However, the only way to get the full mathematical equation layout is in the PDF rendered from \LaTeX.

At a minimum, use the following template to start your documentation.  Feel free to add other tags as well.
```
#' A brief, one line description
#'
#' Detailed description, including equations
#' \deqn{\LaTeX}{ascii}
#' @param ... please mention what functions you pass the dots into
#' @author 
#' @seealso \cr
#' @references 
#' @examples
#' @aliases
#' @export
```
There is more detail about the text formatting and tags in the `roxygen2` package.  A good place to start is with the vignette, which can be read by typing `vignette('roxygen2')` in an R session after loading the package.

## Accept inputs generously

Package functions will allow the user to provide input in any format.  Users are able to pass in a data object without being concerned that the internal calculation function requires a matrix, data.frame, vector, xts, or timeSeries object. 

The functions within `PerformanceAnalytics` assume that input data is organized with asset returns in columns and dates represented in rows. All of the metrics are calculated by column and return values for each column in the results. This is the default arrangement of time series data in xts.

Some sample data is available in the managers data set.
```{r}
library(PerformanceAnalytics, verbose = FALSE, warn.conflicts = FALSE, quietly = TRUE)
data(managers)
options(width=200)
head(managers)
options(width=80)
```
The `managers` data is an xts object that contains columns of monthly returns for six hypothetical asset managers (HAM1 through HAM6), the EDHEC Long-Short Equity hedge fund index, the S&P 500 total returns, and total return series for the US Treasury 10-year bond and 3-month bill. Monthly returns for all series end in December 2006 and begin at different periods starting from January 1996. That data set is used extensively in our examples and should serve as a model for how to expect the user to provide data to your functions.

As you can see from the sample, returns are represented as a decimal number, rather than as a whole number with a percentile sign.  

`PerformanceAnalytics` provides a function, `checkData` that examines input data and coerces it into a specified format.  This function was created to make the different kinds of data classes at least seem more fungible to the end user. It allows the user to pass in a data object without being concerned that the underlying functions require a matrix, data.frame, vector, `xts`, or `timeSeries` object. 

It allows the user to provide any type of data as input: a vector, matrix, data.frame, `xts`, `timeSeries` or `zoo` object.  `checkData` will identify and coerce the data into any of `xts` (the default), `zoo`, "data.frame", "matrix", or "vector".  See `?checkData` for more details.

## Handle missing data

Calculations will be made with the appropriate handling for missing data.  Contributors should not assume that users will provide complete or equal length data sets.  If only equal length data sets are required for the calculation, users should be warned with a message describing how the data was truncated.

Be careful how you use `na.omit` on time series data. That function removes rows with missing data, which may not be the outcome you want when dealing with time series data -- for example, missing data in one series may affect perfectly good data in another.

## Warn the user when making assumptions

If a function encounters an issue where there is a likely work-around to be applied or the function needs to make an assumption about what the user wants to do, the function should do so but inform the user by using `warning`.  For example, in `Return.rebalancing` the function warns the user that it zero-filled any missing data that it encountered.
```
Warning message:
In Return.rebalancing(managers[, 1:4]) :
  NA's detected: filling NA's with zeros
```
See `?warning` for more detail.

## Provide clear error messages when stopping

In cases where the function cannot continue with the inputs provided, it should stop and give a clear reason.  For example, when `Return.rebalancing` encounters a different number of assets and weights it will stop with the message "number of assets is greater than number of columns in return object".  

## Provide multi-column support

There is more than one way to provide multi-column support for calculation functions.  We'll give a couple of short examples here, but peruse the code for other cases.

Sometimes, functions don't need to have explicit handling for columns.  See `Return.calculate` as an example.

If the calculation is simple, use `apply`.  A generic example might have a form like:
```
exampleFunction <- function(R){
  R = checkData(R)
  foo <- function(x) 1+x
  result = apply (R, FUN=foo, MARGIN=2)
  result
}
```
<!-- [is there an outside reference/tutorial for apply?] -->

For functions that may encounter multiple assets and multiple benchmarks, `expand.grid` is useful.  `CAPM.alpha` shows an example of how the columns can be combined, then used with `apply`:
```
pairs = expand.grid(1:NCOL(Ra), 1:NCOL(Rb))
result = apply(pairs, 1, FUN = function(n, xRa, xRb) alpha(xRa[,n[1]], xRb[,n[2]]), xRa = xRa, xRb = xRb)
```
If the calculation is more difficult, resort to simple `for` loops for columns.  Speed optimization can come later, if necessary.

## Provide support for different frequencies of data

Some functions should try to identify the periodicity of the returns data.  For example, `SharpeRatio.annualized` identifies the frequency of the returns data input, and selects the appropriate multiplier for the user (e.g., daily returns would use `252`, monthly `12`, etc.).  It also provides a parameter, `scale`, so that the user can directly specify the number of periods in a year if needed.  

The following snippet shows how `xts::periodicity` can be used to find the frequency of the data, and a variable can be set depending on its outcome.
```
if(is.na(scale)) {
    freq = periodicity(R)
    switch(freq$scale,
        minute = {stop("Data periodicity too high")},
        hourly = {stop("Data periodicity too high")},
        daily = {scale = 252},
        weekly = {scale = 52},
        monthly = {scale = 12},
        quarterly = {scale = 4},
        yearly = {scale = 1}
    )
}
```

The `scale` variable is then set by how `xts` detects the data frequency. 

## Use reclass for time series output

One of the key reasons to use `xts` internally is that it handles conversion to and from other data classes.  This allows us to accept a matrix, convert it to an xts object (assuming the row labels of the matrix are an identifiable date format), calculate something, and convert the result back to a matrix.  The `xts::as.xts` conversion in `checkData` paired with the `xts::reclass` function provide the conversion.  For example:
```
foo <- function(R, ...) {
  x = checkData(R)
  result = x+1 # calculate something
  result = reclass(result,match.to=R) # converts back to what the user provided
  return(result) 
}
```

## Label outputs clearly

Be clear about what the calculation has returned, labeling columns and rows of outputs as necessary, and using key input assumptions where useful.  For example, you might use something like:
```
rownames(result) = paste0("Modified Foo ratio (Risk free = ",Rf,")")
```
Although that might seem like overkill, conditioning the labels is frequently useful for keeping track of key assumptions.

In some cases, it is important to keep data pairs straight.  For example, in functions where a set of returns and a set of benchmarks are possible inputs, individual results should be tagged with both the subject return name and the benchmark return name.  

## Create good tests

Please write tests using the [`tinytest`](https://cran.r-project.org/web/packages/tinytest/index.html) package, which is a lightweight, no-dependency, full-featured package for unit testing.  More tests are always welcome!

    <!-- Can we contribute unit tests? -->
    <!-- Are unit tests required? -->
    <!-- If so, what coverage do you expect? -->
    <!-- In addition to checking for correct results, what errors or failures should I test for? -->
    <!-- What unit testing framework do you use? -->
    <!-- Can you please show me an example or skeleton of a unit test? -->
    <!-- Where may I put my test data? -->
    <!-- Do you really expect me to test charting functions? -->

## How to pass functions and related arguments

We run into places where it makes sense to pass in functions and related arguments on occasion.  Unfortunately, we have not created a standard way to do that in the package, but advanced contributors should look at the function `modify.args` in the package [`quantstrat`](https://github.com/braverock/quantstrat), contained in the `utils.R` file.  You can see how it's called in `applyIndicators()` in `indicators.R`.  Please feel free to reach out for guidance on this.

## Writing chart and table functions

Here's a quick list of guidelines on writing chart functions.  First, use base graphics.  Second, use plot.xts for time series charts.  Where there is functionality missing, please let us know and we will help you figure out a fix or work-around.  Do NOT use menu interaction in chart functions (e.g., to allow a user to select a chart) without direct parameterization.

For tables, please return a labeled `data.frame`.  Format numbers and digits logically, but do not inhibit the use of the table with formatting (e.g., convert numbers to strings).

<!-- # Writing Chart Functions -->
<!-- ## Use base graphics -->
<!-- ## Use plot.xts for time series charts -->
<!-- ## How to structure chart multiples -->
<!-- ## Do NOT use menu interactions -->
<!-- ## Writing to devices -->

<!-- # Writing Table Functions -->
<!-- ## Output to data.frame -->
<!-- ## Formatting and displaying digits -->
<!-- ## Writing to devices -->

# Summary

Thank you for contributing!  We hope this is a reasonable overview that gets you started on the right path to becoming a contributor to our packages.  Please don't hesitate to reach out with any questions or comments -- or even modifications to this guide!

# References

Wilson G, Aruliah DA, Brown CT, Chue Hong NP, Davis M, Guy RT, et al. (2014) Best Practices for Scientific Computing. PLoS Biol 12(1): e1001745. [https://doi.org/10.1371/journal.pbio.1001745](https://doi.org/10.1371/journal.pbio.1001745)

Wilson G, Bryan J, Cranston K, Kitzes J, Nederbragt L, Teal TK (2017) Good enough practices in scientific computing. PLoS Comput Biol 13(6): e1005510. [https://doi.org/10.1371/journal.pcbi.1005510](https://doi.org/10.1371/journal.pcbi.1005510)

<!-- # Appendixes -->
<!-- ## Use foreach for parallel computations -->
<!-- ## Handle long labels when possible -->
<!-- ## Writing to spreadsheets -->
