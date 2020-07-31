rm(list = ls())

devtools::install('.')

library(testthat)

E = myrpkg::default_env
pkgEnv = environment(myrpkg::get_default_env)

expect_identical(E, myrpkg::get_default_env())

# environment(arg) return the package env, but 'arg' has to be a function
expect_identical(pkgEnv, environment(myrpkg::connect))
expect_null(environment(myrpkg::default_env))
expect_null(environment(myrpkg::PersonSingleton))


expect_identical(myrpkg::get_head_of(iris, 10) %>% nrow, 10L)
expect_identical(myrpkg::call_arrange(iris, Petal.Length) %>% head(5L) %>% nrow, 5L)

# although 'dplyr::arrange' is in @importedFrom directive, it is not available
expect_false(exists('arrange'))
# Even when we attache `myrpkg`
library(myrpkg)
expect_false(exists('arrange'))
unloadNamespace('myrpkg')


# Pkg env is located within baseenv
# expect_identical(parent.env(parent.env(pkgEnv)), baseenv())
grandParentEnv = parent.env(parent.env(pkgEnv))
Filter(function(x) grepl('namespace', x, ignore.case = T), ls(grandParentEnv))
Filter(function(x) grepl('import', x, ignore.case = T), ls(grandParentEnv))
