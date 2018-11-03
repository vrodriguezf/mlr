# # CERN Data as FDA
# cern.data = dplyr::bind_rows(
#   purrr::map(list.files("/home/victor/data/CERN/tapeserver_selected_fields/", pattern = "*.csv.gz", full.names = T),
#     ~ read_csv(.))
# )
#
# cern.data.munged = cern.data %>%
#   dplyr::mutate(MSG = ifelse(
#     stri_detect(MSG, regex = "Recall order of FSEQs*"),
#     "Recall order of FSEQs",
#     MSG)
#   ) %>%
#   dplyr::mutate(MSG = as.factor(MSG))
# cern.factor.levels = levels(cern.data.munged$MSG)
#
# cern.data.sub = cern.data.munged %>%
#   group_by(volReqId) %>%
#   arrange(timestamp) %>%
#   tidyr::nest() %>%
#   #slice(1:100000) %>%
#   dplyr::filter(purrr::map_lgl(data, ~ nrow(.) >= 100 & nrow(.) <= 10000)) %>%
#   dplyr::filter(purrr::map_lgl(data, ~ .$MSG[nrow(.)] == "Tape session finished")) # Last message is tape session finished
#
# max.time.points = max(purrr::map_int(cern.data.sub$data, nrow))
#
# cern.data.sub.list = cern.data.sub %>%
#   dplyr::mutate(data = purrr::map(data,
#                            ~ dplyr::summarise(.,
#                                        timestamp = list(timestamp),
#                                        MSG = list(as.integer(MSG)),
#                                        status = status[length(status)]))) %>%
#   tidyr::unnest() %>%
#   as.list %>%
#   purrr::modify_at(.at = "status", factor) %>% # Drop empty factor levels
#   purrr::modify_at(.at = c("timestamp", "MSG"),
#                    .f = ~ t(data.frame(purrr::map(., ~ .[seq(max.time.points)]))))
#
# cern.data.sub.df = data.frame(cern.data.sub.list[c("MSG", "status")]) #Timestamp is not used for now
#
# fd.features = list("MSG" = which(startsWith(colnames(cern.data.sub.df), "MSG.")))
# cern.data.sub.fdf = makeFunctionalData(data = cern.data.sub.df,
#                                        fd.features = fd.features)
# rownames(cern.data.sub.fdf) = NULL
#
# cern.classif.task = makeClassifTask(id = "cern.classification",
#   data = cern.data.sub.fdf,
#   target = "status",
#   positive = "success",
#   fixup.data = "warn")
#
# cern.cluster.task = makeClusterTask(id = "cern.cluster",
#   data = select(cern.data.sub.fdf, -status)
# )
#
# cern.lrn = makeLearner("classif.fda.hmm")
#
# d = getTaskData(cern.classif.task, subset = NULL, target.extra = TRUE, functionals.as = "matrix")
# fd = mlr:::getFunctionalFeatures.data.frame(d$data)
# fd.matrix = fd[[1]]

# crossval(makeLearner("classif.fda.hmm"), cern.classif.task)


# Compare smooth.discrete of the mhsmm vignette with the mlr result
# FIXME Pasar esto a un test
y1 <- as.integer(c(1,1,1,1,2,1,1,NA,1,1,2,1,1,1,2,1,1,1,1,1,2,2,2,2,1,2,2,2,1,2,2,2,1,1,1,1,1,1,1,1,2,2,2,1,1))
mod.compare.mhsmm = mhsmm::smooth.discrete(na.omit(y1))

y1.fdf = makeFunctionalData(
  data = data.frame(t(data.frame(na.omit(y1)))),
  fd.features = list(y1 = 1:length(na.omit(y1)))
)
y1.task = makeClusterTask("test_mhsmm_smooth_discrete_in_mlr", y1.fdf)
mod.compare.mlr = train(
  learner = makeLearner("cluster.fda.hmm", J = mod.compare.mhsmm$model$model$J,
    init = mod.compare.mhsmm$initial$init, trans = mod.compare.mhsmm$initial$trans,
    family.emission = "multinomial", parms.emission = mod.compare.mhsmm$initial$parms.emission),
  task = y1.task
)


# cern.res = tuneParams(
#   learner = makeLearner("classif.fda.hmm", family.emission = "normal"),
#   task = fda.binary.gp.task,
#   resampling = makeResampleDesc("CV", iters = 5L),
#   measures = list(acc),
#   par.set = makeParamSet(
#     makeIntegerVectorParam("J", lower = c(2L, 2L), upper = c(10L, 10L), len = 2)
#   ),
#   control = makeTuneControlGrid(),
#   show.info = TRUE
# )
