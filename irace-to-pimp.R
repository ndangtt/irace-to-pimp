#!/usr/bin/env Rscript

single_quote <- function(s) {
  paste("'",s,"'",sep='')
}

double_quote <- function(s){
  paste('"',s,'"',sep='')
}

read_command_line_args <- function(argv){
  require(argparser, quietly=TRUE)
  
  # wrap text for help string
  wrap_text <- function(ls){
    rs <- strwrap(ls[1],width=100,initial='\n',indent=30,exdent=30,simplify=TRUE)
    if (length(ls)>1){
      for (s1 in ls[2:length(ls)])
        if (substr(s1,1,1)=='\t')
          rs <- c(rs,strwrap(s1,width=100,initial='\t',indent=30,exdent=45,simplify=TRUE))
        else
          rs <- c(rs,strwrap(s1,width=100,initial='',indent=30,exdent=30,simplify=TRUE))
    }
    return (paste(rs,collapse='\n'))
  }
  
  # list of command line arguments
  argParser <- arg_parser("irace-to-pimp command line arguments")
  argParser <- add_argument(argParser, 'irace-rdata-file', help="irace Rdata file")
  argParser <- add_argument(argParser, '--normalise', short='-n', help=wrap_text(c('Normalise the cost metric values before converting to PIMP format. By default, the normalisation is instance-based. However, sometimes there are several instances with the same feature values, and we might want to normalise based on feature values instead. See option -normlisation-scope for details')), flag=TRUE)
  argParser <- add_argument(argParser, '--normalise-scope', short='-ns', help=wrap_text(c('Scope of the normalisation. Values:', '\tinstance: normalisation cost is calculated based on instances','\tfeature: normalisation cost is calculated based on instance features. Instance features must be provided')),
                           default='instance')
  argParser <- add_argument(argParser, '--out-dir', short='-d', help=wrap_text(c('directory where all generated data are stored.')), default='./output')
  argParser <- add_argument(argParser, '--instance-feature-file', short='-fea', help=wrap_text(c('a .csv file containing instance features (one line per instance, sorted in the same order as the list of instances input to irace). The first line contains feature names.')),default=NA)
  argParser <- add_argument(argParser, '--filter-conditions', short='-c', help=wrap_text('Only extract data that satisfies the given conditions. The conditions are in R expression format [default: no filter]'),default=NA)
  argParser <- add_argument(argParser, '--default-configuration-index', short='-i', help=wrap_text(c('Index of default configuration (starting from 1), used by ablation analysis')),default=1)
  
  # read command line arguments
  rs <- parse_args(argParser, argv)
  
  # convert them to argument names used in the code (i.e., hyphen is removed)
  # note: argparser automatically converts "-" to "_" in argument names
  dictNames <- list()
  dictNames[['irace_rdata_file']] <- 'iraceRdataFile'  
  dictNames[['normalise']] <- 'normalise'
  dictNames[['normalise_scope']] <- 'normaliseScope'
  dictNames[['out_dir']] <- 'outDir'
  dictNames[['instance_feature_file']] <- 'instanceFeatureFile'
  dictNames[['filter_conditions']] <- 'filterConditions'
  dictNames[['default_configuration_index']] <- 'defaultConfigurationIndex'
  
  args <- list()
  for (name in names(dictNames))
    args[[dictNames[[name]]]] <- rs[[name]]
  
  return (args)
}

load_irace_rdata <- function(rdataFn){
  if (!file.exists(rdataFn)){
    cat("Error: ",rdataFn," does not exists.")
    stop()
  }
    
  load(rdataFn)
  
  if (!('iraceResults' %in% ls())){
    cat("Error: ",rdataFn," is not a valid irace.Rdata file")
    stop()
  }
  
  # convert all data.frame into data.table
  iraceResults$allConfigurations <- data.table(iraceResults$allConfigurations)
  iraceResults$experiments <- data.table(iraceResults$experiments) 
  
  # some fields are not present in the old version of irace, so we assign them as new version's default values
  if (!('capping' %in% names(iraceResults$scenario)))
    iraceResults$scenario$capping <- FALSE
  
  return(iraceResults)
}

check_commandline_validity <- function(args, rdata){
  if (args$normalise && rdata$scenario$capping){
    cat("WARNING: normalisation for capped data is not tested and can be buggy\n")
  }
}

filter_data <- function(rdata, args){
  # - read rdata$experiments and rdata$allConfigurations, remove all configurations not satistfying s_conditions
  # - also re-index all configurations accordingly, which leads to possible changes in the following variables:
  #     + defaultConfigurationIndex
  #     + rdata$iterationElites
  #     + rdata$experiments
  #     + rdata$experimentLog
  
  conditions <- args$filterConditions
  defaultConfigurationIndex <- args$defaultConfigurationIndex
  
  allConfigurations <- rdata$allConfigurations[order(.ID.)]
  allConfigurations <- allConfigurations[eval(parse(text=conditions))]
  
  # update newDefaultConfigurationIndex
  newDefaultConfigurationIndex <- defaultConfigurationIndex
  if (!(defaultConfigurationIndex %in% allConfigurations[['.ID.']])){
    newDefaultConfigurationIndex <- allConfigurations[['.ID.']][1]
    cat("WARNING: default configuration is eliminated because it does not satisfy condition ",conditions,". Setting configuration ",newDefaultConfigurationIndex," as default configuration instead.\n")
    newDefaultConfigurationIndex <- which(allConfigurations[['.ID.']] == newDefaultConfigurationIndex)[1]
  }
  
  # update rdata$iterationElites
  newIterationElitesIndex <- c()
  for (id in rdata$iterationElites){
    if (id %in% allConfigurations[['.ID.']]){
      newIterationElitesIndex <- c(newIterationElitesIndex, which(allConfigurations[['.ID.']] == id)[1])
    } else {
      cat("WARNING: elite configuration ",id," is eliminated because it does not satisfy condition ", conditions,'\n')
      newIterationElitesIndex <- c(newIterationElitesIndex, -1)
    }
  }
  
  # remove all configurations not satistfying s_conditions in experiments table
  experiments <- rdata$experiments[,allConfigurations[['.ID.']],with=FALSE]
  
  # re-index experiments table
  colnames(experiments) <- as.character(c(1:ncol(experiments)))
  
  # re-index experimentLog table
  lsOldIds <- allConfigurations[['.ID.']]
  experimentLog <- data.table(rdata$experimentLog)
  for (i in c(1:length(lsOldIds))){
    oldId <- lsOldIds[i]
    experimentLog[configuration==oldId]$configuration <- i
  }
  
  # re-index allConfigurations table
  allConfigurations[['.ID.']] <- c(1:nrow(allConfigurations))
  
  return (list(allConfigurations=allConfigurations,experiments=experiments,newDefaultConfigurationIndex=newDefaultConfigurationIndex,newIterationElitesIndex=newIterationElitesIndex, experimentLog=experimentLog))
}

generate_instance_file_and_feature_file <- function(outDir, instances, instanceFeatureFile){
  # Generate instances.txt and features.txt
  
  cat("Generating instance list file and feature file ...\n")
  outInstanceListFile <- paste(outDir,'/instances.txt',sep='')
  outFeatureFile <- paste(outDir,'/features.txt',sep='')
  
  #--- instances.txt ----
  writeLines(instances, con <- file(outInstanceListFile))
  close(con)
  
  #--- features.txt -----
  tFeatures <- data.table(instance=instances)
  
  # if no instance features are provided, make instance index as features
  if (is.na(instanceFeatureFile))
    tFeatures$id <- c(1:length(instances))
  
  # otherwise, add features
  else{
    if (!file.exists(instanceFeatureFile)){
      cat("Error: instance feature file ",instanceFeatureFile," does not exists.")
      stop()
    }
    t <- fread(instanceFeatureFile)
    if (nrow(t) != length(instances)){
      cat("Error: the number of instances in ", instanceFeatureFile, " (",nrow(t),") does not match the instance list given to irace (",length(instances),")")
    }
    tFeatures <- cbind(tFeatures, t)
  }
  
  # write to features.txt
  write.csv(tFeatures,file=outFeatureFile,row.names=FALSE,quote=FALSE)
  
  return (tFeatures)
}

remove_fixed_parameters <- function(parameters, allConfigurations){
  # remove fixed parameters, as we don't need them in the analyses ----
  
  # update parameters
  cat("Removing fixed parameters...\n")
  lsFixedParams <- names(which(parameters$isFixed))
  lsFixedParamsIds <- which(parameters$isFixed)
  if (length(lsFixedParamsIds)>0){
    for (field in c('names','types','switches','hierarchy','isFixed')){
      parameters[[field]] <- parameters[[field]][-lsFixedParamsIds]
    }
    for (field in c('domain','conditions')){
      for (paramName in lsFixedParams)
        parameters[[field]][[paramName]] <- NULL  
    }
    parameters$nbParameters <- parameters$nbParameters - length(lsFixedParams)
    
    # update allConfigurations
    allConfigurations <- allConfigurations[,-lsFixedParams,with=FALSE]
  }
  
  return (list(parameters=parameters,allConfigurations=allConfigurations))
}

generate_parameter_file <- function(outDir,parameters,defaultConfiguration){
  #---- generate param file in smac's format ------
  cat("Generating parameter definition file ...\n")
  outParamFile <- paste(outDir,'/params.pcs',sep='')
  
  # param types, ranges, and default value
  lsParamInfoLines <- c()
  for (param in parameters$names) {
    type <- parameters$types[[param]]
    domain <- parameters$domain[[param]]
    isLogTransform <- FALSE
    if ('transform' %in% names(parameters) && parameters$transform[[param]]=='log')
      isLogTransform <- TRUE
    
    val <- defaultConfiguration[[param]]
    if (is.na(val))
      val <- domain[1]
    
    if (type== 'r' || type =='i') {
      s <- paste(param, ' [', domain[1], ',',domain[2],'] [', val, ']', sep='')
      if (type == 'i')
        s <- paste(s, 'i', sep='')
      if (isLogTransform)
        s <- paste(s,'l',sep='')
    } else {
      s <- paste(param, ' {', paste(domain, collapse = ','), '} [', val, ']', sep='')
    }
    
    lsParamInfoLines <- c(lsParamInfoLines, s)
  }
  
  #   param conditions
  lsConditionLines <- c()
  for (param in parameters$names) {
    conditions <- as.character(parameters$conditions[[param]])
    # convert to SMAC's param format. TODO: this implemetation is too manual and does not cover all cases
    if (conditions != "TRUE" && conditions != "FALSE") {
      conditions <- gsub("%in%", "in", conditions) # remove %
      conditions <- gsub("\"","", conditions) # remove double quote
      conditions <- gsub("\'","", conditions) # remove single quote
      
      # try to replace c() by {}, but the following two lines would not work correctly if the expresssion is sth like: (c()) (the outer brackets will also be replaced). See the next lines for a better implementation
      #conditions <- gsub("c\\(","{",conditions)
      #conditions <- gsub("\\)","}",conditions)
      
      # a better implemetation using sed command, but only works for Unix-based systems
      conditions <- system(paste('echo "', conditions, '"', "| sed 's/(\\([^()]\\+\\))/{\\1}/g'"), intern = TRUE)
      conditions <- gsub('c\\{', '{', conditions) # remove 'c'
      
      # remove "(" and ")" at the beginning and end of the condition
      conditions <- trimws(conditions)
      while(startsWith(conditions, '('))
        conditions <- substring(conditions, 2)
      while(endsWith(conditions, ')'))
        conditions <- substring(conditions, 1, nchar(conditions) - 1)
      
      lsConditionLines <- c(lsConditionLines, paste(param,"|",conditions))
    }
  }
  lsLines <- c(lsParamInfoLines,'\n','#Conditions:',lsConditionLines)
  
  # TODO: forbidden conditions?
  
  f <- file(outParamFile); writeLines(lsLines, f); close(f)
}

generate_runhistory_trajectory <- function(rdata, args, tFeatures){
  #---- reformat rdata$experiments and add extra information into it ---#
  cat("Preprocessing experiment data...\n")
  experiments <- data.table(rdata$experiments)
  
  # instance_id and seed
  tInstanceSeeds <- data.table(rdata$state$.irace$instancesList)
  setnames(tInstanceSeeds,'instance','instance_id')
  tInstanceSeeds <- cbind(instance_seed_id = c(1:nrow(tInstanceSeeds)), tInstanceSeeds)
  
  # add instance names into tInstanceSeeds
  lsInstances <- rdata$scenario$instances
  tInstances <- data.table(instance_id=c(1:length(lsInstances)), instance=lsInstances)
  tInstanceSeeds <- merge(tInstanceSeeds,tInstances,by=c('instance_id'))
  
  # melt experiments
  experiments <- cbind(instance_seed_id=c(1:nrow(experiments)), experiments)
  experiments <- melt.data.table(experiments, id.vars = 'instance_seed_id', variable.name = 'candidate_id', value.name='cost', na.rm = TRUE)
  
  # add instance names and instance_seed_id into experiments
  experiments <- merge(experiments, tInstanceSeeds, by='instance_seed_id')
  
  # normalise cost values if neccessary
  if (args$normalise==TRUE){
    t1 <- experiments # experiments table with minCost and maxCost for normalisation
    
    # get minCost and maxCost over each instance
    if (args$normaliseScope=='instance'){
      t2 <- t1[,list(minCost=min(cost), maxCost=max(cost)), by=c("instance_id")]
      t1 <- merge(t1,t2,by='instance_id')
    } else { # get minCost and maxCost over each feature vector (i.e., over many instances with the same feature values)
      t2 <- merge(t1,tFeatures,by='instance')
      featureNames <- colnames(tFeatures)[-c(1)]
      t3 <- t2[,list(minCost=min(cost),maxCost=max(cost)),by=featureNames]
      t1 <- merge(t1,t3,by=featureNames)
      t1 <- t1[,-featureNames,with=FALSE]
    }
    
    # calculate normalised cost
    # TODO: allow user-defined normalisation method
    t1$cost <- (t1$cost-t1$minCost)/(t1$maxCost-t1$minCost)
    
    # remove rows with normalisedCost == Inf
    t1 <- t1[t1$cost!=Inf]
    
    experiments <- t1[,-c('minCost','maxCost'),with=FALSE]
  }
  
  # add extra information on iteration index & bound (when irace's capping is enabled)
  tLog <- data.table(rdata$experimentLog)
  setnames(tLog,'instance','instance_seed_id')
  setnames(tLog,'configuration','candidate_id')
  tLog$candidate_id <- as.integer(tLog$candidate_id)
  if (rdata$scenario$capping){
    experiments <- merge(experiments,tLog[,c('instance_seed_id','candidate_id','iteration','time','bound'),with=FALSE],by=c('instance_seed_id','candidate_id'))
  } else {
    experiments <- merge(experiments,tLog[,c('instance_seed_id','candidate_id','iteration'),with=FALSE],by=c('instance_seed_id','candidate_id'))
  }
  
  #------- generate runhistory.json ------#
  fn <- paste(args$outDir, '/runhistory.json',sep='')
  f <- file(fn, 'wt')
  
  cat("Generating runhistory.json ...\n")
  
  # configs
  ls_param_names <- colnames(rdata$allConfigurations)[2:(length(colnames(rdata$allConfigurations))-1)]
  cat(paste('{',double_quote('configs'), ': {',sep=''),file=f)
  for (row_id in c(1:nrow(rdata$allConfigurations))) {
    cand <- rdata$allConfigurations[row_id,]
    ls_params_vals <- sapply(c(2:(ncol(cand) - 1)), function(id)
      if (!is.na(cand[[id]])){
        val <- cand[[id]]
        if (rdata$parameters$types[[ls_param_names[id-1]]] == 'c')
          val <- double_quote(cand[[id]])
        paste(double_quote(ls_param_names[id - 1]), ': ',val, sep='')
      } else {
        ""
      }
    )
    ls_params_vals <- ls_params_vals[ls_params_vals != ""]
    s <- paste(ls_params_vals, collapse = ', ', sep='')
    s <- paste(double_quote(row_id),': {',s,'}',sep='')
    if (row_id > 1)
      cat(', ',file=f)
    cat(s,file=f)
  }
  cat('}',file=f)
  
  # data
  cat(paste(', ', double_quote('data'), ': [', sep=''),file=f)
  t <- experiments
  for (row_id in c(1:nrow(t))) {
    row <- t[row_id,]
    cost <- row$cost
    
    if (rdata$scenario$capping){
      time <- row$time
      if (row$time >= row$bound){
        if (row$bound < (rdata$scenario$boundMax - 0.01))
          status <- 'StatusType.CAPPED'
        else
          status <- 'StatusType.TIMEOUT'
      } else
        status <- 'StatusType.SUCCESS'
      
    }  else{
      time <- 0.9
      status <- 'StatusType.SUCCESS'
    }
    
    #rs = [[conf_id, inst, seed], [cost, time, status, {}]]
    s1 <- paste(row$candidate_id, double_quote(row$instance), row$seed, sep = ', ')
    s2 <- paste(as.character(cost), as.character(time), paste('{',double_quote('__enum__'),': ',double_quote(status),'}',sep=''), '{}', sep=', ')
    s <- paste('[[',s1,'], [',s2,']]',sep='')
    if (row_id > 1)
      cat(', ',file=f)
    cat(s,file=f)
  }
  cat(']}',file=f)
  close(f)
  
  #-------------------- traj_aclib2.json ------------------------------#
  fn <- paste(args$outDir,'/traj_aclib2.json',sep='')
  cat("Generating traj_aclib2.json ...\n")
  f <- file(fn,'wt')
  for (iterationId in c(1:length(rdata$iterationElites))){
    confId <- rdata$iterationElites[iterationId]
    
    # if this elite was eliminated due to filterConditions, ignore it
    if (confId == -1)
      next
    
    t1 <- t[(candidate_id == confId) & (iteration<=iterationId)]
    
    # cpu time
    cpu_time <- paste(double_quote('cputime'),': ', confId,sep='')
    total_cpu_time <- paste(double_quote('total_cpu_time'),': null')
    wallclock_time <- paste(double_quote('wallclock_time'),': ',confId,sep='')
    # evaluations
    evaluations <- paste(double_quote('evaluations'),': ',nrow(t1),sep='')
    # cost
    cost <- paste(double_quote('cost'),': ',mean(t1$cost),sep='')
    # configuration string
    cand <- rdata$allConfigurations[confId,]
    ls_params_vals <- sapply(c(2:(ncol(cand) - 1)), function(id)
      if (!is.na(cand[[id]])){
        val <- cand[[id]]
        if (rdata$parameters$types[[ls_param_names[id-1]]] == 'c')
          val <- single_quote(cand[[id]])
        double_quote(paste(ls_param_names[id - 1], '=',single_quote(val), sep=''))
      } else {
        ""
      }
    )
    ls_params_vals <- ls_params_vals[ls_params_vals != ""]
    s <- paste(ls_params_vals, collapse = ', ', sep='')
    configuratrion_string <- paste(double_quote("incumbent"),': [',s,']',sep='')
    # combine everything
    s <- paste('{',paste(cpu_time, evaluations, cost, configuratrion_string, total_cpu_time, wallclock_time, sep=', '),'}',sep='')
    write(s, file=f)
  }
  close(f)
}

generate_scenario_file <- function(outDir, rdata){
  #------ create scenario file -------
  cat('Generating scenario file ...\n')
  scenarioFn <- paste(outDir, '/scenario.txt', sep='')
  lss <- list()
  lss[['algo']] <- rdata$scenario$targetRunner
  lss[['execDir']] <- './'
  if (rdata$scenario$deterministic)
    lss[['deterministic']] <- 'true'
  else
    lss[['deterministic']] <- 'false'
  if (rdata$scenario$capping){
    lss[['run_obj']] <- 'runtime'
    lss[['cutoff_time']] <- rdata$scenario$boundMax
    lss[['overall_obj']] <- paste('par',rdata$scenario$boundPar,sep='')
  } else {
    lss[['run_obj']] <- 'quality'
    lss[['cutoff_time']] <- 1
  }
  lss[['tunerTimeout']] <- 999999
  lss[['overall_obj']] <- 'mean'
  lss[['paramfile']] <- 'params.pcs'
  lss[['instance_file']] <- 'instances.txt'
  lss[['feature_file']] <- 'features.txt'
  lsLines <- sapply(names(lss),function(name) paste(name,'=',lss[[name]],sep=''))
  f <- file(scenarioFn); writeLines(lsLines, f); close(f) 
}

main <- function(argv = commandArgs(trailingOnly = TRUE)){
  # load neccessary libraries
  require(data.table, quietly=TRUE)
  options(scipen=999)
  
  # read command line arguments
  args <- read_command_line_args(argv)
  
  # load irace Rdata
  rdata <- load_irace_rdata(args$iraceRdataFile)
  
  # check command line arguments validity
  check_commandline_validity(args,rdata)
  
  # create output dir if it doesn't exist
  if (!file.exists(args$outDir))
    dir.create(args$outDir)
  
  # filter data
  if (!is.na(args$filterConditions) && trimws(args$filterConditions)!=''){
    rs <- filter_data(rdata=rdata,args=args)
    rdata$allConfigurations <- rs$allConfigurations
    rdata$experiments <- rs$experiments
    rdata$iterationElites <- rs$newIterationElitesIndex
    args$defaultConfigurationIndex <- rs$newDefaultConfigurationIndex
    rdata$experimentLog <- rs$experimentLog
  }
  
  # generate instances.txt and features.txt 
  tFeatures <- generate_instance_file_and_feature_file(outDir=args$outDir, instances=rdata$scenario$instances, instanceFeatureFile=args$instanceFeatureFile)
  
  # remove irace's fixed parameters
  rs <- remove_fixed_parameters(parameters=rdata$parameters, allConfigurations=rdata$allConfigurations)
  rdata$parameters <- rs$parameters
  rdata$allConfigurations <- rs$allConfigurations
  
  # generate params.pcs
  generate_parameter_file(outDir=args$outDir, parameters=rdata$parameters, defaultConfiguration=rdata$allConfigurations[args$defaultConfigurationIndex,])
  
  # generate runhistory.json and traj_aclib2.json
  generate_runhistory_trajectory(rdata=rdata, args=args, tFeatures=tFeatures)
  
  # generate scenario.txt
  generate_scenario_file(outDir=args$outDir,rdata=rdata)
}

debug <- function(){
  #dataDir <- '/home/nttd/Dropbox/St-Andrews/irace-project/examples/002-TemplateDesign/'
  dataDir <- '/home/nttd/Dropbox/St-Andrews/irace-project/examples/LL_dynamic_02-param01-5000/'
  iraceFn <<- paste(dataDir,'irace.Rdata',sep='')
  
  args <- list()
  args[['outDir']] <- paste(dataDir,'./results',sep='')
  args[['normalise']] <- ''
  #args[['instanceFeatureFile']] <- paste(dataDir,'/features.csv',sep='')
  #args[['filterConditions']] <- 'n_templates_middle<=30'
  
  argv <- c()
  for (arg in names(args)){
    argv <- c(argv,paste('--',arg,sep=''))
    val <- args[[arg]]
    if (val != '')
      argv <- c(argv, val)
  }
  
  argv <- c(argv, iraceFn)
  
  main(argv)
}

#debug()
main()
