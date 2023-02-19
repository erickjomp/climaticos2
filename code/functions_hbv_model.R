### pr and pet vectrs of same length

hbv_model <- function(pr,
                      pet,
                      param_soil_module = c(200, 0.8, 1.15),
                      param_routing_module = c(0.9, 0.01, 0.001, 0.5, 0.01), 
                      routing_model = 1, 
                      init_cond_routing = c(0,0,0)) {
  
  soil_module <-
    Soil_HBV(
      model = 1,
      inputData = cbind(pr, pet),
      initCond = c(100, 1),
      param = param_soil_module
    )

  routing_module <-
    Routing_HBV(
      model = routing_model,
      lake = F,
      inputData = as.matrix(soil_module[, "Rech"]),
      initCond = init_cond_routing,
      # initCond = c(0, 0, 0),
      # initCond = c(0,0),
      param = param_routing_module
    )
  
  tf_module <-
    UH(model = 1,
       Qg = routing_module[, "Qg"],
       param = c(1))
  
  return(tf_module)
}

hbv_model_tooptim <- function(param,pr, pet, routing_model = 1, 
                              init_cond_routing = c(0,0,0),
                              qobs, fun_eff, index_run = 1:length(qobs)){
  
  qsim <- hbv_model(pr,pet, param[1:3], param[-(1:3)], 
                    init_cond_routing = init_cond_routing ,
                    routing_model = routing_model)
  fun_eff(qsim[index_run], qobs[index_run])
}