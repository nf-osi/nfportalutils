#' Add activity to entity
#' 
#' Util for adding activity info to a file entity.
#' See also https://help.synapse.org/docs/Provenance.1972470373.html
#' 
#' @param entity Synapse entity id.
#' @param act_name Name of activity.
#' @param act_executed Reference to the the activity executed (URL preferred).
#' @param used_inputs Vector of inputs for this act, e.g. syn ids, links to other data sources, etc.
add_activity <- function(entity, 
                        act_name, 
                        act_executed,
                        used_inputs) {
 
    act <- synapseclient$Activity(name = act_name,
                                  executed = act_executed,
                                  used = used_inputs)
    .syn$setProvenance(entity, activity = act)
}

#' Add activity to multiple entities
#' 
#' Batch provenance for entities.
#' Usually for applying the same activity, but can also be used as a util
#' with any combination of entity, activity, and input mappings
#' for parallelized provenance annotation.
#' 
#' @param entities Vector or list of entities. 
#' @param act_name Vector or list of activity name; if single, applies same activity annotation.
#' @param act_executed Vector or list of reference activity executed; if single, applies same activity annotation.
#' @param used_inputs Vector or list of inputs for each entity.
add_activity_batch <- function(entities, 
                             act_name, 
                             act_executed,
                             used_inputs
                             ) {
  
  Map(
    add_activity(entity, act_name, act_executed, used_inputs),
    entities, act_name, act_executed, used_inputs
  )
}


#' Remove provenance info
#'
#' @inheritParams add_activity_batch
#' @export
delete_provenance <- function(entities) {
  for(i in entities) {
    .syn$deleteProvenance(entity)
  }
}