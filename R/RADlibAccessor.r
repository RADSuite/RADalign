library(Biostrings)

setClass(
    "RADlibAccessor",
    slots = list(
        radv = "list"
    )
    # TODO::add validity check or prototype (default constructor) if needed
)

setGeneric("get_v_regions", function(object) standardGeneric("get_v_regions"))
setGeneric("createRADv", function(object) standardGeneric("createRADv")) # TODO:: need better name for this, maybe "readDatabase"?

setMethod("get_v_regions", "RADv", function(object) {
    # TODO:: figure out how to specify parameters so you can specify which vregion you're looking for
    sequences <- radv[grep("vregion", names(radv), value = TRUE)]

    sequenceSet <- NULL
    for (sequence in sequences) {
        ID <- # TODO:: get ID by getting name of sequence; name = ID (see radv brainstorming doc)
            sequenceSet <- c(sequenceSet, ID = ) # TODO:: convert sequence to Biostring
    }
})

setMethod("createRADv", "RADv", function(object) {})
