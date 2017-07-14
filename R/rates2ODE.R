#' String of current time
#' 
#' These functions gives the current time
#' @keywords time
#' @export
#' @examples
#' 
rates2ODE <- function(stoich,rates){
  outfunc <- function(t,states,params){
    
    dStates<-sapply(apply(stoich,1,function(x){
      paste( "(",x,")*","(",rates,")",collapse="+")
    }),function(text) with(as.list(c(states, params)), eval(parse(text=text))),simplify=TRUE)
    
    names(dStates)<-paste("d",names(states), sep="" )
    list(dStates)
  }
}

#' @rdname rates2ODE
ratestxt_process<-function(ratestxt){
  ratestxt        <- str_replace_all(ratestxt,"\\(([0-9]+)\\)","\\1")
  ratestxt        <- str_replace_all(ratestxt,";","")
  rates_start_loc = str_locate(ratestxt,"\\[")[,1]
  rates_end_loc   = str_locate(ratestxt,"\\]")[,1]
  rates_start     = (1:length(ratestxt))[!is.na(rates_start_loc)]
  rates_end       = (1:length(ratestxt))[!is.na(rates_end_loc)]
  ratestxt        = ratestxt[rates_start:rates_end]
  ratestxt[1]     = str_sub(ratestxt[1],rates_start_loc[!is.na(rates_start_loc)]+1,str_length(ratestxt[1]))
  ratestxt[length(ratestxt)] = str_sub(ratestxt[length(ratestxt)],1,rates_end_loc[!is.na(rates_end_loc)]-1)
  ratestxt        = str_trim(ratestxt)
  ratestxt
}

#' @rdname rates2ODE
parse_stimulus <- function(rates,input_folder){
  stimulus <- scan(file=paste(input_folder,'/',name,"_stimulus.m",sep=""),what="",sep="\n")
  ratesParsed<-rates
  
  matched_rates=str_match_all(rates,"stimulus[0-9]+")
  ratesCoord_id=(1:length(rates))[sapply(str_match_all(matched_rates,"stimulus[0-9]+"),length)>0]
  for (i in 1:length(ratesCoord_id)){
    for (j in 1:length(matched_rates[[ratesCoord_id[i]]] )){
      stimulus_no=str_match(matched_rates[[ratesCoord_id[i]]][j],"[0-9]+")
      matched_stimulus=str_locate_all(stimulus, paste("stim\\{",stimulus_no,"\\}=",sep="") )
      matched_stimulus2=str_locate_all(stimulus, ";" )
      stimulusCoord_id=(1:length(matched_stimulus))[sapply(matched_stimulus,length)>0]
      stimulus_value=str_sub(stimulus[stimulusCoord_id],start=matched_stimulus[[stimulusCoord_id]][2]+1,end=matched_stimulus2[[stimulusCoord_id]][1]-1)
      ratesParsed=str_replace_all(ratesParsed,paste("stimulus",stimulus_no,sep=""),paste("(",stimulus_value,")",sep="") )
    }
  }
  
  ratesParsed
}