
#include <stdlib.h>
#include <string.h>
#include <tcl.h>

enum P_or_R {P=0,R=1,PP=2};
enum Word_types { NONE_WT,W_WT,BRACE_WT,DQUOTE_WT,BRACKET_WT};

struct InstrumenterState
{
  Tcl_Interp *ip;
  InstrumenterState* stack;
  Tcl_Obj *words;
  Tcl_Obj *currentword;
  Word_types wordtype;
  int wordtypeline;
  int wordtypepos;
  int DoInstrument;
  P_or_R OutputType;
  int NeedsNamespaceClose;
  int braceslevel;
  int snitpackageinserted;
  int level;
  Tcl_Obj *newblock[3];

  int line;
  Word_types type;
};

inline Tcl_Obj *Tcl_CopyIfShared(Tcl_Obj *obj)
{
  Tcl_Obj *objnew=obj;
  if(Tcl_IsShared(obj)){
    objnew=Tcl_DuplicateObj(obj);
    Tcl_DecrRefCount(obj);
    Tcl_IncrRefCount(objnew);
  }
  return objnew;
}

inline Tcl_Obj *Tcl_ResetString(Tcl_Obj *obj)
{
  Tcl_Obj *objnew=obj;
  if(Tcl_IsShared(obj)){
    objnew=Tcl_NewStringObj("",-1);
    Tcl_DecrRefCount(obj);
    Tcl_IncrRefCount(objnew);
  } else {
    Tcl_SetStringObj(objnew,"",-1);
  }
  return objnew;
}

inline Tcl_Obj *Tcl_ResetList(Tcl_Obj *obj)
{
  Tcl_Obj *objnew=obj;
  if(Tcl_IsShared(obj)){
    objnew=Tcl_NewListObj(0,NULL);
    Tcl_DecrRefCount(obj);
    Tcl_IncrRefCount(objnew);
  } else {
    Tcl_SetListObj(objnew,0,NULL);
  }
  return objnew;
}


void RamDebuggerInstrumenterInitState(InstrumenterState* is)
{
  is->stack=(InstrumenterState*) malloc(1000*sizeof(InstrumenterState));
  is->words=Tcl_NewListObj(0,NULL);
  Tcl_IncrRefCount(is->words);
  is->currentword=Tcl_NewStringObj("",-1);
  Tcl_IncrRefCount(is->currentword);
  is->wordtype=NONE_WT;
  is->wordtypeline=-1;
  is->wordtypepos=-1;
  /*   = 0 no instrument, consider data; =1 instrument; =2 special case: switch; */
  /*   = 3 do not instrument but go inside */
  is->DoInstrument=0;
  is->NeedsNamespaceClose=0;
  is->braceslevel=0;
  is->level=0;
  is->snitpackageinserted=0;

  Tcl_EvalEx(is->ip,""
    "foreach i [list return break while eval foreach for if else elseif error switch default continue] {\n"
	     "set ::RamDebugger::Instrumenter::colors($i) magenta\n"
	     "}\n"
	     "foreach i [list variable set global incr] {\n"
	     "set ::RamDebugger::Instrumenter::colors($i) green\n"
	     "}",-1,0);
}

int RamDebuggerInstrumenterEndState(InstrumenterState* is)
{
  int i;
  Tcl_Obj* result=NULL;
  char type,buf[1024];

  if(is->wordtype!=NONE_WT && is->wordtype!=W_WT){
    sprintf(buf,"There is a block of type (%c) beginning at line %d "
	    "that is not closed at the end of the file\n",is->wordtype,is->wordtypeline);
    Tcl_SetObjResult(is->ip,Tcl_NewStringObj(buf,-1));
    return TCL_ERROR;
  }
  for(i=0;i<is->level;i++){
    switch(is->stack[i].type){
    case W_WT: type='w'; break;
    case BRACE_WT: type='{'; break;
    case DQUOTE_WT: type='"'; break;
    case BRACKET_WT: type='['; break;
    }
    sprintf(buf,"There is a block of type (%c) beginning at line %d "
	    "that is not closed at the end of the file\n",type,is->stack[i].wordtypeline);
    if(result==NULL) result=Tcl_NewStringObj("",-1);
    Tcl_AppendToObj(result,buf,-1);
  }

  Tcl_DecrRefCount(is->words);
  Tcl_DecrRefCount(is->currentword);

  if(result!=NULL){
    Tcl_SetObjResult(is->ip,result);
    return TCL_ERROR;
  }
  return TCL_OK;
}

int RamDebuggerInstrumenterPushState(InstrumenterState* is,Word_types type,int line)
{
  int i,NewDoInstrument=0,PushState=0,wordslen,intbuf;
  P_or_R NewOutputType;
  Tcl_Obj *word0,*word1,*wordi,*tmpObj;
  char* pword0,*pword1,*pchar,buf[1024];

  if(is->OutputType==P){
    NewOutputType=PP;
  } else {
    NewOutputType=is->OutputType;
  }

  if(type==BRACKET_WT){
    PushState=1;
    if(is->DoInstrument == 1) NewDoInstrument=1;
    else NewDoInstrument=0;
  }
  else {
    Tcl_ListObjLength(is->ip,is->words,&wordslen);
    if(wordslen){
      Tcl_ListObjIndex(is->ip,is->words,0,&word0);
      pword0=Tcl_GetStringFromObj(word0,NULL);
      if(*pword0==':' && *(pword0+1)==':') pword0+=2;
      if(wordslen>1){
	Tcl_ListObjIndex(is->ip,is->words,1,&word1);
	pword1=Tcl_GetStringFromObj(word1,NULL);
      }
    }

    if(wordslen==2 && strcmp(pword0,"constructor")==0)
      NewDoInstrument=1;
    else if(wordslen==1 && strcmp(pword0,"destructor")==0)
      NewDoInstrument=1;
    else if(wordslen==3 && (strcmp(pword0,"proc")==0 || strcmp(pword0,"method")==0 || 
	     strcmp(pword0,"typemethod")==0 || strcmp(pword0,"onconfigure")==0 || 
	     strcmp(pword0,"oncget")==0))
      NewDoInstrument=1;
    else if(is->DoInstrument==0){
      if(wordslen==2 && (strcmp(pword0,"snit::type")==0 || strcmp(pword0,"snit::widget")==0 || 
			 strcmp(pword0,"snit::widgetadaptor")==0))
	PushState=1;
      else if(wordslen>=3 && strcmp(pword0,"namespace")==0 && strcmp(pword1,"eval")==0){
	PushState=1;
	/*                  if { $OutputType == "R" } { */
	/*                      upvar 2 $newblocknameP newblock */
	/*                  } else { upvar 2 $newblocknameR newblock } */
	/*                  append newblock "namespace eval [lindex $words 2] \{\n" */
	/*                  set NeedsNamespaceClose 1 */
      }
    }
    else if(is->DoInstrument==1){
      if(wordslen>0 && strcmp(pword0,"if")==0){
	if(wordslen==2) NewDoInstrument=1;
	else if(wordslen>2 && Tcl_ListObjIndex(is->ip,is->words,wordslen-1,&wordi)==TCL_OK &&
		(strcmp(Tcl_GetStringFromObj(wordi,NULL),"then")==0 ||
		 strcmp(Tcl_GetStringFromObj(wordi,NULL),"else")==0))
	  NewDoInstrument=1;
	else if(wordslen>2 && Tcl_ListObjIndex(is->ip,is->words,wordslen-2,&wordi)==TCL_OK &&
		strcmp(Tcl_GetStringFromObj(wordi,NULL),"elseif")==0)
	  NewDoInstrument=1;
      }
      else if(wordslen>0 && strcmp(pword0,"namespace")==0){
	if(wordslen>=3 && strcmp(pword1,"eval")==0){
	  NewDoInstrument=1;
	  if(is->OutputType==R){
	    Tcl_ListObjIndex(is->ip,is->words,2,&wordi);
	    tmpObj=Tcl_NewListObj(1,&wordi);
	    Tcl_IncrRefCount(tmpObj);
	    sprintf(buf,"namespace eval %s {\n",Tcl_GetStringFromObj(tmpObj,NULL));
	    Tcl_DecrRefCount(tmpObj);
	    Tcl_AppendToObj(is->newblock[P],buf,-1);
	    is->NeedsNamespaceClose=1;
	  }
	}
      }
      else if(wordslen==2 && (strcmp(pword0,"snit::type")==0 || strcmp(pword0,"snit::widget")==0 || 
	       strcmp(pword0,"snit::widgetadaptor")==0)){
	NewDoInstrument=3;
	if(is->OutputType==R){
	  tmpObj=Tcl_NewListObj(0,NULL);
	  Tcl_IncrRefCount(tmpObj);
	  Tcl_ListObjAppendElement(is->ip,tmpObj,word0);
	  Tcl_ListObjAppendElement(is->ip,tmpObj,word1);
	  sprintf(buf,"%s {\n",Tcl_GetStringFromObj(tmpObj,NULL));
	  Tcl_DecrRefCount(tmpObj);
	  is->NeedsNamespaceClose=1;
	}
      }
      else if((wordslen==1 && strcmp(pword0,"catch")==0) ||
	      (wordslen==2 && strcmp(pword0,"while")==0) ||
	      (wordslen>=3 && strcmp(pword0,"foreach")==0) ||
	      (wordslen>=3 && strcmp(pword0,"mk::loop")==0) ||
	      (wordslen>=1 && wordslen<=4 && strcmp(pword0,"for")==0) ||
	      (wordslen>1 && strcmp(pword0,"eval")==0) ||
	      (wordslen>1 && strcmp(pword0,"html::eval")==0) ||
	      (wordslen==3 && strcmp(pword0,"bind")==0)) 
	NewDoInstrument=1;
      else if(wordslen>1 && strcmp(pword0,"uplevel")==0){
	int len=wordslen;
	pchar=pword1;
	if(*pchar=='#') pchar++;
	if(Tcl_GetInt(is->ip,pchar,&intbuf)==TCL_OK) len--;
	if(len>0) NewDoInstrument=1;
      }
      else if(wordslen>0 && strcmp(pword0,"switch")==0){
	for(i=1;i<wordslen;i++){
	  Tcl_ListObjIndex(is->ip,is->words,i,&wordi);
	  if(strcmp(Tcl_GetStringFromObj(wordi,NULL),"--")==0){
	    i++;
	    break;
	  } else if( *(Tcl_GetStringFromObj(wordi,NULL))!='-') break;
	}
	if(wordslen-i==1) NewDoInstrument=2;
      }
    } else if(is->DoInstrument == 2){
      if(wordslen%2) NewDoInstrument=1;
    }
  }
  if(!PushState && !NewDoInstrument) { return 1; }
  
  if(is->level>=0){
    is->stack[is->level].words=is->words;
    Tcl_IncrRefCount(is->stack[is->level].words);
    is->stack[is->level].currentword=is->currentword;
    Tcl_IncrRefCount(is->stack[is->level].currentword);
    is->stack[is->level].wordtype=is->wordtype;
    is->stack[is->level].wordtypeline=is->wordtypeline;
    is->stack[is->level].wordtypepos=is->wordtypepos;
    is->stack[is->level].DoInstrument=is->DoInstrument;
    is->stack[is->level].OutputType=is->OutputType;
    is->stack[is->level].NeedsNamespaceClose=is->NeedsNamespaceClose;
    is->stack[is->level].braceslevel=is->braceslevel;
    is->stack[is->level].line=line;
    is->stack[is->level].type=type;
  }
  is->level++;
    
  is->words=Tcl_ResetList(is->words);
  is->currentword=Tcl_ResetString(is->currentword);
  is->wordtype=NONE_WT;
  is->wordtypeline=-1;
  is->wordtypepos=-1;
  is->DoInstrument=NewDoInstrument;
  is->OutputType=NewOutputType;
  is->NeedsNamespaceClose=0;
  is->braceslevel=0;

  return 0;
}

int RamDebuggerInstrumenterPopState(InstrumenterState* is,Word_types type,int line) {
  
  char buf[1024];
  Word_types last_type;
  int i;

  if(is->level>0){
    last_type=is->stack[is->level-1].type;
    if(type==BRACKET_WT && last_type!=BRACKET_WT) return 1;
  }

  if(type==BRACE_WT){
    if(is->wordtype==W_WT){
      int numopen=0,len,i;
      len=Tcl_GetCharLength(is->currentword);
      for(i=0;i<len;i++){
	switch(Tcl_GetString(is->currentword)[i]){
	case '\\': i++; break;
	case '{': { numopen++; }
	case '}': { numopen--; }
	}
      }
      if(numopen) return 1;
    }
    if(last_type != BRACE_WT){
      for(i=0;i<is->level;i++){
	if(is->stack[i].type==BRACE_WT){
	  sprintf(buf,"Using a close brace (}) in line %d when there is an open brace "
		  "in line %d and an open bracket ([) in line %d"
		  ,line,is->stack[i].line,is->stack[is->level-1].line);
	  Tcl_SetObjResult(is->ip,Tcl_NewStringObj(buf,-1));
	  return -1;
	}
	return 1;
      } 
    }
  }

  int wordslen,isexpand=0;
  Tcl_ListObjLength(is->ip,is->words,&wordslen);
  if(!wordslen && strcmp(Tcl_GetString(is->currentword),"expand")==0) isexpand=1;

  is->level--;
  if(is->level<0) return 0;
  Tcl_DecrRefCount(is->words);
  is->words=is->stack[is->level].words;
  Tcl_DecrRefCount(is->currentword);
  is->currentword=is->stack[is->level].currentword;
  is->wordtype=is->stack[is->level].wordtype;
  is->wordtypeline=is->stack[is->level].wordtypeline;
  is->wordtypepos=is->stack[is->level].wordtypepos;
  is->DoInstrument=is->stack[is->level].DoInstrument;
  is->OutputType=is->stack[is->level].OutputType;
  is->NeedsNamespaceClose=is->stack[is->level].NeedsNamespaceClose;
  is->braceslevel=is->stack[is->level].braceslevel;

  is->words=Tcl_CopyIfShared(is->words);
  if(isexpand) Tcl_ListObjAppendElement(is->ip,is->words,Tcl_NewStringObj("expand",-1));
//   else Tcl_ListObjAppendElement(is->ip,is->words,Tcl_NewStringObj("",-1));

  if(is->NeedsNamespaceClose){
    Tcl_AppendToObj(is->newblock[P],"}\n",-1);
    is->NeedsNamespaceClose=0;
  }
  return 0;
}

int RamDebuggerInstrumenterIsProc(InstrumenterState* is)
{
  int wordslen;
  Tcl_Obj *word0;
  char* pword0;
  Tcl_ListObjLength(is->ip,is->words,&wordslen);
  if(wordslen==0) return 0;
  Tcl_ListObjIndex(is->ip,is->words,0,&word0);
  pword0=Tcl_GetStringFromObj(word0,NULL);
  if(*pword0==':' && *(pword0+1)==':') pword0+=2;

  if(strcmp(pword0,"snit::type")==0 || strcmp(pword0,"snit::widget")==0 || 
     strcmp(pword0,"snit::widgetadaptor")==0 || strcmp(pword0,"proc")==0 || 
     strcmp(pword0,"method")==0 || strcmp(pword0,"typemethod")==0 || 
     strcmp(pword0,"constructor")==0 || strcmp(pword0,"destructor")==0)
    return 1;
  return 0;
}

int RamDebuggerInstrumenterIsProcUpLevel(InstrumenterState* is)
{
  int wordslen;
  Tcl_Obj *word0;
  char* pword0;
  if(is->level<=0)
    return 0;
  Tcl_ListObjLength(is->ip,is->stack[is->level-1].words,&wordslen);
  if(wordslen==0) return 0;
  Tcl_ListObjIndex(is->ip,is->stack[is->level-1].words,0,&word0);
  pword0=Tcl_GetStringFromObj(word0,NULL);
  if(*pword0==':' && *(pword0+1)==':') pword0+=2;

  if(strcmp(pword0,"snit::type")==0 || strcmp(pword0,"snit::widget")==0 || 
     strcmp(pword0,"snit::widgetadaptor")==0 || strcmp(pword0,"proc")==0 || 
     strcmp(pword0,"method")==0 || strcmp(pword0,"typemethod")==0 || 
     strcmp(pword0,"constructor")==0 || strcmp(pword0,"destructor")==0)
    return 1;
  return 0;
}

void RamDebuggerInstrumenterInsertSnitPackage_ifneeded(InstrumenterState* is)
{
  Tcl_Obj *word0;
  char* pword0;
  Tcl_ListObjIndex(is->ip,is->words,0,&word0);
  pword0=Tcl_GetStringFromObj(word0,NULL);
  if(*pword0==':' && *(pword0+1)==':') pword0+=2;

  if(!is->snitpackageinserted &&
     (strcmp(pword0,"snit::type")==0 || strcmp(pword0,"snit::widget")==0 || 
      strcmp(pword0,"snit::widgetadaptor")==0)){
    is->snitpackageinserted=1;
    Tcl_AppendToObj(is->newblock[P],"package require snit\n",-1);
  }
}


/*  newblocknameP is for procs */
/*  newblocknameR is for the rest */
int RamDebuggerInstrumenterDoWork_do(Tcl_Interp *ip,char* block,int filenum,char* newblocknameP,
				   char* newblocknameR,char* blockinfoname,int progress) {

  int i,length,braceslevelNoEval,lastinstrumentedline,
    line,ichar,icharline,consumed,instrument_proc_last_line,wordslen=0,
    quoteintobraceline=-1,quoteintobracepos,fail,commentpos,result;
  Word_types checkExtraCharsAfterCQB;
  Tcl_Obj *blockinfo,*blockinfocurrent,*word0,*wordi,*tmpObj;
  char c,lastc,buf[1024],*pword0=NULL;

  length=strlen(block);
  if(length>1000 && progress){
/*     RamDebugger::ProgressVar 0 1 */
  }

  InstrumenterState instrumenterstate,*is;
  is=&instrumenterstate;
  is->ip=ip;

  is->newblock[P]=Tcl_NewStringObj("",-1);
  Tcl_IncrRefCount(is->newblock[P]);
  is->newblock[PP]=is->newblock[P];
  is->newblock[R]=Tcl_NewStringObj("",-1);
  Tcl_IncrRefCount(is->newblock[R]);
  blockinfo=Tcl_NewListObj(0,NULL);
  Tcl_IncrRefCount(blockinfo);
  blockinfocurrent=Tcl_NewListObj(0,NULL);
  Tcl_IncrRefCount(blockinfocurrent);
  Tcl_ListObjAppendElement(ip,blockinfocurrent,Tcl_NewIntObj(0));
  Tcl_ListObjAppendElement(ip,blockinfocurrent,Tcl_NewStringObj("n",-1));
  RamDebuggerInstrumenterInitState(is);


  is->DoInstrument=1;
  is->OutputType=R;

  Tcl_AppendToObj(is->newblock[P],"# RamDebugger instrumented file. InstrumentProcs=1\n",-1);
  Tcl_AppendToObj(is->newblock[R],"# RamDebugger instrumented file. InstrumentProcs=0\n",-1);

  if(Tcl_ExprBoolean(is->ip,"$::RamDebugger::options(instrument_proc_last_line)",
		     &instrument_proc_last_line)!=TCL_OK) instrument_proc_last_line=0;

  braceslevelNoEval=0;
  checkExtraCharsAfterCQB=NONE_WT;
  lastc=0;
  lastinstrumentedline=-1;
  line=1;
  ichar=0;
  icharline=0;

  for(i=0;i<length;i++){
    c=block[i];

    if(ichar%1000 == 0 && progress){
/*       RamDebugger::ProgressVar [expr {$ichar*100/$length}] */
    }
    if(checkExtraCharsAfterCQB!=NONE_WT){
      if(!strchr(" \t\n}]\\;",c)){
	if(c=='"' && checkExtraCharsAfterCQB==BRACE_WT){
	  /* # nothing */
	} else {
	  char cblocktype;
	  switch(checkExtraCharsAfterCQB){
	  case BRACE_WT: cblocktype='}'; break;
	  case DQUOTE_WT: cblocktype='"'; break;
	  }
	  sprintf(buf,"There is a non valid character (%c) in line %d "
		  "after a closing block with (%c)",c,line,cblocktype);
	  Tcl_SetObjResult(ip,Tcl_NewStringObj(buf,-1));
	  Tcl_DecrRefCount(is->newblock[P]);
	  Tcl_DecrRefCount(is->newblock[R]);
	  Tcl_DecrRefCount(blockinfo);
	  Tcl_DecrRefCount(blockinfocurrent);
	  return TCL_ERROR;
	}
      }
      checkExtraCharsAfterCQB=NONE_WT;
    }
    if(is->DoInstrument==1 && lastinstrumentedline!=line && !strchr(" \t\n#",c) &&
       wordslen==0){
      if(c!='}' || !RamDebuggerInstrumenterIsProcUpLevel(is) || instrument_proc_last_line){
	sprintf(buf,"RDC::F %d %d ; ",filenum,line);
	Tcl_AppendToObj(is->newblock[is->OutputType],buf,-1);
	lastinstrumentedline=line;
      }
    }
    consumed=0;
    switch(c){
    case '"':
      if(lastc != '\\' && wordslen> 0 && *pword0!='#') {
	switch(is->wordtype){
	case NONE_WT:
	  is->wordtype=DQUOTE_WT;
	  is->wordtypeline=line;
	  is->wordtypepos=icharline;
	  consumed=1;
	  break;
	case DQUOTE_WT:
	  is->wordtype=NONE_WT;
	  is->words=Tcl_CopyIfShared(is->words);
	  Tcl_ListObjAppendElement(is->ip,is->words,is->currentword);
	  wordslen++;
	  Tcl_ListObjIndex(is->ip,is->words,0,&word0);
	  pword0=Tcl_GetStringFromObj(word0,NULL);
	  if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	  is->currentword=Tcl_ResetString(is->currentword);
	  consumed=1;
	  checkExtraCharsAfterCQB=DQUOTE_WT;
	  
	  blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("grey",-1));
	  if(is->wordtypeline==line)
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(is->wordtypepos));
	  else Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(0));
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharline+1));
	  is->wordtypeline=0;

	  if(is->OutputType==R && RamDebuggerInstrumenterIsProc(is)){
	    int len,newllen;
	    Tcl_GetStringFromObj(is->newblock[R],&len);
	    newllen=len;
	    Tcl_GetStringFromObj(is->words,&len);
	    newllen-=len;
	    if(lastinstrumentedline==line){
	      sprintf(buf,"RDC::F %d %d ; ",filenum,line);
	      newllen-=strlen(buf);
	    }
	    Tcl_SetObjLength(is->newblock[R],newllen);
	    RamDebuggerInstrumenterInsertSnitPackage_ifneeded(is);
	    Tcl_AppendObjToObj(is->newblock[P],is->words);
	    is->OutputType=P;
	  }
	  break;
	case BRACE_WT:
	  if(quoteintobraceline==-1){
	    quoteintobraceline=line;
	    quoteintobracepos=icharline;
	  } else {
	    if(line==quoteintobraceline){
	      blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	      Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("grey",-1));
	      Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(quoteintobracepos));
	      Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharline+1));
	    }
	    quoteintobraceline=-1;
	  }
	  break;
	}
      }
      break;
      case '{':
	if(lastc != '\\'){
	  switch(is->wordtype){
	  case BRACE_WT:
	    braceslevelNoEval++;
	    break;
	  case DQUOTE_WT: case W_WT:
	    is->braceslevel++;
	    break;
	  default:
	    consumed=1;
	    fail=RamDebuggerInstrumenterPushState(is,BRACE_WT,line);
	    if(fail){
	      is->wordtype=BRACE_WT;
	      is->wordtypeline=line;
	      braceslevelNoEval=1;
	    } else{
	      lastinstrumentedline=line;
	      Tcl_ListObjLength(is->ip,is->words,&wordslen);
	      if(wordslen){
		Tcl_ListObjIndex(is->ip,is->words,0,&word0);
		pword0=Tcl_GetStringFromObj(word0,NULL);
		if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	      }
	    }
	    break;
	  }
	}
	break;
      case '}':
	if(lastc != '\\'){
	  if(is->wordtype==BRACE_WT){
	    braceslevelNoEval--;
	    if(braceslevelNoEval==0){
	      is->wordtype=NONE_WT;
	      is->words=Tcl_CopyIfShared(is->words);
	      Tcl_ListObjAppendElement(is->ip,is->words,is->currentword);
	      wordslen++;
	      Tcl_ListObjIndex(is->ip,is->words,0,&word0);
	      pword0=Tcl_GetStringFromObj(word0,NULL);
	      if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	      if(*pword0!='#' && strcmp(Tcl_GetString(is->currentword),"expand")!=0)
		checkExtraCharsAfterCQB=BRACE_WT;
	      is->currentword=Tcl_ResetString(is->currentword);
	      consumed=1;
	      if(is->OutputType==R && RamDebuggerInstrumenterIsProc(is)){
		int len,newllen;
		Tcl_GetStringFromObj(is->newblock[R],&len);
		newllen=len;
		Tcl_GetStringFromObj(is->words,&len);
		newllen-=len;
		if(lastinstrumentedline==line){
		  sprintf(buf,"RDC::F %d %d ; ",filenum,line);
		  newllen-=strlen(buf);
		}
		Tcl_SetObjLength(is->newblock[R],newllen);
		RamDebuggerInstrumenterInsertSnitPackage_ifneeded(is);
		Tcl_AppendObjToObj(is->newblock[P],is->words);
		is->OutputType=P;
	      }
	    }
	  } else if(is->braceslevel>0) is->braceslevel--;
	  else {
	    Word_types wordtype_before=is->wordtype;
	    fail=RamDebuggerInstrumenterPopState(is,BRACE_WT,line);
	    if(fail==-1){
	      Tcl_DecrRefCount(is->newblock[P]);
	      Tcl_DecrRefCount(is->newblock[R]);
	      Tcl_DecrRefCount(blockinfo);
	      Tcl_DecrRefCount(blockinfocurrent);
	      return TCL_ERROR;
	    }
	    if(!fail){
	      Tcl_ListObjLength(is->ip,is->words,&wordslen);
	      if(wordslen){
		Tcl_ListObjIndex(is->ip,is->words,0,&word0);
		pword0=Tcl_GetStringFromObj(word0,NULL);
		if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	      }
	      if(wordtype_before==DQUOTE_WT){
		sprintf(buf,"Quoted text (\") in line %d contains and invalid brace (})",line);
		Tcl_SetObjResult(is->ip,Tcl_NewStringObj(buf,-1));
		Tcl_DecrRefCount(is->newblock[P]);
		Tcl_DecrRefCount(is->newblock[R]);
		Tcl_DecrRefCount(blockinfo);
		Tcl_DecrRefCount(blockinfocurrent);
		return TCL_ERROR;
	      }
	      consumed=1;
	      if(wordslen){
		Tcl_ListObjIndex(is->ip,is->words,wordslen-1,&wordi);
		if(*pword0!='#' && strcmp(Tcl_GetString(wordi),"expand")!=0)
		  checkExtraCharsAfterCQB=BRACE_WT;
	      }
	    }
	  }
	}
	break;
      case ' ': case '\t':
	if(lastc != '\\'){
	  if(is->wordtype==W_WT){
	    consumed=1;
	    is->wordtype=NONE_WT;
	    is->words=Tcl_CopyIfShared(is->words);
	    Tcl_ListObjAppendElement(is->ip,is->words,is->currentword);
	    wordslen++;
	    Tcl_ListObjIndex(is->ip,is->words,0,&word0);
	    pword0=Tcl_GetStringFromObj(word0,NULL);
	    if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	    is->currentword=Tcl_ResetString(is->currentword);
	    if(is->OutputType==R && RamDebuggerInstrumenterIsProc(is)){
	      int len,newllen;
	      Tcl_GetStringFromObj(is->newblock[R],&len);
	      newllen=len;
	      Tcl_GetStringFromObj(is->words,&len);
	      newllen-=len;
	      if(lastinstrumentedline==line){
		sprintf(buf,"RDC::F %d %d ; ",filenum,line);
		newllen-=strlen(buf);
	      }
	      Tcl_SetObjLength(is->newblock[R],newllen);
	      RamDebuggerInstrumenterInsertSnitPackage_ifneeded(is);
	      
	      Tcl_AppendObjToObj(is->newblock[P],is->words);
	      is->OutputType=P;
	    }
	    Tcl_ListObjIndex(is->ip,is->words,wordslen-1,&wordi);
	    if(RamDebuggerInstrumenterIsProc(is)){
	      int icharlineold=icharline-Tcl_GetCharLength(wordi);
	      blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	      if(wordslen==1)
		Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("magenta",-1));
	      else
		Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("blue",-1));
	      Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharlineold));
	      Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharline));
	    } else{
	      tmpObj=Tcl_GetVar2Ex(is->ip,"::RamDebugger::Instrumenter::colors",
		   Tcl_GetStringFromObj(wordi,NULL),TCL_GLOBAL_ONLY);
	      if(tmpObj){
		int icharlineold=icharline-Tcl_GetCharLength(wordi);
		blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
		Tcl_ListObjAppendElement(is->ip,blockinfocurrent,tmpObj);
		Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharlineold));
		Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharline));
	      }
	    }
	  } else if(is->wordtype==NONE_WT) consumed=1;
	}
	break;
      case '[':
	if(lastc != '\\' && is->wordtype!=BRACE_WT && (wordslen==0 || *pword0!='#')){
	  if(is->wordtype==NONE_WT) is->wordtype=W_WT;
	  consumed=1;
	  RamDebuggerInstrumenterPushState(is,BRACKET_WT,line);
	  Tcl_ListObjLength(is->ip,is->words,&wordslen);
	  if(wordslen){
	    Tcl_ListObjIndex(is->ip,is->words,0,&word0);
	    pword0=Tcl_GetStringFromObj(word0,NULL);
	    if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	  }
	  lastinstrumentedline=line;
	}
	break;
      case ']':
	if(lastc != '\\' && is->wordtype!=BRACE_WT && is->wordtype!=DQUOTE_WT && 
	   (wordslen==0 || *pword0!='#')){
	  fail=RamDebuggerInstrumenterPopState(is,BRACKET_WT,line);
	  if(fail==-1){
	    Tcl_DecrRefCount(is->newblock[P]);
	    Tcl_DecrRefCount(is->newblock[R]);
	    Tcl_DecrRefCount(blockinfo);
	    Tcl_DecrRefCount(blockinfocurrent);
	    return TCL_ERROR;
	  }
	  if(!fail){
	    consumed=1;
	    Tcl_ListObjLength(is->ip,is->words,&wordslen);
	    if(wordslen){
	      Tcl_ListObjIndex(is->ip,is->words,0,&word0);
	      pword0=Tcl_GetStringFromObj(word0,NULL);
	      if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	    }
	  }
	  /* 	note: the word inside words is not correct when using [] */
	}
	break;
      case '\n':
	if(wordslen> 0 && *pword0=='#'){
	  blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("red",-1));
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(commentpos));
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharline));
	} else if(is->wordtype==DQUOTE_WT){
	  blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("grey",-1));
	  if(is->wordtypeline==line)
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(is->wordtypepos));
	  else
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(0));
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharline));
	} else if(is->wordtype==W_WT){
	  tmpObj=Tcl_GetVar2Ex(is->ip,"::RamDebugger::Instrumenter::colors",
			       Tcl_GetStringFromObj(is->currentword,NULL),TCL_GLOBAL_ONLY);
	  if(tmpObj){
	    int icharlineold=icharline-Tcl_GetCharLength(is->currentword);
	    blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,tmpObj);
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharlineold));
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewIntObj(icharline));
	  }
	}
	blockinfo=Tcl_CopyIfShared(blockinfo);
	Tcl_ListObjAppendElement(is->ip,blockinfo,blockinfocurrent);
	line++;
	tmpObj=Tcl_NewIntObj(is->level+braceslevelNoEval);
	Tcl_IncrRefCount(tmpObj);
	blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	Tcl_SetListObj(blockinfocurrent,1,&tmpObj);
	Tcl_DecrRefCount(tmpObj);

	if((is->wordtype==W_WT || is->wordtype==DQUOTE_WT) && is->braceslevel>0){
	  blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("n",-1));
	}
	else if(is->wordtype!=BRACE_WT){
	  consumed=1;
	  if(lastc != '\\' && is->wordtype!=DQUOTE_WT){
	    is->words=Tcl_ResetList(is->words);
	    wordslen=0;
	    is->currentword=Tcl_ResetString(is->currentword);
	    is->wordtype=NONE_WT;

	    if(is->OutputType==P){
	      Tcl_AppendToObj(is->newblock[P],&c,1);
	      is->OutputType=R;
	    }
	    blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("n",-1));
	  } else {
	    if(is->wordtype==W_WT){
	      is->wordtype=NONE_WT;
	      if(strcmp(Tcl_GetStringFromObj(is->currentword,NULL),"\\")!=0) {
		is->words=Tcl_CopyIfShared(is->words);
		Tcl_ListObjAppendElement(is->ip,is->words,is->currentword);
		wordslen++;
		Tcl_ListObjIndex(is->ip,is->words,0,&word0);
		pword0=Tcl_GetStringFromObj(word0,NULL);
		if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	      }
	    }
	    blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	    Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("c",-1));
	  }
	} else{
	  blockinfocurrent=Tcl_CopyIfShared(blockinfocurrent);
	  Tcl_ListObjAppendElement(is->ip,blockinfocurrent,Tcl_NewStringObj("n",-1));
	}
	break;
      case '#':
	if(wordslen==0 && strcmp(Tcl_GetStringFromObj(is->currentword,NULL),"")==0 &&
	   is->wordtype==NONE_WT){
	  consumed=1;
	  is->words=Tcl_CopyIfShared(is->words);
	  Tcl_ListObjAppendElement(is->ip,is->words,Tcl_NewStringObj("#",-1));
	  wordslen++;
	  Tcl_ListObjIndex(is->ip,is->words,0,&word0);
	  pword0=Tcl_GetStringFromObj(word0,NULL);
	  if(*pword0==':' && *(pword0+1)==':') pword0+=2;
	  commentpos=icharline;
	}
	break;
      case ';':
	if(lastc != '\\' && is->wordtype!=BRACE_WT && is->wordtype!=DQUOTE_WT &&
	   wordslen> 0 && *pword0!='#'){
	  consumed=1;
	  is->words=Tcl_ResetList(is->words);
	  wordslen=0;
	  Tcl_ListObjIndex(is->ip,is->words,0,&word0);
	  is->currentword=Tcl_ResetString(is->currentword);
	  is->wordtype=NONE_WT;
	  
	  if(is->OutputType==P){
	    Tcl_AppendToObj(is->newblock[P],&c,1);
	    is->OutputType=R;
	  }
	}
	break;
    }
    
    Tcl_AppendToObj(is->newblock[is->OutputType],&c,1);
    if(!consumed){
      if(is->wordtype==NONE_WT){
	is->wordtype=W_WT;
	is->wordtypeline=line;
      }
      is->currentword=Tcl_CopyIfShared(is->currentword);
      Tcl_AppendToObj(is->currentword,&c,1);
    }
    if(lastc == '\\' && c=='\\')
      lastc=0;
    else lastc=c;
    ichar++;

    if(c=='\t') icharline+=8;
    else if(c!='\n') icharline++;
    else icharline=0;
  }
  blockinfo=Tcl_CopyIfShared(blockinfo);
  Tcl_ListObjAppendElement(is->ip,blockinfo,blockinfocurrent);

  result=RamDebuggerInstrumenterEndState(is);

  Tcl_UpVar(ip,"1",newblocknameP,"newblockP",0);
  Tcl_SetVar2Ex(is->ip,"newblockP",NULL,is->newblock[P],0);
  Tcl_UpVar(ip,"1",newblocknameR,"newblockR",0);
  Tcl_SetVar2Ex(is->ip,"newblockR",NULL,is->newblock[R],0);
  Tcl_UpVar(ip,"1",blockinfoname,"blockinfo",0);
  Tcl_SetVar2Ex(is->ip,"blockinfo",NULL,blockinfo,0);

#ifdef _DEBUG
  char* tmpblockinfo=Tcl_GetString(blockinfo);
#endif
  if(length>1000 && progress){
    /*     RamDebugger::ProgressVar 100 */
  }
  Tcl_DecrRefCount(is->newblock[P]);
  Tcl_DecrRefCount(is->newblock[R]);
  Tcl_DecrRefCount(blockinfo);
  Tcl_DecrRefCount(blockinfocurrent);
  return result;
}





int RamDebuggerInstrumenterDoWork(ClientData clientData, Tcl_Interp *ip, int objc,
				  Tcl_Obj *CONST objv[])
{
  int result,filenum,progress=1;
  if (objc<6) {
    Tcl_WrongNumArgs(ip, 1, objv,
		     "block filenum newblocknameP newblocknameR blockinfoname ?progress?");
    return TCL_ERROR;
  }
  result=Tcl_GetIntFromObj(ip,objv[2],&filenum);
  if(result) return TCL_ERROR;
  if (objc==7){
    result=Tcl_GetIntFromObj(ip,objv[6],&progress);
    if(result) return TCL_ERROR;
  }
  result=RamDebuggerInstrumenterDoWork_do(ip,Tcl_GetString(objv[1]),filenum,Tcl_GetString(objv[3]),
					Tcl_GetString(objv[4]),Tcl_GetString(objv[5]),progress);
  return result;
}

extern "C" DLLEXPORT int Ramdebuggerinstrumenter_Init(Tcl_Interp *interp)
{
#ifdef USE_TCL_STUBS
  const char* retchar=Tcl_InitStubs(interp,"8.4",0);
  //Tk_InitStubs(interp,"8.4",0);
#endif

  Tcl_CreateObjCommand( interp, "RamDebuggerInstrumenterDoWork",RamDebuggerInstrumenterDoWork,
			( ClientData)0, NULL);
  return TCL_OK;
}

