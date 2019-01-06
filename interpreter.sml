(*
Supratik Neupane
person number: 50160008
Created on: March 3, 2018
*)
fun interpreter(input,output)=
let
val inStream = TextIO.openIn input;
val outstream = TextIO.openOut output;
val readLine = TextIO.inputLine inStream;
val firstClass = "firstClass"
(*all possible data types*)
datatype types = integer of int| str of string| boolean of string | name of string| error of string| Unit of types| No of int | firstfun
(*all possible instructions*)
datatype instructions = push of types | pop | add | swap | sub |neg | mul | division | rem |quit|cat| And | Or| Not | Equal | Less| If| bind|End|Let| call
|return | funend| function of string * string |inout of string * string

(*for the function map*)
datatype functions = Fun of string * string * (instructions list) * ((string * types) list) * (functions list) | inOutFunction of string * string * (instructions list) * ((string * types) list) * (functions list)
(*wrappers for function calls*)
datatype wrapper = wfun of functions  | wtype of types


fun readfromfile(readLine : string option) =
case readLine of
NONE => [] (*remove the trailing new line character*)
(*possible pushing strings*)
| SOME(line) => if String.size(line)>4 andalso String.substring(line,0,4)="push" andalso String.substring(String.substring(line,5,size(line)-5),0,1)="\"" (*check the first character for quotation marks*)

then ["push", String.substring(line,5,size(line)-6)]::readfromfile(TextIO.inputLine inStream)

(*any other command*)
else (String.fields Char.isSpace (String.substring(line,0,size(line)-1)))::readfromfile(TextIO.inputLine inStream)

(*output the final stack to the file*)
fun outputToFile([])=(TextIO.closeIn inStream; TextIO.closeOut outstream)
|outputToFile(x::y)=(TextIO.output(outstream, x^"\n"); outputToFile(y))

(*check if the given input is a string*)
fun isInt(x: string)=
case Int.fromString x of
NONE => false
|SOME(_)=>true

(*get the correct instruction from an input string*)
fun getInstruction(x)=
case hd(x) of
  "add"=> add
| "sub"=> sub
| "mul"=> mul
| "div"=> division
| "rem"=>rem
| "swap"=>swap
| "neg"=> neg
| "quit"=>quit
| "pop"=>pop
| "cat"=>cat
| "and"=>And
| "or"=> Or
| "not"=>Not
| "equal"=>Equal
| "lessThan"=>Less
| "if"=> If
| "bind"=>bind
| "end"=>End
| "let"=>Let
| "call"=>call
| "funEnd"=>funend
| "return"=>return
| _=> push(error(":error:"))

(*make a copy of the map*)
fun copymap([],map)=map
|copymap(x::y,map)=copymap(y,x::map)

(*build the final stack*)
fun buildStack(commands,stack: string list)=
case commands of
nil=> stack
| (integer x::y) => if x<0 then  buildStack(y,stack @ ("-"^ substring(Int.toString(x),1,size(Int.toString(x))-1))::[]) else  buildStack(y,stack @ Int.toString(x)::[])
| (str x::y) => buildStack(y, stack @ x::[])
| (boolean x::y) => buildStack(y, stack @ x::[])
| (name x::y) => buildStack(y, stack @ x::[])
| (error x::y) => buildStack(y, stack @ x::[])
| (Unit x::y) => buildStack(y, stack @ (":unit:")::[])
| (fistfun::y)=>buildStack(y,stack @ ("first class")::[])

(*get a list of instructions from the list of strings*)
fun makeList([])=[]
| makeList(x::y) =
(*functions*)
if length (x)>4 then push(error(":error:"))::makeList(y)
else if hd(x)="fun" then(function(hd(tl(x)),hd(tl(tl(x))))::makeList(y))
else if hd(x)="inOutFun" then inout(hd(tl(x)),hd(tl(tl(x))))::makeList(y)
else if length(x) =3 andalso (hd(x)<>"fun" orelse hd(x)<>"inOutFun") then push(error(":error:"))::makeList(y)


(*push*)
else if length(x)=2 andalso hd(x)<>"push" then push(error(":error:"))::makeList(y)
(*string*)
else if length(x)=2 andalso (String.sub(hd(tl(x)),0)) = #"\"" then
(push(str(substring(hd(tl(x)),1,(size(hd(tl(x)))-2))))::makeList(y))(*double check this part*)
(*integer*)
else if length(x)=2 andalso isInt(hd(tl(x))) andalso
Char.notContains (hd(tl(x))) #"." then push(integer(valOf(Int.fromString (hd(tl(x)))))):: makeList(y)
(*floats are invalid*)
else if length(x)=2 andalso isInt(hd(tl(x))) andalso
Char.contains (hd(tl(x))) #"." then push(error(":error:"))::makeList(y)

(*boolean*)
else if length(x)=2 andalso hd(tl(x))=":true:" then (push(boolean(":true:")))::makeList(y)
else if length(x)=2 andalso hd(tl(x))=":false:" then(push(boolean(":false:")))::makeList(y)

(*error*)
else if length(x)=2 andalso hd(tl(x))=":error:" then(push(error(":error:")))::makeList(y)
(*name*)
else if length(x)=2 andalso Char.isAlpha(hd(explode(hd(tl(x)))))
then push(name(hd(tl(x))))::makeList(y)

else if length(x)=2 andalso Char.isAlpha(hd(explode(hd(tl(x)))))=false
then push(error(":error:"))::makeList(y)
(*any arithmetic or simple command*)
else getInstruction(x)::makeList(y)


(*perform boolean and operation*)
fun boolAnd(x,y)=
if x=":true:" andalso y=":true:" then ":true:" else ":false:"
(*perform boolean or operation*)
fun boolOr(x,y)=
if x=":false:" andalso y=":false:" then ":false:" else ":true:"
(*perform boolean not operation*)
fun boolNot(x)=
if x=":true:" then ":false:" else ":true:"
(*perform boolean equal on integers*)
fun boolEquals(x,y)=
if x=y then ":true:" else ":false:"
(*perform boolean lessThan on integers*)
fun boolLess(x,y)=
if y<x then ":true:" else ":false:"

(*get the stored value*)
fun getVal(name(x),[])=name(x)
| getVal(name(x),(y,z)::bindmap)= if x=y then z else getVal(name(x),bindmap)
| getVal(str(x),bindmap)=str(x)
| getVal(boolean(x),bindmap)=boolean(x)
| getVal(error(x),bindmap)=error(x)
| getVal(Unit(x),bindmap)=Unit(x)
| getVal(integer(x),bindmap)=integer(x)



(*get the list after the let scope*)
fun getEnd(Let::rest,y)=getEnd(rest,y+1)
|getEnd(End::rest,y)= if (y-1)=0 then rest else getEnd(rest,y-1)
|getEnd(x::rest,y)=getEnd(rest,y)

(*get the list after the function declaration*)
fun getFunEnd(function(x,y)::rest,z)=getFunEnd(rest,z+1)
|   getFunEnd(inout(x,y)::rest,z)=getFunEnd(rest,z+1)
|   getFunEnd(funend::rest,z)=if (z-1)=0 then rest else getFunEnd(rest,z-1)
|   getFunEnd(x::rest,z)=getFunEnd(rest,z)
(*get the actual function commands*)
fun getFunList(function(x,y)::rest,z,list)=getFunList(rest,z+1,list @ [function(x,y)])
|   getFunList(inout(x,y)::rest,z,list)=getFunList(rest,z+1,list @ [inout(x,y)])
|   getFunList(funend::rest,z,list)=if (z-1)=0 then list  else getFunList(rest,z-1,list @ [funend])
|   getFunList(x::rest,z,list)=getFunList(rest,z,list @ [x])
(*change a binding from a bindingmap*)
fun changeBinding(x, y, (a,b)::map)=
if x=a then(x,y)::map
else changeBinding(x,y,map @[(a,b)])

(*check if an element is in the functionmap*)
fun infuncmap(x,[])=false
| infuncmap(x, Fun(fname,arg, list,bindmap,functionmap)::map)= if x=fname then true else infuncmap(x,map)
| infuncmap(x, inOutFunction(fname,arg, list,bindmap,functionmap)::map)=
if x=fname then true else infuncmap(x,map)
(*check if an element is in the binding map*)
fun inmap(x, [])=false
|inmap(x,(y,z)::map)= if(x=y) then true else inmap(x,map)
(*get the function from the function map*)
fun getFunction(x, Fun(y,arg,list,bindmap,functionmap)::map)=
if x=y then Fun(y,arg,list,bindmap,functionmap) else getFunction(x,map)
|getFunction(x, inOutFunction(y,arg,list,bindmap,functionmap)::map)=
if x=y then inOutFunction(y,arg,list,bindmap,functionmap)
else getFunction(x,map)

(*remove first class function from the map*)
fun removeFromMap(x,Fun(fname,arg,list,bmap,fmap)::map)=if x=fname then map else removeFromMap(x,map @[Fun(fname,arg,list,bmap,fmap)])

(*perfrom the actual interpretation*)
(*quit*)
fun interpret(quit::rest,stack,bindmap,functionmap)=(stack,bindmap,functionmap)
(*end* *double check this*)
|interpret([],stack,bindmap,functionmap)=(stack,bindmap,functionmap)
|interpret(return::rest,stack,bindmap,functionmap)=interpret([],stack,bindmap,functionmap)
|interpret(funend::rest,stack,bindmap,functionmap)=(stack,bindmap,functionmap)
|interpret(End::rest,stack,bindmap,functionmap)=(stack,bindmap,functionmap)



(*functions*)


|interpret(function(fname,arg)::rest,stack,bindmap,functionmap)=(
let
  val endlist = getFunEnd(rest,1)
  val funclist = getFunList(rest,1,[])
  val newfuncmap= copymap(functionmap,[])
  val newmap = Fun(fname,arg,funclist,copymap(bindmap,[]),newfuncmap)

  in

interpret(endlist,Unit(name(fname))::stack,bindmap,newmap::functionmap)
  end
  )

|interpret(inout(fname,arg)::rest,stack,bindmap,functionmap)=(
  let
    val endlist = getFunEnd(rest,1)
    val funclist = getFunList(rest,1,[])
    val newfuncmap= copymap(functionmap,[])
    val newmap = inOutFunction(fname,arg,funclist,copymap(bindmap,[]),newfuncmap)

    in

  interpret(endlist,Unit(name(fname))::stack,bindmap,newmap::functionmap)
    end
    )

(*

Calling the function

*)
|interpret(call::rest,error(z)::name(y)::stack,bindmap,functionmap)=interpret(rest,error(":error:")::error(z)::name(y)::stack,bindmap,functionmap)


(*first class as arguments*)

|interpret(call::rest,firstfun::name(x)::stack,bindmap,functionmap)=(
  if infuncmap(x,functionmap)=false then interpret(rest,error(":error:")::firstfun::name(x)::stack,bindmap,functionmap)
  else

let
val func = getFunction(x,functionmap)

val isin = false
val funclist = case func of Fun(fname, arg,list,bmap,fmap)=>list

val newbindmap = case func of Fun(fname,arg,list,bmap,fmap)=>bmap

val argument = case func of Fun(fname,arg,list,bmap,fmap)=>arg

val updatedmap = newbindmap

    (*need to find a better way to do this lol*)

   val ffunc = case getFunction(firstClass,functionmap) of Fun(a,arg,list,bmap,fmap)=>Fun(argument, arg,list,bmap,fmap)

      val updatedfuncmap = ffunc::removeFromMap(firstClass,functionmap)

         (*get the final element from returning the function*)
         fun getFuncElement(com,map,fanmap)=
         let

         val interpretResult = interpret(com,[],map,fanmap)

         val newStack =  case interpretResult of (a,b,c) =>a
         val newfuncmap = case interpretResult of (a,b,c) =>c
         val newbmap = case interpretResult of (a,b,c) =>b

         in
         if length(newStack)=0 orelse hd(rev(com))<>return then (wtype(No(0)),newbmap)
         else
         case getVal(hd(newStack),newbmap) of name(funcname)=>
         (
         if infuncmap(funcname, newfuncmap) then

         (wfun(getFunction(funcname,newfuncmap)),newbmap)

         else
           (wtype(getVal(hd(newStack),newbmap)),newbmap)
           )
         |_=>(wtype(getVal(hd(newStack),newbmap)),newbmap)
           end

           val xname = "lol"

           val funcresult = getFuncElement(funclist,updatedmap,updatedfuncmap)

           val elem = case funcresult of (a,b) => a

           val iobindmap = case funcresult of(a,b)=>b

             in
             (*error in inout     and adding function to functionmap*)
           case elem of wfun(Fun(functionname,arg,commlist,bindmapppp,funcmappp))=> interpret(rest, firstfun::stack, bindmap, Fun(firstClass,arg,commlist,bindmapppp,funcmappp)::functionmap)

             |_=>(if isin then
             case  elem of wtype(No(0)) =>if inmap(xname,bindmap) then interpret(rest,stack,changeBinding(xname,getVal(name(argument),iobindmap),bindmap),functionmap)
             else interpret(rest,stack,bindmap,functionmap)

             |wtype(element)=> if inmap(xname, bindmap) then interpret(rest,element::stack,changeBinding(xname,getVal(name(argument),iobindmap),bindmap),functionmap)
           else interpret(rest,element::stack,bindmap,functionmap)

             else
             case  elem of wtype(No(0)) =>interpret(rest,stack,bindmap,functionmap)|wtype(element)=>interpret(rest,element::stack,bindmap,functionmap)
           )
             end


  )




(*first calss as arguments end*)

(*first class functions*)


|interpret(call::rest,x::firstfun::stack,bindmap,functionmap)=(
let
val func = getFunction(firstClass,functionmap)

val isin = false
val funclist = case func of Fun(fname, arg,list,bmap,fmap)=>list

val newbindmap = case func of Fun(fname,arg,list,bmap,fmap)=>bmap

val argument = case func of Fun(fname,arg,list,bmap,fmap)=>arg

val updatedmap = if inmap(argument,newbindmap) then changeBinding(argument,getVal(x,bindmap),newbindmap)
                 else (argument,getVal(x,bindmap))::newbindmap

   val ffname = case x of name(g)=>g|_=>"wrongwrongwrong"  (*need to find a better way to do this lol*)

      val updatedfuncmap = if infuncmap(ffname,functionmap)  then (

             let
            val ufunc = getFunction(ffname, functionmap)
             in
            case ufunc of Fun(fname,arg,list,bmap,fmap)=> Fun(argument,arg,list,bmap,fmap)::functionmap
            end

         ) else functionmap

         (*get the final element from returning the function*)
         fun getFuncElement(com,map,fanmap)=
         let

         val interpretResult = interpret(com,[],map,fanmap)

         val newStack =  case interpretResult of (a,b,c) =>a
         val newfuncmap = case interpretResult of (a,b,c) =>c
         val newbmap = case interpretResult of (a,b,c) =>b

         in
         if length(newStack)=0 orelse hd(rev(com))<>return then (wtype(No(0)),newbmap)
         else
         case getVal(hd(newStack),newbmap) of name(funcname)=>
         (
         if infuncmap(funcname, newfuncmap) then

         (wfun(getFunction(funcname,newfuncmap)),newbmap)

         else
           (wtype(getVal(hd(newStack),newbmap)),newbmap)
           )
         |_=>(wtype(getVal(hd(newStack),newbmap)),newbmap)
           end

           val xname = case x of name(i)=>i |_ => "wrongwrongwrong"

           val funcresult = getFuncElement(funclist,updatedmap,updatedfuncmap)

           val elem = case funcresult of (a,b) => a

           val iobindmap = case funcresult of(a,b)=>b

             in
             (*error in inout     and adding function to functionmap*)
           case elem of wfun(Fun(functionname,arg,commlist,bindmapppp,funcmappp))=> interpret(rest, firstfun::stack, bindmap, Fun(firstClass,arg,commlist,bindmapppp,funcmappp)::removeFromMap(firstClass,functionmap))

             |_=>(if isin then
             case  elem of wtype(No(0)) =>if inmap(xname,bindmap) then interpret(rest,stack,changeBinding(xname,getVal(name(argument),iobindmap),bindmap),functionmap)
             else interpret(rest,stack,bindmap,functionmap)

             |wtype(element)=> if inmap(xname, bindmap) then interpret(rest,element::stack,changeBinding(xname,getVal(name(argument),iobindmap),bindmap),functionmap)
           else interpret(rest,element::stack,bindmap,functionmap)

             else
             case  elem of wtype(No(0)) =>interpret(rest,stack,bindmap,functionmap)|wtype(element)=>interpret(rest,element::stack,bindmap,functionmap)
           )
             end


  )


(*normal function call*)
|interpret(call::rest,x::name(y)::stack,bindmap,functionmap)=(
  if infuncmap(y,functionmap)=false then interpret(rest,error(":error:")::x::name(y)::stack,bindmap,functionmap)
  else(
let
val func = getFunction(y,functionmap)

val isin = case func of inOutFunction(fname,arg,list,bmap,fmap)=>true|_=>false

val funclist = case func of Fun(fname,arg,list,bmap,fmap)=>list  | inOutFunction(fname,arg,list,bmap,fmap)=>list

(*get the possible return value from the function call*)
val newbindmap= case func of Fun(fname,arg,list,bmap,fmap)=>bmap  | inOutFunction(fname,arg,list,bmap,fmap)=>bmap

val argument = case func of Fun(fname,arg,list,bmap,fmap)=>arg | inOutFunction(fname,arg,list,bmap,fmap)=>arg




val ffname = case x of name(g)=>g|_=>"wrongwrongwrong"  (*need to find a better way to do this lol*)

val updatedfuncmap = if infuncmap(ffname,functionmap)  then (

  let
val ufunc = getFunction(ffname, functionmap)
  in
case ufunc of Fun(fname,arg,list,bmap,fmap)=> Fun(argument,arg,list,bmap,fmap)::functionmap
  end

  ) else functionmap

val updatedmap = if inmap(argument,newbindmap) then changeBinding(argument,getVal(x,bindmap),newbindmap)
                 else (argument,getVal(x,bindmap))::newbindmap

(*get the final element from returning the function*)
fun getFuncElement(com,map,fanmap)=
let

val interpretResult = interpret(com,[],map,fanmap)

val newStack =  case interpretResult of (a,b,c) =>a
val newfuncmap = case interpretResult of (a,b,c) =>c
val newbmap = case interpretResult of (a,b,c) =>b

in
if length(newStack)=0 orelse hd(rev(com))<>return then (wtype(No(0)),newbmap)
else
case getVal(hd(newStack),newbmap) of name(funcname)=>
(
if infuncmap(funcname, newfuncmap) then

(wfun(getFunction(funcname,newfuncmap)),newbmap)

else
  (wtype(getVal(hd(newStack),newbmap)),newbmap)
  )
|_=>(wtype(getVal(hd(newStack),newbmap)),newbmap)
  end




val xname = case x of name(i)=>i |_ => "wrongwrongwrong"

val funcresult = getFuncElement(funclist,updatedmap,updatedfuncmap)

val elem = case funcresult of (a,b) => a

val iobindmap = case funcresult of(a,b)=>b

  in
  (*error in inout     and adding function to functionmap*)
case elem of wfun(Fun(functionname,arg,commlist,bindmapppp,funcmappp))=> interpret(rest, firstfun::stack, bindmap, Fun(firstClass,arg,commlist,bindmapppp,funcmappp)::functionmap)

  |_=>(if isin then
  case  elem of wtype(No(0)) =>if inmap(xname,bindmap) then interpret(rest,stack,changeBinding(xname,getVal(name(argument),iobindmap),bindmap),functionmap)
  else interpret(rest,stack,bindmap,functionmap)

  |wtype(element)=> if inmap(xname, bindmap) then interpret(rest,element::stack,changeBinding(xname,getVal(name(argument),iobindmap),bindmap),functionmap)
else interpret(rest,element::stack,bindmap,functionmap)

  else
  case  elem of wtype(No(0)) =>interpret(rest,stack,bindmap,functionmap)|wtype(element)=>interpret(rest,element::stack,bindmap,functionmap)
)
  end
  ))



|interpret(call::rest, stack,bindmap,functionmap)= interpret(rest,error(":error:")::stack,bindmap,functionmap)


(*perform the let scope*)

|interpret(Let::rest,stack,bindmap,functionmap)=(

  let



  fun getLetElem(com,map,fmap)=

  let
  val newMap=copymap(map,[])
  val interpretResult = interpret(com,[],newMap,fmap)
  val newstack  = case interpretResult of (a,b,c)=>a

  in
  if length(newstack)=0 then No(0)
  else
hd(newstack)
  end

 val endList = getEnd(rest,1)
  val elem =getLetElem(rest,bindmap,functionmap)
  in
  case  elem of No(0) =>interpret(endList,stack,bindmap,functionmap) |
   _ =>interpret(endList,elem::stack,bindmap,functionmap)
  end
  )





(*push*)
|interpret(push(x)::rest,stack,bindmap,functionmap)=interpret(rest,x::stack,bindmap,functionmap)
(*add*)
|interpret(add::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of integer(a)::integer(b)::[]=>interpret(rest,integer(a+b)::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))

|interpret(add::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*sub*)
|interpret(sub::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of integer(a)::integer(b)::[]=>interpret(rest,integer(b-a)::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))
|interpret(sub::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*mul*)
|interpret(mul::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of integer(a)::integer(b)::[]=>interpret(rest,integer(b*a)::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))
|interpret(mul::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*neg*)
|interpret(neg::rest,x::stack,bindmap,functionmap)=(case getVal(x,bindmap)::[] of integer(a)::[]=>interpret(rest,integer(~a)::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::stack,bindmap,functionmap))
|interpret(neg::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)


(*pop*)
|interpret(pop::rest,x::stack,bindmap,functionmap)=interpret(rest,stack,bindmap,functionmap)
|interpret(pop::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*swap*)
|interpret(swap::rest,x::y::stack,bindmap,functionmap)= interpret(rest,y::x::stack,bindmap,functionmap)
|interpret(swap::rest,stack,bindmap,functionmap)= interpret(rest,error(":error:")::stack,bindmap,functionmap)


(*div*)
|interpret(division::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of integer(a)::integer(b)::[]=>(if a=0 then interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap) else interpret(rest,integer(b div a)::stack,bindmap,functionmap))
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))

|interpret(division::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)
(*rem*)
|interpret(rem::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of integer(a)::integer(b)::[]=>(if a=0 then interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap) else interpret(rest,integer(b mod a)::stack,bindmap,functionmap))
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))
|interpret(rem::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*cat*)
|interpret(cat::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of str(a)::str(b)::[]=>interpret(rest,str(b^a)::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))

|interpret(cat::rest,stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)


(*and*)
|interpret(And::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of boolean(a)::boolean(b)::[]=>interpret(rest,boolean(boolAnd(a,b))::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))
|interpret(And::rest,stack,bindmap,functionmap) = interpret(rest,error(":error:")::stack,bindmap,functionmap)


(*or*)
|interpret(Or::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of boolean(a)::boolean(b)::[]=>interpret(rest,boolean(boolOr(a,b))::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))
|interpret(Or::rest,stack,bindmap,functionmap) = interpret(rest,error(":error:")::stack,bindmap,functionmap)


(*not*)
|interpret(Not::rest,x::stack,bindmap,functionmap)=(case getVal(x,bindmap) of boolean(a)=>interpret(rest,boolean(boolNot(a))::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::stack,bindmap,functionmap))
|interpret(Not::rest,stack,bindmap,functionmap) = interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*equal*)
|interpret(Equal::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of integer(a)::integer(b)::[]=>interpret(rest,boolean(boolEquals(a,b))::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))
|interpret(Equal::rest,stack,bindmap,functionmap) = interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*lessThan*)
|interpret(Less::rest,x::y::stack,bindmap,functionmap)=(case getVal(x,bindmap)::getVal(y,bindmap)::[] of integer(a)::integer(b)::[]=>interpret(rest,boolean(boolLess(a,b))::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::stack,bindmap,functionmap))
|interpret(Less::rest,stack,bindmap,functionmap) = interpret(rest,error(":error:")::stack,bindmap,functionmap)

(*if*)
|interpret(If::rest, x::y::z::stack,bindmap,functionmap)=(case getVal(z,bindmap) of boolean(":true:")=>interpret(rest,y::stack,bindmap,functionmap)
                                             | boolean(":false:")=>interpret(rest,x::stack,bindmap,functionmap)
                                             |_=>interpret(rest,error(":error:")::x::y::z::stack,bindmap,functionmap))
|interpret(If::rest,stack,bindmap,functionmap) = interpret(rest,error(":error:")::stack,bindmap,functionmap)


(*bind* *double check this obviously...*
*fix binding for functions*)
|interpret(bind::rest, error(x)::name(y)::stack,bindmap,functionmap)=interpret(rest,error(":error:")::stack,bindmap,functionmap)
(*binding to name*)

|interpret(bind::rest, name(x)::name(y)::stack,bindmap,functionmap)=

 (*if inmap(x,bindmap) then*)
 if inmap(y,bindmap) then

interpret(rest, Unit(getVal(name(x),bindmap))::stack,changeBinding(y,getVal(name(x),bindmap),bindmap),functionmap)
else
if infuncmap(x,functionmap) then (

let
val func =getFunction(x,functionmap)
in
case func of Fun(fname,arg,functionlist,bmap,fmap)=>
interpret(rest, Unit(getVal(name(x),bindmap))::stack,(y,getVal(name(x),bindmap))::bindmap,Fun(y,arg,functionlist,bmap,fmap)::functionmap)
end)
else
 interpret(rest, Unit(getVal(name(x),bindmap))::stack,(y,getVal(name(x),bindmap))::bindmap,functionmap)

 (*else interpret(rest, error(":error:")::name(x)::name(y)::stack,bindmap ,functionmap)*)

(*bind first class function*)
|interpret(bind::rest,firstfun::name(x)::stack,bindmap,functionmap)=
(
  let val func = getFunction(firstClass, functionmap)
  val funcmap = removeFromMap(firstClass,functionmap)
  in
  case func of Fun(fname,arg,list,bmap,fmap)=> interpret(rest,Unit(firstfun)::stack,bindmap,Fun(x,arg,list,bmap,fmap)::funcmap)
  end
  )
(*not binding to name*)
|interpret(bind::rest,x::name(y)::stack,bindmap,functionmap)=

 if inmap(y,bindmap) then
interpret(rest, Unit(getVal(x,bindmap))::stack,changeBinding(y,x,bindmap),functionmap)

else interpret(rest, Unit(getVal(x,bindmap))::stack,(y,getVal(x,bindmap))::bindmap,functionmap)

|interpret(bind::rest,stack,bindmap,functionmap) = interpret(rest,error(":error:")::stack,bindmap,functionmap)


val interpretResult =interpret(makeList(readfromfile(readLine)),[],[],[])

val finalStack = case interpretResult of (a,b,c)=>a

in

(*makeList(readfromfile(readLine))*)


outputToFile(buildStack(finalStack,[]))


end
