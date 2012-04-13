module Error

open System

let error msg = raise (new System.ApplicationException(msg))

(*** Lexer ***)
let unrecToken token = error ("Unrecognized token: " + token)
let nonTermCom () = error "End-of-file found, '*/' expected"
let brStr () = error "Newline in string literal"
let eofStr () = error "Unterminated string literal"

(*** Parser ***)
let notBothForTo () = error "Cannot specify both 'for' and 'to' time specification."

(*** Grammar ***)
let outRangeTimeSpec (t1:int) = error (String.Format("Invalid time specification: starting time {0}ms before 0", t1))
let invalTimeSpec t1 t2 = error (String.Format("Invalid time specification: starting time {0}ms is after ending time {1}ms", t1, t2))
let overTimeSpec dev t1 t2 = error (String.Format("Overlapping command specification for device {0} at {1}ms - {2}ms", dev, t1, t2))
let dupName name = error ("Duplicate name: " + name)
let noDev dev file = error (String.Format("No such procedure or device named {0} defined in {1}.", dev, file))
let invalBind d0 p1 = error (String.Format("Invalid binding: tried to bind device {0} with procedure {1}.", d0, p1))
let overBind p0 i1 = error (String.Format("Override prohibited: tried to overwrite {1} with procedure {0}.", p0, i1))
let loadLoop file = error ("Include loop is caused by loading the file " + file)
