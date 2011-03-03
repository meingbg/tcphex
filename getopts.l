#(getopts 'lst1 ['lst2])
#   Returns an association list of options and their values built from lst2 or
#     (argv) if it is not given. lst1 is the list of options which require an
#     argument. No other options can have any argument. NIL is used in the
#     return value as a key for the list of arguments from lst2 which are not
#     options. If needed, -- can be used to explicitly mark the end of options.
#     All options must either be a dash followed by a single character, or two
#     dashes followed by one or more characters.
#   Examples:
#   (getopts '(-a -b --cc) '("-ab" "--cc" "dd" "ee"))
#    -> (("-a" . "b") ("--cc" . "dd") (NIL "ee"))
#   (getopts '(-c -d --f) '("-abc11" "-d" "22" "--e" "--f" "33" "44" "55" "66"))
#    -> (("-a" . T) ("-b" . T) ("-c" . "11") ("-d" . "22") ("--e" . T) ("--f" . "33") (NIL "44" "55" "66"))
#   (getopts NIL '("-ab" "--dd" "--" "--ee" "--ff"))
#    -> (("-a" . T) ("-b" . T) ("--dd" . T) (NIL "--ee" "--ff"))

(de getopts (Opts Args)
   (let Args (or Args (argv))
      (make
	 (recur (Args)
	    (let (Arg (car Args)
		    Args (cdr Args)
		    Arg1 (car (chop Arg))
		    Arg2 (pack (head 2 (chop Arg))) )
	       (cond
		  ((= Arg "--")
		   (link (cons NIL Args)) )
		  ((= Arg2 "--")
		   (link (cons Arg (if (member Arg Opts) (pop 'Args) T)))
		   (recurse Args) )
		  ((= Arg1 "-")
		   (link (cons Arg2 (if2 (member Arg2 Opts) (pack (cddr (chop Arg)))
				       @ (pop 'Args) (prog (push 'Args (pack "-" @)) T) T ) ) )
		   (recurse Args) )
		  (Arg
		     (link (cons NIL Arg Args)) ) ) ) ) ) ) )
