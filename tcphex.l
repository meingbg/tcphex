(load (pack *AppDir "/getopts.l"))

(de showhelp ()
   (m 1 "
Usage:
  "*AppExe" [-h]
    Show usage help and exit.
  "*AppExe" [options] listen-port [host] port
    Act as a tcp proxy: listen on localhost:listen-port,
    connect to host:port and display a realtime hexdump of
    data transmitted both ways. Only one connection at a
    time is allowed. The host default is \"localhost\".

Options:
 -a       Align packets the same. By default, packets from
          server has an offset to the right
 -c cnt   Print cnt number of bytes per row. Default is 16.
 -d cnt   Print a delimiter after each cnt character.
          0 means no delimiting. Default is 4.
 -h       Show usage help and exit.
 -l       Use lower case a-f in hex codes
 -n       No output colorization.
 -s       Silent. Don't print messages.
 -v       Verbose.
 -vv      Very verbose.

The defaults are intended to create a nice human readable
output format. For automatic processing, perhaps -and0 would
create something usable.

Output format:
 "(c *MarkOut)"      Data packet from client
 "(c *MarkIn)"      Data packet from server
 "(c *TxtBorder)"        Frame for data
 "(c *TxtDelimiter)"        Delimiter for data
 "(c *NonPrintable)"        Non-printable byte
 "(printable 1)"        When colorization is on, bytes 00-1F are displayed
          using @-_ with colored background
" ) (bye) )

(de main ()
   (setq
					#defaults:
      *OutHost "localhost"
      *Verbosity 1
					#output format settings:
      *Colorize T
      *ByteCount 16
      *DelimCount 4
					#markers and colors:
      *BeforePacket "^J"
      *MarkOut (32 "-> ")
      *MarkOut2 "   "
      *MarkIn (31 "<- ")
      *MarkIn2 "   "
      *Offset "    "
      *TxtBorder (34 "|")
      *TxtDelimiter (34 "-")
      *NonPrintable (36 ".")
      *NoChar NIL
      *NoChar2 " "
      *ConnectionDelimiter "========================================================================="
      )
   (if (getopts '(-c -d))
      (mapcar '((A) (processarg (car A) (cdr A))) @)
      (showhelp) )
   (unless (and (<= 1 *InPort 65535) (<= 1 *OutPort 65535))
      (die "Port numbers must be integers between 1 and 65535") )
   (unless (<= 1 *ByteCount (char 1))
      (die "-c must specify an integer") )
   (unless (<= 0 *DelimCount (char 1))
      (die "-d must specify an integer") )
   (let Port (port *InPort)
      (while (setq *In (accept Port))
	 (m 2 "Incoming connection")
	 (let? *Out (or (connect *OutHost *OutPort)
		       (prog (m 1 "Could not connect to server") (close *In) NIL) )
	    (m 2 "Connected to server")
	    (task *In (process T (make (in *In (while (and (poll *In) (rd -1) (link @)))))))
	    (task *Out (process NIL (make (in *Out (while (and (poll *Out) (rd -1) (link @)))))))
	    (m 3 "Entering loop")
	    (wait NIL (not *Run))
	    (prinl (c *ConnectionDelimiter)) ) )
      (close Port) )
   (bye) )

(de c (O) (cond ((sym? O) O) (*Colorize (list "^[[" (car O) "m" (cadr O) "^[[0m")) ((cddr O) (caddr O)) (T (cadr O))))

(de die @ (m 1 (rest)) (bye))

(de m (Level . @) (when (>= *Verbosity Level) (out 2 (prinl (pack (rest))))))

(de processarg (Var Val)
   (case Var
      ("-a" (off *Offset))
      ("-c" (setq *ByteCount (any Val)))
      ("-d" (setq *DelimCount (any Val)))
      ("-h" (showhelp))
      ("-l" (on *LowerCase))
      ("-n" (off *Colorize))
      ("-s" (setq *Verbosity 0))
      ("-v" (inc '*Verbosity))
      (NIL
	 (unless (member (length Val) (2 3))
	    (die "Command line syntax error") )
	 (setq *InPort (any (pop 'Val)))
	 (when (cdr Val) (setq *OutHost (pop 'Val)))
	 (setq *OutPort (any (pop 'Val))) )
      (T (die "Unknown option: " Var)) ) )

(de process (Out? Data)
   (m 3 "In (process )" 'Out? Out? 'Data Data)
   (when (or Data (stop Out?))
      (m 3 "Processing data")
      (out (if Out? *Out *In) (apply wr Data))
      (dbgwr Out? Data) )
   (m 3 "Returning from (process )") )

(de stop (Out?)
   (close *In) (task *In) (off *In)
   (close *Out) (task *Out) (off *Out)
   (m 3 "In (stop)")
   (m 2 "Connection closed by " (if Out? "client." "server."))
   NIL )

(de dbgwr (Out? Data)
   (let First T
      (recur (First)
	 (when Data
	    (let (Empty NIL Hex NIL Txt NIL)
	       (for N *ByteCount
		  (let Code (pop 'Data)
		     (push 'Txt (printable Code))
		     (push 'Hex (cons " " (if Code (hex2 Code *LowerCase) "  ")))
		     (push 'Empty (unless Code (c *NoChar2)))
		     (when (and (< 0 *DelimCount) (=0 (% N *DelimCount)) (not (= N *ByteCount)))
			(ifn Data
			   (push 'Empty (c *NoChar2))
			   (push 'Txt (c *TxtDelimiter))
			   (push 'Hex (cons " " (c *TxtDelimiter))) ) ) ) )
	       (prinl
		  (pack
		     (when First (c *BeforePacket))
		     (c (if2 Out? First *MarkOut *MarkOut2 *MarkIn *MarkIn2))
		     (unless Out? (c *Offset))
		     (c *TxtBorder)
		     (reverse Txt)
		     (c *TxtBorder)
		     (reverse Empty)
		     (when Out? (c *Offset))
		     (reverse Hex) ) ) )
	    (recurse) ) ) ) )

(de hex2 (Num LowerCase)
   (let H (tail 2 (cons "0" (chop (hex Num))))
      (if LowerCase (lowc (pack H)) (pack H)) ) )

(de printable (Char)
   (c (cond
	 ((not Char) *NoChar)
	 ((and (>= Char 32) (<= Char 126)) (char Char))
	 ((and (>= Char 0) (<= Char 31))
	  (if *Colorize (list 45 (char (+ 64 Char))) *NonPrintable) )
	 (T  *NonPrintable) ) ) )

(unless (= "Success" (catch '(NIL) (main) "Success"))
   (m 1 "Error") )
(bye)
