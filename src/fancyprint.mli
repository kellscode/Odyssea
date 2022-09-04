(**print to ANSI terminal with escape characters that define can define
   style and speed*)

exception UnknownEscapeChar of string
(**raised when an unknown escape character is encountered in
   [print_ANSI_string]*)

val print_ANSI_string : string -> unit
(**[print_ANSI_string str] prints str to the ANSI terminal. There are
   several escape characters that [str] can contain that will not be
   printed and will modify the output:

   - "%n" starts a new line
   - "%r" sets the text color to red
   - "%b" sets the text color to blue
   - "%k" sets the text color to black
   - "%g" sets the text color to green
   - "%y" sets the text color to yellow
   - "%m" sets the text color to magenta
   - "%c" sets the text color to cyan
   - "%w" sets the text color to white
   - "%or" sets the background color to red
   - "%ob" sets the background color to blue
   - "%ok" sets the background color to black
   - "%og" sets the background color to green
   - "%oy" sets the background color to yellow
   - "%om" sets the background color to magenta
   - "%oc" sets the background color to cyan
   - "%ow" sets the background color to white
   - "%u" toggles underline
   - "%i" toggles inverse
   - "%l" toggles bold
   - "%df%" sets the delay between the printing of each character to f,
     where f is a float
   - "%%" prints a percent symbol*)
