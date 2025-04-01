% -*- mode: prolog; -*-

% Nome: Simone Morrone  Matricola: 916063
% Nome: Giada bodini  Matricola: 914545

% urilib-parse.pl

% Module declaration for URI parsing functionality
% Exports predicates for parsing and accessing URI components
:- module(urilib_parse,
	  [ urilib_parse/2,      % Parse URI string into components
	    urilib_scheme/2,     % Access URI scheme
	    urilib_userinfo/2,   % Access URI userinfo 
	    urilib_host/2,       % Access URI host
	    urilib_port/2,       % Access URI port
	    urilib_path/2,       % Access URI path
	    urilib_query/2,      % Access URI query
	    urilib_fragment/2,   % Access URI fragment
	    urilib_display/1,    % Display URI (to stdout)
	    urilib_display/2     % Display URI to specified stream
	  ]).

% Allow urilib_display/1 to be defined in multiple places
:- discontiguous urilib_display/1.

% Return URI components (scheme, userinfo, host, port, path, query, fragment)
urilib_scheme(uri(S,_,_,_,_,_,_), S).
urilib_userinfo(uri(_,UI,_,_,_,_,_), UI).
urilib_host(uri(_,_,H,_,_,_,_), H).
urilib_port(uri(_,_,_,P,_,_,_), P).
urilib_path(uri(_,_,_,_,Path,_,_), Path).
urilib_query(uri(_,_,_,_,_,Q,_), Q).
urilib_fragment(uri(_,_,_,_,_,_,F), F).


% Displays URI components to default output stream
urilib_display(URI) :-
    urilib_display(URI, user_output).

% Displays URI components to specified output stream
urilib_display(uri(Scheme, Userinfo, Host,
		   Port, Path, Query, Fragment), Stream) :-
    format(Stream, "Scheme: '~w'~n", [Scheme]),
    format(Stream, "Userinfo: ~w~n", [Userinfo]), 
    format(Stream, "Host: '~w'~n", [Host]),
    format(Stream, "Port: ~w~n", [Port]),
    format(Stream, "Path: ~w~n", [Path]),
    format(Stream, "Query: ~w~n", [Query]),
    format(Stream, "Fragment: ~w~n", [Fragment]),
    true.

% Parse URI string and display components
urilib_display(urilib_parse(String, _)) :-
    urilib_parse(String, TempURI),
    urilib_display(TempURI, user_output),
    !.


% Primary predicate to parse a URI string into its components
% URIString: Input URI string to parse
% Returns: uri(Scheme, UserInfo, Host, Port, Path, Query, Fragment)
urilib_parse(URIString, uri(SchA,UIA,HA,PA,PathA,QA,FA)) :-
    % Convert input string to character codes
    string_codes(URIString, Codes),
    
    % Parse scheme and get remaining codes
    parse_scheme(Codes, CodesAfterScheme, Scheme),
    
    % Parse rest of URI based on scheme type
    parse_rest(Scheme, CodesAfterScheme, [], 
               Userinfo, Host, Port, Path, Query, Fragment),
    !,  % Cut to prevent backtracking
    
    % Convert all components to atoms
    to_atom(Scheme, SchA),
    to_atom(Userinfo, UIA), 
    to_atom(Host, HA),
    to_atom(Port, PA),
    to_atom(Path, PathA),
    to_atom(Query, QA),
    to_atom(Fragment, FA).

% Fallback clause for invalid URIs
urilib_parse(_, _) :-
    fail.

% Converts a value to an atom based on different input types
% Base cases:
% - Empty list becomes empty list
to_atom([], []) :- !. 

% - If input is already an atom, return it unchanged 
to_atom(Val, Val) :-
    atom(Val), 
    !.

% - If input is a non-empty list, convert it to an atom
to_atom(Val, Atom) :-
    is_list(Val),
    Val \= [],
    !,
    atom_codes(Atom, Val).

% - Default case: return input unchanged
to_atom(Val, Val).


% Parse the scheme component of a URI
% CodesIn: Input list of character codes
% Rem: Remaining codes after scheme parsing
% Scheme: Parsed scheme in lowercase
parse_scheme(CodesIn, Rem, Scheme) :-
    % Parse identifier until non-allowed character
    parse_identifier(CodesIn, Codes2, Ident),
    % Identifier must not be empty
    Ident \= [],
    % Must be followed by colon
    Codes2 = [0':|Codes3],
    % Convert scheme to lowercase
    downcase_atom(Ident, Scheme),
    % Return remaining codes
    Rem = Codes3.


% Base case: an empty input list fails directly
parse_identifier([], _, _) :- 
    fail.

% Parse an identifier starting with a valid character
% [C|Rest]: Input list of characters starting with C
% Out: Remaining characters after the identifier
% Atom: Resulting identifier as an atom
parse_identifier([C|Rest], Out, Atom) :-
    valid_id_char(C),              % First char must be valid
    !,                             % Cut to prevent backtracking
    collect_id_chars(Rest, OutRest, MoreChars), % Collect remaining valid chars
    atom_codes(Atom, [C|MoreChars]),            % Convert to atom
    Out = OutRest.                 % Return remaining chars that weren't 
%part of identifier 


parse_identifier(_, _, _) :-     % Catch-all failure  for invalid identifiers
    fail.


% Collect valid identifier characters recursively
% Base case: empty input -> empty outputs
collect_id_chars([], [], []).

% Recursive case: valid character -> add to result and continue
collect_id_chars([C|Xs], Out, [C|Rs]) :-
    valid_id_char(C), !,  % Cut to prevent backtracking if char is valid
    collect_id_chars(Xs, Out, Rs).

% Stop case: invalid character -> return remaining input unchanged
collect_id_chars(X, X, []) :-
    true.

% Determines if character C is valid in an identifier
% Valid chars are alphanumeric or one of _=+-
valid_id_char(C) :-
    char_type(C, alnum) ;
    memberchk(C, [0'_, 0'=, 0'+, 0'-]).


% Parse remaining URI components based on scheme type
% Each clause handles a different URI scheme with corresponding parsing rules

% Standard schemes (http, https, ftp)
parse_rest(Scheme, CodesIn, _Acc, UI, H, Port, Path, Q, F) :-
    standard_scheme(Scheme), !,
    parse_standard(Scheme, CodesIn, UI, H, Port, Path, Q, F).

% mailto scheme: userinfo@host
parse_rest(mailto, CodesIn, _Acc, UI, H, Port, [], [], []) :- !,
    parse_mailto(CodesIn, [], UI, H),
    default_port(mailto, Port).

% news scheme: just host
parse_rest(news, CodesIn, _Acc, [], H, Port, [], [], []) :- !,
    parse_news(CodesIn, [], H),
    default_port(news, Port).

% tel and fax schemes: just userinfo
parse_rest(S, CodesIn, _Acc, UI, [], Port, [], [], []) :-
    ( S == tel ; S == fax ), !,
    parse_tel_fax(CodesIn, [], UI),
    default_port(S, Port).

% zos scheme: special mainframe path format
parse_rest(zos, CodesIn, _Acc, UI, H, Port, Path, Q, F) :- !,
    parse_zos(CodesIn, UI, H, Port, Path, Q, F).

% Fallback for unsupported schemes
parse_rest(_, _, _, _, _, _, _, _, _) :-
    fail.

% Define which URI schemes follow standard URL syntax 
%(scheme://authority/path?query#fragment)
standard_scheme(http).  % HTTP protocol
standard_scheme(https). % HTTPS protocol 
standard_scheme(ftp).   % File Transfer Protocol

% Define default port numbers for different URI schemes
default_port(http, 80).    % Standard HTTP port
default_port(https, 443).  % Standard HTTPS port 
default_port(ftp, 21).     % Standard FTP port
default_port(zos, 3270).   % Default z/OS port
default_port(mailto, 80).  % Default mailto port
default_port(news, 80).    % Default news port
default_port(tel, 80).     % Default telephone port
default_port(fax, 80).     % Default fax port


% Parse standard URI scheme components like http/https/ftp
% Two cases:
% 1. URI with authority (//userinfo@host:port) 
% 2. URI without authority (just path/query/fragment)
parse_standard(Scheme, CodesIn, UI, H, Port, Path, Q, F) :-
    parse_with_auth(Scheme, CodesIn, UI, H, Port, Path, Q, F).
parse_standard(Scheme, CodesIn, UI, H, Port, Path, Q, F) :-
    parse_without_auth(Scheme, CodesIn, UI, H, Port, Path, Q, F).

% Parse URI with authority component (//userinfo@host:port)
% Scheme: URI scheme (http, https, etc)
% CodesIn: Input character codes
% UI: Userinfo component
% H: Host component
% Port: Port number 
% Path: Path component
% Q: Query component
% F: Fragment component
parse_with_auth(Scheme, CodesIn, UI, H, Port, Path, Q, F) :-
    prefixes("//", CodesIn),                    % Verify // prefix
    remove_prefixes("//", CodesIn, AfterSlashes),      % Remove // prefix
    parse_authority(AfterSlashes, RemAuth, UI, H, P0), % Parse authority section
    port_finder(Scheme, P0, Port),                 % Get port 
    H \= [],                                        % Host must not be empty
    parse_p_q_f(RemAuth, [], Path, Q, F). % Parse rem components


% Parse URI without authority component (just path/query/fragment)
% Scheme: URI scheme (http, https, etc)
% CodesIn: Input character codes
% Uses empty lists for userinfo and host since there's no authority 
% Port: Uses default port for the scheme
% Path: Path component
% Q: Query component 
% F: Fragment component
parse_without_auth(Scheme, CodesIn, [], [], Port, Path, Q, F) :-
    default_port(Scheme, Port),           % Get default port for scheme
    parse_p_q_f(CodesIn, [], Path, Q, F). % Parse remaining parts


% Resolve port number:
% - If no port specified (empty list), use scheme's default port
% - Otherwise use the specified port number directly
port_finder(Scheme, [], Port) :- 
    default_port(Scheme, Port).
port_finder(_, Port, Port).

% Checks if a list of codes starts with the given prefix
prefixes(Prefix, Codes) :-
    string_codes(Prefix, PCodes),  % Convert prefix string to codes
    append(PCodes, _, Codes).      % Check if codes start with prefix

% Removes the prefix from the list of codes and returns remainder 
remove_prefixes(Prefix, Codes, Remainder) :-
    string_codes(Prefix, PCodes),     % Convert prefix string to codes
    append(PCodes, Remainder, Codes). % Split codes into prefix and remainder


% Parse authority component of URI (userinfo@host:port)
% CodesIn: Input character codes
% Rem: Remaining codes after authority parsing
% User: Userinfo component (empty if none)
% Host: Host component (required)
% Port: Port number (empty if none)
parse_authority(CodesIn, Rem, User, Host, Port) :-
    % Try to parse userinfo part before @ symbol
    parse_identifier(CodesIn, Codes2, UI),
    
    % If @ symbol found after identifier
    ( Codes2 = [0'@|Codes3] ->
      % Userinfo must not be empty
      ( UI = [] -> fail
      ; User = UI,
        NextCodes = Codes3
      )
    ; % No @ found - no userinfo
      User = [],
      % Put back any parsed identifier chars
      identifier_back(UI, Codes2, BackCodes),
      NextCodes = BackCodes
    ),
    
    % Parse host component (required)
    parse_host_ip(NextCodes, CodesHost, Host),
    ( Host = [] -> fail ; true ),
    
    % Parse optional port number
    parse_port_present(CodesHost, Rem, Port).


% Helper predicate to put identifier chars back into code list if needed
% identifier_back(+Ident, +Codes, -NewCodes)
% - If Ident is empty, return Codes unchanged 
% - Otherwise convert Ident to codes and prepend to Codes
identifier_back([], Codes, Codes).
identifier_back(Ident, Codes, NewCodes) :-
    Ident \= [],
    atom_codes(Ident, ICodes),
    append(ICodes, Codes, NewCodes).



% Parse either a hostname or IP address from input codes
% CodesIn: Input character codes
% Rem: Remaining codes after parsing
% Host: Parsed host component
parse_host_ip([C|Rest], Rem, Host) :-
    char_type(C, alpha), !,  % If starts with letter, parse as domain name
    parse_d_h([C|Rest], Rem, Host).
parse_host_ip(CodesIn, Rem, Host) :- % Otherwise try parsing as IPv4
    parse_ip(CodesIn, Rem, Host).

% Parse domain name or hostname
% CodesIn: Input character codes 
% Rem: Remaining codes after parsing
% HostAtom: Parsed host as atom with parts joined by dots
parse_d_h(CodesIn, Rem, HostAtom) :-
    parse_host_part(CodesIn, Codes2, HP),    % Parse first part
    HP \= [],                                % Must not be empty
    parse_rest_host(Codes2, Rem, TParts), % Parse additional parts
    ( TParts = [] ->                         % If no additional parts
      HostAtom = HP                        % Use first part only
    ; TParts \= [],                          % Otherwise join all parts
      atomic_list_concat([HP|TParts], '.', HostAtom) % with dots
    ).

% Parse additional parts of a hostname after first part (separated by dots)
% [0'.|Rest]: Input codes starting with a dot
% Rem: Remaining codes after parsing all parts
% [HP|T]: List of host parts as atoms
parse_rest_host([0'.|Rest], Rem, [HP|T]) :-
    !,                              % Cut to prevent backtracking if dot found
    parse_host_part(Rest, R2, HP),  % Parse next part after dot
    HP \= [],                       % Part must not be empty
    parse_rest_host(R2, Rem, T). % Recursively parse remaining parts

% Base case: no more dots found, return empty list of additional parts
parse_rest_host(Codes, Codes, []).

% Parse a single host part (e.g. "www" in "www.example.com")
% [C0|Rest]: Input codes starting with alphabetic character
% Out: Remaining codes after parsing this part
% Atom: Parsed host part as atom
parse_host_part([C0|Rest], Out, Atom) :-
    char_type(C0, alpha),               % First char must be a letter
    parse_host_chars(Rest, OutChars, Out), % Parse remaining valid chars
    atom_codes(Atom, [C0|OutChars]).    % Convert to atom

% Parse remaining characters in host part
% [C|Xs]: Input codes 
% Cs: List of parsed character codes
% Out: Remaining codes after parsing
parse_host_chars([C|Xs], Cs, Out) :-
    ( char_type(C, alnum) ; C =:= 0'_; C =:= 0'- ), % Valid chars
    !,                                % Cut to prevent backtracking
    parse_host_chars(Xs, RestCs, Out), % Parse remaining chars recursively
    Cs = [C|RestCs].                  % Build list of parsed chars
parse_host_chars(X, [], X).         % Base case: no more valid chars


% Parse IPv4 address of format N1.N2.N3.N4 where each N is 0-255
% Codes: Input character codes
% Rem: Remaining codes after parsing IPv4
% HostIp: Parsed IPv4 address as atom (e.g. "192.168.1.1") 
parse_ip(Codes, Rem, HostIp) :-
    parse_octet(Codes, C2, O1),      % Parse first octet
    O1 \= [],                        % Must not be empty
    remove_char(0'., C2, C3),       % Consume dot separator
    parse_octet(C3, C4, O2),         % Parse second octet
    O2 \= [],                        % Must not be empty  
    remove_char(0'., C4, C5),       % Consume dot separator
    parse_octet(C5, C6, O3),         % Parse third octet
    O3 \= [],                        % Must not be empty
    remove_char(0'., C6, C7),       % Consume dot separator
    parse_octet(C7, Rem, O4),        % Parse fourth octet
    O4 \= [],                        % Must not be empty
    maplist(number_codes, [N1,N2,N3,N4],[O1,O2,O3,O4]), % Convert to numbers
    N1 >= 0, N1 =< 255,             % Validate octet ranges
    N2 >= 0, N2 =< 255,
    N3 >= 0, N3 =< 255, 
    N4 >= 0, N4 =< 255,
    atomic_list_concat([N1,N2,N3,N4], '.', HostIp). % Join with dots

% Parse a single octet (0-255) of an IPv4 address
% [D|Ds]: Input codes starting with a digit
% Rest: Remaining codes after parsing octet
% [D|More]: List of digit codes making up the octet
parse_octet([D|Ds], Rest, [D|More]) :-
    char_type(D, digit), !,  % First char must be digit
    parse_octet_digits(Ds, Rest, More).
parse_octet(_, _, []) :- fail. % Fail if no valid octet

% Parse remaining digits in an octet
% [D|Ds]: Input codes 
% Rest: Remaining codes after all digits
% [D|More]: List of parsed digit codes
parse_octet_digits([D|Ds], Rest, [D|More]) :-
    char_type(D, digit), !,  % If next char is digit, include it
    parse_octet_digits(Ds, Rest, More). 
parse_octet_digits(R, R, []). % No more digits, return rest

% Consume expected character from input
% Char: Character to consume
% [Char|R]: Input codes starting with Char
% R: Remaining codes after consuming Char
remove_char(Char, [Char|R], R) :- !. % Found expected char
remove_char(_, _, _) :- fail. % Did not find expected char

% Parse optional port number after colon in authority component
% [0':|Rest]: Input codes starting with colon
% Rem: Remaining codes after parsing port
% Port: Parsed port number
parse_port_present([0':|Rest], Rem, Port) :-
    !,                           % Cut since we found a colon
    parse_digits(Rest, Digits, Rem), % Parse sequence of digits
    Digits \= [],               % Port must not be empty
    number_codes(Num, Digits),  % Convert digit codes to number
    Port = Num.
% No port specified - return input unchanged and empty port
parse_port_present(Codes, Codes, []).

% Parse remaining path, query and fragment components
% CodesIn: Input codes
% Remainder: Remaining codes after parsing
% Path: Path component
% Query: Query component  
% Fragment: Fragment component
parse_p_q_f(CodesIn, Remainder, Path, Query, Fragment) :-
    parse_opt_path(CodesIn, RemPath, Path),      % Parse optional path
    parse_query(RemPath, RemQuery, Query),        % Parse optional query
    parse_fragment(RemQuery, Remainder, Fragment). % Parse optional fragment

% Parse an optional path component starting with forward slash
% [0'/|Rest]: Input codes starting with slash
% Rem: Remaining codes after parsing path
% PathAtom: Parsed path as atom
parse_opt_path([0'/|Rest], Rem, PathAtom) :-
    !,                                        % Cut to prevent backtracking
    parse_mand_path(Rest, Rem, PathAtom). % Parse rest as mandatory path

% Parse an optional path component that doesn't start with slash
% If no path found, return empty list 
% Codes: Input codes
% Rem: Remaining codes after parsing path
% PathAtom: Parsed path as atom
parse_opt_path(Codes, Rem, PathAtom) :-
    parse_mand_path(Codes, Rem, PathAtom),
    !.
parse_opt_path(Codes, Codes, []).  % No path found - return empty

% Parse a mandatory path component (must have at least one segment)
% Base cases:
% - Empty input -> empty path
parse_mand_path([], [], []) :- !.
% - Stop at query marker
parse_mand_path([0'?|R], [0'?|R], []) :- !.
% - Stop at fragment marker
parse_mand_path([0'#|R], [0'#|R], []) :- !.

% Main case:
% - Parse first path segment as identifier
% - Parse remaining segments after slashes
% - Join segments with / separator into final path atom
parse_mand_path(CodesIn, Rem, PathAtom) :-
    parse_identifier(CodesIn, Codes2, FirstSegment),
    parse_path_segments(Codes2, Rem, Segments),
    ( Segments = [] ->
      PathAtom = FirstSegment
    ; atomic_list_concat([FirstSegment|Segments], '/', PathAtom)
    ).

% Parse remaining path segments after slashes
% [0'/|Rest]: Input codes starting with slash
% Out: Remaining codes after parsing all segments
% [Seg|T]: List of path segments parsed from input
parse_path_segments([0'/|Rest], Out, [Seg|T]) :-
    !, % Cut to prevent backtracking if slash found
    parse_identifier(Rest, C2, Seg), % Parse identifier after slash as segment
    parse_path_segments(C2, Out, T). % Recursively parse remaining segments

% Base case: no more slashes found
% Return input codes unchanged and empty list of segments
% Base case: no more slashes found
% Return input codes unchanged and empty list of segments
parse_path_segments(Codes, Codes, []).

% Parse query component if present after ? character
% [0'?|Xs]: Input codes starting with ?
% Rem: Remaining codes after parsing query
% Query: Parsed query as atom
parse_query([0'?|Xs], Rem, Query) :-
    !, % Cut to prevent backtracking
    collect_query_chars(Xs, BeforeHash, Rem), % Collect query chars until # or end
    BeforeHash \= [], % Query must not be empty
    atom_codes(Query, BeforeHash). % Convert to atom

% No query present - return input unchanged and empty query
parse_query(Codes, Codes, []).

% Collect characters for URI query component
% Base case - empty input -> empty result
collect_query_chars([], [], []) :- !.

% Stop collecting at # character (start of fragment)
collect_query_chars([0'#|R], [], [0'#|R]) :- !.

% Collect valid query characters recursively
% [C|Xs]: Input character codes
% [C|Rs]: Collected query character codes 
% Rem: Remaining codes after query
collect_query_chars([C|Xs], [C|Rs], Rem) :-
    valid_query_char(C), !,  % Character must be valid
    collect_query_chars(Xs, Rs, Rem).

% Invalid character - fail
collect_query_chars(_, _, _) :-
    fail.

% Check if character is valid in query component
% Valid chars are alphanumeric or one of _=+-
valid_query_char(C) :-
    char_type(C, alnum) ;
    memberchk(C, [0'_, 0'=, 0'+, 0'-]).


% Parse fragment component if present after # character
% [0'#|Xs]: Input codes starting with #
% Rem: Remaining codes after parsing fragment (empty list)
% Frag: Parsed fragment as atom
parse_fragment([0'#|Xs], Rem, Frag) :-
    !, % Cut to prevent backtracking
    Xs \= [], % Fragment must not be empty 
    valid_fragment_chars(Xs), % Validate fragment characters
    atom_codes(Frag, Xs), % Convert fragment chars to atom
    Rem = []. % No remaining codes after fragment

% No fragment present - return input unchanged and empty fragment
parse_fragment(Codes, Codes, []).

% Check if all characters in the fragment component are valid
% Valid fragment chars include alphanumeric and _=+-
% Base case: empty list is valid
valid_fragment_chars([]).

% Recursive case: check each character
% [C|Cs]: List of characters starting with C
% Valid if C is alphanumeric or one of _=+-
valid_fragment_chars([C|Cs]) :-
    (char_type(C, alnum) ;
     memberchk(C, [0'_, 0'=, 0'-, 0'+])),
    !,  % Cut to prevent backtracking
    % Recursively validate remaining chars in fragment
    valid_fragment_chars(Cs). 


% Parse mailto URI of form userinfo@host
% CodesIn: Input codes
% Rem: Remaining codes after parsing
% UI: Userinfo component (required)
% Host: Host component (optional)
parse_mailto(CodesIn, Rem, UI, Host) :-
    parse_identifier(CodesIn, Codes2, Ident), % Parse userinfo
    ( Ident = [] -> fail ; UI = Ident ),      % Userinfo required
    ( Codes2 = [0'@|Codes3] ->                % If @ found
      parse_host_ip(Codes3, Rem, Host),    % Parse host part
      ( Host = [] -> fail ; true )            % Host must not be empty
    ; Host = [],                              % No host if no @
      Rem = Codes2                            % Return remaining codes
    ).



% parse_news(+CodesIn, -Rem, -Host).
% news => hostname/IP address
% - CodesIn: Input character codes starting with host
% - Rem: Remaining codes after parsing host  
% - Host: Parsed hostname/IP address, must not be empty

parse_news(CodesIn, Rem, Host) :-
    parse_host_ip(CodesIn, Rem, Host),
    ( Host = [] -> fail ; true ).


% Parse telephone/fax URIs that contain only userinfo component
% CodesIn: Input character codes containing phone/fax number
% Rem: Remaining codes after parsing
% UI: Userinfo component (required - the phone/fax number)
parse_tel_fax(CodesIn, Rem, UI) :-
    parse_identifier(CodesIn, Codes2, Ident), % Parse number as identifier
    ( Ident = [] -> fail ; UI = Ident ),      % Number required, fail if empty
    Rem = Codes2.                             % Return remaining codes


% Parse a z/OS URI which has two formats:
% 1. With authority: zos://authority/path?query#fragment
% 2. Without authority: zos:path?query#fragment
parse_zos(CodesIn, UI, Host, Port, Path, Q, F) :-
    % Check if URI starts with "//" indicating authority component
    ( prefixes("//", CodesIn) ->
      % Case with //authority
      remove_prefixes("//", CodesIn, AfterSlashes),
      % Parse authority component (userinfo@host:port)
      parse_authority(AfterSlashes, RemAuth, UI, Host, P0),
      % Use default port if none specified
      ( P0 = [] -> default_port(zos, Port) ; Port = P0 ),
      % Host is required
      ( Host = [] -> fail ; true ),
      
      % Check for path starting with /
      ( RemAuth = [0'/|Rest] ->
	% Handle empty path after /
	( Rest = [] -> 
          Path = [],
          RemPath = Rest
	; parse_zos_path(Rest, RemPath, Path)
	)
      ; % No path
	Path = [],
	RemPath = RemAuth
      ),
      % Parse optional query and fragment
      parse_query(RemPath, RemQuery, Q),
      parse_fragment(RemQuery, _, F)
    ;
    % Case without //authority 
    UI = [],                    % No userinfo
    Host = [],                  % No host
    default_port(zos, Port),    % Use default port
    
    % Parse path - can start with / or not
    ( CodesIn = [0'/|Rest] ->
      parse_zos_path(Rest, RemPath, Path),
      ( Path \= [] -> true ; fail )  % Path required if starts with /
    ; parse_zos_path_no_slash(CodesIn, RemPath, Path)
    ),
    % Parse optional query and fragment
    parse_query(RemPath, RemQuery, Q),
    parse_fragment(RemQuery, _, F)
    ).


% Parse a z/OS path component that doesn't start with a slash
% Base cases: empty input, query marker ?, or fragment marker # -> empty path
parse_zos_path_no_slash([], [], []) :- !.
parse_zos_path_no_slash([0'?|R], [0'?|R], []) :- !.
parse_zos_path_no_slash([0'#|R], [0'#|R], []) :- !.
% Otherwise parse as z/OS path with id44 and optional id8
parse_zos_path_no_slash(Codes, Rem, Path) :-
    parse_zos_id44_id8(Codes, Rem, Path).

% Parse a z/OS path component that starts with a slash
% Base cases: empty input, query marker ?, or fragment marker # -> empty path 
parse_zos_path([], [], []) :- !.
parse_zos_path([0'?|R], [0'?|R], []) :- !.
parse_zos_path([0'#|R], [0'#|R], []) :- !.
% Otherwise parse as z/OS path with id44 and optional id8
parse_zos_path(CodesIn, Rem, Path) :-

    % Parse z/OS path consisting of id44 and optional id8 components
    % CodesIn: Input codes starting with first path char
    % Out: Remaining codes after parsing path
    % Path: Full path as atom with id44 and optional id8
    parse_zos_id44_id8(CodesIn, Rem, Path).

% Parse id44 part followed by optional id8 in parentheses
% [C0|Rest]: Input codes starting with alphabetic char
% Out: Remaining codes after parsing path
% FullPath: Full path with id44 and optional id8 combined
parse_zos_id44_id8([C0|Rest], Out, FullPath) :-
    char_type(C0, alpha),                      % First char must be letter
    parse_zos_id44_chars(Rest, Out2, 1, Cs),  % Parse rest of id44 chars
    append([C0], Cs, FullId44),               % Combine first char with rest
    length(FullId44, Len44),                  % Get length of id44
    Len44 =< 44,                              % Must be 44 chars or less
    \+ final_dot(FullId44),               % Must not end with dot
    atom_codes(Id44, FullId44),               % Convert id44 to atom
    ( Out2 = [0'( |R2] ->                     % If followed by open paren
		  parse_id8(R2, R3, Id8Codes),            % Parse id8 part
		  R3 = [0')|Final],                       % Must end with close paren
      atom_codes(Id8, Id8Codes),              % Convert id8 to atom
      atomic_list_concat([Id44, '(', Id8, ')'], FullPath), % Join parts
      Out = Final                             % Return remaining codes
    ; Out = Out2,                             % No id8 - just return id44
      FullPath = Id44
    ).
parse_zos_id44_id8(_, _, []) :- fail.


% Parse characters in z/OS id44 component 
% Base case: empty input -> empty result with count preserved
parse_zos_id44_chars([], [], _, []) :- !.

% Parse valid id44 characters (alphanumeric or dot)
% Count tracks length to ensure <= 44 chars
parse_zos_id44_chars([C|Rest], Out, Count, [C|Cs]) :-
    ( char_type(C, alnum) ; C =:= 0'. ),  % Valid char is alphanumeric or dot
    Count1 is Count + 1,                   % Increment count
    Count1 =< 44, !,                       % Ensure <= 44 chars
    parse_zos_id44_chars(Rest, Out, Count1, Cs). % Parse remaining chars

% Stop parsing id44 at special characters
% Requires at least 1 char was parsed
parse_zos_id44_chars([C|Rest], Out, Count, []) :-
    ( C =:= 0'? ; C =:= 0'# ; C =:= 0'/ ; C =:= 0'( ), % Stop chars
					  !,
					  Out = [C|Rest],  % Return remaining input including stop char
					  Count >= 1.      % Must have parsed at least 1 character in id44

% Check if list ends with a dot character
final_dot(L) :-
    append(_, [0'.], L).  % List can be split into some prefix and final dot

% Parse id8 component starting with alphabetic character
% [C0|Rest]: Input codes starting with letter
% Out: Remaining codes after id8
% [C0|More]: List of parsed id8 character codes
parse_id8([C0|Rest], Out, [C0|More]) :-
    char_type(C0, alpha),                % First char must be letter
    parse_id8_chars(Rest, Out, More, 1). % Parse remaining chars
parse_id8(_, _, []) :- fail.           % Fail if no valid id8

% Parse remaining characters in id8 component
% [C|Xs]: Input codes
% Out: Remaining codes after id8
% [C|Rs]: List of parsed id8 chars
% Count: Running count of chars (max 8)
parse_id8_chars([C|Xs], Out, [C|Rs], Count) :-
    char_type(C, alnum),           % Must be alphanumeric
    Count1 is Count + 1,           % Increment count
    Count1 =< 8, !,                % Ensure <= 8 chars
    parse_id8_chars(Xs, Out, Rs, Count1). % Parse more chars
parse_id8_chars(X, X, [], _).    % No more valid chars - return rest

% Parse sequence of digits
% [D|Ds]: Input codes
% [D|More]: List of parsed digit codes
% R: Remaining codes after digits
parse_digits([D|Ds], [D|More], R) :-
    char_type(D, digit), !,        % Must be digit
    parse_digits(Ds, More, R).     % Parse more digits
parse_digits(Rest, [], Rest).    % No more digits - return rest
