% urilib-parse.pl
% Autore: Simone Morrone 916063

% Predicato principale: urilib_parse/2
% urilib_parse(+URIString, -URI) restituisce una struttura URI se la stringa è valida.
urilib_parse(URIString, uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    string_codes(URIString, Codes),
    parse_scheme(Codes, Rest1, Scheme),
    (parse_authority(Rest1, Rest2, Userinfo, Host, Port) -> true; Rest2 = Rest1, Userinfo = [], Host = [], Port = 80),
    (parse_path(Rest2, Rest3, Path) -> true; Rest3 = Rest2, Path = []),
    (parse_query(Rest3, Rest4, Query) -> true; Rest4 = Rest3, Query = []),
    (parse_fragment(Rest4, [], Fragment) -> true; Fragment = []).

% Parsing dello schema
parse_scheme(Codes, Rest, Scheme) :-
    extract_until_char(Codes, 58, Rest, SchemeCodes), % 58 = ':'
    atom_codes(Scheme, SchemeCodes).

extract_until_char([Char|Codes], Char, Codes, []).
extract_until_char([C|Codes], Char, Rest, [C|Result]) :-
    C \= Char,
    extract_until_char(Codes, Char, Rest, Result).

collect_identifier([C|Codes], Rest, [C|Result]) :-
    (char_type(C, alnum) ; member(C, "_=+-")),
    collect_identifier(Codes, Rest, Result).
collect_identifier(Rest, Rest, []).

% Parsing dell'autorità (userinfo, host, port)
parse_authority([47, 47|Codes], Rest, Userinfo, Host, Port) :- % 47 = '/'
    extract_until_delimiter(Codes, [47, 63, 35], Rest1, AuthorityCodes), % Stops at '/', '?' or '#'
    parse_authority_components(AuthorityCodes, Userinfo, Host, Port),
    Rest = Rest1.
parse_authority(Codes, Codes, [], [], []).

parse_authority_components(Codes, Userinfo, Host, Port) :-
    (append(UserinfoCodes, [64|HostPortCodes], Codes) -> % 64 = '@'
        atom_codes(Userinfo, UserinfoCodes),
        parse_host_port(HostPortCodes, Host, Port)
    ;   Userinfo = [],
        parse_host_port(Codes, Host, Port)).

parse_host_port(Codes, Host, Port) :-
    (append(HostCodes, [58|PortCodes], Codes) -> % 58 = ':'
        atom_codes(Host, HostCodes),
        collect_digits(PortCodes, [], PortDigitCodes),
        number_codes(Port, PortDigitCodes)
    ;   atom_codes(Host, Codes),
        Port = 80).

parse_userinfo(Codes, Rest, Userinfo) :-
    append(UserinfoCodes, [64|Rest], Codes), % 64 = '@'
    !,
    atom_codes(Userinfo, UserinfoCodes).
parse_userinfo(Codes, Codes, []).

parse_host(Codes, Rest, Host) :-
    collect_identifier(Codes, Rest, HostCodes),
    atom_codes(Host, HostCodes).

parse_port([58|Codes], Rest, Port) :- % 58 = ':'
    collect_digits(Codes, Rest, PortCodes),
    number_codes(Port, PortCodes).
parse_port(Codes, Codes, 80). % Default port

collect_digits([C|Codes], Rest, [C|Result]) :-
    char_type(C, digit),
    collect_digits(Codes, Rest, Result).
collect_digits(Rest, Rest, []).

% Parsing del path
parse_path([47|Codes], Rest, Path) :- % 47 = '/'
    extract_until_delimiter(Codes, [63, 35], Rest, PathCodes), % 63 = '?', 35 = '#'
    atom_codes(Path, PathCodes).
parse_path(Codes, Codes, []).

extract_until_delimiter([C|Codes], Delimiters, Rest, [C|Result]) :- %estraggo caratteri fino delimitatori
    \+ member(C, Delimiters), %c non deve essere delimitatore
    extract_until_delimiter(Codes, Delimiters, Rest, Result).
extract_until_delimiter(Rest, _, Rest, []).

% Parsing della query
parse_query([63|Codes], Rest, Query) :- % 63 = '?'
    collect_identifier(Codes, Rest, QueryCodes),
    atom_codes(Query, QueryCodes).
parse_query(Codes, Codes, []).

% Parsing del frammento
parse_fragment([35|Codes], Rest, Fragment) :- % 35 = '#'
    extract_until_delimiter(Codes, [], Rest, FragmentCodes),
    atom_codes(Fragment, FragmentCodes).
parse_fragment(Codes, Codes, []).

% Predicato per visualizzare la URI
urilib_display(uri(Scheme, Userinfo, Host, Port, Path, Query, Fragment)) :-
    format("Scheme: ~w~n", [Scheme]),
    format("Userinfo: ~w~n", [Userinfo]),
    format("Host: ~w~n", [Host]),
    format("Port: ~w~n", [Port]),
    format("Path: ~w~n", [Path]),
    format("Query: ~w~n", [Query]),
    format("Fragment: ~w~n", [Fragment]).
