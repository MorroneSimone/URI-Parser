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

% Parsing e validazione dello schema
parse_scheme(Codes, Rest, Scheme) :-
    extract_until_char(Codes, 58, Rest, SchemeCodes), % 58 = ':'
    validate_scheme_chars(SchemeCodes),
    atom_codes(Scheme, SchemeCodes).

% Validazione dei caratteri dello schema
validate_scheme_chars([]).
validate_scheme_chars([C|Rest]) :-
    (char_type(C, alnum) ; member(C, [43, 61, 45, 95 ])),
    validate_scheme_chars(Rest).

extract_until_char([Char|Codes], Char, Codes, []).
extract_until_char([C|Codes], Char, Rest, [C|Result]) :-
    C \= Char,
    extract_until_char(Codes, Char, Rest, Result).

collect_identifier([C|Codes], Rest, [C|Result]) :-
    (char_type(C, alnum) ; member(C, [43, 61, 45, 95 ])),
    collect_identifier(Codes, Rest, Result).
collect_identifier(Rest, Rest, []).

% Parsing dell'autorità con validazione IP
parse_authority([47, 47|Codes], Rest, Userinfo, Host, Port) :- % 47 = '/'
    extract_until_delimiter(Codes, [47, 63, 35], Rest1, AuthorityCodes), % Stops at '/', '?' or '#'
    parse_authority_components(AuthorityCodes, Userinfo, Host, Port),
    Rest = Rest1.
parse_authority(Codes, Codes, [], [], []).


valid_userinfo_char(C) :-
    (char_type(C, alnum); member(C, [43,45,95,61])).


parse_authority_components(Codes, Userinfo, Host, Port) :-
    (append(UserinfoCodes, [64|HostPortCodes], Codes) -> % 64 = '@'
        (maplist(valid_userinfo_char, UserinfoCodes) ->
            print("vero")
            
        ;
            throw('Invalid UserInfo format')
        ),    
        atom_codes(Userinfo, UserinfoCodes),
        parse_host_port(HostPortCodes, Host, Port)
    ;   Userinfo = [],
        parse_host_port(Codes, Host, Port)).

% Parsing di host e porta
parse_host_port(Codes, Host, Port) :-
    (append(HostCodes, [58|PortCodes], Codes) -> % 58 = ':'
        parse_host(HostCodes, Host),
        validate_port(PortCodes, Port)
    ;   parse_host(Codes, Host),
        Port = 80).

% Validazione della porta migliorata
validate_port(PortCodes, Port) :-
    (all_digits(PortCodes) ->
        (number_codes(Port, PortCodes) ->
            (Port >= 0, Port =< 65535 ->
                true
            ;
                throw('Port number must be between 0 and 65535')
            )
        ;
            throw('Invalid port number format')
        )
    ;
        throw('Port must contain only digits')
    ).

% Verifica che tutti i caratteri siano digit
all_digits([]).
all_digits([C|Rest]) :-
    char_type(C, digit),
    all_digits(Rest).

% Parsing dell'host con supporto per IP e domini
parse_host(Codes, Host) :-
    % Prima verifichiamo se è un indirizzo IP valido
    (is_valid_ip_format(Codes) ->
        parse_ip_address(Codes, Host)
    ;   % Se non è un IP, trattiamo come dominio
        parse_domain(Codes, Host)).

% Verifica preliminare del formato IP
is_valid_ip_format(Codes) :-
    % Conta i punti e verifica che ci siano solo numeri e punti
    count_dots(Codes, 3),
    all_ip_chars(Codes).

count_dots([], 0).
count_dots([46|Rest], N) :-  % 46 = '.'
    count_dots(Rest, N1),
    N is N1 + 1.
count_dots([C|Rest], N) :-
    C \= 46,
    count_dots(Rest, N).

all_ip_chars([]).
all_ip_chars([C|Rest]) :-
    (char_type(C, digit) ; C = 46),  % digit o punto
    all_ip_chars(Rest).

% Parsing dell'indirizzo IP
parse_ip_address(Codes, Host) :-
    parse_ip_parts(Codes, [], Parts),
    length(Parts, 4),
    maplist(validate_ip_part, Parts),
    atomic_list_concat(Parts, '.', Host).

% Parsing ricorsivo delle parti dell'IP
parse_ip_parts([], CurrentPart, [LastPart]) :-
    % Converte l'ultima parte in numero
    number_codes(LastPart, CurrentPart).
parse_ip_parts([46|Rest], CurrentPart, [Part|RestParts]) :-  % 46 = '.'
    % Quando troviamo un punto, convertiamo la parte corrente in numero
    number_codes(Part, CurrentPart),
    parse_ip_parts(Rest, [], RestParts).
parse_ip_parts([Digit|Rest], CurrentPart, Parts) :-
    % Accumula le cifre della parte corrente
    char_type(Digit, digit),
    append(CurrentPart, [Digit], NewCurrentPart),
    parse_ip_parts(Rest, NewCurrentPart, Parts).

% Validazione parte IP
validate_ip_part(Part) :-
    (number(Part) ->
        (Part >= 0 ->
            (Part =< 255 ->
                true
            ;
                throw('IP part must be less than or equal to 255')
            )
        ;
            throw('IP part must be greater than or equal to 0')
        )
    ;
        throw('IP part must be a number')
    ).

% Parsing del dominio normale
parse_domain(Codes, Host) :-
    atom_codes(Host, Codes).

parse_userinfo(Codes, Rest, Userinfo) :-
    append(UserinfoCodes, [64|Rest], Codes), % 64 = '@'
    !,
    atom_codes(Userinfo, UserinfoCodes).
parse_userinfo(Codes, Codes, []).

% Parsing del path
parse_path([47|Codes], Rest, Path) :- % 47 = '/'
    extract_until_delimiter(Codes, [63, 35], Rest, PathCodes), % 63 = '?', 35 = '#'
    atom_codes(Path, PathCodes).
parse_path(Codes, Codes, []).

extract_until_delimiter([C|Codes], Delimiters, Rest, [C|Result]) :-
    \+ member(C, Delimiters),
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