# Modulo urilib-parse

Libreria Prolog per il parsing e l'analisi di URI (Uniform Resource Identifiers).

## Panoramica
Questo modulo implementa un parser completo per URI secondo le specifiche standard,
supportando vari schemi URI e fornendo funzionalità di accesso ai singoli componenti.

## Schemi URI Supportati
- HTTP/HTTPS (`http://`, `https://`)
- FTP (`ftp://`)
- Mailto (`mailto:`)
- News (`news:`) 
- Tel/Fax (`tel:`, `fax:`)
- z/OS (`zos:`)

## Installazione
1. Prerequisiti:
   - SWI-Prolog versione 7.0 o superiore
   - Sistema operativo compatibile (Windows/Linux/MacOS)

2. Installazione:
   ```prolog
   - Scaricare il file urilib-parse.pl
   - Copiarlo nella directory di lavoro del proprio progetto
   ```

## Utilizzo

### Caricamento del Modulo
```prolog
?- [urilib_parse].
```

### Predicati Principali
- `urilib_parse/2`: Parser principale per stringhe URI
- `urilib_scheme/2`: Estrae lo schema dell'URI
- `urilib_userinfo/2`: Estrae le informazioni utente
- `urilib_host/2`: Estrae l'host
- `urilib_port/2`: Estrae la porta
- `urilib_path/2`: Estrae il percorso
- `urilib_query/2`: Estrae la query
- `urilib_fragment/2`: Estrae il frammento
- `urilib_display/1`: Visualizza i componenti dell'URI
- `urilib_display/2`: Visualizza su stream specificato

### Esempi di Utilizzo

1. Parsing di un URI HTTP:
```prolog
?- urilib_parse("http://user:pass@example.com:8080/path?q=1#frag", URI).
URI = uri(http, user:pass, example.com, 8080, path, q=1, frag).
```

2. Estrazione componenti:
```prolog
?- urilib_parse("https://example.com/path", URI),
   urilib_scheme(URI, Scheme),
   urilib_host(URI, Host).
Scheme = https,
Host = example.com.
```

3. URI mailto:
```prolog
?- urilib_parse("mailto:user@domain.com", URI).
URI = uri(mailto, user, domain.com, 80, [], [], []).
```

4. Visualizzazione formattata:
```prolog
?- urilib_parse("ftp://ftp.example.com/file.txt", URI),
   urilib_display(URI).
Scheme: 'ftp'
Userinfo: []
Host: 'ftp.example.com'
Port: 21
Path: file.txt
Query: []
Fragment: []
```

## Gestione Errori
Il modulo implementa una robusta gestione degli errori:
- Validazione della sintassi URI
- Controllo dei componenti obbligatori
- Verifica dei valori ammissibili (es. range porte)

## Note Tecniche
- Supporto completo per IPv4
- Gestione di porte personalizzate
- Parsing ricorsivo dei componenti
- Validazione conforme agli standard URI

## Test e Debug
Per verificare il corretto funzionamento:
```prolog
% Test base
?- urilib_parse("http://example.com", URI).

% Test componenti
?- urilib_parse("http://user@host:80/path?q#f", URI),
   urilib_display(URI).
```

## Autore
Simone Morrone (Matricola: 916063)
Giada Bodini (Matricola: 914545)

## Licenza e Disclaimer
Questo software è fornito "così com'è", senza garanzie di alcun tipo.
L'autore non è responsabile per eventuali danni derivanti dal suo utilizzo.