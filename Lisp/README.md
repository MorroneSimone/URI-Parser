# Documentazione della Libreria URI Parser in Lisp

Libreria Common Lisp per il parsing, la validazione e la manipolazione di URI (Uniform Resource Identifiers).

## Panoramica
Questa libreria implementa un parser completo per URI conforme agli standard,
con supporto per vari schemi e una robusta gestione degli errori.

## Struttura URI
La libreria utilizza una struttura dati personalizzata (`uri-struct`) con i seguenti campi:
- **scheme**: Il protocollo dell'URI (es. "http", "mailto")
- **userinfo**: Informazioni di autenticazione
- **host**: Nome del dominio o indirizzo IP
- **port**: Numero di porta
- **path**: Percorso della risorsa
- **query**: Parametri di query
- **fragment**: Identificatore del frammento

## Schemi URI Supportati
1. **Standard**:
   - HTTP (`http://`)
   - HTTPS (`https://`)
   - FTP (`ftp://`)

2. **Speciali**:
   - Mailto (`mailto:`)
   - News (`news:`)
   - Tel (`tel:`)
   - Fax (`fax:`)
   - z/OS (`zos:`)

## Installazione e Setup
1. **Prerequisiti**:
   - Interprete Common Lisp (SBCL, CLISP, CCL)
   - Editor con supporto Lisp (Emacs/SLIME consigliato)

2. **Installazione**:
   ```lisp
   (load "urilib-parse.lisp")
   ```

## Utilizzo Dettagliato

### 1. Parsing Base
```lisp
; URI semplice
(urilib-parse "http://example.com")

; URI complesso
(urilib-parse "http://user:pass@example.com:8080/path?q=1#frag")
```

### 2. Accesso ai Componenti
```lisp
(let ((uri (urilib-parse "https://example.com/path")))
  (format t "Schema: ~A~%" (urilib-scheme uri))
  (format t "Host: ~A~%" (urilib-host uri))
  (format t "Path: ~A~%" (urilib-path uri)))
```

### 3. Schemi Speciali
```lisp
; Mailto
(urilib-parse "mailto:user@domain.com")

; Tel
(urilib-parse "tel:+1-234-567-8900")

; z/OS
(urilib-parse "zos://system.example.com:5000/dataset(member)")
```

### 4. Visualizzazione
```lisp
(let ((uri (urilib-parse "ftp://ftp.example.com/file.txt")))
  (urilib-display uri))

; Output su file
(with-open-file (stream "uri-info.txt" :direction :output)
  (urilib-display uri stream))
```

## Gestione degli Errori

La libreria implementa controlli rigorosi e segnala errori specifici per:

1. **Errori di Schema**:
   - Schema mancante o non supportato
   - Formato schema non valido

2. **Errori di Autorità**:
   - Host non valido
   - Porta fuori range (1-65535)
   - Userinfo malformato

3. **Errori di Path**:
   - Path malformato
   - Caratteri non validi
   - Struttura z/OS non valida

4. **Altri Errori**:
   - Query malformata
   - Fragment non valido
   - Sintassi URI non valida

## Validazione

### Host
- Supporto per hostname:
  - Lettere, numeri, punti
  - No punti consecutivi
  - No inizio/fine con punto
  
- Supporto per IPv4:
  - Quattro ottetti (0-255)
  - Separati da punti
  - No zeri iniziali

### Path
- Validazione percorso standard:
  - Caratteri permessi
  - Struttura corretta
  
- Validazione z/OS:
  - ID44 (1-44 caratteri)
  - ID8 opzionale (1-8 caratteri)
  - Formato dataset corretto

## Test e Debug
```lisp
(urilib-parse "http://example.com")

(let ((uri (urilib-parse "http://user@host:80/path?q#f")))
  (urilib-display uri))

; Test errori
(handler-case
    (urilib-parse "invalid:///uri")
  (error (c)
    (format t "Errore: ~A~%" c)))
```

## Note Tecniche
- Parsing ricorsivo dei componenti
- Gestione efficiente delle stringhe
- Validazione conforme agli standard URI
- Supporto completo Unicode per hostnames

## Performance e Limitazioni
- Parsing efficiente O(n) dove n è la lunghezza dell'URI
- Gestione memoria ottimizzata
- Limitazioni note:
  - No supporto IPv6
  - No URI relativi
  - Limite lunghezza URI: dipende dall'implementazione Lisp

## Autori
- Simone Morrone (Matricola: 916063)
- Giada Bodini (Matricola: 914545)

## Licenza
Questo software è fornito "così com'è", senza garanzie.
Gli autori non sono responsabili per eventuali danni derivanti dal suo utilizzo.
