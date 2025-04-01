
;;;; -*- Mode: Lisp -*-
;;;; uri-parse.lisp

;;; uri-struct.  
;;; Structure for holding URI components.
;;; scheme: string - the URI scheme (http, https, ftp, special schemes.)
;;; userinfo: string - authentication info (NIL if empty)
;;; host: string - domain name or IP address (NIL if empty)
;;; port: integer - network port
;;; path: string - resource path (NIL if empty)
;;; query: string - URI query  (NIL if empty)
;;; fragment: string - URI fragment (NIL if empty)

(defstruct uri-struct
  scheme
  userinfo
  host
  (port nil :type (or null integer))
  path
  query
  fragment)


;;; urilib-display - prints the content of uri-struct to stream
;;; uri: uri-struct - structure containing the URI components
;;; stream: output-stream - stream to write to (default: t for standard output)
;;; returns: t if stream is standard output, nil otherwise

(defun urilib-display (uri &optional (stream t))
  (format stream "Schema:~13T~S~%" (string-upcase (or (urilib-scheme uri) 
						      "NIL")))
  (format stream "Userinfo:~13T~S~%" (or (urilib-userinfo uri) "NIL"))
  (format stream "Host:~13T~S~%" (or (urilib-host uri) "NIL"))
  (format stream "Port:~13T~D~%" (or (urilib-port uri) "NIL"))
  (format stream "Path:~13T~S~%" (or (urilib-path uri) "NIL"))
  (format stream "Query:~13T~S~%" (or (urilib-query uri) "NIL"))
  (format stream "Fragment:~13T~S" (or (urilib-fragment uri) "NIL"))
  (if (not (equal stream T))
      (close stream)
      T))

;;; Accessor functions for uri-struct components
;;; Each function takes a uri-struct as input and 
;;; returns the corresponding component
;;; Path is returned as NIL if empty to prevent empty strings

(defun urilib-scheme (uri)
  (uri-struct-scheme uri))
(defun urilib-userinfo (uri)
  (uri-struct-userinfo uri))
(defun urilib-host (uri)
  (uri-struct-host uri))
(defun urilib-port (uri)
  (uri-struct-port uri))
(defun urilib-path (uri)
  (let ((path (uri-struct-path uri)))
    (if (string= path "") nil path)))
(defun urilib-query (uri)
  (uri-struct-query uri))
(defun urilib-fragment (uri)
  (uri-struct-fragment uri))



;;; URILIB-PARSE - MAIN FUNCTIOPN - Parse a URI string into its component parts
;;;
;;; Parses a URI string and returns a URI structure 
;;; containing the parsed components.
;;; Handles standard URI schemes (http, https, ftp) and special schemes 
;;; (mailto, news, tel, fax, zos).
;;;
;;; Parameters:
;;;   URI-STRING - A string containing the URI to parse
;;;
;;; Returns:
;;;   A URI structure with the following fields:
;;;   - SCHEME    - The URI scheme (e.g., "http", "mailto")
;;;   - USERINFO  - User information component 
;;;   - HOST      - Host name/address
;;;   - PORT      - Port number (defaults based on scheme)
;;;   - PATH      - Path component
;;;   - QUERY     - URI query
;;;   - FRAGMENT  - URI fragment
;;;
;;; Signals an error if:
;;;   - The scheme is missing or invalid
;;;   - The scheme is not one of the supported schemes
;;;
;;; Default ports:
;;;   - HTTP:  80
;;;   - HTTPS: 443
;;;   - FTP:   21
;;;   - Others: 80

(defun urilib-parse (uri-string)
  (multiple-value-bind (scheme rest)
      (parse-scheme uri-string)
    (unless scheme
      (error "Invalid scheme"))
    (unless (member scheme '("http" "https" "ftp" "mailto" 
                             "news" "tel" "fax" "zos")
                    :test #'string-equal)
      (error "Invalid scheme"))
    
    (if (member scheme '("mailto" "news" "tel" "fax" "zos")
                :test #'string-equal)
	
        (multiple-value-bind (userinfo host port path query fragment)
            (parse-special-scheme scheme rest)
          (make-uri-struct
           :scheme scheme
           :userinfo userinfo
           :host host
           :port (or port 80)
           :path path
           :query query
           :fragment fragment))
        
        (multiple-value-bind (userinfo host port remaining)
            (parse-auth rest)
          (multiple-value-bind (path query fragment)
              (parse-p-q-f (or remaining ""))
            (make-uri-struct
             :scheme scheme
             :userinfo userinfo
             :host host
             :port (or port
                       (cond ((string-equal scheme "http") 80)
                             ((string-equal scheme "https") 443)
                             ((string-equal scheme "ftp") 21)
                             (t 80)))
             :path path
             :query query
             :fragment fragment))))))


;;; parse-scheme - Extract and validate the scheme component from a URI string
;;; 
;;; Parameters:
;;;   STR - Input URI string
;;;
;;; Returns:
;;;   Two values:
;;;   1. The scheme string (lowercase) if valid, NIL if invalid
;;;   2. The rest of the URI string after the scheme
;;;
;;; A valid scheme must:
;;; - Start with a letter
;;; - Contain only letters, digits, and special chars (_, =, +, -)
;;; - Have at least one character before the colon

(defun parse-scheme (str)
  (let ((colon-pos (position #\: str)))
    (when (and colon-pos
               (> colon-pos 0)
               (valid-char (char str 0))
               (every #'valid-alpha (subseq str 0 colon-pos)))
      (values (subseq str 0 colon-pos)
              (subseq str (1+ colon-pos))))))

;;; parse-special-scheme - Parses URIs for special schemes 
;;; (mailto, news, tel, fax, zos)
;;; 
;;; Parameters:
;;;   SCHEME - The URI scheme (e.g., "mailto", "news", etc.)
;;;   REST - The remainder of the URI string after the scheme
;;;
;;; Returns:
;;;   Multiple values representing URI components:
;;;   - userinfo, host, port, path, query, fragment

(defun parse-special-scheme (scheme rest)
  (case (intern (string-upcase scheme) "KEYWORD")
    (:mailto (parse-mailto rest))
    (:news (parse-news rest))
    (:tel (parse-tel-fax rest "tel"))
    (:fax (parse-tel-fax rest "fax"))
    (:zos (parse-zos rest))))


;;; -------------------------------------------------------------
;;; parse-mailto
;;; -------------------------------------------------------------
;;; Parses a mailto URI string into its components.
;;; Signals:
;;; Signals an error if:
;;; - userinfo component is invalid
;;; - host is present but empty or invalid

(defun parse-mailto (str)
  (let ((at-pos (position #\@ str)))
    (if at-pos
        (let ((userinfo (subseq str 0 at-pos))
              (host (subseq str (1+ at-pos))))
          (unless (valid-user userinfo)
            (error "Invalid mailto userinfo"))
          (unless (and (> (length host) 0)
                       (valid-host host))
            (error "Invalid mailto host"))
          (values userinfo host nil nil nil nil))
        (progn
          (unless (valid-user str)
            (error "Invalid mailto userinfo"))
          (values str nil nil nil nil nil)))))    


;;; ----------------------------------------------------------------------------
;;; parse-news 
;;; ----------------------------------------------------------------------------
;;;
;;; Signals:
;;;   ERROR if:
;;;   - Input string is empty
;;;   - Input string starts with "//"
;;;   - Host is invalid according to valid-host

(defun parse-news (str)
  (when (or (zerop (length str))
            (and (>= (length str) 2)
                 (string= "//" (subseq str 0 2))))
    (error "Invalid news URI"))
  (unless (valid-host str)
    (error "Invalid news host"))
  (values nil str nil nil nil nil))



;;; ----------------------------------------------------------------------------
;;; parse-Tel/Fax 
;;; ----------------------------------------------------------------------------
;;;
;;; Parameters:
;;; scheme - Which URI scheme ("tel" or "fax")
;;;
;;; Signals:
;;; - Errors if str is empty
;;; - Errors if str starts with "//"
;;; - Errors if str contains invalid userinfo characters

(defun parse-tel-fax (str scheme)
  (when (or (zerop (length str))
            (and (>= (length str) 2)
                 (string= "//" (subseq str 0 2))))
    (error (format nil "Invalid ~A URI" scheme)))
  (unless (valid-user str)
    (error (format nil "Invalid ~A userinfo" scheme)))
  (values str nil nil nil nil nil))


;;; ----------------------------------------------------------------------------
;;; parse-ZOS
;;; ----------------------------------------------------------------------------
;;;
;;; Signals:
;;;   ERROR if:
;;;   - ZOS path structure is invalid
;;;   - Authority components are invalid when present

(defun parse-zos (str)
  (if (and (>= (length str) 2)
           (string= "//" (subseq str 0 2)))
      
      (multiple-value-bind (userinfo host port remaining)
          (parse-auth str)
        (when remaining
          (let* ((path-str (if (char= (char remaining 0) #\/)
                               (subseq remaining 1)
                               remaining))
                 (q-pos (position #\? path-str))
                 (f-pos (position #\# path-str))
                 (path-end (or q-pos f-pos (length path-str)))
                 (path (subseq path-str 0 path-end))
                 (query (when q-pos
                          (if f-pos
                              (subseq path-str (1+ q-pos) f-pos)
                              (subseq path-str (1+ q-pos)))))
                 (fragment (when f-pos
                             (subseq path-str (1+ f-pos)))))
            (unless (parse-zos-path-structure path)
              (error "Invalid ZOS path"))
            (values userinfo host port path query fragment))))
      
      (let* ((path-str (if (and (> (length str) 0)
				(char= (char str 0) #\/))
                           (subseq str 1)
                           str))
             (q-pos (position #\? path-str))
             (f-pos (position #\# path-str))
             (path-end (or q-pos f-pos (length path-str)))
             (path (subseq path-str 0 path-end))
             (query (when q-pos
                      (if f-pos
                          (subseq path-str (1+ q-pos) f-pos)
                          (subseq path-str (1+ q-pos)))))
             (fragment (when f-pos
			 (subseq path-str (1+ f-pos)))))
        (unless (parse-zos-path-structure path)
          (error "Invalid ZOS path"))
        (values nil nil nil path query fragment))))


;;; check-zos-id44 
;;;
;;; Returns: T if valid, NIL if invalid
;;;
;;; Valid ID44 must:
;;; - Be 1-44 characters long
;;; - Start with a letter
;;; - Not end with a period
;;; - Contain only letters, digits, and periods

(defun check-zos-id44 (str)
  (and (> (length str) 0)
       (<= (length str) 44)
       (valid-char (char str 0))
       (not (char= (char str (1- (length str))) #\.))
       (every (lambda (c)
                (or (valid-char c)
                    (valid-digit c)
                    (char= c #\.)))
              str)))

;;; check-zos-id8 
;;;
;;; Returns: T if valid, NIL if invalid 
;;;
;;; Valid ID8 must:
;;; - Be 1-8 characters long
;;; - Start with a letter
;;; - Contain only letters and digits

(defun check-zos-id8 (str)
  (and (> (length str) 0)
       (<= (length str) 8)
       (valid-char (char str 0))
       (every (lambda (c)
                (or (valid-char c)
                    (valid-digit c)))
              str)))

;;; parse-zos-path-structure
;;;
;;; Returns: T if valid, NIL if invalid
;;;
;;; Format: ID44[(ID8)]
;;; - ID44 must be a valid dataset name
;;; - Optional ID8 member name in parentheses
;;; - No spaces allowed

(defun parse-zos-path-structure (str)
  (let* ((open-paren (position #\( str :from-end t))
         (close-paren (and open-paren
                           (position #\) str :start (1+ open-paren))))
         (id44 (if open-paren
                   (subseq str 0 open-paren)
                   str)))
    (and (check-zos-id44 id44)
         (or (null open-paren)
             (and close-paren
                  (= close-paren (1- (length str)))
                  (> close-paren (1+ open-paren))
                  (check-zos-id8 (subseq str (1+ open-paren)
                                         close-paren)))))))

;;; ----------------------------------------------------------------------------
;;; split-q-f - Split a URI string into path, query and fragment 
;;; ----------------------------------------------------------------------------
;;;
;;; Returns:
;;;   Multiple values:
;;;   1. Path component (everything before ? or #)
;;;   2. Query component (between ? and #, or after ? if no #) - nil if no query
;;;   3. Fragment component (after #) - nil if no fragment

(defun split-q-f (str)
  (let ((q-pos (position #\? str))        ; Find position of query marker
	(f-pos (position #\# str)))           ; Find position of fragment marker
    (cond
      ;; Both query and fragment present
      ((and q-pos f-pos)
       (values (subseq str 0 q-pos)
               (subseq str (1+ q-pos) f-pos)
               (subseq str (1+ f-pos))))
      ;; Only query present 
      (q-pos
       (values (subseq str 0 q-pos)
               (subseq str (1+ q-pos))
               nil))
      ;; Only fragment present
      (f-pos
       (values (subseq str 0 f-pos)
               nil
               (subseq str (1+ f-pos))))
      ;; Neither present - return whole string as path
      (t
       (values str nil nil)))))


;;; ----------------------------------------------------------------------------
;;; parse-auth - Parse authority component of a URI
;;; ----------------------------------------------------------------------------
;;;
;;; Returns:
;;;   Multiple values:
;;;   1. userinfo - User info component (nil if not present)
;;;   2. host - Host component 
;;;   3. port - Port number (nil if not specified)
;;;   4. remaining - Rest of the URI string after authority
;;;
;;; Signals:
;;;   ERROR if:
;;;   - Authority section is present (starts with //) but empty
;;;   - Invalid userinfo, host or port components
;;;
;;; Info:
;;; - Authority section starts with //
;;; - Authority ends at first /, ? or # character
;;; - Components parsed by parse-auth-sub

(defun parse-auth (str)
  (if (and (>= (length str) 2)
           (string= "//" (subseq str 0 2)))
      (let* ((auth-start (subseq str 2))
             (path-pos (position-if
			(lambda (c)
                          (member c '(#\/ #\? #\#)))
			auth-start))
             (auth-part (if path-pos
                            (subseq auth-start 0 path-pos)
                            auth-start))
             (remaining (when path-pos
                          (subseq auth-start path-pos))))
        (when (zerop (length auth-part))
          (error "Empty authority"))
        (multiple-value-bind (userinfo host port)
            (parse-auth-sub auth-part)
          (values userinfo host port remaining)))
      (values nil nil nil str)))

;;; ----------------------------------------------------------------------------
;;; parse-auth-sub - Parse URI authority components(userinfo, host, port)
;;; ----------------------------------------------------------------------------
;;;
;;; Returns:
;;;   Multiple values:
;;;   1. userinfo - User info component (nil if not present)
;;;   2. host - Host component
;;;   3. port - Port number as integer (nil if not present)
;;;
;;; Signals:
;;; - Empty userinfo error if @ present but no userinfo
;;; - Invalid userinfo error if userinfo fails validation
;;; - Empty host error if host component is empty
;;; - Invalid host error if host fails validation 
;;; - Invalid port error if port is not 1-65535

(defun parse-auth-sub (str)
  (let* ((at-pos (position #\@ str))
         (userinfo (when at-pos (subseq str 0 at-pos)))
         (host-port (if at-pos (subseq str (1+ at-pos)) str))
         (colon-pos (position #\: host-port)))
    
    (when (and at-pos (zerop (length userinfo)))
      (error "Empty userinfo"))
    
    (when userinfo
      (unless (valid-user userinfo)
        (error "Invalid userinfo")))
    
    (let* ((host (subseq host-port 0 colon-pos))
           (port-str (when colon-pos (subseq host-port (1+ colon-pos))))
           (port (when port-str
                   (parse-integer port-str :junk-allowed t))))
      
      (when (zerop (length host))
        (error "Empty host"))
      
      (unless (valid-host host)
        (error "Invalid host"))
      
      (when (and port-str
                 (or (null port)
                     (not (<= 1 port 65535))))
        (error "Invalid port"))
      
      (values userinfo host port))))

;;; ----------------------------------------------------------------------------
;;; valid-hostname-format - Validate hostname format recursively
;;; ----------------------------------------------------------------------------
;;;
;;; Parameters:
;;;   str - The hostname string to validate
;;;   pos - Current position in the string
;;;
;;; Returns:
;;;   T if string matches hostname format, NIL otherwise
;;;
;;; Valid hostname must:
;;; - Not have consecutive dots (handled by parent)
;;; - Have dots separated by valid chars
;;; - Have only letters, numbers, and dots
;;; - Have segments start with letters
;;; - Not end with a dot
;;; - Not start or end with a digit

(defun valid-hostname-format (str pos)
  (cond ((>= pos (length str)) t)                    ; End of string - valid
        ((char= (char str pos) #\.)                  ; Found a dot
         (and (> pos 0)                              ; Not at start
              (< (1+ pos) (length str))              ; Not at end
              (valid-char (char str (1+ pos)))         ; Next char is letter
              (valid-hostname-format str (1+ pos)))) ; Check rest
        ((or (valid-char (char str pos))               ; Letter or digit
             (valid-digit (char str pos)))
         (valid-hostname-format str (1+ pos)))      ; Check rest
        (t nil)))                                    ; Invalid char

;;; ----------------------------------------------------------------------------
;;; valid-hostname - Validate hostname string format
;;; ----------------------------------------------------------------------------
;;; 
;;;
;;; Parameters:
;;;   str - The hostname string to validate
;;;
;;; Returns:
;;;   T if string is a valid hostname, NIL otherwise
;;;
;;; Valid hostname must:
;;; - Not be empty
;;; - Start with a letter
;;; - Not end with a dot
;;; - Not contain consecutive dots
;;; - Meet hostname format rules checked by valid-hostname-format

(defun valid-hostname (str)
  (and (> (length str) 0)
       (valid-char (char str 0))
       (not (char= (char str (1- (length str))) #\.))
       (not (search ".." str))
       (valid-hostname-format str 0)))

;;; ----------------------------------------------------------------------------
;;; valid-host - Validate hostname string format
;;; ----------------------------------------------------------------------------
;;;
;;; A valid host must be:
;;; - Not empty
;;; - Either a valid IP address (e.g. 192.168.1.1)
;;; - Or a valid hostname (e.g. example.com)

(defun valid-host (str)
  (and (> (length str) 0)
       (or (valid-ip str)
           (valid-hostname str))))

;;; ----------------------------------------------------------------------------
;;;  parse-ip - Parse a number in an IP address
;;; ----------------------------------------------------------------------------
;;;
;;; Parameters:
;;;   str - String containing IP address
;;;   pos - Starting position to parse from
;;;
;;; Returns:
;;;   Multiple values:
;;;   1. Number value if valid (0-255), nil otherwise  
;;;   2. Position after the number
;;;
;;; Delegates to parse-ip-helper for actual parsing

(defun parse-ip (str pos)
  (parse-ip-helper str pos 0 nil))

;;; ----------------------------------------------------------------------------
;;;  parse-ip-helper - Helper function to parse IP address numbers
;;; ----------------------------------------------------------------------------
;;; Parameters:
;;;   str - String containing IP address
;;;   pos - Current position in string
;;;   acc - Accumulated value of current number
;;;   seen-digit - Flag indicating if at least one digit was seen
;;;
;;; Returns:
;;;   Multiple values:
;;;   1. Number value if valid (0-255), nil otherwise
;;;   2. Position after the number
;;;
;;; A valid IP number must:
;;; - Have at least one digit
;;; - Be between 0 and 255
;;; - Contain only digits

(defun parse-ip-helper (str pos acc seen-digit)
  (if (>= pos (length str))
      (when (and seen-digit (<= 0 acc 255))
        (values acc pos))
      (let ((c (char str pos)))
        (if (valid-digit c)
            (parse-ip-helper
             str
             (1+ pos)
             (+ (* 10 acc) (- (char-code c) (char-code #\0)))
             t)
            (when (and seen-digit (<= 0 acc 255))
              (values acc pos))))))

;;; ----------------------------------------------------------------------------
;;; valid-ip-helper - Helper function to validate IP address format
;;; ----------------------------------------------------------------------------
;;;
;;; Parameters:
;;;   str - String containing IP address
;;;   pos - Current position in string
;;;   parts - Number of IP address parts processed so far
;;;
;;; Returns:
;;;   T if string is a valid IP address, NIL otherwise
;;;
;;; A valid IP address must:
;;; - Have exactly 4 parts
;;; - Each part must be 0-255
;;; - Parts must be separated by dots
;;; - No trailing or leading dots
;;; - No empty parts

(defun valid-ip-helper (str pos parts)
  (if (= parts 4)
      (= pos (length str))
      (multiple-value-bind (num next-pos)
          (parse-ip str pos)
        (when num
          (if (>= next-pos (length str))
              (= parts 3)
              (when (char= (char str next-pos) #\.)
                (valid-ip-helper str (1+ next-pos) (1+ parts))))))))

;;; ----------------------------------------------------------------------------
;;; valid-ip - Helper function to validate IP address format
;;; ----------------------------------------------------------------------------
;;; Valid IP addresses must:
;;;   - Contain exactly 4 octets separated by dots
;;;   - Each octet must be a number between 0-255
;;;   - No leading zeros allowed
;;;
;;; Returns:
;;;   T if valid IP address format, NIL otherwise
;;; Info:
;;;   Uses valid-ip-helper for recursive validation

(defun valid-ip (str)
  (valid-ip-helper str 0 0))


;;; ----------------------------------------------------------------------------
;;; parse-p-q-f - Parse path, query and fragment components of a URI
;;; ----------------------------------------------------------------------------
;;;
;;; Parameters:
;;;   str - String containing path, query and fragment components
;;;
;;; Returns:
;;;   Multiple values:
;;;   1. Path component (with leading / removed)
;;;   2. Query component (nil if not present)  
;;;   3. Fragment component (nil if not present)
;;;
;;; Signals:
;;;   ERROR if:
;;;   - @ symbol present without authority section
;;;   - Path is just "/"
;;;   - Query present but empty
;;;   - Fragment present but empty
;;;   - Path format invalid

(defun parse-p-q-f (str)
  (if (or (null str) (string= str ""))
      (values nil nil nil)
      (progn
        ;; Check for @ without authority
        (when (and (position #\@ str)
                   (not (position #\/ str :end (position #\@ str))))
          (error "@ without authority"))
	
        ;; Check for empty path
        (when (string= str "/")
          (error "Empty path"))
	
        ;; Split into components and validate
        (multiple-value-bind (path-part query fragment)
            (split-q-f str)
          (when (and query (zerop (length query)))
            (error "Empty query"))
          (when (and fragment (zerop (length fragment)))
            (error "Empty fragment"))
          (unless (valide-path path-part)
            (error "Invalid path"))
          ;; Return components with leading / removed from path
          (values
           (if (and (> (length path-part) 0)
                    (char= (char path-part 0) #\/))
               (subseq path-part 1)
               path-part)
           query
           fragment)))))                  

;;; valide-path - Validate path component of a URI
;;;
;;; Parameters:
;;;   str - The path string to validate
;;;
;;; Returns:
;;;   T if path is valid, NIL otherwise
;;;
;;; Valid path must:
;;; - Accept empty string
;;; - Not end with a forward slash
;;; - Contain only letters, digits, and forward slashes

(defun valide-path(str)
  (or (string= str "")
      (and (not (char= (char str (1- (length str))) #\/)) 
           (every (lambda (c)
                    (or (valid-char c)
                        (valid-digit c)
                        (char= c #\/)))
                  str))))


;;; ----------------------------------------------------------------------------
;;; valid-user - Validate userinfo string format
;;; ----------------------------------------------------------------------------
;;;
;;; Returns:
;;;   T if string is valid userinfo, NIL otherwise
;;;
;;; Valid userinfo must:
;;; - Not be empty
;;; - Contain only letters, digits, and special chars (_, =, +, -)

(defun valid-user (str)
  (and (> (length str) 0)
       (every (lambda (c)
                (or (valid-char c)
                    (valid-digit c)
                    (valid-special c)))
              str)))



;;; ----------------------------------------------------------------------------
;;; UTILITIES
;;; ----------------------------------------------------------------------------

(defun valid-char (char)
  (and (characterp char)
       (or (char<= #\A char #\Z)
           (char<= #\a char #\z))))

(defun valid-digit (char)
  (and (characterp char)
       (char<= #\0 char #\9)))

(defun valid-special (char)
  (member char '(#\_ #\= #\+ #\-)))

(defun valid-alpha (char)
  (or (valid-char char)
      (valid-digit char)
      (valid-special char)))




