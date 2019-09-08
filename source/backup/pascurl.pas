unit pascurl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Unixtype, Sockets;

type
  CURL = type Pointer;
  CURLSH = type Pointer;
  curl_socket_t = type Integer;
  curl_off_t = type Longint;

  (* linked-list structure for the CURLOPT_QUOTE option (and other) *)
  curl_slist = record
    data : PChar;
    next : ^curl_slist;
  end;

  curl_httppost = record
    next : ^curl_httppost;      (* next entry in the list *)
    name : PChar;               (* pointer to allocated name *)
    namelength : Longint;       (* length of name length *)
    contents : PChar;           (* pointer to allocated data contents *)
    contentslength : Longint;   (* length of contents field, see also
                                   CURL_HTTPPOST_LARGE *)
    buffer : PChar;             (* pointer to allocated buffer contents *)
    bufferLength : Longint;     (* length of buffer field *)
    contenttype : PChar;        (* Content-Type *)
    contentheader : ^curl_slist;(* list of extra headers for this form *)
    more : ^curl_httppost;      (* if one field name has more than one
                                   file, this link should link to following
                                   files *)
    flags : Longint;            (* as defined below *)
    showfilename : PChar;       (* The file name to show. If not set, the
                                   actual file name will be used (if this
                                   is a file part) *)
    userp : Pointer;            (* custom pointer used for
                                   HTTPPOST_CALLBACK posts *)
    contentlen : curl_off_t;    (* alternative length of contents
                                   field. Used if CURL_HTTPPOST_LARGE is
                                   set. Added in 7.46.0 *)
  end;

const
  (* specified content is a file name *)
  CURL_HTTPPOST_FILENAME = 1 shl 0;
  (* specified content is a file name *)
  CURL_HTTPPOST_READFILE = 1 shl 1;
  (* name is only stored pointer do not free in formfree *)
  CURL_HTTPPOST_PTRNAME = 1 shl 2;
  (* contents is only stored pointer do not free in formfree *)
  CURL_HTTPPOST_PTRCONTENTS = 1 shl 3;
  (* upload file from buffer *)
  CURL_HTTPPOST_BUFFER = 1 shl 4;
  (* upload file from pointer contents *)
  CURL_HTTPPOST_PTRBUFFER = 1 shl 5;
  (* upload file contents by using the regular read callback to get the data and
     pass the given pointer as custom pointer *)
  CURL_HTTPPOST_CALLBACK = 1 shl 6;
  (* use size in 'contentlen', added in 7.46.0 *)
  CURL_HTTPPOST_LARGE = 1 shl 7;

type
  (* enum for the different supported SSL backends *)
  curl_sslbackend = (
    CURLSSLBACKEND_NONE = 0,
    CURLSSLBACKEND_OPENSSL = 1,
    CURLSSLBACKEND_GNUTLS = 2,
    CURLSSLBACKEND_NSS = 3,
    CURLSSLBACKEND_OBSOLETE4 = 4,
    CURLSSLBACKEND_GSKIT = 5,
    CURLSSLBACKEND_POLARSSL = 6,
    CURLSSLBACKEND_WOLFSSL = 7,
    CURLSSLBACKEND_SCHANNEL = 8,
    CURLSSLBACKEND_DARWINSSL = 9,
    CURLSSLBACKEND_AXTLS = 10,
    CURLSSLBACKEND_MBEDTLS = 11,

    (* aliases for library clones and renames *)
    CURLSSLBACKEND_LIBRESSL = CURLSSLBACKEND_OPENSSL,
    CURLSSLBACKEND_BORINGSSL = CURLSSLBACKEND_OPENSSL,
    CURLSSLBACKEND_CYASSL = CURLSSLBACKEND_WOLFSSL
  );

  (* This is the CURLOPT_PROGRESSFUNCTION callback proto. It is now considered
     deprecated but was the only choice up until 7.31.0 *)
  curl_progress_callback = function (clientp : Pointer; dltotal : Double;
    dlnow : Double; ultotal : Double; ulnow : Double) : Interger of object;

  (* This is the CURLOPT_XFERINFOFUNCTION callback proto. It was introduced in
     7.32.0, it avoids floating point and provides more detailed information. *)
  curl_xferinfo_callback = function (clientp : Pointer; dltotal : curl_off_t;
    dlnow : curl_off_t; ultotal : curl_off_t; ulnow : curl_off_t) : Integer
    of object;

  curl_write_callback = function (buffer : PChar; size : LongWord; nitems :
    LongWord; outstream : Pointer) : LongWord of object;

  (* enumeration of file types *)
  curlfiletype = (
    CURLFILETYPE_FILE = 0,
    CURLFILETYPE_DIRECTORY,
    CURLFILETYPE_SYMLINK,
    CURLFILETYPE_DEVICE_BLOCK,
    CURLFILETYPE_DEVICE_CHAR,
    CURLFILETYPE_NAMEDPIPE,
    CURLFILETYPE_SOCKET,
    CURLFILETYPE_DOOR, (* is possible only on Sun Solaris now *)
    CURLFILETYPE_UNKNOWN (* should never occur *)
  );

const
  CURLFINFOFLAG_KNOWN_FILENAME = 1 shl 0;
  CURLFINFOFLAG_KNOWN_FILETYPE = 1 shl 1;
  CURLFINFOFLAG_KNOWN_TIME = 1 shl 2;
  CURLFINFOFLAG_KNOWN_PERM = 1 shl 3;
  CURLFINFOFLAG_KNOWN_UID = 1 shl 4;
  CURLFINFOFLAG_KNOWN_GID = 1 shl 5;
  CURLFINFOFLAG_KNOWN_SIZE = 1 shl 6;
  CURLFINFOFLAG_KNOWN_HLINKCOUNT = 1 shl 7;

type
  (* Content of this structure depends on information which is known and is
   achievable (e.g. by FTP LIST parsing). Please see the url_easy_setopt(3) man
   page for callbacks returning this structure -- some fields are mandatory,
   some others are optional. The FLAG field has special meaning. *)
  curl_fileinfo = record
    filename : PChar;
    filetype : curlfiletype;
    time : time_t;
    perm : Cardinal;
    uid : Integer;
    gid : Integer;
    size : curl_off_t;
    hardlinks : Longint;
    strings : record
      (* If some of these fields is not NULL, it is a pointer to b_data. *)
      time : PChar;
      perm : PChar;
      user : PChar;
      group : PChar;
      target : PChar; (* pointer to the target filename of a symlink *)
    end;
    flags : Cardinal;
    (* used internally *)
    b_data : PChar;
    b_size : LongWord;
    b_used : LongWord;
  end;

const
  (* return codes for CURLOPT_CHUNK_BGN_FUNCTION *)
  CURL_CHUNK_BGN_FUNC_OK = 0;
  CURL_CHUNK_BGN_FUNC_FAIL = 1; (* tell the lib to end the task *)
  CURL_CHUNK_BGN_FUNC_SKIP = 2; (* skip this chunk over *)

type
  (* if splitting of data transfer is enabled, this callback is called before
   download of an individual chunk started. Note that parameter "remains" works
   only for FTP wildcard downloading (for now), otherwise is not used *)
  curl_chunk_bgn_callback = function (const transfer_info : Pointer;
    ptr : Pointer; remains : Integer) : Longint of object;

const
  (* return codes for CURLOPT_CHUNK_END_FUNCTION *)
  CURL_CHUNK_END_FUNC_OK = 0;
  CURL_CHUNK_END_FUNC_FAIL = 1; (* tell the lib to end the task *)

type
  (* If splitting of data transfer is enabled this callback is called after
   download of an individual chunk finished.
   Note! After this callback was set then it have to be called FOR ALL chunks.
   Even if downloading of this chunk was skipped in CHUNK_BGN_FUNC.
   This is the reason why we don't need "transfer_info" parameter in this
   callback and we are not interested in "remains" parameter too. *)
  curl_chunk_end_callback = function (ptr : Pointer) : Longint of object;

const
  (* return codes for FNMATCHFUNCTION *)
  CURL_FNMATCHFUNC_MATCH = 0; (* string corresponds to the pattern *)
  CURL_FNMATCHFUNC_NOMATCH = 1; (* pattern doesn't match the string *)
  CURL_FNMATCHFUNC_FAIL = 2; (* an error occurred *)

type
  (* callback type for wildcard downloading pattern matching. If the
   string matches the pattern, return CURL_FNMATCHFUNC_MATCH value, etc. *)
  curl_fnmatch_callback = function (ptr : Pointer; const pattern : PChar;
    const str : PChar) : Integer of object;

const
  (* These are the return codes for the seek callbacks *)
  CURL_SEEKFUNC_OK = 0;
  CURL_SEEKFUNC_FAIL = 1; (* fail the entire transfer *)
  CURL_SEEKFUNC_CANTSEEK = 2; (* tell libcurl seeking can't be done, so
                                 libcurl might try other means instead *)

type
  curl_seek_callback = function (instream : Pointer; offset : curl_off_t;
    origin : Integer) : Integer of object;

const
  (* This is a return code for the read callback that, when returned, will
   signal libcurl to immediately abort the current transfer. *)
  CURL_READFUNC_ABORT = 0x10000000;
  (* This is a return code for the read callback that, when returned, will
   signal libcurl to pause sending data on the current transfer. *)
  CURL_READFUNC_PAUSE = 0x10000001;

type
  curl_read_callback = function (buffer : PChar; size : LongWord;
    nitems : LongWord; instream : Pointer) : LongWord of object;

  curlsocktype = (
    CURLSOCKTYPE_IPCXN, (* socket created for a specific IP connection *)
    CURLSOCKTYPE_ACCEPT, (* socket created by accept() call *)
    CURLSOCKTYPE_LAST (* never use *)
  );

const
  (* The return code from the sockopt_callback can signal information back
   to libcurl: *)
  CURL_SOCKOPT_OK = 0;
  CURL_SOCKOPT_ERROR = 1; (* causes libcurl to abort and return
                             CURLE_ABORTED_BY_CALLBACK *)
  CURL_SOCKOPT_ALREADY_CONNECTED = 2;

type
  curl_sockopt_callback = function (clientp : Pointer; curlfd : curl_socket_t;
    purpose : curlsocktype) : Integer of object;

  pcurl_sockaddr = ^curl_sockaddr;
  curl_sockaddr = record
    family : Integer;
    socktype : Integer;
    protocol : Integer;
    addrlen : Cardinal; (* addrlen was a socklen_t type before 7.18.0 but it
                           turned really ugly and painful on the systems that
                           lack this type *)
    addr : sockaddr;
  end;

  curl_opensocket_callback = function (clientp : Pointer;
    purpose : curlsocktype; address : pcurl_sockaddr) : curl_socket_t of object;

  curl_closesocket_callback = function (clientp : Pointer;
    item : curl_socket_t) : Integer of object;
const
  CURL_SOCKET_BAD = -1;

  (* The maximum receive buffer size configurable via CURLOPT_BUFFERSIZE. *)
  CURL_MAX_READ_SIZE = 524288;

  (* Tests have proven that 20K is a very bad buffer size for uploads on
     Windows, while 16K for some odd reason performed a lot better.
     We do the ifndef check to allow this value to easier be changed at build
     time for those who feel adventurous. The practical minimum is about
     400 bytes since libcurl uses a buffer of this size as a scratch area
     (unrelated to network send operations). *)
  CURL_MAX_WRITE_SIZE = 16384;

  (* The only reason to have a max limit for this is to avoid the risk of a bad
   server feeding libcurl with a never-ending header that will cause reallocs
   infinitely *)
  CURL_MAX_HTTP_HEADER = (100 * 1024);

  (* This is a magic return code for the write callback that, when returned,
   will signal libcurl to pause receiving on the current transfer. *)
   CURL_WRITEFUNC_PAUSE = 0x10000001;

implementation

end.

