(******************************************************************************)
(*                                 libPasCURL                                 *)
(*                 object pascal wrapper around cURL library                  *)
(*                        https://github.com/curl/curl                        *)
(*                                                                            *)
(* Copyright (c) 2019                                       Ivan Semenkov     *)
(* https://github.com/isemenkov/libpascurl                  ivan@semenkov.pro *)
(*                                                          Ukraine           *)
(******************************************************************************)
(*                                                                            *)
(* Module:          Unit 'pascurl'                                            *)
(* Functionality:   Provides  TSession class  which present  cURL session  to *)
(*                  assign  request  params.  And TResponse  class to getting *)
(*                  information from server response.                         *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(* This source  is free software;  you can redistribute  it and/or modify  it *)
(* under the terms of the GNU General Public License as published by the Free *)
(* Software Foundation; either version 3 of the License.                      *)
(*                                                                            *)
(* This code is distributed in the  hope that it will  be useful, but WITHOUT *)
(* ANY  WARRANTY;  without even  the implied  warranty of MERCHANTABILITY  or *)
(* FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License for *)
(* more details.                                                              *)
(*                                                                            *)
(* A copy  of the  GNU General Public License is available  on the World Wide *)
(* Web at <http://www.gnu.org/copyleft/gpl.html>. You  can also obtain  it by *)
(* writing to the Free Software Foundation, Inc., 51  Franklin Street - Fifth *)
(* Floor, Boston, MA 02110-1335, USA.                                         *)
(*                                                                            *)
(******************************************************************************)

unit pascurl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libpascurl, math, typinfo;

type
  { TTimeInterval }

  TTimeInterval = class
  public
    type
      TMicrosecondRange = 0 .. 999;
      TMillisecondRange = 0 .. 999;
      TSecondRange = 0 .. 59;
      TMinuteRange = 0 .. 59;
      THourRange = type QWord;

      { TMicrosecond }

      TMicrosecond = class
      private
        FMicroseconds : TMicrosecondRange;
      public
        constructor Create;
        constructor Create (AInterval : TMicrosecondRange);
        constructor Create (AInterval : TMicrosecond);
        destructor Destroy; override;

        property Value : TMicrosecondRange read FMicroseconds
          write FMicroseconds;
      end;

      { TMillisecond }

      TMillisecond = class
      private
        FMilliseconds : TMillisecondRange;
      public
        constructor Create;
        constructor Create (AInterval : TMillisecondRange);
        constructor Create (AInterval : TMillisecond);
        destructor Destroy; override;

        property Value : TMillisecondRange read FMilliseconds
          write FMilliseconds;
      end;

      { TSecond }

      TSecond = class
      private
        FSeconds : TSecondRange;
      public
        constructor Create;
        constructor Create (AInterval : TSecondRange);
        constructor Create (AInterval : TSecond);
        destructor Destroy; override;

        property Value : TSecondRange read FSeconds write FSeconds;
      end;

      { TMinute }

      TMinute = class
      private
        FMinutes : TMinuteRange;
      public
        constructor Create;
        constructor Create (AInterval : TMinuteRange);
        constructor Create (AInterval : TMinute);
        destructor Destroy; override;

        property Value : TMinuteRange read FMinutes write FMinutes;
      end;

      { THour }

      THour = class
      private
        FHours : THourRange;
      public
        constructor Create;
        constructor Create (AInterval : THourRange);
        constructor Create (AInterval : THour);
        destructor Destroy; override;

        property Value : QWord read FHours write FHours;
      end;
  private
    FMicroseconds : TMicrosecond;
    FMilliseconds : TMillisecond;
    FSeconds : TSecond;
    FMinutes : TMinute;
    FHours : THour;

    function GetMicroseconds : QWord; inline;
    procedure SetMicroseconds (AValue : Qword); inline;
    function GetMilliseconds : QWord; inline;
    procedure SetMilliseconds (AValue : QWord); inline;
    function GetSeconds : QWord; inline;
    procedure SetSeconds (AValue : QWord); inline;
    function GetMinutes : QWord; inline;
    procedure SetMinutes (AValue : QWord); inline;
    function GetHours : QWord; inline;
    procedure SetHours (AValue : QWord); inline;
  public
    constructor Create;
    destructor Destroy; override;

    function ToMicroseconds : QWord; inline;
    function ToMilliseconds : QWord; inline;
    function ToSeconds : QWord; inline;
    function ToMinutes : QWord; inline;
    function ToHours : QWord; inline;
    function {%H-}ToString(ASuffix : string = '') : string; inline;

    property Microseconds : QWord read GetMicroseconds write SetMicroseconds;
    property us : QWord read GetMicroseconds write SetMicroseconds;
    property Milliseconds : QWord read GetMilliseconds write SetMilliseconds;
    property ms : QWord read GetMilliseconds write SetMilliseconds;
    property Seconds : QWord read GetSeconds write SetSeconds;
    property s : QWord read GetSeconds write SetSeconds;
    property Minutes : QWord read GetMinutes write SetMinutes;
    property m : QWord read GetMinutes write SetMinutes;
    property Hours : QWord read GetHours write SetHours;
    property h : QWord read GetHours write SetHours;
  end;

  { TDataSize }

  TDataSize = class
  public
    type
      TByteRange = 0 .. 1023;
      TKilobyteRange = 0 .. 1023;
      TMegabyteRange = 0 .. 1023;
      TGigabyteRange = type QWord;

      { TByte }

      TByte = class
      private
        FBytes : TByteRange;
      public
        constructor Create;
        constructor Create (ASize : TByteRange);
        constructor Create (ASize : TByte);
        destructor Destroy; override;

        property Value : TByteRange read FBytes write FBytes;
      end;

      { TKilobyte }

      TKilobyte = class
      private
        FKilobytes : TKilobyteRange;
      public
        constructor Create;
        constructor Create (ASize : TKilobyteRange);
        constructor Create (ASize : TKilobyte);
        destructor Destroy; override;

        property Value : TKilobyteRange read FKilobytes write FKilobytes;
      end;

      { TMegabyte }

      TMegabyte = class
      private
        FMegabytes : TMegabyteRange;
      public
        constructor Create;
        constructor Create (ASize : TMegabyteRange);
        constructor Create (ASize : TMegabyte);
        destructor Destroy; override;

        property Value : TMegabyteRange read FMegabytes write FMegabytes;
      end;

      { TGigabyte }

      TGigabyte = class
      private
        FGigabytes : TGigabyteRange;
      public
        constructor Create;
        constructor Create (ASize : TGigabyteRange);
        constructor Create (ASize : TGigabyte);
        destructor Destroy; override;

        property Value : TGigabyteRange read FGigabytes write FGigabytes;
      end;
  private
    FBytes : TByte;
    FKilobytes : TKilobyte;
    FMegabytes : TMegabyte;
    FGigabytes : TGigabyte;

    function GetBytes : QWord; inline;
    procedure SetBytes (ASize : QWord); inline;
    function GetKilobytes : QWord; inline;
    procedure SetKilobytes (ASize : QWord); inline;
    function GetMegabytes : QWord; inline;
    procedure SetMegabytes (ASize : QWord); inline;
    function GetGigabytes : QWord; inline;
    procedure SetGigabytes (ASize : QWord); inline;
  public
    constructor Create;
    destructor Destroy; override;

    function ToBytes : QWord; inline;
    function ToKilobytes : QWord; inline;
    function ToMegabytes : QWord; inline;
    function ToGigabytes : QWord; inline;
    function {%H-}ToString (ASuffix : string = '') : string; inline;

    property Bytes : QWord read GetBytes write SetBytes;
    property b : QWord read GetBytes write SetBytes;
    property Kilobytes : QWord read GetKilobytes write SetKilobytes;
    property KiB : QWord read GetKilobytes write SetKilobytes;
    property Megabytes : QWord read GetMegabytes write SetMegabytes;
    property MiB : QWord read GetMegabytes write SetMegabytes;
    property Gigabytes : QWord read GetGigabytes write SetGigabytes;
    property GiB : QWord read GetGigabytes write SetGigabytes;
  end;

  { TSession }
  { Present cURL session to assign request params }

  TSession = class
  public
    type
     (**
      * Callback for writting received data
      *)
      TDownloadFunction = function (buffer : PChar; size : LongWord) : LongWord
        of object;

     (**
      * Callback for data uploads
      *)
      TUploadFunction = function (buffer : PChar; size : LongWord) : LongWord
        of object;

      { TLinkedList }

      TLinkedList = class
      private
        FList : pcurl_slist;
      public
        constructor Create;
        destructor Destroy; override;

        procedure Append (AString : string);
      end;

      { TOptionsProperty }

      TOptionsProperty = class
      public
        type
          (**
           * Allows an application to select what kind of IP addresses to use
           * when resolving host names. This is only interesting when using host
           * names that resolve addresses using more than one version of IP.
           *)
          TIPResolve = (
            (**
             * Default, resolves addresses to all IP versions that your system
             * allows.
             *)
            IPRESOLVE_WHATEVER            = Longint(CURL_IPRESOLVE_WHATEVER),

            (**
             * Resolve to IPv4 addresses.
             *)
            IPRESOLVE_V4                        = Longint(CURL_IPRESOLVE_V4),

            (**
             * Resolve to IPv6 addresses.
             *)
            IPRESOLVE_V6                        = Longint(CURL_IPRESOLVE_V6)
          );
      private
        FHandle : CURL;

        procedure SetNoSignal (AEnable : Boolean);
        procedure SetAddressScope (AScope : Longint);
        procedure SetInterface (AInterface : string);
        procedure SetUnixSocketPath (APath : string);
        procedure SetAbstractUnixSocketPath (APath : string);
        procedure SetBufferSize (ASize : TDataSize);
        procedure SetFailOnError (AFailOnError : Boolean);
        procedure SetPathAsIs (ALeaveIt : Boolean);
        procedure SetConvertCRLF (AEnable : Boolean);
        procedure SetUpkeepInterval(ATime: TTimeInterval);
        procedure SetUploadFileSize (ASize : curl_off_t);
        procedure SetUploadBufferSize (ASize : TDataSize);
        procedure SetTimeout (ATime : TTimeInterval);
        procedure SetLowSpeedLimit (ASize : TDataSize);
        procedure SetLowSpeedTime (ATime : TTimeInterval);
        procedure SetMaxUploadSpeed (ASize : TDataSize);
        procedure SetMaxDownloadSpeed (ASize : TDataSize);
        procedure SetMaxConnections (AConn : Longint);
        procedure SetForceReuseConnection (AEnable : Boolean);
        procedure SetCloseConnectionAfterUse (AEnable : Boolean);
        procedure SetMaxReuseConnectionTime (ATime : TTimeInterval);
        procedure SetConnectionTimeout (ATime : TTimeInterval);
        procedure SetIPResolve (AResolve : TIPResolve);
        procedure SetHappyEyeballsTimeout (ATime : TTimeInterval);
        procedure SetPrivateData (AData : Pointer);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

       (**
        * Skip all signal handling
        *
        * If it is TRUE, libcurl will not use any functions that install signal
        * handlers or any functions that cause signals to be sent to the
        * process. This option is here to allow multi-threaded unix applications
        * to still set/use all timeout options etc, without risking getting
        * signals.
        * If this option is set and libcurl has been built with the standard
        * name resolver, timeouts will not occur while the name resolve takes
        * place. Consider building libcurl with the c-ares or threaded resolver
        * backends to enable asynchronous DNS lookups, to enable timeouts for
        * name resolves without the use of signals.
        * Setting NoSignal to TRUE makes libcurl NOT ask the system to ignore
        * SIGPIPE signals, which otherwise are sent by the system when trying to
        * send data to a socket which is closed in the other end. libcurl makes
        * an effort to never cause such SIGPIPEs to trigger, but some operating
        * systems have no way to avoid them and even on those that have there
        * are some corner cases when they may still happen, contrary to our
        * desire. In addition, using CURLAUTH_NTLM_WB authentication could cause
        * a SIGCHLD signal to be raised.
        *)
        property NoSignal : Boolean write SetNoSignal default False;

       (**
        * Set scope id for IPv6 addresses
        *
        * Pass a long specifying the scope id value to use when connecting to
        * IPv6 addresses.
        *)
        property AddressScope : Longint write SetAddressScope default 0;

       (**
        * Source interface for outgoing trafic
        *
        * This sets the interface name to use as outgoing network interface.
        * The name can be an interface name, an IP address, or a host name.
        * If the parameter starts with "if!" then it is treated as only as
        * interface name and no attempt will ever be named to do treat it as an
        * IP address or to do name resolution on it. If the parameter starts
        * with "host!" it is treated as either an IP address or a hostname.
        * Hostnames are resolved synchronously. Using the if! format is highly
        * recommended when using the multi interfaces to avoid allowing the
        * code to block.
        *)
        property InterfaceName : string write SetInterface;

       (**
        * Set UNIX domain socket
        *
        * Enables the use of Unix domain sockets as connection endpoint and
        * sets the path to path. If path is '' (empty string), then Unix domain
        * sockets are disabled.
        * When enabled, curl will connect to the Unix domain socket instead of
        * establishing a TCP connection to a host. Since no TCP connection is
        * created, curl does not need to resolve the DNS hostname in the URL.
        * The maximum path length on Cygwin, Linux and Solaris is 107. On other
        * platforms it might be even less.
        *)
        property UnixSocketPath : string write SetUnixSocketPath;

       (**
        * Set an abstract Unix domain socket
        *
        * Enables the use of an abstract Unix domain socket instead of
        * establishing a TCP connection to a host.
        *)
        property AbstractUnixSocketPath : string
          write SetAbstractUnixSocketPath;

       (**
        * Set preffered receive buffer size
        *
        * Specifying your preferred size for the receive buffer in libcurl. The
        * main point of this would be that the write callback gets called more
        * often and with smaller chunks. Secondly, for some protocols, there's
        * a benefit of having a larger buffer for performance.
        * The minimum buffer size allowed to be set is 1024 bytes.
        *)
        property BufferSize : TDataSize write SetBufferSize;

       (**
        * Request failure on HTTP response >= 400
        *
        * Tells the library to fail the request if the HTTP code returned is
        * equal to or larger than 400. The default action would be to return
        * the page normally, ignoring that code.
        *)
        property FailOnError : Boolean write SetFailOnError default False;

       (**
        * Do not handle dot dot sequences
        *
        * Tell libcurl to not alter the given path before passing it on to the
        * server.
        * This instructs libcurl to NOT squash sequences of "/../" or "/./"
        * that may exist in the URL's path part and that is supposed to be
        * removed according to RFC 3986 section 5.2.4.
        * Some server implementations are known to (erroneously) require the
        * dot dot sequences to remain in the path and some clients want to pass
        * these on in order to try out server implementations.
        * By default libcurl will merge such sequences before using the path.
        *)
        property PathAsIs : Boolean write SetPathAsIs default False;

       (**
        * Enable/disable CRLF conversion
        *
        * If the value is set to True, libcurl converts Unix newlines to CRLF
        * newlines on transfers. Disable this option again by setting the value
        * to False.
        *)
        property ConvertCRLF : Boolean write SetConvertCRLF default False;

        (**
         * Set size of the input file to send off
         *
         * When uploading a file to a remote site, filesize should be used to
         * tell libcurl what the expected size of the input file is.
         * To unset this value again, set it to -1.
         * When sending emails using SMTP, this command can be used to specify
         * the optional SIZE parameter for the MAIL FROM command.
         * This option does not limit how much data libcurl will actually send,
         * as that is controlled entirely by what the read callback returns, but
         * telling one value and sending a different amount may lead to errors.
         *)
        property UploadFileSize : curl_off_t write SetUploadFileSize;

        (**
         * Set preferred upload buffer size
         *
         * Pass a long specifying your preferred size (in bytes) for the upload
         * buffer in libcurl. It makes libcurl uses a larger buffer that gets
         * passed to the next layer in the stack to get sent off. In some setups
         * and for some protocols, there's a huge performance benefit of having
         * a larger upload buffer.
         * This is just treated as a request, not an order. You cannot be
         * guaranteed to actually get the given size.
         * The upload buffer size is by default 64 kilobytes. The maximum buffer
         * size allowed to be set is 2 megabytes. The minimum buffer size
         * allowed to be set is 16 kilobytes.
         * Since curl 7.61.1 the upload buffer is allocated on-demand - so if
         * the handle isn't used for upload, this buffer will not be allocated
         * at all.
         *)
        property UploadBufferSize : TDataSize write SetUploadBufferSize;

        (**
         * Set maximum time the request is allowed to take
         *
         * Timeout - the maximum time that you allow the libcurl transfer
         * operation to take. Normally, name lookups can take a considerable
         * time and limiting operations risk aborting perfectly normal
         * operations. This option may cause libcurl to use the SIGALRM signal
         * to timeout system calls.
         *)
        property Timeout : TTimeInterval write SetTimeout;

        (**
         * Set low speed limit in bytes per second
         *
         * It contains the average transfer speed per second that the
         * transfer should be below during LowSpeedTime for libcurl to consider
         * it to be too slow and abort.
         *)
        property LowSpeedLimit : TDataSize write SetLowSpeedLimit;

        (**
         * Set low speed limit time period
         *
         * It contains the time in number seconds that the transfer speed should
         * be below the LowSpeedLimit for the library to consider it too slow
         * and abort.
         *)
        property LowSpeedTime : TTimeInterval write SetLowSpeedTime;

        (**
         * Rate limit data upload speed
         *
         *  If an upload exceeds this speed the transfer will pause to keep the
         * speed less than or equal to the parameter value. Defaults to
         * unlimited speed.
         * This option doesn't affect transfer speeds done with FILE:// URLs.
         *)
        property MaxUploadSpeed : TDataSize write SetMaxUploadSpeed;

        (**
         * Rate limit data download speed
         *
         * If a download exceeds this speed the transfer will pause to keep the
         * speed less than or equal to the parameter value. Defaults to
         * unlimited speed.
         * This option doesn't affect transfer speeds done with FILE:// URLs.
         *)
        property MaxDownloadSpeed : TDataSize write SetMaxDownloadSpeed;

        (**
         * Maximum connection cache size
         *
         * The set amount will be the maximum number of simultaneously open
         * persistent connections that libcurl may cache in the pool associated
         * with this handle. The default is 5, and there isn't much point in
         * changing this value unless you are perfectly aware of how this works
         * and changes libcurl's behaviour. This concerns connections using any
         * of the protocols that support persistent connections.
         * When reaching the maximum limit, curl closes the oldest one in the
         * cache to prevent increasing the number of open connections.
         * If you already have performed transfers with this curl handle,
         * setting a smaller MaxConnections than before may cause open
         * connections to get closed unnecessarily.
         *)
        property MaxConnections : Longint write SetMaxConnections default 5;

        (**
         * Force a new connection to be use
         *
         * Set to False to make the next transfer use a new (fresh) connection by
         * force instead of trying to re-use an existing one. This option should
         * be used with caution and only if you understand what it does as it
         * may seriously impact performance.
         * Related functionality is CURLOPT_FORBID_REUSE which makes sure the
         * connection is closed after use so that it won't be re-used.
         * Set fresh to True to have libcurl attempt re-using an existing
         * connection (default behavior).
         *)
        property ForceReuseConnection : Boolean write SetForceReuseConnection
          default True;

        (**
         * Make connection get closed at once after one
         *
         * Set close to True to make libcurl explicitly close the connection
         * when done with the transfer. Normally, libcurl keeps all connections
         * alive when done with one transfer in case a succeeding one follows
         * that can re-use them. This option should be used with caution and
         * only if you understand what it does as it can seriously impact
         * performance.
         * Set to False to have libcurl keep the connection open for possible
         * later re-use (default behavior).
         *)
        property CloseConnectionAfterUse : Boolean
          write SetCloseConnectionAfterUse default False;

        (**
         * Max idle time allowed for reusing a connection
         *
         * Pass a parameter containing maxage - the maximum time that you allow
         * an existing connection to have to be considered for reuse for this
         * request.
         * The "connection cache" that holds previously used connections. When a
         * new request is to be done, it will consider any connection that
         * matches for reuse. The CURLOPT_MAXAGE_CONN limit prevents libcurl
         * from trying very old connections for reuse, since old connections
         * have a high risk of not working and thus trying them is a performance
         * loss and sometimes service loss due to the difficulties to figure out
         * the situation. If a connection is found in the cache that is older
         * than this set maxage, it will instead be closed.
         *)
        property MaxReuseConnectionTime : TTimeInterval
          write SetMaxReuseConnectionTime;

        (**
         * Timeout for the connect phase
         *
         * It should contain the maximum time that you allow the connection
         * phase to the server to take. This only limits the connection phase,
         * it has no impact once it has connected. Set to zero to switch to the
         * default built-in connection timeout - 300 seconds. See also the
         * CURLOPT_TIMEOUT option.
         * In unix-like systems, this might cause signals to be used unless
         * CURLOPT_NOSIGNAL is set.
         *)
        property ConnectionTimeout : TTimeInterval write SetConnectionTimeout;

        (**
         * Specify which IP protocol version to use
         *
         * Allows an application to select what kind of IP addresses to use when
         * resolving host names. This is only interesting when using host names
         * that resolve addresses using more than one version of IP.
         *)
        property IPResolve : TIPResolve write SetIPResolve
          default IPRESOLVE_WHATEVER;

        (**
         * Head start for IPv6 for happy eyeballs
         *
         * Happy eyeballs is an algorithm that attempts to connect to both IPv4
         * and IPv6 addresses for dual-stack hosts, preferring IPv6 first for
         * timeout. If the IPv6 address cannot be connected to within that time
         * then a connection attempt is made to the IPv4 address in parallel.
         * The first connection to be established is the one that is used.
         * The range of suggested useful values for timeout is limited. Happy
         * Eyeballs RFC 6555 says "It is RECOMMENDED that connection attempts be
         * paced 150-250 ms apart to balance human factors against network
         * load." libcurl currently defaults to 200 ms. Firefox and Chrome
         * currently default to 300 ms.
         *)
        property HappyEyeballsTimeout : TTimeInterval write
          SetHappyEyeballsTimeout;

        (**
         * Connection upkeep interval
         *
         * Some protocols have "connection upkeep" mechanisms. These mechanisms
         * usually send some traffic on existing connections in order to keep
         * them alive; this can prevent connections from being closed due to
         * overzealous firewalls, for example.
         * Currently the only protocol with a connection upkeep mechanism is
         * HTTP/2: when the connection upkeep interval is exceeded and
         * curl_easy_upkeep is called, an HTTP/2 PING frame is sent on the
         * connection.
         *)
        property UpkeepInterval : TTimeInterval write SetUpkeepInterval;

        (**
         * Store a private pointer
         *
         * Pass a Pointer as parameter, pointing to data that should be
         * associated with this curl handle. The pointer can subsequently be
         * retrieved using curl_easy_getinfo with the CURLINFO_PRIVATE option.
         * libcurl itself never does anything with this data.
         *)
        property PrivateData : Pointer write SetPrivateData;
      end;

      { TSecurityProperty }

      TSecurityProperty = class
      public
        type
          TAuthMethod = (
            AUTH_NONE,

           (**
            * HTTP Basic authentication. This is the default choice, and the
            * only method that is in wide-spread use and supported virtually
            * everywhere. This sends the user name and password over the network
            * in plain text, easily captured by others.
            *)
            AUTH_BASIC,

           (**
            * HTTP Digest authentication. Digest authentication is defined in
            * RFC 2617 and is a more secure way to do authentication over public
            * networks than the regular old-fashioned Basic method.
            *)
            AUTH_DIGEST,

           (**
            * HTTP Negotiate (SPNEGO) authentication. Negotiate authentication
            * is defined in RFC 4559 and is the most secure way to perform
            * authentication over HTTP.
            *)
            AUTH_NEGOTIATE,

           (**
            * Same as AUTH_NEGOTIATE
            *)
            AUTH_GSSAPI,

           (**
            * HTTP NTLM authentication. A proprietary protocol invented and used
            * by Microsoft. It uses a challenge-response and hash concept
            * similar to Digest, to prevent the password from being
            * eavesdropped.
            *)
            AUTH_NTLM,

           (**
            * HTTP Digest authentication with an IE flavor. Digest
            * authentication is defined in RFC 2617 and is a more secure way to
            * do authentication over public networks than the regular
            * old-fashioned Basic method. The IE flavor is simply that libcurl
            * will use a special "quirk" that IE is known to have used before
            * version 7 and that some servers require the client to use.
            *)
            AUTH_DIGEST_IE,

           (**
            * NTLM delegating to winbind helper. Authentication is performed by
            * a separate binary application that is executed when needed. The
            * name of the application is specified at compile time but is
            * typically /usr/bin/ntlm_auth
            *)
            AUTH_NTLM_WB,

           (**
            * HTTP Bearer token authentication, used primarily in OAuth 2.0
            * protocol.
            *)
            AUTH_BEARER,

           (**
            * This is sets all bits and thus makes libcurl pick any it finds
            * suitable. libcurl will automatically select the one it finds most
            * secure.
            *)
            AUTH_ANY,

           (**
            * This is sets all bits except Basic and thus makes libcurl pick any
            * it finds suitable. libcurl will automatically select the one it
            * finds most secure.
            *)
            AUTH_ANYSAFE
        );

        TAuthMethods = set of TAuthMethod;

        TTLSAuthMethod = (
         (**
          * TLS-SRP authentication. Secure Remote Password authentication for
          * TLS is defined in RFC 5054 and provides mutual authentication if
          * both sides have a shared secret.
          *)
          SRP
        );

        TNETRCOption = (
         (**
          * The use of the ~/.netrc file is optional, and information in the
          * URL is to be preferred. The file will be scanned for the host and
          * user name (to find the password only) or for the host only, to find
          * the first user name and password after that machine, which ever
          * information is not specified.
          *)
          NETRC_OPTIONAL                    = Longint(CURL_NETRC_OPTIONAL),

         (**
          * The library will ignore the ~/.netrc file.
          *)
          NETRC_IGNORED                     = Longint(CURL_NETRC_IGNORED){%H-},

         (**
          * The use of the ~/.netrc file is required, and information in the URL
          * is to be ignored. The file will be scanned for the host and user
          * name (to find the password only) or for the host only, to find the
          * first user name and password after that machine, which ever
          * information is not specified.
          *)
          NETRC_REQUIRED                    = Longint(CURL_NETRC_REQUIRED)
        );
      private
        FHandle : CURL;

        procedure SetUserPassword (AUserpwd : string);
        procedure SetUsername (AName : string);
        procedure SetPassword (APassword : string);
        procedure SetTLSUsername (AName : string);
        procedure SetTLSPassword (APassword : string);
        procedure SetTLSAuth (AMethod : TTLSAuthMethod);
        procedure SetAllowUsernameInURL (AAllow : Boolean);
        procedure SetAuthServiceName (AName : string);
        procedure SetNetrc (AOption : TNETRCOption);
        procedure SetNetrcFile (AFile : string);
        procedure SetUnrestrictedAuth (AEnable : Boolean);
        procedure SetSSLCertificate (ACertificate : string);
        procedure SetSSLCertificateType (AType : string);
        procedure SetSSLKey (AKey : string);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

       (**
        * Allow/disallow specifying user name in the url
        *)
        property AllowUsernameInURL : Boolean write SetAllowUsernameInURL
          default True;

       (**
        * User name and password to use in authentification
        *
        * Login details string for the connection. The format of which is:
        * [user name]:[password].
        * When using Kerberos V5 authentication with a Windows based server,
        * you should specify the user name part with the domain name in order
        * for the server to successfully obtain a Kerberos Ticket. If you don't
        * then the initial part of the authentication handshake may fail.
        * When using NTLM, the user name can be specified simply as the user
        * name without the domain name should the server be part of a single
        * domain and forest.
        * To specify the domain name use either Down-Level Logon Name or UPN
        * (User Principal Name) formats. For example, EXAMPLE\user and
        * user@example.com respectively.
        * When using HTTP and FollowLocation, libcurl might perform several
        * requests to possibly different hosts. libcurl will only send this
        * user and password information to hosts using the initial host name,
        * so if libcurl follows locations to other hosts it will not send the
        * user and password to those. This is enforced to prevent accidental
        * information leakage.
        *)
        property UserPassword : string write SetUserPassword;

       (**
        * User name to use in authentication
        *
        * Sets the user name to be used in protocol authentication. You should
        * not use this option together with the (older) UserPassword option.
        * When using Kerberos V5 authentication with a Windows based server,
        * you should include the domain name in order for the server to
        * successfully obtain a Kerberos Ticket. If you don't then the initial
        * part of the authentication handshake may fail.
        * When using NTLM, the user name can be specified simply as the user
        * name without the domain name should the server be part of a single
        * domain and forest.
        * To include the domain name use either Down-Level Logon Name or UPN
        * (User Principal Name) formats. For example, EXAMPLE\user and
        * user@example.com respectively.
        *)
        property Username : string write SetUsername;

       (**
        * Password to use in authentication
        *
        * The Password option should be used in conjunction with the Username
        * option.
        *)
        property Password : string write SetPassword;

       (**
        * User name to use for TLS authentication
        *)
        property TLSUsername : string write SetTLSUsername;

       (**
        * Password to use for TLS authentication
        *
        * Requires that the TLSUsername option also be set.
        *)
        property TLSPassword : string write SetTLSPassword;

       (**
        * Set TLS authentication methods
        *)
        property TLSAuth : TTLSAuthMethod write SetTLSAuth;

       (**
        * Authentication service name
        *
        * String holding the name of the service for DIGEST-MD5, SPNEGO and
        * Kerberos 5 authentication mechanisms. The default service names are
        * "ftp", "HTTP", "imap", "pop" and "smtp". This option allows you to
        * change them.
        *)
        property AuthServiceName : string write SetAuthServiceName;

       (**
        * Request then .netrc is used
        *
        * This parameter controls the preference level of libcurl between using
        * user names and passwords from your ~/.netrc file, relative to user
        * names and passwords in the URL supplied with URL. On Windows, libcurl
        * will use the file as %HOME%/_netrc, but you can also tell libcurl a
        * different file name to use with NetrcFile.
        *)
        property Netrc : TNETRCOption write SetNetrc default NETRC_IGNORED;

       (**
        * File name to read .netrc info from
        *
        * String containing the full path name to the file you want libcurl to
        * use as .netrc file. If this option is omitted, and Netrc is set,
        * libcurl will attempt to find a .netrc file in the current user's home
        * directory.
        *)
        property NetrcFile : string write SetNetrcFile;

       (**
        * Send credentials to other hosts too
        *
        * Set the parameter to True to make libcurl continue to send
        * authentication (user+password) credentials when following locations,
        * even when hostname changed. This option is meaningful only when
        * setting FollowRedirect.
        * By default, libcurl will only send given credentials to the initial
        * host name as given in the original URL, to avoid leaking username +
        * password to other sites.
        *)
        property UnrestrictedAuth : Boolean write SetUnrestrictedAuth
          default False;

        (**
         * Set SSL client certificate
         *
         * Pass a pointer to a zero terminated string as parameter. The string
         * should be the file name of your client certificate. The default
         * format is "P12" on Secure Transport and "PEM" on other engines, and
         * can be changed with CURLOPT_SSLCERTTYPE.
         * With NSS or Secure Transport, this can also be the nickname of the
         * certificate you wish to authenticate with as it is named in the
         * security database. If you want to use a file from the current
         * directory, please precede it with "./" prefix, in order to avoid
         * confusion with a nickname.
         * (Schannel only) Client certificates must be specified by a path
         * expression to a certificate store. (Loading PFX is not supported; you
         * can import it to a store first). You can use
         * "<store location>lt;store name>lt;thumbprint>" to refer to a
         * certificate in the system certificates store, for example,
         * "CurrentUser\MY\934a7ac6f8a5d579285a74fa61e19f23ddfe8d7a".
         * Thumbprint is usually a SHA-1 hex string which you can see in
         * certificate details. Following store locations are supported:
         * CurrentUser, LocalMachine, CurrentService, Services,
         * CurrentUserGroupPolicy, LocalMachineGroupPolicy,
         * LocalMachineEnterprise.
         * When using a client certificate, you most likely also need to provide
         * a private key with CURLOPT_SSLKEY.
         *)
        property SSLCertificate : string write SetSSLCertificate;

        (**
         * Specify type of the client SSL certificate
         *
         * Pass a pointer to a zero terminated string as parameter. The string
         * should be the format of your certificate. Supported formats are "PEM"
         * and "DER", except with Secure Transport. OpenSSL (versions 0.9.3 and
         * later) and Secure Transport (on iOS 5 or later, or OS X 10.7 or
         * later) also support "P12" for PKCS#12-encoded files.
         *)
        property SSLCertificateType : string write SetSSLCertificateType;

        (**
         * Specify private keyfile for TLS and SSL client certificate
         *
         * Pass a pointer to a zero terminated string as parameter. The string
         * should be the file name of your private key. The default format is
         * "PEM" and can be changed with CURLOPT_SSLKEYTYPE.
         * (iOS and Mac OS X only) This option is ignored if curl was built
         * against Secure Transport. Secure Transport expects the private key to
         * be already present in the keychain or PKCS#12 file containing the
         * certificate.
         *)
        property SSLKey : string write SetSSLKey;
      end;

      { TProtocolProperty }

      TProtocolProperty = class
      public
        type
          TProtocol = (
          (**
           * DICT is a dictionary network protocol, it allows clients to ask
           * dictionary servers about a meaning or explanation for words. See
           * RFC 2229. Dict servers and clients use TCP port 2628.
           *)
          PROTOCOL_DICT,

          (**
           * FILE is not actually a "network" protocol. It is a URL scheme that
           * allows you to tell curl to get a file from the local file system
           * instead of getting it over the network from a remote server. See
           * RFC 1738.
           *)
          PROTOCOL_FILE,

          (**
           * FTP stands for File Transfer Protocol and is an old (originates in
           * the early 1970s) way to transfer files back and forth between a
           * client and a server. See RFC 959. It has been extended greatly over
           * the years. FTP servers and clients use TCP port 21 plus one more
           * port, though the second one is usually dynamically established
           * during communication.
           *)
          PROTOCOL_FTP,

          (**
           * FTPS stands for Secure File Transfer Protocol. It follows the
           * tradition of appending an 'S' to the protocol name to signify that
           * the protocol is done like normal FTP but with an added SSL/TLS
           * security layer. See RFC 4217.
           * This protocol is problematic to use through firewalls and other
           * network equipment.
           *)
          PROTOCOL_FTPS,

          (**
           * Designed for "distributing, searching, and retrieving documents
           * over the Internet", Gopher is somewhat of the grand father to HTTP
           * as HTTP has mostly taken over completely for the same use cases.
           * See RFC 1436. Gopher servers and clients use TCP port 70.
           *)
          PROTOCOL_GOPHER,

          (**
           * The Hypertext Transfer Protocol, HTTP, is the most widely used
           * protocol for transferring data on the web and over the Internet.
           * See RFC 7230 for HTTP/1.1 and RFC 7540 for HTTP/2. HTTP servers and
           * clients use TCP port 80.
           *)
          PROTOCOL_HTTP,

          (**
           * Secure HTTP is HTTP done over an SSL/TLS connection. See RFC 2818.
           * HTTPS servers and clients use TCP port 443, unless they speak
           * HTTP/3 which then uses QUIC and is done over UDP...
           *)
          PROTOCOL_HTTPS,

          (**
           * The Internet Message Access Protocol, IMAP, is a protocol for
           * accessing, controlling and "reading" email. See RFC 3501. IMAP
           * servers and clients use TCP port 143. Whilst connections to the
           * server start out as cleartext, SSL/TLS communication may be
           * supported by the client explicitly requesting to upgrade the
           * connection using the STARTTLS command. See RFC 2595.
           *)
          PROTOCOL_IMAP,

          (**
           * Secure IMAP is IMAP done over an SSL/TLS connection. Such
           * connections implicitly start out using SSL/TLS and as such servers
           * and clients use TCP port 993 to communicate with each other. See
           * RFC 8314.
           *)
          PROTOCOL_IMAPS,

          (**
           * The Lightweight Directory Access Protocol, LDAP, is a protocol for
           * accessing and maintaining distributed directory information.
           * Basically a database lookup. See RFC 4511. LDAP servers and clients
           * use TCP port 389.
           *)
          PROTOCOL_LDAP,

          (**
           * Secure LDAP is LDAP done over an SSL/TLS connection.
           *)
          PROTOCOL_LDAPS,

          (**
           * The Post Office Protocol version 3 (POP3) is a protocol for
           * retrieving email from a server. See RFC 1939. POP3 servers and
           * clients use TCP port 110. Whilst connections to the server start
           * out as cleartext, SSL/TLS communication may be supported by the
           * client explicitly requesting to upgrade the connection using the
           * STLS command. See RFC 2595.
           *)
          PROTOCOL_POP3,

          (**
           * Secure POP3 is POP3 done over an SSL/TLS connection. Such
           * connections implicitly start out using SSL/TLS and as such servers
           * and clients use TCP port 995 to communicate with each other. See
           * RFC 8314.
           *)
          PROTOCOL_POP3S,

          (**
           * The Real-Time Messaging Protocol (RTMP) is a protocol for streaming
           * audio, video and data. RTMP servers and clients use TCP port 1935.
           *)
          PROTOCOL_RTMP,
          PROTOCOL_RTMPE,
          PROTOCOL_RTMPS,
          PROTOCOL_RTMPT,
          PROTOCOL_RTMPTE,
          PROTOCOL_RTMPTS,

          (**
           * The Real Time Streaming Protocol (RTSP) is a network control
           * protocol to control streaming media servers. See RFC 2326. RTSP
           * servers and clients use TCP and UDP port 554.
           *)
          PROTOCOL_RTSP,

          (**
           * The Secure Copy (SCP) protocol is designed to copy files to and
           * from a remote SSH server. SCP servers and clients use TCP port 22.
           *)
          PROTOCOL_SCP,

          (**
           * The SSH File Transfer Protocol (SFTP) that provides file access,
           * file transfer, and file management over a reliable data stream.
           * SFTP servers and clients use TCP port 22.
           *)
          PROTOCOL_SFTP,

          (**
           * The Server Message Block (SMB) protocol is also known as CIFS. It
           * is an application-layer network protocol mainly used for providing
           * shared access to files, printers, and serial ports and
           * miscellaneous communications between nodes on a network. SMB
           * servers and clients use TCP port 445.
           *)
          PROTOCOL_SMB,
          PROTOCOL_SMBS,

          (**
           * The Simple Mail Transfer Protocol (SMTP) is a protocol for email
           * transmission. See RFC 5321. SMTP servers and clients use TCP port
           * 25. Whilst connections to the server start out as cleartext,
           * SSL/TLS communication may be supported by the client explicitly
           * requesting to upgrade the connection using the STARTTLS command.
           * See RFC 3207.
           *)
          PROTOCOL_SMTP,

          (**
           * Secure SMTP, sometimes called SSMTP, is SMTP done over an SSL/TLS
           * connection. Such connections implicitly start out using SSL/TLS and
           * as such servers and clients use TCP port 465 to communicate with
           * each other. See RFC 8314.
           *)
          PROTOCOL_SMTPS,

          (**
           * TELNET is an application layer protocol used over networks to
           * provide a bidirectional interactive text-oriented communication
           * facility using a virtual terminal connection. See RFC 854. TELNET
           * servers and clients use TCP port 23.
           *)
          PROTOCOL_TELNET,

          (**
           * The Trivial File Transfer Protocol (TFTP) is a protocol for doing
           * simple file transfers over UDP to get a file from or put a file
           * onto a remote host. TFTP servers and clients use UDP port 69.
           *)
          PROTOCOL_TFTP
        );

        TProtocols = set of TProtocol;
      private
        FHandle : CURL;

        procedure SetAllowedProtocols (AProtocols : TProtocols);
        procedure SetAllowedRedirectProtocols (AProtocols : TProtocols);
        procedure SetDefaultProtocol (AProtocol : TProtocol);
        procedure SetFollowRedirect (AFollow : Boolean);
        procedure SetMaxRedirects (AAmount : Longint);
        procedure SetNoBody (ANoBody : Boolean);
        procedure SetVerbose (AEnable : Boolean);
        procedure SetIncludeHeader (AIncludeHeader : Boolean);
        procedure SetIgnoreContentLength (AIgnoreLength : Boolean);
        procedure SetTransferEncoding (AEncoding : Boolean);

        procedure SetRemotePort (APort : Word);
        procedure SetUpload (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set allowed protocols
         *
         * Limits what protocols libcurl may use in the transfer. This allows
         * you to have a libcurl built to support a wide range of protocols but
         * still limit specific transfers to only be allowed to use a subset of
         * them. By default libcurl will accept all protocols it supports
         *)
        property Protocols : TProtocols write SetAllowedProtocols
          default [PROTOCOL_DICT, PROTOCOL_FILE, PROTOCOL_FTP, PROTOCOL_FTPS,
          PROTOCOL_GOPHER, PROTOCOL_HTTP, PROTOCOL_HTTPS, PROTOCOL_IMAP,
          PROTOCOL_IMAPS, PROTOCOL_LDAP, PROTOCOL_LDAPS, PROTOCOL_POP3,
          PROTOCOL_POP3S, PROTOCOL_RTMP, PROTOCOL_RTMPE, PROTOCOL_RTMPS,
          PROTOCOL_RTMPT, PROTOCOL_RTMPTE, PROTOCOL_RTMPTS, PROTOCOL_RTSP,
          PROTOCOL_SCP, PROTOCOL_SFTP, PROTOCOL_SMB, PROTOCOL_SMBS,
          PROTOCOL_SMTP, PROTOCOL_SMTPS, PROTOCOL_TELNET, PROTOCOL_TFTP];

        (**
         * Set protocols allowed to redirect to
         *
         * Limits what protocols libcurl may use in a transfer that it follows
         * to in a redirect when FollowRedirect is enabled. This allows you to
         * limit specific transfers to only be allowed to use a subset of
         * protocols in redirections.
         *)
        property RedirectProtocols : TProtocols
          write SetAllowedRedirectProtocols
          default [PROTOCOL_HTTP, PROTOCOL_HTTPS, PROTOCOL_FTP, PROTOCOL_FTPS];

        (**
         * Default protocol to use if the URL is missing a scheme name
         *
         * This option does not change the default proxy protocol (http).
         * Without this option libcurl would make a guess based on the host.
         *)
        property DefaultProtocol : TProtocol write SetDefaultProtocol;

        (**
         * Follow HTTP 3XXX redirects
         *
         * Tells the library to follow any Location: header that the server
         * sends as part of an HTTP header in a 3xx response. The Location:
         * header can specify a relative or an absolute URL to follow.
         * libcurl will issue another request for the new URL and follow new
         * Location: headers all the way until no more such headers are
         * returned. libcurl limits what protocols it automatically follows to.
         * By default libcurl will allow HTTP, HTTPS, FTP and FTPS on redirect.
         *)
        property FollowRedirect : Boolean write SetFollowRedirect default True;

        (**
         * Meximum numbers of redirects allowed
         *
         * Setting the limit to 0 will make libcurl refuse any redirect.
         * Set it to -1 for an infinite number of redirects.
         *)
        property MaxRedirects : Longint write SetMaxRedirects default -1;

        (**
         * Do the download request without getting the body
         *
         * Tells libcurl to not include the body-part in the output when doing
         * what would otherwise be a download. For HTTP(S), this makes libcurl
         * do a HEAD request. For most other protocols it means just not asking
         * to transfer the body data.
         *)
        property NoBody : Boolean write SetNoBody default False;

        (**
         * Set verbose mode on/off
         *
         * Make the library display a lot of verbose information about its
         * operations. Very useful for libcurl and/or protocol debugging and
         * understanding. The verbose information will be sent to stderr.
         *)
        property VerboseMode : Boolean write SetVerbose default False;

        (**
         * Pass headers to the data stream
         *
         * Ask libcurl to include the headers in the data stream.
         * When asking to get the headers passed to the body, it is not possible
         * to accurately separate them again without detailed knowledge about
         * the protocol in use.
         *)
        property IncludeHeader : Boolean write SetIncludeHeader default False;

        (**
         * Ignore content length
         *
         * Ignore the Content-Length header in the HTTP response and ignore
         * asking for or relying on it for FTP transfers.
         * This is useful for HTTP with Apache 1.x (and similar servers) which
         * will report incorrect content length for files over 2 gigabytes. If
         * this option is used, curl will not be able to accurately report
         * progress, and will simply stop the download when the server ends the
         * connection. It is also useful with FTP when for example the file is
         * growing while the transfer is in progress which otherwise will
         * unconditionally cause libcurl to report error.
         *)
        property IgnoreContentLength : Boolean write SetIgnoreContentLength
          default False;

        (**
         * Ask for HTTP Transfer Encoding
         *
         * Add a request for compressed Transfer Encoding in the outgoing HTTP
         * request. If the server supports this and so desires, it can respond
         * with the HTTP response sent using a compressed Transfer-Encoding that
         * will be automatically uncompressed by libcurl on reception.
         *)
        property TransferEncoding : Boolean write SetTransferEncoding
          default False;

        (**
         * Set remote port number to work with
         *
         * This option sets number to be the remote port number to connect to,
         * instead of the one specified in the URL or the default port for the
         * used protocol.
         * Usually, you just let the URL decide which port to use but this
         * allows the application to override that.
         *)
        property Port : Word write SetRemotePort;

        (**
         * Enable data upload
         *
         * The long parameter upload set to True tells the library to prepare
         * for and perform an upload. The CURLOPT_READDATA and
         * CURLOPT_INFILESIZE or CURLOPT_INFILESIZE_LARGE options are also
         * interesting for uploads. If the protocol is HTTP, uploading means
         * using the PUT request unless you tell libcurl otherwise.
         * Using PUT with HTTP 1.1 implies the use of a "Expect: 100-continue"
         * header. You can disable this header with CURLOPT_HTTPHEADER as usual.
         * If you use PUT to an HTTP 1.1 server, you can upload data without
         * knowing the size before starting the transfer if you use chunked
         * encoding. You enable this by adding a header like "Transfer-Encoding:
         * chunked" with CURLOPT_HTTPHEADER. With HTTP 1.0 or without chunked
         * transfer, you must specify the size.
         *)
        property Upload : Boolean write SetUpload default False;
      end;

      { TTCPProperty }

      TTCPProperty = class
      private
        FHandle : CURL;

        procedure SetTCPFastOpen (AEnable : Boolean);
        procedure SetTCPNoDelay (AEnable : Boolean);
        procedure SetTCPKeepalive (ASendProbe : Boolean);
        procedure SetTCPKeepIdle (ATime : TTimeInterval);
        procedure SetTCPKeepInterval (ATime : TTimeInterval);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Enable/disable TCP Fast Open
         *
         * TCP Fast Open (RFC7413) is a mechanism that allows data to be carried
         * in the SYN and SYN-ACK packets and consumed by the receiving end
         * during the initial connection handshake, saving up to one full
         * round-trip time (RTT).
         *)
        property FastOpen : Boolean write SetTCPFastOpen default False;

        (**
         * Disable TCP's Nagle algorithm on this connection
         *
         * The purpose of this algorithm is to try to minimize the number of
         * small packets on the network (where "small packets" means TCP
         * segments less than the Maximum Segment Size (MSS) for the network).
         * Maximizing the amount of data sent per TCP segment is good because it
         * amortizes the overhead of the send. However, in some cases small
         * segments may need to be sent without delay. This is less efficient
         * than sending larger amounts of data at a time, and can contribute to
         * congestion on the network if overdone.
         *)
        property NoDelay : Boolean write SetTCPNoDelay default True;

        (**
         * Enable/disable TCP keep-alive probing
         *
         * If set, TCP keepalive probes will be sent. The delay and frequency of
         * these probes can be controlled by the TCPKeepIdle and TCPKeepInterval
         * options, provided the operating system supports them. Set to False
         * (default behavior) to disable keepalive probes
         *)
        property Keepalive : Boolean write SetTCPKeepalive default False;

        (**
         * Set TCP keep-alive idle time wait
         *
         * Sets the delay, that the operating system will wait while the
         * connection is idle before sending keepalive probes. Not all operating
         * systems support this option.
         *)
        property KeepIdle : TTimeInterval write SetTCPKeepIdle;

        (**
         * Set TCP keep-alive interval
         *
         * Sets the interval, that the operating system will wait between
         * sending keepalive probes. Not all operating systems support this
         * option.
         *)
         property KeepInterval : TTimeInterval write SetTCPKeepInterval;
      end;

      TProxyProperty = class;

      { TSOCKS5Property }

      TSOCKS5Property = class
      private
        FHandle : CURL;

        procedure SetSOCKS5Auth (AMethod : TSecurityProperty.TAuthMethods);
        procedure SetSOCKS5GSSAPIServiceName (AName : string);
        procedure SetSOCKS5GSSAPINegotiation (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set allowed methods for SOCKS5 proxy authentication
         *
         * Tell libcurl which authentication method(s) are allowed for SOCKS5
         * proxy authentication. The only supported flags are AUTH_BASIC, which
         * allows username/password authentication, AUTH_GSSAPI, which allows
         * GSS-API authentication, and AUTH_NONE, which allows no
         * authentication.
         *)
        property Auth : TSecurityProperty.TAuthMethods write SetSOCKS5Auth
          default [AUTH_BASIC, AUTH_GSSAPI];

        (**
         * SOCKS5 proxy authentication service name
         *
         * String holding the name of the service. The default service name for
         * a SOCKS5 server is "rcmd". This option allows you to change it.
         *)
        property GSSAPIServiceName : string write SetSOCKS5GSSAPIServiceName;

        (**
         * Set socks proxy gssapi nogotiation protection
         *
         * As part of the gssapi negotiation a protection mode is negotiated.
         * The RFC 1961 says in section 4.3/4.4 it should be protected, but the
         * NEC reference implementation does not. If enabled, this option allows
         * the unprotected exchange of the protection mode negotiation.
         *)
        property GSSAPINegotiation : Boolean write SetSOCKS5GSSAPINegotiation;
      end;

      { TProxyProperty }

      TProxyProperty = class
      public
        type
          (**
           * Proxy protocol type
           *)
          TProxyType = (
            (**
             * HTTP Proxy
             *)
            PROXY_HTTP                        = Longint(CURLPROXY_HTTP),

            (**
             * HTTP 1.0 Proxy. This is very similar to CURLPROXY_HTTP except it
             * uses HTTP/1.0 for any CONNECT tunnelling. It does not change the
             * HTTP version of the actual HTTP requests
             *)
            PROXY_HTTP_1_0                    = Longint(CURLPROXY_HTTP_1_0),

            (**
             * HTTPS Proxy
             *)
            PROXY_HTTPS                       = Longint(CURLPROXY_HTTPS),

            (**
             * SOCKS4 Proxy
             *)
            PROXY_SOCKS4                      = Longint(CURLPROXY_SOCKS4),

            (**
             * SOCKS5 Proxy
             *)
            PROXY_SOCKS5                      = Longint(CURLPROXY_SOCKS5),

            (**
             * SOCKS4a Proxy. Proxy resolves URL hostname
             *)
            PROXY_SOCKS4A                     = Longint(CURLPROXY_SOCKS4A),

            (**
             * SOCKS5 Proxy. Proxy resolves URL hostname.
             *)
            PROXY_SOCKS5_HOSTNAME
              = Longint(CURLPROXY_SOCKS5_HOSTNAME)
          );
      private
        FHandle : CURL;
        FSOCKS5 : TSOCKS5Property;

        procedure SetPreProxy (APreProxy : string);
        procedure SetProxy (AProxy : string);
        procedure SetPort (APort : Longint);
        procedure SetProxyType (AType : TProxyType);
        procedure SetProxyServiceName (AName : string);
        procedure SetNoProxyHosts (AHosts : string);
        procedure SetHttpProxyTunnel (AEnable : Boolean);
        procedure SetProxyUserPassword (AUserpwd : string);
        procedure SetProxyUsername (AName : string);
        procedure SetProxyPassword (APassword : string);
        procedure SetProxyTLSUsername (AName : string);
        procedure SetProxyTLSPassword (APassword : string);
        procedure SetProxyTLSAuth (AMethod : TSecurityProperty.TTLSAuthMethod);
        procedure SetProxyHTTPAuth (AMethod : TSecurityProperty.TAuthMethods);
        procedure SetHAProxyHeader (ASend : Boolean);
        procedure SetProxySSLCertificate (ACertificate : string);
        procedure SetProxySSLCertificateType (AType : string);
        procedure SetProxySSLKey (AKey : string);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        property SOCKS5 : TSOCKS5Property read FSOCKS5 write FSOCKS5;

        (**
         * Set pre-proxy to use
         *
         * Set the preproxy to use for the upcoming request. The parameter
         * should be a string holding the host name or dotted numerical IP
         * address. A numerical IPv6 address must be written within [brackets].
         * To specify port number in this string, append :[port] to the end of
         * the host name. The proxy's port number may optionally be specified
         * with the separate option Proxy. If not specified, libcurl will
         * default to using port 1080 for proxies.
         * A pre proxy is a SOCKS proxy that curl connects to before it connects
         * to the HTTP(S) proxy specified in the CURLOPT_PROXY option. The pre
         * proxy can only be a SOCKS proxy.
         * The pre proxy string should be prefixed with [scheme]:// to specify
         * which kind of socks is used. Use socks4://, socks4a://, socks5:// or
         * socks5h:// (the last one to enable socks5 and asking the proxy to do
         * the resolving, also known as CURLPROXY_SOCKS5_HOSTNAME type) to
         * request the specific SOCKS version to be used. Otherwise SOCKS4 is
         * used as default. Setting the pre proxy string to "" (an empty string)
         * will explicitly disable the use of a pre proxy.
         *)
        property PreProxy : string write SetPreProxy;

        (**
         * Set proxy to use
         *
         * Set the proxy to use for the upcoming request. The parameter should
         * be a string holding the host name or dotted numerical IP address. A
         * numerical IPv6 address must be written within [brackets].
         * To specify port number in this string, append :[port] to the end of
         * the host name. If not specified, libcurl will default to using port
         * 1080 for proxies.
         * The proxy string may be prefixed with [scheme]:// to specify which
         * kind of proxy is used.
         * http://    HTTP Proxy. Default when no scheme or proxy type is
         *            specified.
         * https://   HTTPS Proxy.
         * socks4://  SOCKS4 Proxy.
         * socks4a:// SOCKS4a Proxy. Proxy resolves URL hostname.
         * socks5://  SOCKS5 Proxy.
         * socks5h:// SOCKS5 Proxy. Proxy resolves URL hostname.
         * When you tell the library to use an HTTP proxy, libcurl will
         * transparently convert operations to HTTP even if you specify an FTP
         * URL etc.
         * Setting the proxy string to "" (an empty string) will explicitly
         * disable the use of a proxy, even if there is an environment variable
         * set for it. A proxy host string can also include protocol scheme
         * (http://) and embedded user + password.
         *)
        property Proxy : string write SetProxy;

        (**
         * Port number the proxy listens on
         *
         * Set the proxy port to connect to unless it is specified in the proxy
         * string.
         *)
        property Port : Longint write SetPort;

        (**
         * Proxy protocol type
         *)
        property Protocol : TProxyType write SetProxyType default PROXY_HTTP;

        (**
         * Proxy authentication service name
         *
         * String holding the name of the service. The default service name is
         * "HTTP" for HTTP based proxies and "rcmd" for SOCKS5. This option
         * allows you to change it.
         *)
        property ServiceName : string write SetProxyServiceName;

        (**
         * Disable proxy use for specific hosts
         *
         * The string consists of a comma separated list of host names that do
         * not require a proxy to get reached, even if one is specified. The
         * only wildcard available is a single * character, which matches all
         * hosts, and effectively disables the proxy. Each name in this list is
         * matched as either a domain which contains the hostname, or the
         * hostname itself. For example, example.com would match example.com,
         * example.com:80, and www.example.com, but not www.notanexample.com or
         * example.com.othertld. If the name in the noproxy list has a leading
         * period, it is a domain match against the provided host name. This way
         * ".example.com" will switch off proxy use for both "www.example.com"
         * as well as for "foo.example.com".
         * Setting the noproxy string to "" (an empty string) will explicitly
         * enable the proxy for all host names, even if there is an environment
         * variable set for it.
         * Enter IPv6 numerical addresses in the list of host names without
         * enclosing brackets: "example.com,::1,localhost"
         *)
        property NoProxyHosts : string write SetNoProxyHosts;

        (**
         * Tunnel through HTTP proxy
         *
         * Make libcurl tunnel all operations through the HTTP proxy (set with
         * Proxy property). There is a big difference between using a proxy and
         * to tunnel through it.
         * Tunneling means that an HTTP CONNECT request is sent to the proxy,
         * asking it to connect to a remote host on a specific port number and
         * then the traffic is just passed through the proxy. Proxies tend to
         * white-list specific port numbers it allows CONNECT requests to and
         * often only port 80 and 443 are allowed.
         *)
        property HttpTunnel : Boolean write SetHttpProxyTunnel;

        (**
         * User name and password to use for proxy authentification
         *
         * Pass a parameter, which should be [user name]:[password] to use for
         * the connection to the HTTP proxy. Both the name and the password will
         * be URL decoded before use, so to include for example a colon in the
         * user name you should encode it as %3A. (This is different to how
         * UserPassword is used - beware.)
         *)
        property UserPassword : string write SetProxyUserPassword;

        (**
         * User name to use for proxy authentication
         *
         * Sets the user name to be used in protocol authentication with the
         * proxy.
         *)
        property Username : string write SetProxyUsername;

        (**
         * Password to use with proxy authentication
         *
         * The option should be used in conjunction with the ProxyUsername
         * option.
         *)
        property Password : string write SetProxyPassword;

        (**
         * User name to use for proxy TLS authentication
         *)
        property TLSUsername : string write SetProxyTLSUsername;

        (**
         * Password to use for proxy TLS
         *
         * Requires that the ProxyTLSUsername option also be set.
         *)
        property TLSPassword : string write SetProxyTLSPassword;

        (**
         * Set proxy TLS authentication methods
         *)
        property TLSAuth : TSecurityProperty.TTLSAuthMethod
          write SetProxyTLSAuth;

        (**
         * Set HTTP proxy authentication methods to try
         *
         * Tell libcurl which HTTP authentication method(s) you want it to use
         * for your proxy authentication. If more than one bit is set, libcurl
         * will first query the site to see what authentication methods it
         * supports and then pick the best one you allow it to use. For some
         * methods, this will induce an extra network round-trip.
         *)
        property HTTPAuth : TSecurityProperty.TAuthMethods
          write SetProxyHTTPAuth default [AUTH_BASIC];

        (**
         * Send HAProxy PROXY protocol v.1 header
         *
         * Tells the library to send an HAProxy PROXY protocol v1 header at
         * beginning of the connection. The default action is not to send this
         * header.
         * This option is primarily useful when sending test requests to a
         * service that expects this header.
         *)
        property HAProxyProtocol : Boolean write SetHAProxyHeader default False;

        (**
         * Set SSL proxy client certificate
         *
         * This option is for connecting to an HTTPS proxy, not an HTTPS server.
         * Pass a pointer to a zero terminated string as parameter. The string
         * should be the file name of your client certificate used to connect to
         * the HTTPS proxy. The default format is "P12" on Secure Transport and
         * "PEM" on other engines, and can be changed with
         * CURLOPT_PROXY_SSLCERTTYPE.
         * With NSS or Secure Transport, this can also be the nickname of the
         * certificate you wish to authenticate with as it is named in the
         * security database. If you want to use a file from the current
         * directory, please precede it with "./" prefix, in order to avoid
         * confusion with a nickname.
         * When using a client certificate, you most likely also need to provide
         * a private key with CURLOPT_PROXY_SSLKEY.
         *)
        property SSLCertificate : string write SetProxySSLCertificate;

        (**
         * Specify type of the proxy client SSL sertificate
         *
         * Pass a pointer to a zero terminated string as parameter. The string
         * should be the format of your client certificate used when connecting
         * to an HTTPS proxy.
         * Supported formats are "PEM" and "DER", except with Secure Transport.
         * OpenSSL (versions 0.9.3 and later) and Secure Transport (on iOS 5 or
         * later, or OS X 10.7 or later) also support "P12" for PKCS#12-encoded
         * files.
         *)
        property SSLCertificateType : string write SetProxySSLCertificateType;

        (**
         * Specify private keyfile for TLS and SSL proxy client certificate
         *
         * Pass a pointer to a zero terminated string as parameter. The string
         * should be the file name of your private key used for connecting to
         * the HTTPS proxy. The default format is "PEM" and can be changed with
         * CURLOPT_PROXY_SSLKEYTYPE.
         * (iOS and Mac OS X only) This option is ignored if curl was built
         * against Secure Transport. Secure Transport expects the private key to
         * be already present in the keychain or PKCS#12 file containing the
         * certificate.
         *)
        property SSLKey : string write SetProxySSLKey;
      end;

      { TDNSProperty }

      TDNSProperty = class
      private
        FHandle : CURL;

        procedure SetDNSCacheTimeout (ATimeout : TTimeInterval);
        procedure SetDNSGlobalCache (AEnable : Boolean);
        procedure SetDNSoverHTTPS (AUrl : string);
        procedure SetDNSInterface (AInterface : string);
        procedure SetDNSLocalIP4 (AAddress : string);
        procedure SetDNSLocalIP6 (AAddress : string);
        procedure SetDNSServers (AServers : string);
        procedure SetDNSShuffleAddresses (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set life-time for DNS cache entries
         *
         * Name resolves will be kept in memory and used for this time interval.
         * Set to zero to completely disable caching.
         *)
        property CacheTimeout : TTimeInterval write SetDNSCacheTimeout;

        (**
         * Enable/disable global DNS cache
         *
         * Tells curl to use a global DNS cache that will survive between easy
         * handle creations and deletions. This is not thread-safe and this will
         * use a global variable.
         *
         * WARNING: this option is considered obsolete. Stop using it. Switch
         * over to using the share interface instead!
         *)
        property GlobalCache : Boolean write SetDNSGlobalCache;

        (**
         * Provide the DNS-over-HTTPS URL
         *
         * Pass in a string to a URL for the DOH server to use for name
         * resolving. The parameter should be a string which must be URL-encoded
         * in the following format: "https://host:port/path". It MUST specify a
         * HTTPS URL. Disable DOH use again by setting this option to '' (empty
         * string).
         *)
        property DNSoverHTTPS : string write SetDNSoverHTTPS;

        (**
         * Set interface to speak DNS over
         *
         * Set the name of the network interface that the DNS resolver should
         * bind to. This must be an interface name (not an address). Set this
         * option to '' (empty string) to use the default setting (don't bind to
         * a specific interface).
         *)
        property InterfaceName : string write SetDNSInterface;

        (**
         * IPv4 address to bind DNS resolves to
         *
         * Set the local IPv4 address that the resolver should bind to. The
         * argument should be string and contain a single numerical IPv4 address
         * as a string. Set this option to '' (empty string) to use the default
         * setting (don't bind to a specific IP address).
         *)
        property LocalIP4 : string write SetDNSLocalIP4;

        (**
         * IPv6 address to bind DNS resolves to
         *
         * Set the local IPv6 address that the resolver should bind to. The
         * argument should be string and contain a single IPv6 address as a
         * string. Set this option to '' (empty string) to use the default
         * setting (don't bind to a specific IP address).
         *)
        property LocalIP6 : string write SetDNSLocalIP6;

        (**
         * Set preferred DNS servers
         *
         * Pass a string that is the list of DNS servers to be used instead of
         * the system default. The format of the dns servers option is:
         * host[:port][,host[:port]]...
         * For example:
         * 192.168.1.100,192.168.1.101,3.4.5.6
         *)
        property Servers : string write SetDNSServers;

        (**
         * Shuffle addresses when a hostname returns more that one
         *
         * When a name is resolved and more than one IP address is returned,
         * shuffle the order of all returned addresses so that they will be used
         * in a random order. This is similar to the ordering behavior of
         * gethostbyname which is no longer used on most platforms.
         * Addresses will not be reshuffled if a name resolution is completed
         * using the DNS cache. CURLOPT_DNS_CACHE_TIMEOUT can be used together
         * with this option to reduce DNS cache timeout or disable caching
         * entirely if frequent reshuffling is needed.
         * Since the addresses returned will be reordered randomly, their order
         * will not be in accordance with RFC 3484 or any other deterministic
         * order that may be generated by the system's name resolution
         * implementation. This may have performance impacts and may cause IPv4
         * to be used before IPv6 or vice versa.
         *)
        property ShuffleAddresses : Boolean write SetDNSShuffleAddresses;
      end;

      { THTTPCookie }

      THTTPCookie = class
      private
        FHandle : CURL;

        procedure SetCookie (ACookie : string);
        procedure SetCookieFile (AFile : string);
        procedure SetCookieJar (AFile : string);
        procedure SetCookieSession (ACreate : Boolean);
        procedure SetCookieList (ACookie : string);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set contents of HTTP Cookie header
         *
         * Will be used to set a cookie in the HTTP request. The format of the
         * string should be NAME=CONTENTS, where NAME is the cookie name and
         * CONTENTS is what the cookie should contain.
         * If you need to set multiple cookies, set them all using a single
         * option concatenated like this: "name1=content1; name2=content2;" etc.
         * This option sets the cookie header explicitly in the outgoing
         * request(s). If multiple requests are done due to authentication,
         * followed redirections or similar, they will all get this cookie
         * passed on.
         * The cookies set by this option are separate from the internal cookie
         * storage held by the cookie engine and will not be modified by it. If
         * you enable the cookie engine and either you've imported a cookie of
         * the same name (e.g. 'foo') or the server has set one, it will have no
         * effect on the cookies you set here. A request to the server will send
         * both the 'foo' held by the cookie engine and the 'foo' held by this
         * option. To set a cookie that is instead held by the cookie engine and
         * can be modified by the server use CookieList.
         * Using this option multiple times will only make the latest string
         * override the previous ones.
         * This option will not enable the cookie engine. Use CookieFile or
         * CookieJar to enable parsing and sending cookies automatically.
         *)
        property Cookie : string write SetCookie;

        (**
         * File name to read cookie from
         *
         * It should point to the file name of your file holding cookie data to
         * read. The cookie data can be in either the old Netscape / Mozilla
         * cookie data format or just regular HTTP headers (Set-Cookie style)
         * dumped to a file.
         * It also enables the cookie engine, making libcurl parse and send
         * cookies on subsequent requests with this handle.
         * Given an empty or non-existing file or by passing the empty string
         * ("") to this option, you can enable the cookie engine without reading
         * any initial cookies. If you tell libcurl the file name is "-" (just a
         * single minus sign), libcurl will instead read from stdin.
         * This option only reads cookies. To make libcurl write cookies to
         * file, see CookieJar.
         * Exercise caution if you are using this option and multiple transfers
         * may occur. If you use the Set-Cookie format and don't specify a
         * domain then the cookie is sent for any domain (even after redirects
         * are followed) and cannot be modified by a server-set cookie. If a
         * server sets a cookie of the same name then both will be sent on a
         * future transfer to that server, likely not what you intended. To
         * address these issues set a domain in Set-Cookie (doing that will
         * include sub-domains) or use the Netscape format.
         * If you use this option multiple times, you just add more files to
         * read. Subsequent files will add more cookies.
         *)
        property CookieFile : string write SetCookieFile;

        (**
         * File name to store cookies to
         *
         * This will make libcurl write all internally known cookies to the
         * specified file when curl_easy_cleanup is called. If no cookies are
         * known, no file will be created. Specify "-" as filename to instead
         * have the cookies written to stdout. Using this option also enables
         * cookies for this session, so if you for example follow a location it
         * will make matching cookies get sent accordingly.
         * Note that libcurl doesn't read any cookies from the cookie jar. If
         * you want to read cookies from a file, use CookieFile.
         * If the cookie jar file can't be created or written to (when the
         * curl_easy_cleanup is called), libcurl will not and cannot report an
         * error for this.
         * Since 7.43.0 cookies that were imported in the Set-Cookie format
         * without a domain name are not exported by this option.
         *)
        property CookieJar : string write SetCookieJar;

        (**
         * Start a new cookie session
         *
         * Mark this as a new cookie "session". It will force libcurl to ignore
         * all cookies it is about to load that are "session cookies" from the
         * previous session. By default, libcurl always stores and loads all
         * cookies, independent if they are session cookies or not. Session
         * cookies are cookies without expiry date and they are meant to be
         * alive and existing for this "session" only.
         *)
        property CookieNewSession : Boolean write SetCookieSession;

        (**
         * Add to or manipulate cookies held in memory
         *
         * Such a cookie can be either a single line in Netscape / Mozilla
         * format or just regular HTTP-style header (Set-Cookie: ...) format.
         * This will also enable the cookie engine. This adds that single cookie
         * to the internal cookie store.
         * Exercise caution if you are using this option and multiple transfers
         * may occur. If you use the Set-Cookie format and don't specify a
         * domain then the cookie is sent for any domain (even after redirects
         * are followed) and cannot be modified by a server-set cookie. If a
         * server sets a cookie of the same name (or maybe you've imported one)
         * then both will be sent on a future transfer to that server, likely
         * not what you intended. To address these issues set a domain in
         * Set-Cookie (doing that will include sub-domains) or use the Netscape
         * format.
         * Additionally, there are commands available that perform actions if
         * you pass in these exact strings:
         * ALL
         * erases all cookies held in memory
         * SESS
         * erases all session cookies held in memory
         * FLUSH
         * writes all known cookies to the file specified by CookieJar
         * RELOAD
         * loads all cookies from the files specified by CookieFile
         *)
        property CookieList : string write SetCookieList;
      end;

      { THTTPProperty }

      THTTPProperty = class
      public
        type
          THTTPStatusCode = (
          (**
           * Error status code, not possible as normal!
           *)
          HTTP_STATUS_UNKNOWN                                  = 0,

          (**
           * The server, has received the request headers and the client should
           * proceed to send the request body (in the case of a request for
           * which a body needs to be sent; for example, a POST request).
           * Sending a large request body to a server after a request has been
           * rejected for inappropriate headers would be inefficient. To have a
           * server check the request's headers, a client must send Expect:
           * 100-continue as a header in its initial request and receive a 100
           * Continue status code in response before sending the body. If the
           * client receives an error code such as 403 (Forbidden) or 405
           * (Method Not Allowed) then it shouldn't send the request's body. The
           * response 417 Expectation Failed indicates that the request should
           * be repeated without the Expect header as it indicates that the
           * server doesn't support expectations (this is the case, for example,
           * of HTTP/1.0 servers).
           *)
          HTTP_CONTINUE                                        = 100,

          (**
           * The requester has asked the server to switch protocols and the
           * server has agreed to do so.
           *)
          HTTP_SWITCHING_PROTOCOL                              = 101,

          (**
           * A WebDAV request may contain many sub-requests involving file
           * operations, requiring a long time to complete the request. This
           * code indicates that the server has received and is processing the
           * request, but no response is available yet. This prevents the client
           * from timing out and assuming the request was lost.
           *)
          HTTP_PROCESSING                                      = 102,

          (**
           * Used to return some response headers before final HTTP message.
           *)
          HTTP_EARLY_HINTS                                     = 103,

          (**
           * Used in the resumable requests proposal to resume aborted PUT or
           * POST requests.
           *)
          { UNOFFICIAL CODE }
        { HTTP_CHECKPOINT                                      = 103, }

          (**
           * Standard response for successful HTTP requests. The actual response
           * will depend on the request method used. In a GET request, the
           * response will contain an entity corresponding to the requested
           * resource. In a POST request, the response will contain an entity
           * describing or containing the result of the action.
           *)
          HTTP_OK                                              = 200,

          (**
           * The request has been fulfilled, resulting in the creation of a new
           * resource.
           *)
          HTTP_CREATED                                         = 201,

          (**
           * The request has been accepted for processing, but the processing
           * has not been completed. The request might or might not be
           * eventually acted upon, and may be disallowed when processing
           * occurs.
           *)
          HTTP_ACCEPTED                                        = 202,

          (**
           * The server is a transforming proxy (e.g. a Web accelerator) that
           * received a 200 OK from its origin, but is returning a modified
           * version of the origin's response.
           *)
          HTTP_NON_AUTHORITATIVE_INFORMATION                   = 203,

          (**
           * The server successfully processed the request and is not returning
           * any content.
           *)
          HTTP_NO_CONTENT                                      = 204,

          (**
           * The server successfully processed the request, but is not returning
           * any content. Unlike a 204 response, this response requires that the
           * requester reset the document view.
           *)
          HTTP_RESET_CONTENT                                   = 205,

          (**
           * The server is delivering only part of the resource (byte serving)
           * due to a range header sent by the client. The range header is used
           * by HTTP clients to enable resuming of interrupted downloads, or
           * split a download into multiple simultaneous streams.
           *)
          HTTP_PARTIAL_CONTENT                                 = 206,

          (**
           * The message body that follows is by default an XML message and can
           * contain a number of separate response codes, depending on how many
           * sub-requests were made.
           *)
          HTTP_MULTI_STATUS                                    = 207,

          (**
           * The members of a DAV binding have already been enumerated in a
           * preceding part of the (multistatus) response, and are not being
           * included again.
           *)
          HTTP_ALREADY_REPORTED                                = 208,

          (**
           * Used as a catch-all error condition for allowing response bodies to
           * flow through Apache when ProxyErrorOverride is enabled. When
           * ProxyErrorOverride is enabled in Apache, response bodies that
           * contain a status code of 4xx or 5xx are automatically discarded by
           * Apache in favor of a generic response or a custom response
           * specified by the ErrorDocument directive.
           *)
          { UNOFFICIAL CODE }
          HTTP_THIS_IS_FINE__APACHE_WEB_SERVER                 = 218,

          (**
           * The server has fulfilled a request for the resource, and the
           * response is a representation of the result of one or more
           * instance-manipulations applied to the current instance.
           *)
          HTTP_IM_USED                                         = 226,

          (**
           * Indicates multiple options for the resource from which the client
           * may choose (via agent-driven content negotiation). For example,
           * this code could be used to present multiple video format options,
           * to list files with different filename extensions, or to suggest
           * word-sense disambiguation.
           *)
          HTTP_MULTIPLE_CHOICES                                = 300,

          (**
           * This and all future requests should be directed to the given URI.
           *)
          HTTP_MOVED_PERMANENTLY                               = 301,

          (**
           * Tells the client to look at (browse to) another URL. 302 has been
           * superseded by 303 and 307. This is an example of industry practice
           * contradicting the standard. The HTTP/1.0 specification (RFC 1945)
           * required the client to perform a temporary redirect (the original
           * describing phrase was "Moved Temporarily"), but popular browsers
           * implemented 302 with the functionality of a 303 See Other.
           * Therefore, HTTP/1.1 added status codes 303 and 307 to distinguish
           * between the two behaviours. However, some Web applications and
           * frameworks use the 302 status code as if it were the 303.
           *)
          HTTP_FOUND                                           = 302,

          (**
           * The response to the request can be found under another URI using
           * the GET method. When received in response to a POST
           * (or PUT/DELETE), the client should presume that the server has
           * received the data and should issue a new GET request to the given
           * URI.
           *)
          HTTP_SEE_OTHER                                       = 303,

          (**
           * Indicates that the resource has not been modified since the version
           * specified by the request headers If-Modified-Since or
           * If-None-Match. In such case, there is no need to retransmit the
           * resource since the client still has a previously-downloaded copy.
           *)
          HTTP_NOT_MODIFIED                                    = 304,

          (**
           * The requested resource is available only through a proxy, the
           * address for which is provided in the response. For security
           * reasons, many HTTP clients (such as Mozilla Firefox and Internet
           * Explorer) do not obey this status code.
           *)
          HTTP_USE_PROXY                                       = 305,

          (**
           * No longer used. Originally meant "Subsequent requests should use
           * the specified proxy."
           *)
          HTTP_SWITCH_PROXY                                    = 306,

          (**
           * In this case, the request should be repeated with another URI;
           * however, future requests should still use the original URI. In
           * contrast to how 302 was historically implemented, the request
           * method is not allowed to be changed when reissuing the original
           * request. For example, a POST request should be repeated using
           * another POST request.
           *)
          HTTP_TEMPORARY_REDIRECT                              = 307,

          (**
           * The request and all future requests should be repeated using
           * another URI. 307 and 308 parallel the behaviors of 302 and 301, but
           * do not allow the HTTP method to change. So, for example, submitting
           * a form to a permanently redirected resource may continue smoothly.
           *)
          HTTP_PERMANENT_REDIRECT                              = 308,

          (**
           * The server cannot or will not process the request due to an
           * apparent client error (e.g., malformed request syntax, size too
           * large, invalid request message framing, or deceptive request
           * routing).
           *)
          HTTP_BAD_REQUEST                                     = 400,

          (**
           * Similar to 403 Forbidden, but specifically for use when
           * authentication is required and has failed or has not yet been
           * provided. The response must include a WWW-Authenticate header field
           * containing a challenge applicable to the requested resource. See
           * Basic access authentication and Digest access authentication. 401
           * semantically means "unauthorised", the user does not have valid
           * authentication credentials for the target resource.
           * Note: Some sites incorrectly issue HTTP 401 when an IP address is
           * banned from the website (usually the website domain) and that
           * specific address is refused permission to access a website.
           *)
          HTTP_UNAUTHORIZED                                    = 401,

          (**
           * Reserved for future use. The original intention was that this code
           * might be used as part of some form of digital cash or micropayment
           * scheme, as proposed, for example, by GNU Taler, but that has not
           * yet happened, and this code is not usually used. Google Developers
           * API uses this status if a particular developer has exceeded the
           * daily limit on requests. Sipgate uses this code if an account does
           * not have sufficient funds to start a call. Shopify uses this code
           * when the store has not paid their fees and is temporarily disabled.
           *)
          HTTP_PAYMENT_REQUIRED                                = 402,

          (**
           * The request contained valid data and was understood by the server,
           * but the server is refusing action. This may be due to the user not
           * having the necessary permissions for a resource or needing an
           * account of some sort, or attempting a prohibited action (e.g.
           * creating a duplicate record where only one is allowed). This code
           * is also typically used if the request provided authentication via
           * the WWW-Authenticate header field, but the server did not accept
           * that authentication. The request SHOULD NOT be repeated.
           *)
          HTTP_FORBIDDEN                                       = 403,

          (**
           * The requested resource could not be found but may be available in
           * the future. Subsequent requests by the client are permissible.
           *)
          HTTP_NOT_FOUND                                       = 404,

          (**
           * A request method is not supported for the requested resource; for
           * example, a GET request on a form that requires data to be presented
           * via POST, or a PUT request on a read-only resource.
           *)
          HTTP_METHOD_NOT_ALLOWED                              = 405,

          (**
           * The requested resource is capable of generating only content not
           * acceptable according to the Accept headers sent in the request.
           *)
          HTTP_NOT_ACCEPTABLE                                  = 406,

          (**
           * The client must first authenticate itself with the proxy.
           *)
          HTTP_PROXY_AUTHENTIFICATION_REQUIRED                 = 407,

          (**
           * The server timed out waiting for the request. According to HTTP
           * specifications: "The client did not produce a request within the
           * time that the server was prepared to wait. The client MAY repeat
           * the request without modifications at any later time."
           *)
          HTTP_REQUEST_TIMEOUT                                 = 408,

          (**
           * Indicates that the request could not be processed because of
           * conflict in the current state of the resource, such as an edit
           * conflict between multiple simultaneous updates.
           *)
          HTTP_CONFLICT                                        = 409,

          (**
           * Indicates that the resource requested is no longer available and
           * will not be available again. This should be used when a resource
           * has been intentionally removed and the resource should be purged.
           * Upon receiving a 410 status code, the client should not request the
           * resource in the future. Clients such as search engines should
           * remove the resource from their indices. Most use cases do not
           * require clients and search engines to purge the resource, and a
           * "404 Not Found" may be used instead.
           *)
          HTTP_GONE                                            = 410,

          (**
           * The request did not specify the length of its content, which is
           * required by the requested resource.
           *)
          HTTP_LENGTH_REQUIRED                                 = 411,

          (**
           * The server does not meet one of the preconditions that the
           * requester put on the request header fields.
           *)
          HTTP_PRECONDITION_FAILED                             = 412,

          (**
           * The request is larger than the server is willing or able to
           * process. Previously called "Request Entity Too Large".
           *)
          HTTP_PAYLOAD_TOO_LARGE                               = 413,

          (**
           * The URI provided was too long for the server to process. Often the
           * result of too much data being encoded as a query-string of a GET
           * request, in which case it should be converted to a POST request.
           * Called "Request-URI Too Long" previously.
           *)
          HTTP_URI_TOO_LONG                                    = 414,

          (**
           * The request entity has a media type which the server or resource
           * does not support. For example, the client uploads an image as
           * image/svg+xml, but the server requires that images use a different
           * format.
           *)
          HTTP_UNSUPPORTED_MEDIA_TYPE                          = 415,

          (**
           * The client has asked for a portion of the file (byte serving), but
           * the server cannot supply that portion. For example, if the client
           * asked for a part of the file that lies beyond the end of the file.
           * Called "Requested Range Not Satisfiable" previously.
           *)
          HTTP_RANGE_NOT_SATISFIABLE                           = 416,

          (**
           * The server cannot meet the requirements of the Expect
           * request-header field.
           *)
          HTTP_EXPECTATION_FAILED                              = 417,

          (**
           * This code was defined in 1998 as one of the traditional IETF April
           * Fools' jokes, in RFC 2324, Hyper Text Coffee Pot Control Protocol,
           * and is not expected to be implemented by actual HTTP servers. The
           * RFC specifies this code should be returned by teapots requested to
           * brew coffee. This HTTP status is used as an Easter egg in some
           * websites, including Google.com.
           *)
          HTTP_IM_A_TEAPOT                                     = 418,

          (**
           * Not a part of the HTTP standard, 419 Authentication Timeout denotes
           * that previously valid authentication has expired. It is used as an
           * alternative to 401 Unauthorized in order to differentiate from
           * otherwise authenticated clients being denied access to specific
           * server resources.
           *)
          { UNOFFICIAL CODE }
        { HTTP_AUTHENTICATION_TIMEOUT                          = 419, }

          (**
           * Used by the Laravel Framework when a CSRF Token is missing or
           * expired.
           *)
          { UNOFFICIAL CODE }
          HTTP_PAGE_EXPIRED__LARAVEL_FRAMEWORK                 = 419,

          (**
           * Not part of the HTTP standard, but defined by Spring in the
           * HttpStatus class to be used when a method failed. This status code
           * is deprecated by Spring.
           *)
          { UNOFFICIAL CODE }
          HTTP_METHOD_FAILURE__SPRING_FRAMEWORK                = 420,

          (**
           * Not part of the HTTP standard, but returned by version 1 of the
           * Twitter Search and Trends API when the client is being rate
           * limited. Other services may wish to implement the 429 Too Many
           * Requests response code instead.
           *)
          { UNOFFICIAL CODE }
        { HTTP_ENHANCE_YOUR_CALM__TWITTER                      = 420, }

          (**
           * The request was directed at a server that is not able to produce a
           * response (for example because of connection reuse).
           *)
          HTTP_MISDIRECTED_REQUEST                             = 421,

          (**
           * The request was well-formed but was unable to be followed due to
           * semantic errors.
           *)
          HTTP_UNPROCESSABLE_ENTITY                            = 422,

          (**
           * The resource that is being accessed is locked.
           *)
          HTTP_LOCKED                                          = 423,

          (**
           * The request failed because it depended on another request and that
           * request failed (e.g., a PROPPATCH).
           *)
          HTTP_FAILED_DEPENDENCY                               = 424,

          (**
           * Indicates that the server is unwilling to risk processing a request
           * that might be replayed.
           *)
          HTTP_TOO_EARLY                                       = 425,

          (**
           * The client should switch to a different protocol such as TLS/1.0,
           * given in the Upgrade header field.
           *)
          HTTP_UPGRADE_REQUIRED                                = 426,

          (**
           * The origin server requires the request to be conditional. Intended
           * to prevent the 'lost update' problem, where a client GETs a
           * resource's state, modifies it, and PUTs it back to the server, when
           * meanwhile a third party has modified the state on the server,
           * leading to a conflict.
           *)
          HTTP_PRECONDITION_REQUIRED                           = 428,

          (**
           * The user has sent too many requests in a given amount of time.
           * Intended for use with rate-limiting schemes.
           *)
          HTTP_TOO_MANY_REQUESTS                               = 429,

          (**
           * Used by Shopify, instead of the 429 Too Many Requests response
           * code, when too many URLs are requested within a certain time frame.
           *)
          { UNOFFICIAL CODE }
          HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE__SHOPIFY        = 430,

          (**
           * The server is unwilling to process the request because either an
           * individual header field, or all the header fields collectively, are
           * too large.
           *)
          HTTP_REQUEST_HEADER_FIELDS_TOO_LARGE                 = 431,

          (**
           * A Microsoft extension. The client's session has expired and must
           * log in again.
           *)
          { UNOFFICIAL CODE }
          HTTP_LOGIN_TIMEOUT__MICROSOFT                        = 440,

          (**
           * A non-standard status code used to instruct nginx to close the
           * connection without sending a response to the client, most commonly
           * used to deny malicious or malformed requests.
           *)
          { UNOFFICIAL CODE }
          HTTP_CONNECTION_CLOSED_WITHOUT_RESPONSE__NGINX       = 444,

          (**
           * A Microsoft extension. The request should be retried after
           * performing the appropriate action. Often search-engines or custom
           * applications will ignore required parameters. Where no default
           * action is appropriate, the Aviongoo website sends a "HTTP/1.1 Retry
           * with valid parameters: param1, param2, . . ." response. The
           * applications may choose to learn, or not.
           *)
          { UNOFFICIAL CODE }
          HTTP_RETRY_WITH__MICROSOFT                           = 449,

          (**
           * A Microsoft extension. This error is given when Windows Parental
           * Controls are turned on and are blocking access to the given
           * webpage.
           *)
          { UNOFFICIAL CODE }
          HTTP_BLOCKED_BY_WINDOWS_PARENTAL_CONTROLS__MICROSOFT = 450,

          (**
           * A server operator has received a legal demand to deny access to a
           * resource or to a set of resources that includes the requested
           * resource. The code 451 was chosen as a reference to the novel
           * Fahrenheit 451 (see the Acknowledgements in the RFC).
           *)
          HTTP_UNAVAILABLE_FOR_LEGAL_REASONS                   = 451,

          (**
           * Client closed the connection with the load balancer before the idle
           * timeout period elapsed. Typically when client timeout is sooner
           * than the Elastic Load Balancer's timeout.
           *)
          { UNOFFICIAL CODE }
          HTTP_460__AWS_ELASTIC_LOAD_BALANCER                  = 460,

          (**
           * The load balancer received an X-Forwarded-For request header with
           * more than 30 IP addresses.
           *)
          { UNOFFICIAL CODE }
          HTTP_463__AWS_ELASTIC_LOAD_BALANCER                  = 463,

          (**
           * Nginx internal code similar to 431 but it was introduced earlier in
           * version 0.9.4 (on January 21, 2011).
           *)
          { UNOFFICIAL CODE }
          HTTP_REQUEST_HEADER_TOO_LARGE__NGINX                 = 494,

          (**
           * An expansion of the 400 Bad Request response code, used when the
           * client has provided an invalid client certificate.
           *)
          { UNOFFICIAL CODE }
          HTTP_SSL_SERTIFICATE_ERROR__NGINX                    = 495,

          (**
           * An expansion of the 400 Bad Request response code, used when a
           * client  certificate is required but not provided.
           *)
          { UNOFFICIAL CODE }
          HTTP_SSL_SERTIFICATE_REQUIRED__NGINX                 = 496,

          (**
           * An expansion of the 400 Bad Request response code, used when the
           * client has made a HTTP request to a port listening for HTTPS
           * requests.
           *)
          { UNOFFICIAL CODE }
          HTTP_REQUEST_SENT_TO_HTTPS_PORT__NGINX               = 497,

          (**
           * Returned by ArcGIS for Server. A code of 498 indicates an expired
           * or otherwise invalid token.
           *)
          { UNOFFICIAL CODE }
          HTTP_INVALID_TOKEN__ESRI                             = 498,

          (**
           * Returned by ArcGIS for Server. Code 499 indicates that a token is
           * required but was not submitted.
           *)
          { UNOFFICIAL CODE }
          HTTP_TOKEN_REQUIRED__ESRI                            = 499,

          (**
           * Used when the client has closed the request before the server could
           * send a response.
           *)
          { UNOFFICIAL CODE }
        { HTTP_CLIENT_CLOSED_REQUEST__NGINX                    = 499, }

          (**
           * A generic error message, given when an unexpected condition was
           * encountered and no more specific message is suitable.
           *)
          HTTP_INTERNAL_SERVER_ERROR                           = 500,

          (**
           * The server either does not recognize the request method, or it
           * lacks the ability to fulfil the request. Usually this implies
           * future availability (e.g., a new feature of a web-service API).
           *)
          HTTP_NOT_IMPLEMENTED                                 = 501,

          (**
           * The server was acting as a gateway or proxy and received an invalid
           * response from the upstream server.
           *)
          HTTP_BAD_GETEWAY                                     = 502,

          (**
           * The server cannot handle the request (because it is overloaded or
           * down for maintenance). Generally, this is a temporary state.
           *)
          HTTP_SERVICE_UNAVAIBLE                               = 503,

          (**
           * The server was acting as a gateway or proxy and did not receive a
           * timely response from the upstream server.
           *)
          HTTP_GATEWAY_TIMEOUT                                 = 504,

          (**
           * The server does not support the HTTP protocol version used in the
           * request.
           *)
          HTTP_VERSION_NOT_SUPPORTED                           = 505,

          (**
           * Transparent content negotiation for the request results in a
           * circular reference.
           *)
          HTTP_VARIANT_ALSO_NEGOTIATES                         = 506,

          (**
           * The server is unable to store the representation needed to complete
           * the request.
           *)
          HTTP_INSUFFICIENT_STORAGE                            = 507,

          (**
           * The server detected an infinite loop while processing the request
           * (sent instead of 208 Already Reported).
           *)
          HTTP_LOOP_DETECTED                                   = 508,

          (**
           * The server has exceeded the bandwidth specified by the server
           * administrator; this is often used by shared hosting providers to
           * limit the bandwidth of customers.
           *)
          { UNOFFICIAL CODE }
          HTTP_BANDWIDTH_LIMIT_EXCEEDED__APACHE_WEB_SERVER     = 509,

          (**
           * Further extensions to the request are required for the server to
           * fulfil it.
           *)
          HTTP_NOT_EXTENDED                                    = 510,

          (**
           * The client needs to authenticate to gain network access. Intended
           * for use by intercepting proxies used to control access to the
           * network (e.g., "captive portals" used to require agreement to Terms
           * of Service before granting full Internet access via a Wi-Fi
           * hotspot).
           *)
          HTTP_NETWORK_AUTHENTICATION_REQUIRED                 = 511,

          (**
           * This status code is not specified in any RFCs, but is used by
           * CloudFlare's reverse proxies to signal an "unknown connection issue
           * between CloudFlare and the origin web server" to a client in front
           * of the proxy.
           *)
          { UNOFFICIAL CODE }
          HTTP_ORIGIN_ERROR__CLOUDFLARE                        = 520,

          (**
           * This status code is not specified in any RFCs, but is used by
           * CloudFlare's reverse proxies to indicate that the origin webserver
           * refused the connection.
           *)
          { UNOFFICIAL CODE }
          HTTP_WEB_SERVER_IS_DOWN__CLOUDFLARE                  = 521,

          (**
           * This status code is not specified in any RFCs, but is used by
           * CloudFlare's reverse proxies to signal that a server connection
           * timed out.
           *)
          { UNOFFICIAL CODE }
          HTTP_CONNECTION_TIMED_OUT__CLOUDFLARE                = 522,

          (**
           * This status code is not specified in any RFCs, but is used by
           * CloudFlare's reverse proxies to signal a resource that has been
           * blocked by the administrator of the website or proxy itself.
           *)
          { UNOFFICIAL CODE }
          HTTP_PROXY_DECLINED_REQUEST__CLOUDFLARE              = 523,

          (**
           * This status code is not specified in any RFCs, but is used by
           * CloudFlare's reverse proxies to signal a network read timeout
           * behind the proxy to a client in front of the proxy.
           *)
          { UNOFFICIAL CODE }
          HTTP_A_TIMEOUT_OCCURED__CLOUDFLARE                   = 524,

          (**
           * Cloudflare could not negotiate a SSL/TLS handshake with the origin
           * server.
           *)
          { UNOFFICIAL CODE }
          HTTP_SSL_HANDSHAKE_FAILED__CLOUDFLARE                = 525,

          (**
           * Used by Cloudflare and Cloud Foundry's gorouter to indicate failure
           * to validate the SSL/TLS certificate that the origin server
           * presented.
           *)
          { UNOFFICIAL CODE }
          HTTP_INVALID_SSL_CERTIFICATE__CLOUDFLARE             = 526,

          (**
           * Error 527 indicates that the request timed out or failed after the
           * WAN connection had been established.
           *)
          { UNOFFICIAL CODE }
          HTTP_RAILGUN_ERROR__CLOUDFLARE                       = 527,

          (**
           * Used by the Pantheon web platform to indicate a site that has been
           * frozen due to inactivity.
           *)
          { UNOFFICIAL CODE }
          HTTP_SITE_IS_FROZEN__PATHEON                         = 530,

          (**
           * Error 530 indicates that the requested host name could not be  on
           * resolved the Cloudflare network to an origin server.
           *)
          { UNOFFICIAL CODE }
        { HTTP_ORIGIN_DNS_ERROR__CLOUDFLARE                    = 530, }

          (**
           * Used by some HTTP proxies to signal a network read timeout behind
           * the proxy to a client in front of the proxy.
           *)
          { UNOFFICIAL CODE }
          HTTP_NETWORK_READ_TIMEOUT_ERROR                      = 598,

          (**
           * This status code is not specified in any RFCs, but is used by some
           * HTTP proxies to signal a network connect timeout behind the proxy
           * to a client in front of the proxy.
           *)
          { UNOFFICIAL CODE }
          HTTP_NETWORK_CONNECT_TIMEOUT_ERROR                   = 599
        );

        THTTPVersion = (
          HTTP_VERSION_UNKNOWN                                 = 0,

          (**
           * Enforce HTTP 1.0 requests.
           *)
          HTTP_VERSION_1_0                  = Longint(CURL_HTTP_VERSION_1_0),

          (**
           * Enforce HTTP 1.1 requests.
           *)
          HTTP_VERSION_1_1                  = Longint(CURL_HTTP_VERSION_1_1),

          (**
           * Attempt HTTP 2 requests. libcurl will fall back to HTTP 1.1 if
           * HTTP 2 can't be negotiated with the server.
           *)
          HTTP_VERSION_2_0                  = Longint(CURL_HTTP_VERSION_2_0),

          (**
           * Attempt HTTP 2 over TLS (HTTPS) only. libcurl will fall back to
           * HTTP 1.1 if HTTP 2 can't be negotiated with the HTTPS server. For
           * clear text HTTP servers, libcurl will use 1.1.
           *)

          HTTP_VERSION_2TLS                 = Longint(CURL_HTTP_VERSION_2TLS),

          (**
           * Issue non-TLS HTTP requests using HTTP/2 without HTTP/1.1 Upgrade.
           * It requires prior knowledge that the server supports HTTP/2
           * straight away. HTTPS requests will still do HTTP/2 the standard way
           * with negotiated protocol version in the TLS handshake.
           *)
          HTTP_VERSION_2_PRIOR_KNOWLEDGE
            = Longint(CURL_HTTP_VERSION_2_PRIOR_KNOWLEDGE),

          (**
           * Setting this value will make libcurl attempt to use HTTP/3 directly
           * to server given in the URL. Note that this cannot gracefully
           * downgrade to earlier HTTP version if the server doesn't support
           * HTTP/3. For more reliably upgrading to HTTP/3, set the preferred
           * version to something lower and let the server announce its HTTP/3
           * support via Alt-Svc:. See TSession.HTTP.AltSvc.
           *)
          HTTP_VERSION_3_0                  = Longint(CURL_HTTP_VERSION_3)
        );

      TPostRedirect = (
        REDIRECT_POST_NONE,

        (**
         * Tells the library to respect RFC 7231 (section 6.4.2 to 6.4.4) and
         * not convert POST requests into GET requests when following a 301
         * redirection
         *)
        REDIRECT_POST_301,

        (**
         * Makes libcurl maintain the request method after a 302 redirect
         *)
        REDIRECT_POST_302,

        (**
         * Makes libcurl maintain the request method after a 303 redirect
         *)
        REDIRECT_POST_303,

        REDIRECT_POST_ALL
      );

      TPostRedirects = set of TPostRedirect;

      TEncoding = (
       (**
        * Non-compressed
        *)
        ENCODE_NONE,

        (**
         * Requests the server to compress its response using the zlib algorithm
         *)
        ENCODE_DEFLATE,

       (**
        * Requests the gzip algorithm
        *)
        ENCODE_GZIP,

       (**
        * Requests the brotli algorithm
        *)
        ENCODE_BR
      );

      TEncodings = set of TEncoding;

      { TAltSvc }
      (**
       * Setting for the alt-svc engine
       *)
      TAltSvc = (
        (**
         * No Alt-Svc treatment.
         *)
        ALTSVC_DISABLE                 = 0,

        (**
         * If an Alt-Svc: header is received, this instructs libcurl to switch
         * to one of those alternatives asap rather than to save it and use for
         * the next request. (Not currently supported).
         *)
        ALTSVC_IMMEDIATELY             = Longint(CURLALTSVC_IMMEDIATELY),

        (**
         * Do not write the alt-svc cache back to the file specified with
         * TSession.HTTP.AltSvcCacheFile even if it gets updated. By default a
         * file specified with that option will be read and written to as deemed
         * necessary.
         *)
        ALTSVC_READONLYFILE            = Longint(CURLALTSVC_READONLYFILE),

        (**
         * Accept alternative services offered over HTTP/1.1.
         *)
        ALTSVC_H1                      = Longint(CURLALTSVC_H1),

        (**
         * Accept alternative services offered over HTTP/2. This will only be
         * used if libcurl was also built to actually support HTTP/2, otherwise
         * this bit will be ignored.
         *)
        ALTSVC_H2                      = Longint(CURLALTSVC_H2),

        (**
         * Accept alternative services offered over HTTP/3. This will only be
         * used if libcurl was also built to actually support HTTP/3, otherwise
         * this bit will be ignored.
         *)
        ALTSVC_H3                      = Longint(CURLALTSVC_H3)
      );

      TAltSvcs = set of TAltSvc;

      private
        FHandle : CURL;
        FCookie : THTTPCookie;

        procedure SetUserAgent (AAgent : string);
        procedure SetAutoReferer (AUpdateHeaders : Boolean);
        procedure SetHTTPAuth (AMethod : TSecurityProperty.TAuthMethods);
        procedure SetUnrestrictedAuth (ASend : Boolean);
        procedure SetPostRedirect (ARedirect : TPostRedirects);
        procedure SetPutMethod (AEnable : Boolean);
        procedure SetPostMethod (AEnable : Boolean);
        procedure SetPostFields (AData : string);
        procedure SetPostFieldsSize (ASize : Longint);
        procedure SetAcceptEncoding (AEncodings : TEncodings);
        procedure SetTransferEncoding (AEnable : Boolean);
        procedure SetReferer (AWhere : string);
        procedure SetAltSvcCacheFile (AFile : string);
        procedure SetAltSvcCtrl (AAltSvc : TAltSvcs);
        procedure SetGetMethod (AEnable : Boolean);
        procedure SetRequestTarget (ATarget : string);
        procedure SetHttpVersion (AVersion : THTTPVersion);
        procedure SetHttp09Allowed (AEnable : Boolean);
        procedure SetIgnoreContentLength (AIgnore : Boolean);
        procedure SetHttpContentDecoding (AEnable : Boolean);
        procedure SetHttpTransferDecoding (AEnable : Boolean);
        procedure SetExpect100Timeout (ATimeout : TTimeInterval);
        procedure SetPipeWait (AEnable : Boolean);
        procedure SetStreamDepends (ADependHandle : CURL);
        procedure SetStreamDependsExclusive (ADependHandle : CURL);
        procedure SetStreamWeight (AWeight : Longint);
        procedure SetRange (ARange : string);
        procedure SetStartTransferFrom (AFrom : curl_off_t);
        procedure SetCustomRequest (ARequest : string);
        procedure SetTimeModification (AEnable : Boolean);
        procedure SetMaxDownloadFileSize (ASize : TDataSize);
        procedure SetConnectOnly (AEnable : Boolean);
        procedure SetKeepSendingOnError (AKeepSending : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        property Cookie : THTTPCookie read FCookie write FCookie;

        (**
         * Set HTTP user-agent header
         *
         * It will be used to set the User-Agent: header in the HTTP request
         * sent to the remote server. This can be used to fool servers or
         * scripts.
         *)
        property UserAgent : string write SetUserAgent;

        (**
         * Automatically update the referer header
         *
         * When enabled, libcurl will automatically set the Referer: header
         * field in HTTP requests where it follows a Location: redirect.
         *)
        property AutoReferer : Boolean write SetAutoReferer default True;

        (**
         * Tell libcurl which authentication method(s) you want it to use
         * speaking to the remote server.
         *)
        property HTTPAuth : TSecurityProperty.TAuthMethods
          write SetHTTPAuth default [AUTH_BASIC];

        (**
         * Send credentials to other hosts too
         *
         * Make libcurl continue to send authentication (user+password)
         * credentials when following locations, even when hostname changed.
         * By default, libcurl will only send given credentials to the initial
         * host name as given in the original URL, to avoid leaking username +
         * password to other sites.
         *)
        property UnrestrictedAuth : Boolean write SetUnrestrictedAuth;

        (**
         * How to act on an HTTP POST redirect
         *)
        property PostRedirect : TPostRedirects write SetPostRedirect
          default [REDIRECT_POST_NONE];

        (**
         * Make an HTTP PUT request
         *
         * This option is deprecated since version 7.12.1. Use Upload!
         *)
        property Put : Boolean write SetPutMethod default False;

        (**
         * Request an HTTP POST
         *)
        property Post : Boolean write SetPostMethod default False;

        (**
         * Specify data to POST to server
         *
         * Pass the full data to send in an HTTP POST operation. You must make
         * sure that the data is formatted the way you want the server to
         * receive it. libcurl will not convert or encode it for you in any way.
         * For example, the web server may assume that this data is url-encoded.
         *)
        property PostFields : string write SetPostFields;

        (**
         * Size of POST data
         *
         * If you want to post data to the server without having libcurl do a
         * strlen() to measure the data size, this option must be used. When
         * this option is used you can post fully binary data, which otherwise
         * is likely to fail. If this size is set to -1, the library will use
         * strlen() to get the size.
         *)
        property PostFieldsSize : Longint write SetPostFieldsSize default -1;

        (**
         * Enables automatic decompression of HTTP
         *
         * Sets the contents of the Accept-Encoding: header sent in an HTTP
         * request, and enables decoding of a response when a Content-Encoding:
         * header is received.
         * libcurl potentially supports several different compressed encodings
         * depending on what support that has been built-in.
         * Alternatively, you can specify exactly the encoding or list of
         * encodings you want in the response. Four encodings are supported:
         * identity, meaning non-compressed, deflate which requests the server
         * to compress its response using the zlib algorithm, gzip which
         * requests the gzip algorithm and (since curl 7.57.0) br which is
         * brotli.
         *)
        property AcceptEncoding : TEncodings write SetAcceptEncoding
          default [ENCODE_NONE];

        (**
         * Ask for HTTP Transfer Encoding
         *
         * Adds a request for compressed Transfer Encoding in the outgoing HTTP
         * request. If the server supports this and so desires, it can respond
         * with the HTTP response sent using a compressed Transfer-Encoding that
         * will be automatically uncompressed by libcurl on reception.
         * Transfer-Encoding differs slightly from the Content-Encoding you ask
         * for with AcceptEncoding in that a Transfer-Encoding is strictly meant
         * to be for the transfer and thus MUST be decoded before the data
         * arrives in the client. Traditionally, Transfer-Encoding has been much
         * less used and supported by both HTTP clients and HTTP servers.
         *)
        property TransferEncoding : Boolean write SetTransferEncoding
          default False;

        (**
         * Set the HTTP referer header
         *
         * It will be used to set the Referer: header in the http request sent
         * to the remote server. This can be used to fool servers or scripts.
         * You can also set any custom header with Header.
         *)
        property Referer : string write SetReferer;

        (**
         * Set alt-svc cache file name
         *
         * Pass in a pointer to a filename to instruct libcurl to use that file
         * as the Alt-Svc cache to read existing cache contents from and
         * possibly also write it back to a after a transfer.
         * Specify a blank file name ("") to make libcurl not load from a file
         * at all.
         *)
        property AltSvcCacheFile : string write SetAltSvcCacheFile;

        (**
         * Control alt-svc behavior
         *
         * Populate the correct set of features to instruct libcurl how to
         * handle Alt-Svc for the transfers using this handle.
         * libcurl will only accept Alt-Svc headers over a secure transport,
         * meaning HTTPS. It will also only complete a request to an alternative
         * origin if that origin is properly hosted over HTTPS. These
         * requirements are there to make sure both the source and the
         * destination are legitimate.
         * Setting any bit will enable the alt-svc engine.
         *)
        property AltSvcCtrl : TAltSvcs write SetAltSvcCtrl;

        (**
         * Ask for an HTTP GET request
         *
         * This forces the HTTP request to get back to using GET. Usable if a
         * POST, HEAD, PUT, etc has been used previously using the same curl
         * handle.
         * When setting Get, it will automatically set NoBody to False and
         * Upload to False.
         *)
        property Get : Boolean write SetGetMethod;

        (**
         * Specify an alternative target for this request
         *
         * Pass a string which libcurl uses in the upcoming request instead of
         * the path as extracted from the URL.
         *)
        property RequestTarget : string write SetRequestTarget;

        (**
         * Specify HTTP protocol version to use
         *
         * Note that the HTTP version is just a request. libcurl will still
         * prioritize to re-use an existing connection so it might then re-use a
         * connection using a HTTP version you haven't asked for.
         *)
        property HTTPVersion : THTTPVersion write SetHttpVersion;

        (**
         * Allow HTTP/0.9 response
         *
         * A HTTP/0.9 response is a server response entirely without headers and
         * only a body. You can connect to lots of random TCP services and still
         * get a response that curl might consider to be HTTP/0.9!
         *)
        property HTTP09Allow : Boolean write SetHttp09Allowed;

        (**
         * Ignore content length
         *
         * Ignore the Content-Length header in the HTTP response and ignore
         * asking for or relying on it for FTP transfers.
         * This is useful for HTTP with Apache 1.x (and similar servers) which
         * will report incorrect content length for files over 2 gigabytes. If
         * this option is used, curl will not be able to accurately report
         * progress, and will simply stop the download when the server ends the
         * connection.
         * It is also useful with FTP when for example the file is growing while
         * the transfer is in progress which otherwise will unconditionally
         * cause libcurl to report error.
         * Only use this option if strictly necessary.
         *)
        property IgnoreContentLength : Boolean write SetIgnoreContentLength
          default False;

        (**
         * Enable/disable HTTP content decoding
         *
         * Libcurl has no default content decoding but requires you to use
         * AcceptEncoding for that.
         *)
        property ContentDecoding : Boolean write SetHttpContentDecoding
          default True;

        (**
         * Enable/disable HTTP transfer decoding
         *
         * libcurl does chunked transfer decoding by default unless this option
         * is set False;
         *)
        property TransferDecoding : Boolean write SetHttpTransferDecoding
          default True;

        (**
         * Timeout for Expect: 100 - CONTINUE response
         *
         * Tell libcurl the number of milliseconds to wait for a server response
         * with the HTTP status 100 (Continue), 417 (Expectation Failed) or
         * similar after sending an HTTP request containing an Expect:
         * 100-continue header. If this times out before a response is received,
         * the request body is sent anyway.
         *)
        property Expect100Timeout : TTimeInterval write SetExpect100Timeout;

        (**
         * Wait for pipelining/multiplexing
         *
         * Tell libcurl to prefer to wait for a connection to confirm or deny
         * that it can do pipelining or multiplexing before continuing.
         * When about to perform a new transfer that allows pipelining or
         * multiplexing, libcurl will check for existing connections to re-use
         * and pipeline on. If no such connection exists it will immediately
         * continue and create a fresh new connection to use.
         * By setting this option to True - and having CURLMOPT_PIPELINING
         * enabled for the multi handle this transfer is associated with -
         * libcurl will instead wait for the connection to reveal if it is
         * possible to pipeline/multiplex on before it continues. This enables
         * libcurl to much better keep the number of connections to a minimum
         * when using pipelining or multiplexing protocols.
         * The effect thus becomes that with this option set, libcurl prefers to
         * wait and re-use an existing connection for pipelining rather than the
         * opposite: prefer to open a new connection rather than waiting.
         * The waiting time is as long as it takes for the connection to get up
         * and for libcurl to get the necessary response back that informs it
         * about its protocol and support level.
         *)
        property PipeWait : Boolean write SetPipeWait default False;

        (**
         * Set stream this transfer depends on
         *
         * Pass a CURL pointer in dephandle to identify the stream within the
         * same connection that this stream is depending upon. This option
         * clears the exclusive bit and is mutually exclusive to the
         * StreamDependsExclusive option.
         * The spec says "Including a dependency expresses a preference to
         * allocate resources to the identified stream rather than to the
         * dependent stream."
         * This option can be set during transfer.
         * dephandle must not be the same as handle, that will cause this
         * function to return an error. It must be another easy handle, and it
         * also needs to be a handle of a transfer that will be sent over the
         * same HTTP/2 connection for this option to have an actual effect.
         *)
        property StreamDepends : CURL write SetStreamDepends;

        (**
         * Set stream this transfer depends on exclusively
         *
         * Pass a CURL pointer in dephandle to identify the stream within the
         * same connection that this stream is depending upon exclusively. That
         * means it depends on it and sets the Exclusive bit.
         * The spec says "Including a dependency expresses a preference to
         * allocate resources to the identified stream rather than to the
         * dependent stream."
         * Setting a dependency with the exclusive flag for a reprioritized
         * stream causes all the dependencies of the new parent stream to become
         * dependent on the reprioritized stream.
         * This option can be set during transfer.
         * dephandle must not be the same as handle, that will cause this
         * function to return an error. It must be another easy handle, and it
         * also needs to be a handle of a transfer that will be sent over the
         * same HTTP/2 connection for this option to have an actual effect.
         *)
        property StreamDependsExclusive : CURL write SetStreamDependsExclusive;

        (**
         * Set numerical stream weight
         *
         * Set the long weight to a number between 1 and 256.
         * When using HTTP/2, this option sets the individual weight for this
         * particular stream used by the easy handle. Setting and using weights
         * only makes sense and is only usable when doing multiple streams over
         * the same connections, which thus implies that you use
         * CURLMOPT_PIPELINING.
         * This option can be set during transfer and will then cause the
         * updated weight info get sent to the server the next time an HTTP/2
         * frame is sent to the server.
         * See section 5.3 of RFC 7540 for protocol details:
         * https://httpwg.github.io/specs/rfc7540.html#StreamPriority
         * Streams with the same parent should be allocated resources
         * proportionally based on their weight. So if you have two streams
         * going, stream A with weight 16 and stream B with weight 32, stream B
         * will get two thirds (32/48) of the available bandwidth (assuming the
         * server can send off the data equally for both streams).
         *)
        property StreamWeight : Longint write SetStreamWeight;

        (**
         * Set byte range to request
         *
         * Pass a char * as parameter, which should contain the specified range
         * you want to retrieve. It should be in the format "X-Y", where either
         * X or Y may be left out and X and Y are byte indexes.
         * HTTP transfers also support several intervals, separated with commas
         * as in "X-Y,N-M". Using this kind of multiple intervals will cause the
         * HTTP server to send the response document in pieces (using standard
         * MIME separation techniques). Unfortunately, the HTTP standard (RFC
         * 7233 section 3.1) allows servers to ignore range requests so even
         * when you set Range for a request, you may end up getting the full
         * response sent back.
         * For HTTP PUT uploads this option should not be used, since it may
         * conflict with other options. If you need to upload arbitrary parts of
         * a file (like for Amazon's web services) support is limited. We
         * suggest set resume position using CURLOPT_RESUME_FROM, set end
         * (resume+size) position using CURLOPT_INFILESIZE and seek to the
         * resume position before initiating the transfer for each part. For
         * more information refer to
         * https://curl.haxx.se/mail/lib-2019-05/0012.html
         *)
        property Range : string write SetRange;

        (**
         * Set a point to resume transfer from
         *
         * It contains the offset in number of bytes that you want the transfer
         * to start from. Set this option to 0 to make the transfer start from
         * the beginning (effectively disabling resume).
         *)
        property StartTransferFrom : curl_off_t write SetStartTransferFrom
          default 0;

        (**
         * Custom string for request
         *
         * When you change the request method by setting CustomRequest to
         * something, you don't actually change how libcurl behaves or acts in
         * regards to the particular request method, it will only change the
         * actual string sent in the request.
         * Instead of GET or HEAD when performing HTTP based requests. This is
         * particularly useful, for example, for performing an HTTP DELETE
         * request.
         * For example:
         * When you tell libcurl to do a HEAD request, but then specify a GET
         * though a custom request libcurl will still act as if it sent a HEAD.
         * To switch to a proper HEAD use CURLOPT_NOBODY, to switch to a proper
         * POST use CURLOPT_POST or CURLOPT_POSTFIELDS and to switch to a proper
         * GET use CURLOPT_HTTPGET.
         * Many people have wrongly used this option to replace the entire
         * request with their own, including multiple headers and POST contents.
         * While that might work in many cases, it will cause libcurl to send
         * invalid requests and it could possibly confuse the remote server
         * badly. Use CURLOPT_POST and CURLOPT_POSTFIELDS to set POST data. Use
         * CURLOPT_HTTPHEADER to replace or extend the set of headers sent by
         * libcurl. Use CURLOPT_HTTP_VERSION to change HTTP version.
         *)
        property CustomRequest : string write SetCustomRequest;

        (**
         * Get the modification time of the remote resource
         *
         * If it is True, libcurl will attempt to get the modification time of
         * the remote document in this operation. This requires that the remote
         * server sends the time or replies to a time querying command. The
         * curl_easy_getinfo function with the CURLINFO_FILETIME argument can be
         * used after a transfer to extract the received time (if any).
         *)
        property TimeModification : Boolean write SetTimeModification;

        (**
         * Maximum file size allowed to download
         *
         * This allows you to specify the maximum size (in bytes) of a file to
         * download. If the file requested is found larger than this value, the
         * transfer will not start and CURLE_FILESIZE_EXCEEDED will be returned.
         * The file size is not always known prior to download, and for such
         * files this option has no effect even if the file transfer ends up
         * being larger than this given limit. This concerns both FTP and HTTP
         * transfers.
         *)
        property MaxDownloadFileSize : TDataSize write SetMaxDownloadFileSize;

        (**
         * Stop when connected to target server
         *
         * Tells the library to perform all the required proxy authentication
         * and connection setup, but no data transfer, and then return.
         * The option can be used to simply test a connection to a server, but
         * is more useful when used with the CURLINFO_ACTIVESOCKET option to
         * curl_easy_getinfo as the library can set up the connection and then
         * the application can obtain the most recently used socket for special
         * data transfers.
         * Transfers marked connect only will not reuse any existing connections
         * and connections marked connect only will not be allowed to get
         * reused.
         *)
        property ConnectOnly : Boolean write SetConnectOnly default False;

        (**
         * Keep sending on early HTTP response >= 300
         *
         * Tells the library to keep sending the request body if the HTTP code
         * returned is equal to or larger than 300. The default action would be
         * to stop sending and close the stream or connection.
         *)
        property KeepSendingOnError : Boolean write SetKeepSendingOnError
          default False;
      end;

      { TIMAPProperty }

      TIMAPProperty = class
      private
        FHandle : CURL;

        procedure SetLoginOptions (AOptions : string);
        procedure SetSASLAuthzid (AAuthzid : string);
        procedure SetSASLIR (ASend : Boolean);
        procedure SetXOAuth2Bearer (AToken : string);
        procedure SetCustomRequest (ARequest : string);
        procedure SetConnectOnly (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Set login options
         *
         * For more information about the login options please see RFC 2384, RFC
         * 5092 and IETF draft draft-earhart-url-smtp-00.txt
         * LoginOptions can be used to set protocol specific login options, such
         * as the preferred authentication mechanism via "AUTH=NTLM" or
         * "AUTH=*", and should be used in conjunction with the Username option.
         *)
        property LoginOptions : string write SetLoginOptions;

        (**
         * Authorisation identity (identity to act as)
         *
         * Authorisation identity (authzid) for the transfer. Only applicable to
         * the PLAIN SASL authentication mechanism where it is optional.
         * When not specified only the authentication identity (authcid) as
         * specified by the username will be sent to the server, along with the
         * password. The server will derive a authzid from the authcid when not
         * provided, which it will then uses internally.
         * When the authzid is specified, the use of which is server dependent,
         * it can be used to access another user's inbox, that the user has been
         * granted access to, or a shared mailbox for example.
         *)
        property SASLAuthzid : string write SetSASLAuthzid;

        (**
         * Enable/disable sending initial response in first packet
         *
         * curl will send the initial response to the server in the first
         * authentication packet in order to reduce the number of ping pong
         * requests. Only applicable to the following supporting SASL
         * authentication mechanisms:
         * Login * Plain * GSSAPI * NTLM * OAuth 2.0
         *)
        property SASLInitialResponse : Boolean write SetSASLIR default False;

        (**
         * Specify OAuth 2.0 access token
         *
         * OAuth 2.0 Bearer Access Token for use with HTTP, IMAP, POP3 and SMTP
         * servers that support the OAuth 2.0 Authorization Framework.
         *)
        property XOAuth2BearerToken : string write SetXOAuth2Bearer;

        (**
         * Custom string for request
         *
         * When you change the request method by setting CustomRequest to
         * something, you don't actually change how libcurl behaves or acts in
         * regards to the particular request method, it will only change the
         * actual string sent in the request.
         * Instead of LIST when issuing IMAP based requests.
         *)
        property CustomRequest : string write SetCustomRequest;

        (**
         * Stop when connected to target server
         *
         * Tells the library to perform all the required proxy authentication
         * and connection setup, but no data transfer, and then return.
         * The option can be used to simply test a connection to a server, but
         * is more useful when used with the CURLINFO_ACTIVESOCKET option to
         * curl_easy_getinfo as the library can set up the connection and then
         * the application can obtain the most recently used socket for special
         * data transfers.
         * Transfers marked connect only will not reuse any existing connections
         * and connections marked connect only will not be allowed to get
         * reused.
         *)
        property ConnectOnly : Boolean write SetConnectOnly default False;
      end;

      { TFTPProperty }

      TFTPProperty = class
      public
        type
          TFTPStatusCode = (
          (**
           * Error status code, not possible as normal!
           *)
          FTP_STATUS_UNKNOWN                                   = 0,

          (**
           * 1xx Positive Preliminary reply
           *
           * The requested action is being initiated; expect another reply
           * before proceeding with a new command. (The user-process sending
           * another command before the completion reply would be in violation
           * of protocol; but server-FTP processes should queue any commands
           * that arrive while a preceding command is in progress.) This type of
           * reply can be used to indicate that the command was accepted and the
           * user-process may now pay attention to the data connections, for
           * implementations where simultaneous monitoring is difficult. The
           * server-FTP process may send at most, one 1xx reply per command.
           *)

           (**
            * The requested action is being initiated, expect another reply
            * before proceeding with a new command.
            *)
          FTP_REQUEST_INIT_WAIT_NEXT_RESPONSE                  = 100,

          (**
           * Restart marker replay. In this case, the text is exact and not left
           * to the particular implementation; it must read: MARK yyyy = mmmm
           * where yyyy is User-process data stream marker, and mmmm server's
           * equivalent marker (note the spaces between markers and "=").
           *)
          FTP_RESTART_MARKER_REPLY                             = 110,

          (**
           * Service ready in nnn minutes.
           *
           * This indicates that the server accepted and has begun the
           * processing of the command. The client should wait until another
           * response is received before proceeding.
           *)
          FTP_BEGUN_PROCESSING_WAIT_NEXT_RESPONSE              = 120,

          (**
           * Data connection already open; transfer starting.
           *
           * The server may use this code in response to a command initiating a
           * file transfer if the data connection is already established. After
           * sending or receiving this response, the server or client may begin
           * sending data over the data connection.
           *)
           FTP_DATA_CONNECTION_ALREADY_OPENED                  = 125,

          (**
           * File status okay; about to open data connection.
           *
           * The server may use this reply code in response to a command
           * initiating a file transfer before establishing the data connection
           * over which the file transfer will occur. A PASV or PORT command
           * should have been issued prior to the command receiving the 150
           * response code. After sending or receiving this response, the
           * server/client may begin sending data over the data connection.
           *)
          FTP_INIT_FILE_TRANSFER                               = 150,

          (**
           * 2xx Positive Completion reply
           *
           * The requested action has been successfully completed. A new request
           * may be initiated.
           *)

          (**
           * The requested action has been successfully completed.
           *
           * A server issues a 200 response code if a command is accepted and
           * successfully processed.
           *)
          FTP_SUCCESS_COMPLETE                                 = 200,

          (**
           * Command not implemented, superfluous at this site.
           *
           * A 202 response is meant to indicate that the command is recognized
           * but not implemented by the server because it has no use or meaning
           * to the server. It is considered a successful reply because the
           * client can continue its FTP transaction as if the command was
           * successfully completed on the server.
           *)
          FTP_COMMAND_NOT_IMPLEMENTED                          = 202,

          (**
           * System status, or system help reply.
           *
           * A 211 code is given in response to commands asking for status or
           * help from the server. The information contained along with the
           * response is intended for user consumption and rarely has a meaning
           * to the client process itself.
           *)
          FTP_SYSTEM_STATUS                                    = 211,

          (**
           * Directory status.
           *
           * A 212 code is given in response to a command asking for directory
           * status information. A STAT command that includes a path parameter
           * is one command where a 212 response would be expected.
           *)
          FTP_DIRECTORY_STATUS_INFORMATION                     = 212,

          (**
           * File status.
           *
           * A 213 code may be given in response to a command asking for status
           * information on a file transfer. A STAT command issued during a file
           * transfer is one command where a 213 response code would be
           * expected.
           *)
          FTP_FILE_TRANSFER_STATUS_INFORMATION                 = 213,

          (**
           * Help message. Explains how to use the server or the meaning of a
           * particular non-standard command. This reply is useful only to the
           * human user.
           *
           * A 214 code is used in response to the HELP command to provide
           * information on how to use the server or the meaning of a particular
           * non-standard command. This response is useful only to the human
           * user and commonly contains a list of commands recognized by the
           * server.
           *)
          FTP_HELP_INFORMATION                                 = 214,

          (**
           * NAME system type. Where NAME is an official system name from the
           * registry kept by IANA.
           *
           * A 215 code is given in response to the SYST command. The response
           * should contain an official system name from the list in the
           * Assigned Numbers
           *)
          FTP_SYSTEM_NAME                                      = 215,

          (**
           * Service ready for new user.
           *
           * A 220 code is sent in response to a new user connecting to the FTP
           * server to indicate that the server is ready for the new client. It
           * can also be sent in response to a REIN command, which is meant to
           * reset the connection to the moment the client first connected to
           * the server.
           *)
          FTP_SERVER_READY                                     = 220,

          (**
           * Service closing control connection.
           *
           * A 221 code is sent over the control connection in response to the
           * client's QUIT command. It is sent immediately before the control
           * connection is closed by the server.
           *)
          FTP_DATA_CONNECTION_CLOSED                           = 221,

          (**
           * Data connection open; no transfer in progress.
           *
           * A 225 code is sent in response to the ABOR command when the data
           * connection is still open but there is no file transfer in progress
           * to abort.
           *)
          FTP_DATA_CONNECTION_OPEN_NO_TRANSFER                 = 225,

          (**
           * Closing data connection. Requested file action successful (for
           * example, file transfer or file abort).
           *
           * A 226 reply code is sent by the server before closing the data
           * connection after successfully processing the previous client
           * command affecting the data connection. In most cases, it signals
           * the completion of a file transfer. It can also be sent in response
           * to the ABOR command, which signals that the current file transfer
           * was successfully terminated.
           *)
          FTP_DATA_CONNECTION_CLOSE                            = 226,

          (**
           * Entering Passive Mode (h1,h2,h3,h4,p1,p2).
           *
           * A 227 code is the response given by the server to the PASV command.
           * It indicates that the server is ready for the client to connect to
           * it for the purpose of establishing a data connection. The format of
           * this response is important because the client software must be
           * capable of parsing out the connection information it contains. The
           * values h1 to h4 are the IP addresses that the server is listening
           * on.
           *)
          FTP_ENTERING_PASSIVE_MODE                            = 227,

          (**
           * Entering Long Passive Mode (long address, port).
           *)
          FTP_ENTERING_LONG_PASSIVE_MODE                       = 228,

          (**
           * Entering Extended Passive Mode (|||port|).
           *)
          FTP_ENTERING_EXTENDED_PASSIVE_MODE                   = 229,

          (**
           * User logged in, proceed. Logged out if appropriate.
           *
           * The server sends a 230 code in response to a command that has
           * provided sufficient credentials to the server to grant the user
           * access to the FTP server.
           *)
          FTP_USER_LOGGEDIN                                    = 230,

          (**
           * User logged out; service terminated.
           *)
          FTP_USER_LOGGEDOUT                                   = 231,

          (**
           * Logout command noted, will complete when transfer done.
           *
           * A 232 code may be sent in response to a USER command if the server
           * is willing to allow the user access based on previously exchanged
           * security data.
           *)
          FTP_LOGOUT_NOTED                                     = 232,

          (**
           * Specifies that the server accepts the authentication mechanism
           * specified by the client, and the exchange of security data is
           * complete. A higher level nonstandard code created by Microsoft.
           *
           * A 234 code is sent in response to the AUTH command when the
           * requested security mechanism is accepted and negotiation of the
           * secured connection can begin.
           *)
          FTP_SECURITY_ACCEPTED_NEGOTIATION_CONNECTION_BEGIN   = 234,

          (**
           * Requested file action okay, completed.
           *
           * A 250 code is sent in response to the successful completion of a
           * file related command or user working directory related command.
           *)
          FTP_RESPONSE_SUCCESSFUL_COMPLETION                   = 250,

          (**
           * "PATHNAME" created.
           *
           * A 257 code is used as a successful response to the creation of a
           * new directory from the MKD command or in response to a PWD command.
           *)
          FTP_DIRECTORY_CREATE_SUCCESSFUL                      = 257,

          (**
           * 3xx Positive Intermediate reply
           *
           * The command has been accepted, but the requested action is being
           * held in abeyance, pending receipt of further information. The user
           * should send another command specifying this information. This reply
           * is used in command sequence groups.
           *)

          (**
           * The command has been accepted, but the requested action is on hold,
           * pending receipt of further information.
           *)
          FTP_ACCEPTED_REQUEST_HOLD                            = 300,

          (**
           * User name okay, need password.
           *
           * A 331 code is sent in response to the USER command when a password
           * is required for the login to continue. It is considered as a
           * positive intermediate response and the client should immediately
           * respond with a PASS command.
           *)
          FTP_USERNAME_OK                                      = 331,

          (**
           * Need account for login.
           *
           * A 332 code is sent in response to a login-related command where an
           * account is required to continue the login process. It is considered
           * as a positive intermediate response and the client should
           * immediately follow with an ACCT command.
           *)
          FTP_NEED_ACCOUNT_TO_LOGIN                            = 332,

          (**
           * Requested file action pending further information
           *
           * A 350 response code is sent by the server in response to a
           * file-related command that requires further commands in order for
           * the operation to be completed. The original FTP specification
           * identifies two instances where this reply code can be used. The
           * first is in response to a REST command, which would indicate that
           * the server has received the restart marker and is pending the
           * initiation of file transfer to resume the transfer at the marker
           * point. The second is in positive response to the RNFR command where
           * the server is waiting for an RNTOcommand to complete the file
           * rename operation.
           *)
          FTP_NEED_INFORMATION                                 = 350,

          (**
           * 4xx Transient Negative Completion reply
           *
           * The command was not accepted and the requested action did not take
           * place, but the error condition is temporary and the action may be
           * requested again. The user should return to the beginning of the
           * command sequence, if any. It is difficult to assign a meaning to
           * "transient", particularly when two distinct sites (Server- and
           * User-processes) have to agree on the interpretation. Each reply in
           * the 4xx category might have a slightly different time value, but
           * the intent is that the user-process is encouraged to try again. A
           * rule of thumb in determining if a reply fits into the 4xx or the
           * 5xx (Permanent Negative) category is that replies are 4xx if the
           * commands can be repeated without any change in command form or in
           * properties of the User or Server (e.g., the command is spelled the
           * same with the same arguments used; the user does not change his
           * file access or user name; the server does not put up a new
           * implementation.)
           *)

          (**
           * The command was not accepted and the requested action did not take
           * place, but the error condition is temporary and the action may be
           * requested again.
           *)
          FTP_NOT_ACCEPTED                                     = 400,

          (**
           * Service not available, closing control connection. This may be a
           * reply to any command if the service knows it must shut down.
           *
           * A 421 response code indicates that while the service is still
           * running, the service is unavailable at the time of connection. It
           * indicates that the server will be restarting as soon as it finishes
           * processing pending operations (usually any file transfers currently
           * in progress). It is considered a transient negative response, which
           * means the client is encouraged to issue the same command again at a
           * later time when it can be accepted.
           *)
          FTP_SERVICE_NOT_AVAILABLE                            = 421,

          (**
           * Can't open data connection.
           *
           * A 425 response code may be sent in response to any command
           * requiring the usage of a data connection if the server is unable to
           * open a data connection. This is considered a transient negative
           * reply as it is considered to be a temporary condition. It may
           * indicate that the server does not immediately have the resources
           * available to open a data connection. In this case, the client is
           * encouraged to restart the FTP transaction and try again.
           *)
          FTP_DATA_CONNECTION_NOT_AVAILABLE                    = 425,

          (**
           * Connection closed; transfer aborted.
           *
           * A 426 response code may be sent in response to any command
           * requiring the usage of a data connection. It is considered a
           * transient negative reply as it is considered to be a temporary
           * condition. It is usually sent when the data connection is
           * unexpectedly closed before the completion of a data transfer. In
           * this case, the client is encouraged to restart the FTP transaction
           * and try again.
           *)
          FTP_TRANSFER_ABORTED                                 = 426,

          (**
           * Invalid username or password
           *)
          FTP_INVALID_USERNAME_OR_PASSWORD                     = 430,

          (**
           * Requested host unavailable.
           *)
          FTP_HOST_UNAVAILABLE                                 = 434,

          (**
           * Requested file action not taken.
           *
           * A 450 response code may be sent in response to any command
           * requiring the server to access a local file. It is a transient
           * negative response as the error is considered a temporary one. It is
           * usually sent when the server is unable to gain access to a required
           * file at the time the command is received. In this case, the client
           * is encouraged to restart the FTP transaction and try again.
           *)
          FTP_FILE_ACTION_ABORTED                              = 450,

          (**
           * Requested action aborted. Local error in processing.
           *
           * A 451 response code may be sent in response to any command
           * initiating a file transfer. It is a transient negative response,
           * which means the error condition is a temporary one. It is usually
           * sent in response to the server encountering an unexpected local
           * error when processing data it is transferring or receiving. In this
           * case, the client is encouraged to restart the FTP transaction and
           * try again.
           *)
          FTP_LOCAL_ERROR                                      = 451,

          (**
           * Requested action not taken. Insufficient storage space in system.
           * File unavailable (e.g., file busy).
           *
           * A 452 response code may be given in response to any command
           * requiring the server to store transferred data it receives from the
           * client (a file upload). It is a transient negative response as the
           * error is considered a temporary one. It is usually sent because the
           * server does not have storage space to save the received data. In
           * this case, the client is encouraged to restart the FTP transaction
           * and try again.
           *)
          FTP_INSUFFICIENT_STORAGE_SPACE                       = 452,

          (**
           * 5xx Permanent Negative Completion reply
           *
           * The command was not accepted and the requested action did not take
           * place. The User-process is discouraged from repeating the exact
           * request (in the same sequence). Even some "permanent" error
           * conditions can be corrected, so the human user may want to direct
           * his User-process to reinitiate the command sequence by direct
           * action at some point in the future (e.g., after the spelling has
           * been changed, or the user has altered his directory
           * status.)
           *)

          (**
           * Syntax error, command unrecognized and the requested action did not
           * take place. This may include errors such as command line too long.
           *
           * A 500 response code may be sent in response to any command that the
           * server is unable to recognize. It is a permanent negative response,
           * which means the client is discouraged from sending the command
           * again since the server will respond with the same reply code. It
           * usually means that the client has sent a command to the server that
           * the server does not recognize. This may be due to an error in the
           * spelling or formatting of the command itself or that the command is
           * newer than the FTP implementation in place on the server or is a
           * proprietary command of another server implementation.
           *)
          FTP_COMMAND_UNRECOGNIZED                             = 500,

          (**
           * Syntax error in parameters or arguments.
           *
           * A 501 response code may be sent in response to any command that
           * requires or supports the optional use of a parameter or argument.
           * It is a permanent negative response, which means the client is
           * discouraged from sending the exact command and parameter(s) again
           * since the server will respond with the same response code. It
           * differs from the 500 response code in that the server recognizes
           * the command present but is unable to take the requested action due
           * to a syntax error in the parameter(s) present with the command.
           * Sending the same command to the server with a corrected parameter
           * may result in a different response code.
           *)
          FTP_PARAMETERS_SYNTAX_ERROR                          = 501,

          (**
           * Command not implemented.
           *
           * A 502 code may be sent in response to any FTP command that the
           * server does not support. It is a permanent negative reply, which
           * means the client is discouraged from sending the command again
           * since the server will respond with the same reply code. The
           * original FTP specification dictates a minimum implementation for
           * all FTP servers with a list of required commands. Because of this,
           * a 502 reply code should not be sent in response to a required
           * command.
           *)
          FTP_COMMAND_NOT_SUPPORT                              = 502,

          (**
           * Bad sequence of commands.
           *
           * A 503 response code may be sent in response to any command that
           * requires the successful processing of previous commands first. It
           * is a permanent negative reply, which means the client is
           * discouraged from immediately sending the command again since the
           * server will respond with the same reply code. For example, a file
           * rename requires a successful RNFR command before the RNTO command
           * can be sent. Sending the RNTOcommand first will result in a 503
           * response code.
           *)
          FTP_COMMAND_BAD_SEQUENCE                             = 503,

          (**
           * Command not implemented for that parameter.
           *
           * A 504 response code can be sent in response to any command using a
           * parameter that is not supported by the server. It is a permanent
           * negative response, which means the client is discouraged from
           * sending the command again since the server will respond with the
           * same response code. Issuing the same command with a different
           * parameter may result in a different response code.
           *)
          FTP_PARAMETER_NOT_SUPPORTED_BY_SERVER                = 504,

          (**
           * Not logged in.
           *
           * A 530 response code may be sent in response to any command that
           * requires a user to log in before the command is processed. Some
           * servers may reply to all commands with a 530 response code until
           * the client logs in.
           *)
          FTP_NEED_LOGIN                                       = 530,

          (**
           * Need account for storing files.
           *
           * A 532 response code may be sent in response to any command
           * involving the storage or manipulation of files on the server. It is
           * a permanent negative response, which means the client is
           * discouraged from sending the command again since the server will
           * respond with the same response code. Providing account information
           * first and sending the command again can result in a different
           * response code.
           *)
          FTP_NEED_ACCOUNT_FOR_STORIG_FILES                    = 532,

          (**
           * Could Not Connect to Server - Policy Requires SSL
           *
           * A 534 response code can be issued in response to any command that
           * the server is unwilling to process due to its security policy. It
           * is a permanent negative response, which means the client is
           * discouraged from sending the command again since the server will
           * respond with the same response code. It usually means that the
           * server requires a certain level of security to exist on the
           * connection before processing the command or that it is unwilling to
           * process a command that would provide for decreased security.
           *)
          FTP_REQUIRE_HIGHER_SECURITY_LEVEL                    = 534,

          (**
           * Requested action not taken. File unavailable (e.g., file not found,
           * no access).
           *
           * A 550 response code may be sent in response to any command
           * requiring the server to access a local file. It is a permanent
           * negative response, which means the client is discouraged from
           * sending the command again since the server will respond with the
           * same response code. It is usually due to a command requiring access
           * to a file that does not exist or that the user does not have access
           * rights to.
           *)
          FTP_FILE_UNAVAILABLE                                 = 550,

          (**
           * Requested action aborted. Page type unknown.
           *
           * A 551 response code may be sent in response to any command
           * requiring the server to store information locally. It is a
           * permanent negative reply, which means the client is discouraged
           * from sending the command again since the server will respond with
           * the same response code. It is only applicable when the page file
           * structure is being used (through a STRU P command).
           *)
          FTP_PAGE_FILE_STRUCTURE_UNKNOWN                      = 551,

          (**
           * Requested file action aborted. Exceeded storage allocation (for
           * current directory or dataset).
           *
           * A 552 response code may be sent in response to any command
           * requiring the server to store received information locally. It is a
           * permanent negative response, which means the client is discouraged
           * from sending the command again since the server will respond with
           * the same reply code. It usually indicates that the logged in user
           * has exceeded the storage space allocated to their user account by
           * the administrator.
           *)
          FTP_STORAGE_SPACE_EXCEEDED                           = 552,

          (**
           * Requested action not taken. File name not allowed.
           *
           * A 553 response code may be given in response to any command
           * requiring or supporting the use of a file name as a parameter. It
           * is a permanent negative reply, which means the client is
           * discouraged from sending the command again since the server will
           * respond with the same reply code. It is usually due to the file
           * name contained as a parameter violating the file naming policies
           * existing on the server. Issuing the command again with a different
           * file name may result in a different reply code.
           *)
          FTP_FILENAME_NOT_ALLOWED                             = 553,

          (**
           * 6xx Protected reply
           *
           * The RFC 2228 introduced the concept of protected replies to
           * increase security over the FTP communications. The 6xx replies are
           * Base64 encoded protected messages that serves as responses to
           * secure commands. When properly decoded, these replies fall into the
           * above categories.
           *)

          (**
           * Replies regarding confidentiality and integrity
           *)
          FTP_CONFIDENTIALITY_AND_INTEGRITY                    = 600,

          (**
           * Integrity protected reply.
           *)
          FTP_INTEGRITY_PROTECTED_REPLY                        = 631,

          (**
           * Confidentiality and integrity protected reply.
           *)
          FTP_CONFIDENTIALITY_AND_INTEGRITY_REPLY              = 632,

          (**
           * Confidentiality protected reply.
           *)
          FTP_CONFIDENTIALITY_REPLY                            = 633
        );

        TCreateDirs = (
          CREATE_DIR_NONE  = Longint(CURLFTP_CREATE_DIR_NONE),

          (**
           * libcurl will attempt to create any remote directory that it fails
           * to "move" into.
           *)
          CREATE_DIR       = Longint(CURLFTP_CREATE_DIR),

          (**
           * Tells libcurl to retry the CWD command again if the subsequent MKD
           * command fails.
           *)
          CREATE_DIR_RETRY = Longint(CURLFTP_CREATE_DIR_RETRY)
        );

        TAuth = (
          (**
           * Allow libcurl to decide.
           *)
          AUTH_DEFAULT     = Longint(CURLFTPAUTH_DEFAULT),

          (**
           * Try "AUTH SSL" first, and only if that fails try "AUTH TLS".
           *)
          AUTH_SSL         = Longint(CURLFTPAUTH_SSL),

          (**
           * Try "AUTH TLS" first, and only if that fails try "AUTH SSL".
           *)
          AUTH_TLS         = Longint(CURLFTPAUTH_TLS)
        );

        (* Clear Command Channel *)
        TSSL_CCC = (
          (**
           * Don't attempt to use CCC.
           *)
          CCC_NONE         = Longint(CURLFTPSSL_CCC_NONE),

          (**
           * Do not initiate the shutdown, but wait for the server to do it. Do
           * not send a reply.
           *)
          CCC_PASSIVE      = Longint(CURLFTPSSL_CCC_PASSIVE),

          (**
           * Initiate the shutdown and wait for a reply.
           *)
          CCC_ACTIVE       = Longint(CURLFTPSSL_CCC_ACTIVE)
        );

        TFileMethod = (
          (**
           * libcurl does a single CWD operation for each path part in the given
           * URL. For deep hierarchies this means many commands. This is how RFC
           * 1738 says it should be done. This is the default but the slowest
           * behavior.
           *)
          METHOD_MULTICWD  = Longint(CURLFTPMETHOD_MULTICWD),

          (**
           * libcurl does no CWD at all. libcurl will do SIZE, RETR, STOR etc
           * and give a full path to the server for all these commands. This is
           * the fastest behavior.
           *)
          METHOD_NOCWD     = Longint(CURLFTPMETHOD_NOCWD),

          (**
           * libcurl does one CWD with the full target directory and then
           * operates on the file "normally" (like in the multicwd case). This
           * is somewhat more standards compliant than 'nocwd' but without the
           * full penalty of 'multicwd'.
           *)
          METHOD_SINGLECWD = Longint(CURLFTPMETHOD_SINGLECWD)
        );
      private
        FHandle : CURL;

        procedure SetFTPPort (APort : string);
        procedure SetAppendUpload (AEnable : Boolean);
        procedure SetUseEPRT (AEnable : Boolean);
        procedure SetUseEPSV (AEnable : Boolean);
        procedure SetUsePRET (AEnable : Boolean);
        procedure SetCreateMissingDirs (ACreate : TCreateDirs);
        procedure SetResponseTimeout (ATimeout : TTimeInterval);
        procedure SetAlternativeToUser (ACmd : string);
        procedure SetSkipPASVIP (AEnable : Boolean);
        procedure SetSSLAuth (AAuth : TAuth);
        procedure SetSSLCCC (ACCC : TSSL_CCC);
        procedure SetAccountInfo (AAccount : string);
        procedure SetFileMethod (AMethod : TFileMethod);
        procedure SetTransferText (AEnable : Boolean);
        procedure SetProxyTransferMode (AEnable : Boolean);
        procedure SetRange (ARange : string);
        procedure SetStartTransferFrom (AFrom : curl_off_t);
        procedure SetCustomRequest (ARequest : string);
        procedure SetTimeModification (AEnable : Boolean);
        procedure SetDirListOnly (AEnable : Boolean);
        procedure SetMaxDownloadFileSize (ASize : TDataSize);
        procedure SetAcceptTimeout (ATime : TTimeInterval);
        procedure SetWildcardMatch (AMatch : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * Make FTP transfer active
         *
         * It specifies that the FTP transfer will be made actively and the
         * given string will be used to get the IP address to use for the FTP
         * PORT instruction.
         * The PORT instruction tells the remote server to connect to our
         * specified IP address. The string may be a plain IP address, a host
         * name, a network interface name (under Unix) or just a '-' symbol to
         * let the library use your system's default IP address. Default FTP
         * operations are passive, and thus won't use PORT.
         * The address can be followed by a ':' to specify a port, optionally
         * followed by a '-' to specify a port range. If the port specified is
         * 0, the operating system will pick a free port. If a range is provided
         * and all ports in the range are not available, libcurl will report
         * CURLE_FTP_PORT_FAILED for the handle. Invalid port/range settings are
         * ignored. IPv6 addresses followed by a port or portrange have to be in
         * brackets. IPv6 addresses without port/range specifier can be in
         * brackets.
         *)
        property Port : string write SetFTPPort;

        (**
         * Enable appending to the remote file
         *
         * Tells the library to append to the remote file instead of overwrite
         * it. This is only useful when uploading to an FTP site.
         *)
        property UploadAppend : Boolean write SetAppendUpload;

        (**
         * Enable/disable use of EPRT with FTP
         *
         * It tells curl to use the EPRT command when doing active FTP downloads
         * (which is enabled by Port). Using EPRT means that it will first
         * attempt to use EPRT before using PORT, but if you pass zero to this
         * option, it will not try using EPRT, only plain PORT.
         * If the server is an IPv6 host, this option will have no effect as
         * EPRT is necessary then.
         *)
        property UseEPRT : Boolean write SetUseEPRT;

        (**
         * Enable/disable use of EPSV
         *
         * It tells curl to use the EPSV command when doing passive FTP
         * downloads (which it does by default). Using EPSV means that it will
         * first attempt to use EPSV before using PASV, but if you pass zero to
         * this option, it will not try using EPSV, only plain PASV.
         * If the server is an IPv6 host, this option will have no effect as of
         * 7.12.3.
         *)
        property UseEPSV : Boolean write SetUseEPSV;

        (**
         * Enable the PRET command
         *
         * It tells curl to send a PRET command before PASV (and EPSV). Certain
         * FTP servers, mainly drftpd, require this non-standard command for
         * directory listings as well as up and downloads in PASV mode. Has no
         * effect when using the active FTP transfers mode.
         *)
        property UsePRET : Boolean write SetUsePRET;

        (**
         * Create missing dirs for FTP and SFTP
         *
         * Telling libcurl to create the dir. If the value is CREATE_DIR,
         * libcurl will attempt to create any remote directory that it fails to
         * "move" into.
         * For FTP requests, that means a CWD command fails. CWD being the
         * command that changes working directory.
         * For SFTP requests, libcurl will attempt to create the remote
         * directory if it can't obtain a handle to the target-location. The
         * creation will fail if a file of the same name as the directory to
         * create already exists or lack of permissions prevents creation.
         * Setting create to CREATE_DIR_RETRY, tells libcurl to retry the CWD
         * command again if the subsequent MKD command fails. This is especially
         * useful if you're doing many simultaneous connections against the same
         * server and they all have this option enabled, as then CWD may first
         * fail but then another connection does MKD before this connection and
         * thus MKD fails but trying CWD works!
         *)
        property CreateMissingDirs : TCreateDirs write SetCreateMissingDirs;

        (**
         * Time allowed to wait for FTP response
         *
         * Causes libcurl to set a timeout period on the amount of time that the
         * server is allowed to take in order to send a response message for a
         * command before the session is considered dead. While libcurl is
         * waiting for a response, this value overrides TSession.Timeout. It is
         * recommended that if used in conjunction with TSession.Timeout, you
         * set ResponseTimeout to a value smaller than TSession.Timeout.
         *)
        property ResponseTimeout : TTimeInterval write SetResponseTimeout;

        (**
         * Command to use instead of USER with FTP
         *
         * Pass a string which will be used to authenticate if the usual FTP
         * "USER user" and "PASS password" negotiation fails. This is currently
         * only known to be required when connecting to Tumbleweed's Secure
         * Transport FTPS server using client certificates for authentication.
         *)
        property AlternativeToUser : string write SetAlternativeToUser;

        (**
         * Ignore the IP address in the PASV response
         *
         * If skip it instructs libcurl to not use the IP address the server
         * suggests in its 227-response to libcurl's PASV command when libcurl
         * connects the data connection. Instead libcurl will re-use the same IP
         * address it already uses for the control connection. But it will use
         * the port number from the 227-response.
         * This option thus allows libcurl to work around broken server
         * installations that due to NATs, firewalls or incompetence report the
         * wrong IP address back.
         *)
        property SkipPASVIP : Boolean write SetSkipPASVIP default False;

        (**
         * Set order in which to attempt TLS vs SSL when using FTP
         *
         * Pass a one of the values from TAuth, to alter how libcurl issues
         * "AUTH TLS" or "AUTH SSL" when FTP over SSL is activated. This is only
         * interesting if TSession.UseSSL is also set.
         *)
        property Auth : TAuth write SetSSLAuth;

        (**
         * Switch off SSL again with FTP after auth
         *
         * If enabled, this option makes libcurl use CCC (Clear Command
         * Channel). It shuts down the SSL/TLS layer after authenticating. The
         * rest of the control channel communication will be unencrypted. This
         * allows NAT routers to follow the FTP transaction. Pass a long using
         * one of the values TSSL_CCC;
         *)
        property SSLClearCommandChannel : TSSL_CCC write SetSSLCCC;

        (**
         * Set account info for FTP
         *
         * When an FTP server asks for "account data" after user name and
         * password has been provided, this data is sent off using the ACCT
         * command.
         *)
        property AccountInfo : string write SetAccountInfo;

        (**
         * Select directory traversing method for FTP
         *
         * This option exists because some server implementations aren't
         * compliant to what the standards say should work.
         *)
        property FileMethod : TFileMethod write SetFileMethod
          default METHOD_MULTICWD;

        (**
         * Request a text based transfer to FTP
         *
         * Tells the library to use ASCII mode for FTP transfers, instead of the
         * default binary transfer. For win32 systems it does not set the stdout
         * to binary mode. This option can be usable when transferring text data
         * between systems with different views on certain characters, such as
         * newlines or similar.
         * libcurl does not do a complete ASCII conversion when doing ASCII
         * transfers over FTP. This is a known limitation/flaw that nobody has
         * rectified. libcurl simply sets the mode to ASCII and performs a
         * standard transfer.
         *)
        property TransferText : Boolean write SetTransferText default False;

        (**
         * Append FTP transfer mode to URL for proxy
         *
         * Tells libcurl to set the transfer mode (binary or ASCII) for FTP
         * transfers done via an HTTP proxy, by appending ;type=a or ;type=i to
         * the URL. Without this setting, or it being set to 0 (zero, the
         * default), TransferText has no effect when doing FTP via a proxy.
         * Beware that not all proxies support this feature.
         *)
        property ProxyTransferMode : Boolean write SetProxyTransferMode
          default False;

        (**
         * Set byte range to request
         *
         * Pass a char * as parameter, which should contain the specified range
         * you want to retrieve. It should be in the format "X-Y", where either
         * X or Y may be left out and X and Y are byte indexes.
         * Pass a '' (empty string) to this option to disable the use of ranges.
         *)
        property Range : string write SetRange;

        (**
         * Set a point to resume transfer from
         *
         * It contains the offset in number of bytes that you want the transfer
         * to start from. Set this option to 0 to make the transfer start from
         * the beginning (effectively disabling resume). For FTP, set this
         * option to -1 to make the transfer start from the end of the target
         * file (useful to continue an interrupted upload).
         * When doing uploads with FTP, the resume position is where in the
         * local/source file libcurl should try to resume the upload from and it
         * will then append the source file to the remote target file.
         *)
        property StartTransferFrom : curl_off_t write SetStartTransferFrom
          default 0;

        (**
         * Custom string for request
         *
         * When you change the request method by setting CustomRequest to
         * something, you don't actually change how libcurl behaves or acts in
         * regards to the particular request method, it will only change the
         * actual string sent in the request.
         * Instead of LIST and NLST when performing FTP directory listings.
         *)
        property CustomRequest : string write SetCustomRequest;

        (**
         * Get the modification time of the remote resource
         *
         * If it is True, libcurl will attempt to get the modification time of
         * the remote document in this operation. This requires that the remote
         * server sends the time or replies to a time querying command. The
         * curl_easy_getinfo function with the CURLINFO_FILETIME argument can be
         * used after a transfer to extract the received time (if any).
         *)
        property TimeModification : Boolean write SetTimeModification;

        (**
         * Ask for names only in a directory listing
         *
         * For FTP and SFTP based URLs a parameter set to True tells the library
         * to list the names of files in a directory, rather than performing a
         * full directory listing that would normally include file sizes, dates
         * etc.
         * Note: For FTP this causes a NLST command to be sent to the FTP
         * server. Beware that some FTP servers list only files in their
         * response to NLST; they might not include subdirectories and symbolic
         * links.
         * Setting this option to 1 also implies a directory listing even if the
         * URL doesn't end with a slash, which otherwise is necessary.
         *)
        property DirListOnly : Boolean write SetDirListOnly default False;

        (**
         * Maximum file size allowed to download
         *
         * This allows you to specify the maximum size (in bytes) of a file to
         * download. If the file requested is found larger than this value, the
         * transfer will not start and CURLE_FILESIZE_EXCEEDED will be returned.
         * The file size is not always known prior to download, and for such
         * files this option has no effect even if the file transfer ends up
         * being larger than this given limit. This concerns both FTP and HTTP
         * transfers.
         *)
        property MaxDownloadFileSize : TDataSize write SetMaxDownloadFileSize;

        (**
         * Timeout waiting for FTP server to connect back
         *
         * Telling libcurl the maximum wait for a server to connect back to
         * libcurl when an active FTP connection is used.
         *)
        property AcceptTimeout : TTimeInterval write SetAcceptTimeout;

        (**
         * Enable directory wildcard transfers
         * [This feature is only supported for FTP download]
         *
         * Transfer multiple files according to a file name pattern. The pattern
         * can be specified as part of the Url option, using an fnmatch-like
         * pattern (Shell Pattern Matching) in the last part of URL (file name).
         *
         * A brief introduction of its syntax follows:
         *
         * * - ASTERISK
         * ftp://example.com/some/path/*.txt (for all txt's from the root
         * directory). Only two asterisks are allowed within the same pattern
         * string.
         *
         * ? - QUESTION MARK
         * Question mark matches any (exactly one) character.
         * ftp://example.com/some/path/photo?.jpeg
         *
         * [ - BRACKET EXPRESSION
         * The left bracket opens a bracket expression. The question mark and
         * asterisk have no special meaning in a bracket expression. Each
         * bracket expression ends by the right bracket and matches exactly one
         * character. Some examples follow:
         * [a-zA-Z0-9] or [f-gF-G] - character interval
         * [abc] - character enumeration
         * [^abc] or [!abc] - negation
         * [[:name:]] class expression. Supported classes are alnum,lower,
         * space, alpha, digit, print, upper, blank, graph, xdigit.
         * [][-!^] - special case - matches only '-', ']', '[', '!' or '^'.
         * These characters have no special purpose.
         * [\[\]\\] - escape syntax. Matches '[', ']' or '´.
         * Using the rules above, a file name pattern can be constructed:
         * ftp://example.com/some/path/[a-z[:upper:]\\].jpeg
         *)
        property WildcardMatch : Boolean write SetWildcardMatch;
      end;

      { TSMTPProperty }

      TSMTPProperty = class
      private
        FHandle : CURL;

        procedure SetMailFrom (AFrom : string);
        procedure SetMailAuth (AAuth : string);
        procedure SetCustomRequest (ARequest : string);
        procedure SetConnectOnly (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * SMTP sender address
         *
         * This should be used to specify the sender's email address when
         * sending SMTP mail with libcurl.
         * An originator email address should be specified with angled brackets
         * (<>) around it, which if not specified will be added automatically.
         * If this parameter is not specified then an empty address will be sent
         * to the mail server which may cause the email to be rejected.
         *)
        property From : string write SetMailFrom;

        (**
         * SMTP authentication address
         *
         * This will be used to specify the authentication address (identity) of
         * a submitted message that is being relayed to another server.
         * This optional parameter allows co-operating agents in a trusted
         * environment to communicate the authentication of individual messages
         * and should only be used by the application program, using libcurl, if
         * the application is itself a mail server acting in such an
         * environment. If the application is operating as such and the AUTH
         * address is not known or is invalid, then an empty string should be
         * used for this parameter.
         * Unlike From and Recipients, the address should not be specified
         * within a pair of angled brackets (<>). However, if an empty string is
         * used then a pair of brackets will be sent by libcurl as required by
         * RFC 2554.
         *)
        property Auth : string write SetMailAuth;

        (**
         * Custom string for request
         *
         * When you change the request method by setting CustomRequest to
         * something, you don't actually change how libcurl behaves or acts in
         * regards to the particular request method, it will only change the
         * actual string sent in the request.
         * Instead of a HELP or VRFY when issuing SMTP based requests.
         * For example:
         * Normally a multiline response is returned which can be used, in
         * conjunction with CURLOPT_MAIL_RCPT, to specify an EXPN request. If
         * the CURLOPT_NOBODY option is specified then the request can be used
         * to issue NOOP and RSET commands.
         *)
        property CustomRequest : string write SetCustomRequest;

        (**
         * Stop when connected to target server
         *
         * Tells the library to perform all the required proxy authentication
         * and connection setup, but no data transfer, and then return.
         * The option can be used to simply test a connection to a server, but
         * is more useful when used with the CURLINFO_ACTIVESOCKET option to
         * curl_easy_getinfo as the library can set up the connection and then
         * the application can obtain the most recently used socket for special
         * data transfers.
         * Transfers marked connect only will not reuse any existing connections
         * and connections marked connect only will not be allowed to get
         * reused.
         *)
        property ConnectOnly : Boolean write SetConnectOnly default False;
      end;

      { TTFTPProperty }

      TTFTPProperty = class
      private
        FHandle : CURL;

        procedure SetBlockSize (ASize : Longint);
        procedure SetNoOptions (AEnable : Boolean);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (**
         * TFTP block size
         *
         * Specify blocksize to use for TFTP data transmission. Valid range as
         * per RFC 2348 is 8-65464 bytes. The default of 512 bytes will be used
         * if this option is not specified. The specified block size will only
         * be used pending support by the remote server. If the server does not
         * return an option acknowledgement or returns an option acknowledgement
         * with no blksize, the default of 512 bytes will be used.
         *)
        property BlockSize : Longint write SetBlockSize default 512;

        (**
         * Do not send TFTP options request.
         *
         * Set True to exclude all TFTP options defined in RFC 2347, RFC 2348
         * and RFC 2349 from read and write requests (RRQs/WRQs).
         * This option improves interop with some legacy servers that do not
         * acknowledge or properly implement TFTP options. When this option is
         * used BlockSize is ignored.
         *)
        property NoOptions : Boolean write SetNoOptions default False;
      end;

      { TRTSPProperty }

      TRTSPProperty = class
      public
        type
          RTSPRequest = (
          (**
           * Used to retrieve the available methods of the server. The
           * application is responsible for parsing and obeying the response.
           * The session ID is not needed for this method.
           *)
          REQUEST_OPTIONS  = Longint(CURL_RTSPREQ_OPTIONS),

          (**
           * Used to get the low level description of a stream. The application
           * should note what formats it understands in the 'Accept:' header.
           * Unless set manually, libcurl will automatically fill in 'Accept:
           * application/sdp'. Time-condition headers will be added to Describe
           * requests if the CURLOPT_TIMECONDITION option is active. (The
           * session ID is not needed for this method)
           *)
          REQUEST_DESCRIBE = Longint(CURL_RTSPREQ_DESCRIBE),

          (**
           * When sent by a client, this method changes the description of the
           * session. For example, if a client is using the server to record a
           * meeting, the client can use Announce to inform the server of all
           * the meta-information about the session. ANNOUNCE acts like an HTTP
           * PUT or POST just like CURL_RTSPREQ_SET_PARAMETER
           *)
          REQUEST_ANNOUNCE = Longint(CURL_RTSPREQ_ANNOUNCE),

          (**
           * Setup is used to initialize the transport layer for the session.
           * The application must set the desired Transport options for a
           * session by using the CURLOPT_RTSP_TRANSPORT option prior to calling
           * setup. If no session ID is currently set with
           * TSession.RTSP.SessionId, libcurl will extract and use the session
           * ID in the response to this request. The session ID is not needed
           * for this method.
           *)
          REQUEST_SETUP = Longint(CURL_RTSPREQ_SETUP),

          (**
           * Send a Play command to the server. Use the Range option to
           * modify the playback time (e.g. 'npt=10-15').
           *)
          REQUEST_PLAY = Longint(CURL_RTSPREQ_PLAY),

          (**
           * Send a Pause command to the server. Use the Range option
           * with a single value to indicate when the stream should be halted.
           * (e.g. npt='25')
           *)
          REQUEST_PAUSE = Longint(CURL_RTSPREQ_PAUSE),

          (**
           * This command terminates an RTSP session. Simply closing a
           * connection does not terminate the RTSP session since it is valid to
           * control an RTSP session over different connections.
           *)
          REQUEST_TEARDOWN = Longint(CURL_RTSPREQ_TEARDOWN),

          (**
           * Retrieve a parameter from the server. By default, libcurl will
           * automatically include a Content-Type: text/parameters header on all
           * non-empty requests unless a custom one is set. GET_PARAMETER acts
           * just like an HTTP PUT or POST (see CURL_RTSPREQ_SET_PARAMETER).
           * Applications wishing to send a heartbeat message (e.g. in the
           * presence of a server-specified timeout) should send use an empty
           * GET_PARAMETER request.
           *)
          REQUEST_GET_PARAMETER = Longint(CURL_RTSPREQ_GET_PARAMETER),

          (**
           * Set a parameter on the server. By default, libcurl will
           * automatically include a Content-Type: text/parameters header unless
           * a custom one is set. The interaction with SET_PARAMETER is much
           * like an HTTP PUT or POST. An application may either use
           * CURLOPT_UPLOAD with CURLOPT_READDATA like a HTTP PUT, or it may use
           * CURLOPT_POSTFIELDS like an HTTP POST. No chunked transfers are
           * allowed, so the application must set the CURLOPT_INFILESIZE in the
           * former and CURLOPT_POSTFIELDSIZE in the latter. Also, there is no
           * use of multi-part POSTs within RTSP.
           *)
          REQUEST_SET_PARAMETER = Longint(CURL_RTSPREQ_SET_PARAMETER),

          (**
           * Used to tell the server to record a session. Use the Range
           * option to modify the record time.
           *)
          REQUEST_RECORD = Longint(CURL_RTSPREQ_RECORD),

          (**
           * This is a special request because it does not send any data to the
           * server. The application may call this function in order to receive
           * interleaved RTP data. It will return after processing one read
           * buffer of data in order to give the application a chance to run.
           *)
          REQUEST_RECEIVE = Longint(CURL_RTSPREQ_RECEIVE)
        );
      private
        FHandle : CURL;

        procedure SetRequest (AReq : Longint);
        procedure SetSessionID (AId : string);
        procedure SetStreamURI (AURI : string);
        procedure SetTransport (ATransport : string);
        procedure SetClientCSeq (ACSeq : Longint);
        procedure SetServerCSeq (ACSeq : Longint);
        procedure SetRange (ARange : string);
      public
        constructor Create (AHandle : CURL);
        destructor Destroy; override;

        (*
         * Specify RTSP request
         *
         * Tell libcurl what kind of RTSP request to make. Pass one of the
         * following RTSP enum values as a long in the request argument. Unless
         * noted otherwise, commands require the Session ID to be initialized.
         *)
        property Request : Longint write SetRequest;

        (**
         * Set RTSP session ID
         *
         * Pass a string as a parameter to set the value of the current RTSP
         * Session ID for the handle. Useful for resuming an in-progress
         * session. Once this value is set to any non-NULL value, libcurl will
         * return CURLE_RTSP_SESSION_ERROR if ID received from the server does
         * not match. If unset (or set to NULL), libcurl will automatically set
         * the ID the first time the server sets it in a response.
         *)
        property SessionID : string write SetSessionID;

        (**
         * Set RTSP stream URI
         *
         * Set the stream URI to operate on by passing a string. For example, a
         * single session may be controlling rtsp://foo/twister/audio and
         * rtsp://foo/twister/video and the application can switch to the
         * appropriate stream using this option. If unset, libcurl will default
         * to operating on generic server options by passing '*' in the place of
         * the RTSP Stream URI. This option is distinct from TSession.Url. When
         * working with RTSP, the StreamURI indicates what URL to send to the
         * server in the request header while the TSession.Url indicates where
         * to make the connection to. (e.g. the TSession.Url for the above
         * examples might be set to rtsp://foo/twister
         *)
        property StreamURI : string write SetStreamURI;

        (**
         * Set RTSP Transport: header
         *
         * Pass a string to tell libcurl what to pass for the Transport: header
         * for this RTSP session. This is mainly a convenience method to avoid
         * needing to set a custom Transport: header for every SETUP request.
         * The application must set a Transport: header before issuing a SETUP
         * request.
         *)
        property Transport : string write SetTransport;

        (**
         * Set the RTSP client CSEQ number
         *
         * Pass a long to set the CSEQ number to issue for the next RTSP
         * request. Useful if the application is resuming a previously broken
         * connection. The CSEQ will increment from this new number henceforth.
         *)
        property ClientCSeq : Longint write SetClientCSeq default 0;

        (**
         * Set the RTSP server CSEQ number
         *
         * Pass a long to set the CSEQ number to expect for the next RTSP
         * Server->Client request.
         * NOTE: this feature (listening for Server requests) is unimplemented.
         *)
        property ServerCSeq : Longint write SetServerCSeq default 0;

        (**
         * Set byte range to request
         *
         * Pass a char * as parameter, which should contain the specified range
         * you want to retrieve. It should be in the format "X-Y", where either
         * X or Y may be left out and X and Y are byte indexes.
         * For RTSP, the formatting of a range should follow RFC 2326 Section
         * 12.29. For RTSP, byte ranges are not permitted. Instead, ranges
         * should be given in npt, utc, or smpte formats.
         * Pass a '' (empty string) to this option to disable the use of ranges.
         *)
        property Range : string write SetRange;
      end;

  protected
    FHandle : CURL;
    FBuffer : TMemoryStream;
    FUploadOffset : Int64;
    FOptions : TOptionsProperty;
    FProtocol : TProtocolProperty;
    FTCP : TTCPProperty;
    FProxy : TProxyProperty;
    FDNS : TDNSProperty;
    FSecurity : TSecurityProperty;
    FHTTP : THTTPProperty;
    FIMAP : TIMAPProperty;
    FFTP : TFTPProperty;
    FTFTP : TTFTPProperty;
    FSMTP : TSMTPProperty;
    FRTSP : TRTSPProperty;

    FDownloadFunction : TDownloadFunction;
    FUploadFunction : TUploadFunction;
  protected
    (**
     * Callback for writting received data
     *
     * This callback function gets called by libcurl as soon as there is data
     * received that needs to be saved. For most transfers, this callback gets
     * called many times and each invoke delivers another chunk of data. ptr
     * points to the delivered data, and the size of that data is nmemb; size is
     * always 1.
     *)
    class function WriteFunctionCallback (ptr : PChar; size : LongWord;
      nmemb : LongWord; data : Pointer) : LongWord; static; cdecl;

    (**
     * Callback for data uploads
     *
     * This callback function gets called by libcurl as soon as it needs to read
     * data in order to send it to the peer - like if you ask it to upload or
     * post data to the server. The data area pointed at by the pointer buffer
     * should be filled up with at most size multiplied with nitems number of
     * bytes by your function.
     *)
    class function ReadFunctionCallback (buf : PChar; size : LongWord;
      nitems : LongWord; data : Pointer) : LongWord; static; cdecl;

    (**
     * Save received data to the inner buffer
     *)
    function Write (ptr : PChar; size : LongWord; nmemb : LongWord) : LongWord;
      inline;

    (**
     * Write uploads data to the buf
     *)
    function Read (buf : PChar; size : LongWord; nitems : LongWord) : LongWord;
      inline;

    function IsOpened : Boolean;
    procedure SetUrl (url : string);
    procedure SetLocalPort (APort : Word);
    procedure SetLocalPortRange (ARange : Longint);
    procedure SetConnectTo (AList : TLinkedList);
  public
    function ExtractProtocol (AUrl : string) : TProtocolProperty.TProtocol;
  public
    constructor Create;
    destructor Destroy; override;

    property Options : TOptionsProperty read FOptions write FOptions;
    property Security : TSecurityProperty read FSecurity write FSecurity;
    property Protocol : TProtocolProperty read FProtocol write FProtocol;
    property TCP : TTCPProperty read FTCP write FTCP;
    property Proxy : TProxyProperty read FProxy write FProxy;
    property DNS : TDNSProperty read FDNS write FDNS;
    property HTTP : THTTPProperty read FHTTP write FHTTP;
    property IMAP : TIMAPProperty read FIMAP write FIMAP;
    property FTP : TFTPProperty read FFTP write FFTP;
    property TFTP : TTFTPProperty read FTFTP write FTFTP;
    property SMTP : TSMTPProperty read FSMTP write FSMTP;
    property RTSP : TRTSPProperty read FRTSP write FRTSP;

    (**
     * Check if session opened and correctly
     *)
    property Opened : Boolean read IsOpened;

    (**
     * Set the URL to use in the request
     *
     * The parameter should be a string which must be URL-encoded in the
     * following format: scheme://host:port/path
     * libcurl doesn't validate the syntax or use this variable until the
     * transfer is issued.
     * If the given URL is missing a scheme name (such as "http://" or "ftp://"
     * etc) then libcurl will make a guess based on the host. If the outermost
     * sub-domain name matches DICT, FTP, IMAP, LDAP, POP3 or SMTP then that
     * protocol will be used, otherwise HTTP will be used.
     * The host part of the URL contains the address of the server that you want
     * to connect to. This can be the fully qualified domain name of the server,
     * the local network name of the machine on your network or the IP address
     * of the server or machine represented by either an IPv4 or IPv6 address.
     * http://www.example.com/
     * http://hostname/
     * http://192.168.0.1/
     * http://[2001:1890:1112:1::20]/
     * It is also possible to specify the user name, password and any supported
     * login options as part of the host, for the following protocols, when
     * connecting to servers that require authentication:
     * http://user:password@www.example.com
     * ftp://user:password@ftp.example.com
     * smb://domain%2fuser:password@server.example.com
     * imap://user:password;options@mail.example.com
     * pop3://user:password;options@mail.example.com
     * smtp://user:password;options@mail.example.com
     *)
    property Url : string write SetUrl;

    (**
     * Set local port number to use for socket
     *
     * This sets the local port number of the socket used for the connection.
     * 0, disabled - use whatever the system thinks is fine
     *)
     property Port : Word write SetLocalPort;

     (**
     * Number of additional local ports to try
     *
     * Pass a long. The range argument is the number of attempts libcurl will
     * make to find a working local port number. It starts with the given
     * LocalPort and adds one to the number for each retry. Setting this option
     * to 1 or below will make libcurl do only one try for the exact port
     * number. Port numbers by nature are scarce resources that will be busy at
     * times so setting this value to something too low might cause unnecessary
     * connection setup failures.
     *)
     property PortRange : Longint write SetLocalPortRange default 1;
  public

    (**
     * Connect to a specific host and port instead of thr URL's host and port
     *
     * Pass a pointer to a linked list of strings with "connect to" information
     * to use for establishing network connections with this handle. The linked
     * list should be a fully valid list of struct curl_slist structs properly
     * filled in. Use curl_slist_append to create the list and
     * curl_slist_free_all to clean up an entire list.
     * Each single string should be written using the format
     * HOST:PORT:CONNECT-TO-HOST:CONNECT-TO-PORT where HOST is the host of the
     * request, PORT is the port of the request, CONNECT-TO-HOST is the host
     * name to connect to, and CONNECT-TO-PORT is the port to connect to.
     * The first string that matches the request's host and port is used.
     * Dotted numerical IP addresses are supported for HOST and CONNECT-TO-HOST.
     * A numerical IPv6 address must be written within [brackets].
     * Any of the four values may be empty. When the HOST or PORT is empty, the
     * host or port will always match (the request's host or port is ignored).
     * When CONNECT-TO-HOST or CONNECT-TO-PORT is empty, the "connect to"
     * feature will be disabled for the host or port, and the request's host or
     * port will be used to establish the network connection.
     * This option is suitable to direct the request at a specific server, e.g.
     * at a specific cluster node in a cluster of servers.
     * The "connect to" host and port are only used to establish the network
     * connection. They do NOT affect the host and port that are used for
     * TLS/SSL (e.g. SNI, certificate verification) or for the application
     * protocols.
     * In contrast to CURLOPT_RESOLVE, the option CURLOPT_CONNECT_TO does not
     * pre-populate the DNS cache and therefore it does not affect future
     * transfers of other easy handles that have been added to the same multi
     * handle.
     * The "connect to" host and port are ignored if they are equal to the host
     * and the port in the request URL, because connecting to the host and the
     * port in the request URL is the default behavior.
     * If an HTTP proxy is used for a request having a special "connect to" host
     * or port, and the "connect to" host or port differs from the request's
     * host and port, the HTTP proxy is automatically switched to tunnel mode
     * for this specific request. This is necessary because it is not possible
     * to connect to a specific host or port in normal (non-tunnel) mode.
     * When this option is passed to curl_easy_setopt, libcurl will not copy the
     * entire list so you must keep it around until you no longer use this
     * handle for a transfer before you call curl_slist_free_all on the list.
     *)
    property ConnectTo : TLinkedList write SetConnectTo;

    (**
     * Callback for writing received data
     *
     * This callback function gets called by libcurl as soon as there is data
     * received that needs to be saved.
     * This option shares the same semantics as UnixSocketPath in which
     * documentation more details can be found. Internally, these two options
     * share the same storage and therefore only one of them can be set per
     * handle.
     *)
    property OnDownload : TDownloadFunction read FDownloadFunction
      write FDownloadFunction;

    (**
     * Callback for data uploads
     *
     * This callback function gets called by libcurl as soon as it needs to read
     * data in order to send it to the peer - like if you ask it to upload or
     * post data to the server.
     *)
    property OnUpload : TUploadFunction read FUploadFunction
      write FUploadFunction;
  end;

  { TResponse }
  { Getting information from server response. }

  TResponse = class
  protected
    session : TSession;
    hasInfo : Boolean;
    errorBuffer : array [0 .. CURL_ERROR_SIZE] of char;
  protected
    function IsOpened : Boolean;
    function CheckErrors : Boolean;
    function GetErrorMessage : string;
    function GetEffectiveUrl : string;
    function GetRedirectUrl : string;
    function GetContentType : string;
    function GetPrimaryIP : string;
    function GetLocalIP : string;
    function GetResponseCode : Longint;
    function GetContent : string;
    function GetVerifySSLResult : boolean;
    function GetVerifySSLProxyResult : boolean;
    function GetConnectResponseCode : Longint;
    function GetHttpVersion : TSession.THTTPProperty.THTTPVersion;
    function GetRedirectCount : Longint;
    function GetUploaded : TDataSize;
    function GetDownloaded : TDataSize;
    function GetDownloadSpeed : TDataSize;
    function GetUploadSpeed : TDataSize;
    function GetHeaderSize : TDataSize;
    function GetRequestSize : TDataSize;
    function GetContentLengthDownload : LongWord;
    function GetContentLengthUpload : LongWord;
    function GetNumConnects : Longint;
    function GetPrimaryPort : Longint;
    function GetLocalPort : Longint;
    function GetFileTime : Int64;
    function GetTotalTime : TTimeInterval;
    function GetNameLookup : TTimeInterval;
    function GetConnectTime : TTimeInterval;
    function GetAppConnectTime : TTimeInterval;
    function GetPretransferTime : TTimeInterval;
    function GetStartTransferTime : TTimeInterval;
    function GetRedirectTime : TTimeInterval;
    function GetRetryAfterDelay : TTimeInterval;
    function GetOsErrno : Longint;
    function GetLastSocket : Longint;
    function GetActiveSocket : curl_socket_t;
    function GetFTPEntryPath : string;
    function GetConditionUnmet : Boolean;
    function GetRTSPSessionID : string;
    function GetRTSPClientCSeq : Longint;
    function GetRTSPServerCSeq : Longint;
    function GetRTSPReceivedCSeq : Longint;
    function GetScheme : string;
    function GetPrivateData : Pointer;
  public
    constructor Create (s : TSession);

    (**
     * Perform a bloking file transfer
     *
     * Will perform the transfer as described in the TSession options.
     * Performs the entire request in a blocking manner and returns when done,
     * or if it failed.
     *)
    property Opened : Boolean read IsOpened;

    (**
     * Check if has errors on last request
     *)
    property HasErrors : Boolean read CheckErrors;

    (**
     * Return last error message or empty string if none
     *)
    property ErrorMessage : string read GetErrorMessage;

    (**
     * Get the last used URL
     *
     * Get the last used effective URL. In cases when you've asked libcurl to
     * follow redirects, it may very well not be the same value you set.
     *)
    property EffectiveUrl : string read GetEffectiveUrl;

    (**
     * Get the URL a redirect would go to
     *)
    property RedirectUrl : string read GetRedirectUrl;

    (**
     * Get Content-Type
     *
     * This is the value read from the Content-Type: field. If you get empty,
     * it means that the server didn't send a valid Content-Type header or that
     * the protocol used doesn't support this.
     *)
    property ContentType : string read GetContentType;

    (**
     * Get IP address of last connection
     *)
    property PrimaryIP : string read GetPrimaryIP;

    (**
     * Get local IP address of last connection
     *)
    property LocalIP : string read GetLocalIP;

    (**
     * Get the last response code
     *)
    property ResponseCode : Longint read GetResponseCode;

    (**
     * Get the response content
     *)
    property Content : string read GetContent;

    (**
     * Get the result of the certificate verification
     *)
    property VerifySSLResult : boolean read GetVerifySSLResult;

    (**
     * Get the result of the proxy certificate verification
     *
     * This is only used for HTTPS proxies.
     *)
    property VerifySSLProxyResult : boolean read GetVerifySSLProxyResult;

    (**
     * Get the CONNECT response code
     *
     * Last received HTTP proxy response code to a CONNECT request.
     *)
    property ConnectResponseCode : Longint read GetConnectResponseCode;

    (**
     * Get the HTTP version used in the connection
     *)
    property HttpVersion : TSession.THTTPProperty.THttpVersion
      read GetHttpVersion;

    (**
     * Get the number of redirects
     *)
    property RedirectCount : Longint read GetRedirectCount;

    (**
     * Get the number of uploaded bytes
     *)
    property Uploaded : TDataSize read GetUploaded;

    (**
     * Get the number of downloaded bytes
     *)
    property Downloaded : TDataSize read GetDownloaded;

    (**
     * Get download speed per second
     *)
    property DownloadSpeed : TDataSize read GetDownloadSpeed;

    (**
     * Get upload speed per second
     *)
    property UploadSpeed : TDataSize read GetUploadSpeed;

    (**
     * Get size of retrieved headers
     *)
    property HeaderSize : TDataSize read GetHeaderSize;

    (**
     * Get size of sent request
     *)
    property RequestSize : TDataSize read GetRequestSize;

    (**
     * Get content-length of download
     *)
    property ContentLengthDownload : LongWord read GetContentLengthDownload;

    (**
     * Get the specified size of the upload
     *)
    property ContentLengthUpload : LongWord read GetContentLengthUpload;

    (**
     * Get number of created connections
     *)
    property NumConnects : Longint read GetNumConnects;

    (**
     * Get the latest destination port number
     *)
    property PrimaryPort : Longint read GetPrimaryPort;

    (**
     * Get the latest local port number
     *)
    property LocalPort : Longint read GetLocalPort;

    (**
     * Get the remote time of the retrieved document
     *)
    property FileTime : Int64 read GetFileTime;

    (**
     * Get total time of previous transfer
     *)
    property TotalTime : TTimeInterval read GetTotalTime;

    (**
     * Get the name lookup time
     *)
    property NameLookup : TTimeInterval read GetNameLookup;

    (**
     * Get the time until connect
     *)
    property ConnectTime : TTimeInterval read GetConnectTime;

    (**
     * Get the time until the SSL/SSH handshake is completed
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property AppConnectTime : TTimeInterval read GetAppConnectTime;

    (**
     * Get the time until the file transfer start
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property PretransferTime : TTimeInterval read GetPretransferTime;

    (**
     * Get time until the first byte is received
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property StartTransferTime : TTimeInterval read GetStartTransferTime;

    (**
     * Get the time for all redirection steps
     *
     * When a redirect is followed, the time from each request is added together.
     *)
    property RedirectTime : TTimeInterval read GetRedirectTime;

    (**
     * Returns the Retry-After retry delay
     *
     * The information from the "Retry-After:" header. Returns zero delay if
     * there was no header.
     *)
    property RetryAfterDelay : TTimeInterval read GetRetryAfterDelay;

    (**
     * Get errno number from last connect failure
     *)
    property OsErrno : Longint read GetOsErrno;

    (**
     * Get the last socket used
     *
     * If the socket is no longer valid, -1 is returned. When you finish working
     * with the socket, you must call curl_easy_cleanup() as usual and let
     * libcurl close the socket and cleanup other resources associated with the
     * handle.
     *)
    property LastSocket : Longint read GetLastSocket;

    (**
     * Get the active socket
     *)
    property ActiveSocket : curl_socket_t read GetActiveSocket;

    (**
     * Get entry path in FTP server
     *)
    property FTPEntryPath : string read GetFTPEntryPath;

    (**
     * Get info on unmet time conditional
     *
     * Receive the TRUE if the condition provided in the previous request didn't
     * match. Alas, if this returns a TRUE you know that the reason you didn't
     * get data in return is because it didn't fulfill the condition.
     *)
    property ConditionUnmet : Boolean read GetConditionUnmet;

    (**
     * Get RTSP session ID
     *
     * Applications wishing to resume an RTSP session on another connection
     * should retrieve this info before closing the active connection.
     *)
    property RTSPSessionId : string read GetRTSPSessionId;

    (**
     * Get the next RTSP client CSeq
     *
     * Receive the next CSeq that will be used by the application.
     *)
    property RTSPClientCSeq : Longint read GetRTSPClientCSeq;

    (**
     * Get the next RTSP server CSeq
     *)
    property RTSPServerCSeq : Longint read GetRTSPServerCSeq;

    (**
     * Get the recently received CSeq
     *)
    property RTSPReceivedCSeq : Longint read GetRTSPReceivedCSeq;

    (**
     * Get the URL scheme (sometimes called protocol) used in the connection
     *)
    property Scheme : string read GetScheme;

    (**
     * Get private pointer
     *
     * Pass a pointer to a char pointer to receive the pointer to the private
     * data associated with the curl handle (set with the CURLOPT_PRIVATE).
     * Please note that for internal reasons, the value is returned as a char
     * pointer, although effectively being a Pointer.
     *)
    property PrivateData : Pointer read GetPrivateData;
  end;

implementation

{ TSession.TLinkedList }

constructor TSession.TLinkedList.Create;
begin
  FList := nil;
end;

destructor TSession.TLinkedList.Destroy;
begin
  curl_slist_free_all(FList);
  inherited Destroy;
end;

procedure TSession.TLinkedList.Append(AString: string);
begin
  FList := curl_slist_append(FList, PChar(AString));
end;

{ TDataSize }

function TDataSize.GetBytes: QWord;
begin
  Result := FBytes.Value;
end;

procedure TDataSize.SetBytes(ASize: QWord);
begin
  if ASize <= High(TByteRange) then
  begin
    FBytes.Value := ASize;
  end else
  begin
    FBytes.Value := ASize mod 1024;
    Kilobytes := ASize div 1024;
  end;
end;

function TDataSize.GetKilobytes: QWord;
begin
  Result := FKilobytes.Value;
end;

procedure TDataSize.SetKilobytes(ASize: QWord);
begin
  if ASize <= High(TKilobyteRange) then
  begin
    FKilobytes.Value := ASize;
  end else
  begin
    FKilobytes.Value := ASize mod 1024;
    Megabytes := ASize div 1024;
  end;
end;

function TDataSize.GetMegabytes: QWord;
begin
  Result := FMegabytes.Value;
end;

procedure TDataSize.SetMegabytes(ASize: QWord);
begin
  if ASize <= High(TMegabyteRange) then
  begin
    FMegabytes.Value := ASize;
  end else
  begin
    FMegabytes.Value := ASize mod 1024;
    Gigabytes := ASize div 1024;
  end;
end;

function TDataSize.GetGigabytes: QWord;
begin
  Result := FGigabytes.Value;
end;

procedure TDataSize.SetGigabytes(ASize: QWord);
begin
  FGigabytes.Value := ASize;
end;

constructor TDataSize.Create;
begin
  FBytes := TByte.Create;
  FKilobytes := TKilobyte.Create;
  FMegabytes := TMegabyte.Create;
  FGigabytes := TGigabyte.Create;
end;

destructor TDataSize.Destroy;
begin
  FreeAndNil(FBytes);
  FreeAndNil(FKilobytes);
  FreeAndNil(FMegabytes);
  FreeAndNil(FGigabytes);

  inherited Destroy;
end;

function TDataSize.ToBytes: QWord;
begin
  Result := 0;
  if FGigabytes.Value > 0 then
    Result := Result + (FGigabytes.Value * 1073741824);
  if FMegabytes.Value > 0 then
    Result := Result + (FMegabytes.Value * 1048576);
  if FKilobytes.Value > 0 then
    Result := Result + (FKilobytes.Value * 1024);
  if FBytes.Value > 0 then
    Result := Result + FBytes.Value;
end;

function TDataSize.ToKilobytes: QWord;
begin
  Result := 0;
  if FGigabytes.Value > 0 then
    Result := Result + (FGigabytes.Value * 1048576);
  if FMegabytes.Value > 0 then
    Result := Result + (FMegabytes.Value * 1024);
  if FKilobytes.Value > 0 then
    Result := Result + FKilobytes.Value;
end;

function TDataSize.ToMegabytes: QWord;
begin
  Result := 0;
  if FGigabytes.Value > 0 then
    Result := Result + (FGigabytes.Value * 1024);
  if FMegabytes.Value > 0 then
    Result := Result + FMegabytes.Value;
end;

function TDataSize.ToGigabytes: QWord;
begin
  Result := FGigabytes.Value;
end;

function TDataSize.ToString (ASuffix : string): string;
begin
  if FGigabytes.Value > 0 then
  begin
    Result := Format('%0.2d,%0.2d',
      [FGigabytes.Value, FMegabytes.Value]) + ' GiB' + ASuffix;
  end else
  if FMegabytes.Value > 0 then
  begin
    Result := Format('%0.2d,%0.2d',
      [FMegabytes.Value, FKilobytes.Value]) + ' MiB' + ASuffix;
  end else
  if FKilobytes.Value > 0 then
  begin
    Result := Format('%0.2d,%0.2d',
      [FKilobytes.Value, FBytes.Value]) + ' KiB' + ASuffix;
  end else
  begin
    Result := Format('%0.2d', [FBytes.Value]) + ' B' + ASuffix;
  end;
end;

{ TDataSize.TGigabyte }

constructor TDataSize.TGigabyte.Create;
begin
  FGigabytes := 0;
end;

constructor TDataSize.TGigabyte.Create(ASize: TGigabyteRange);
begin
  FGigabytes := ASize;
end;

constructor TDataSize.TGigabyte.Create(ASize: TGigabyte);
begin
  FGigabytes := ASize.Value;
end;

destructor TDataSize.TGigabyte.Destroy;
begin
  inherited Destroy;
end;

{ TDataSize.TMegabyte }

constructor TDataSize.TMegabyte.Create;
begin
  FMegabytes := 0;
end;

constructor TDataSize.TMegabyte.Create(ASize: TMegabyteRange);
begin
  FMegabytes := ASize;
end;

constructor TDataSize.TMegabyte.Create(ASize: TMegabyte);
begin
  FMegabytes := ASize.Value;
end;

destructor TDataSize.TMegabyte.Destroy;
begin
  inherited Destroy;
end;

{ TDataSize.TKilobyte }

constructor TDataSize.TKilobyte.Create;
begin
  FKilobytes := 0;
end;

constructor TDataSize.TKilobyte.Create(ASize: TKilobyteRange);
begin
  FKilobytes := ASize;
end;

constructor TDataSize.TKilobyte.Create(ASize: TKilobyte);
begin
  FKilobytes := ASize.Value;
end;

destructor TDataSize.TKilobyte.Destroy;
begin
  inherited Destroy;
end;

{ TDataSize.TByte }

constructor TDataSize.TByte.Create;
begin
  FBytes := 0;
end;

constructor TDataSize.TByte.Create(ASize: TByteRange);
begin
  FBytes := ASize;
end;

constructor TDataSize.TByte.Create(ASize: TByte);
begin
  FBytes := ASize.Value;
end;

destructor TDataSize.TByte.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval }

function TTimeInterval.GetMicroseconds: QWord;
begin
  Result := FMicroseconds.Value;
end;

procedure TTimeInterval.SetMicroseconds(AValue: Qword);
begin
  if AValue <= High(TMicrosecondRange) then
  begin
    FMicroseconds.Value := AValue;
  end else
  begin
    FMicroseconds.Value := AValue mod 1000;
    Milliseconds := AValue div 1000;
  end;
end;

function TTimeInterval.GetMilliseconds: QWord;
begin
  Result := FMilliseconds.Value;
end;

procedure TTimeInterval.SetMilliseconds(AValue: QWord);
begin
  if AValue <= High(TMillisecondRange) then
  begin
    FMilliseconds.Value := AValue;
  end else
  begin
    FMilliseconds.Value := AValue mod 1000;
    Seconds := AValue div 1000;
  end;
end;

function TTimeInterval.GetSeconds: QWord;
begin
  Result := FSeconds.Value;
end;

procedure TTimeInterval.SetSeconds(AValue: QWord);
begin
  if AValue <= High(TSecondRange) then
  begin
    FSeconds.Value := AValue;
  end else
  begin
    FSeconds.Value := AValue mod 60;
    Minutes := AValue div 60;
  end;
end;

function TTimeInterval.GetMinutes: QWord;
begin
  Result := FMinutes.Value;
end;

procedure TTimeInterval.SetMinutes(AValue: QWord);
begin
  if AValue <= High(TMinuteRange) then
  begin
    FMinutes.Value := AValue;
  end else
  begin
    FMinutes.Value := AValue mod 60;
    Hours := AValue div 60;
  end;
end;

function TTimeInterval.GetHours: QWord;
begin
  Result := FHours.Value;
end;

procedure TTimeInterval.SetHours(AValue: QWord);
begin
  FHours.Value := AValue;
end;

function TTimeInterval.ToString(ASuffix : string): string;
begin
  Result := Format('%0.2d:%0.2d:%0.2d.%0.3d%0.3d',
    [FHours.Value, FMinutes.Value, FSeconds.Value, FMilliseconds.Value,
    FMicroseconds.Value]) + ASuffix;
end;

constructor TTimeInterval.Create;
begin
  FMicroseconds := TMicrosecond.Create;
  FMilliseconds := TMillisecond.Create;
  FSeconds := TSecond.Create;
  FMinutes := TMinute.Create;
  FHours := THour.Create;
end;

destructor TTimeInterval.Destroy;
begin
  FreeAndNil(FMicroseconds);
  FreeAndNil(FMilliseconds);
  FreeAndNil(FSeconds);
  FreeAndNil(FMinutes);
  FreeAndNil(FHours);
  inherited Destroy;
end;

function TTimeInterval.ToMicroseconds: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 3600000000);
  if FMinutes.Value > 0 then
    Result := Result + (FMinutes.Value * 60000000);
  if FSeconds.Value > 0 then
    Result := Result + (FSeconds.Value * 1000000);
  if FMilliseconds.Value > 0 then
    Result := Result + (FMilliseconds.Value * 1000);
  if FMicroseconds.Value > 0 then
    Result := Result + FMicroseconds.Value;
end;

function TTimeInterval.ToMilliseconds: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 3600000);
  if FMinutes.Value > 0 then
    Result := Result + (FMinutes.Value * 60000);
  if FSeconds.Value > 0 then
    Result := Result + (FSeconds.Value * 1000);
  if FMilliseconds.Value > 0 then
    Result := Result + FMilliseconds.Value;
end;

function TTimeInterval.ToSeconds: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 3600);
  if FMinutes.Value > 0 then
    Result := Result + (FMinutes.Value * 60);
  if FSeconds.Value > 0 then
    Result := Result + FSeconds.Value;
end;

function TTimeInterval.ToMinutes: QWord;
begin
  Result := 0;
  if FHours.Value > 0 then
    Result := Result + (FHours.Value * 60);
  if FMinutes.Value > 0 then
    Result := Result + FMinutes.Value;
end;

function TTimeInterval.ToHours: QWord;
begin
  Result := FHours.Value;
end;

{ TTimeInterval.THour }

constructor TTimeInterval.THour.Create;
begin
  FHours := 0;
end;

constructor TTimeInterval.THour.Create(AInterval: THour);
begin
  FHours := AInterval.FHours;
end;

constructor TTimeInterval.THour.Create(AInterval: THourRange);
begin
  FHours := AInterval;
end;

destructor TTimeInterval.THour.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TMinute }

constructor TTimeInterval.TMinute.Create;
begin
  FMinutes := 0;
end;

constructor TTimeInterval.TMinute.Create(AInterval: TMinuteRange);
begin
  FMinutes := AInterval;
end;

constructor TTimeInterval.TMinute.Create(AInterval: TMinute);
begin
  FMinutes := AInterval.FMinutes;
end;

destructor TTimeInterval.TMinute.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TSecond }

constructor TTimeInterval.TSecond.Create;
begin
  FSeconds := 0;
end;

constructor TTimeInterval.TSecond.Create(AInterval: TSecondRange);
begin
  FSeconds := AInterval;
end;

constructor TTimeInterval.TSecond.Create(AInterval: TSecond);
begin
  FSeconds := AInterval.FSeconds;
end;

destructor TTimeInterval.TSecond.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TMillisecond }

constructor TTimeInterval.TMillisecond.Create;
begin
  FMilliseconds := 0;
end;

constructor TTimeInterval.TMillisecond.Create(AInterval: TMillisecondRange);
begin
  FMilliseconds := AInterval;
end;

constructor TTimeInterval.TMillisecond.Create(AInterval: TMillisecond);
begin
  FMilliseconds := AInterval.FMilliseconds;
end;

destructor TTimeInterval.TMillisecond.Destroy;
begin
  inherited Destroy;
end;

{ TTimeInterval.TMicrosecond }

constructor TTimeInterval.TMicrosecond.Create;
begin
  FMicroseconds := 0;
end;

constructor TTimeInterval.TMicrosecond.Create(AInterval: TMicrosecondRange);
begin
  FMicroseconds := AInterval;
end;

constructor TTimeInterval.TMicrosecond.Create(AInterval: TMicrosecond);
begin
  FMicroseconds := AInterval.FMicroseconds;
end;

destructor TTimeInterval.TMicrosecond.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TRTSPProperty }

procedure TSession.TRTSPProperty.SetRequest(AReq: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_RTSP_REQUEST, AReq);
end;

procedure TSession.TRTSPProperty.SetSessionID(AId: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_RTSP_SESSION_ID, PChar(AId));
end;

procedure TSession.TRTSPProperty.SetStreamURI(AURI: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_RTSP_STREAM_URI, PChar(AURI));
end;

procedure TSession.TRTSPProperty.SetTransport(ATransport: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_RTSP_TRANSPORT, PChar(ATransport));
end;

procedure TSession.TRTSPProperty.SetClientCSeq(ACSeq: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_RTSP_CLIENT_CSEQ, ACSeq);
end;

procedure TSession.TRTSPProperty.SetServerCSeq(ACSeq: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_RTSP_SERVER_CSEQ, ACSeq);
end;

procedure TSession.TRTSPProperty.SetRange(ARange: string);
begin
  if ARange <> '' then
    curl_easy_setopt(FHandle, CURLOPT_RANGE, PChar(ARange))
  else
    curl_easy_setopt(FHandle, CURLOPT_RANGE, 0);
end;

constructor TSession.TRTSPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TRTSPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TTFTPProperty }

procedure TSession.TTFTPProperty.SetBlockSize(ASize: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_TFTP_BLKSIZE, ASize);
end;

procedure TSession.TTFTPProperty.SetNoOptions(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TFTP_NO_OPTIONS, Longint(AEnable));
end;

constructor TSession.TTFTPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TTFTPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TSMTPProperty }

procedure TSession.TSMTPProperty.SetMailFrom(AFrom: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAIL_FROM, PChar(AFrom));
end;

procedure TSession.TSMTPProperty.SetMailAuth(AAuth: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAIL_AUTH, PChar(AAuth));
end;

procedure TSession.TSMTPProperty.SetCustomRequest(ARequest: string);
begin
  if ARequest <> '' then
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, PChar(ARequest))
  else
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, 0);
end;

procedure TSession.TSMTPProperty.SetConnectOnly(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_CONNECT_ONLY, Longint(AEnable));
end;

constructor TSession.TSMTPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TSMTPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.THTTPCookie }

procedure TSession.THTTPCookie.SetCookie(ACookie: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIE, PChar(ACookie));
end;

procedure TSession.THTTPCookie.SetCookieFile(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIEFILE, PChar(AFile));
end;

procedure TSession.THTTPCookie.SetCookieJar(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIEJAR, PChar(AFile));
end;

procedure TSession.THTTPCookie.SetCookieSession(ACreate: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIESESSION, Longint(ACreate));
end;

procedure TSession.THTTPCookie.SetCookieList(ACookie: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_COOKIELIST, PChar(ACookie));
end;

constructor TSession.THTTPCookie.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.THTTPCookie.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TFTPProperty }

procedure TSession.TFTPProperty.SetFTPPort(APort: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTPPORT, PChar(APort));
end;

procedure TSession.TFTPProperty.SetAppendUpload(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_APPEND, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetUseEPRT(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_USE_EPRT, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetUseEPSV(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_USE_EPSV, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetUsePRET(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_USE_PRET, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetCreateMissingDirs(ACreate: TCreateDirs);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_CREATE_MISSING_DIRS, Longint(ACreate));
end;

procedure TSession.TFTPProperty.SetResponseTimeout(ATimeout: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_RESPONSE_TIMEOUT,
    Longint(ATimeout.Seconds));
end;

procedure TSession.TFTPProperty.SetAlternativeToUser(ACmd: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_ALTERNATIVE_TO_USER, PChar(ACmd));
end;

procedure TSession.TFTPProperty.SetSkipPASVIP(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_SKIP_PASV_IP, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetSSLAuth(AAuth: TAuth);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTPSSLAUTH, Longint(AAuth));
end;

procedure TSession.TFTPProperty.SetSSLCCC(ACCC: TSSL_CCC);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_SSL_CCC, Longint(ACCC));
end;

procedure TSession.TFTPProperty.SetAccountInfo(AAccount: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_ACCOUNT, PChar(AAccount));
end;

procedure TSession.TFTPProperty.SetFileMethod(AMethod: TFileMethod);
begin
  curl_easy_setopt(FHandle, CURLOPT_FTP_FILEMETHOD, Longint(AMethod));
end;

procedure TSession.TFTPProperty.SetTransferText(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TRANSFERTEXT, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetProxyTransferMode(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_TRANSFER_MODE, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetRange(ARange: string);
begin
  if ARange <> '' then
    curl_easy_setopt(FHandle, CURLOPT_RANGE, PChar(ARange))
  else
    curl_easy_setopt(FHandle, CURLOPT_RANGE, 0);
end;

procedure TSession.TFTPProperty.SetStartTransferFrom(AFrom: curl_off_t);
begin
  curl_easy_setopt(FHandle, CURLOPT_RESUME_FROM_LARGE, AFrom);
end;

procedure TSession.TFTPProperty.SetCustomRequest(ARequest: string);
begin
  if ARequest <> '' then
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, PChar(ARequest))
  else
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, 0);
end;

procedure TSession.TFTPProperty.SetTimeModification(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FILETIME, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetDirListOnly(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_DIRLISTONLY, Longint(AEnable));
end;

procedure TSession.TFTPProperty.SetMaxDownloadFileSize(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAXFILESIZE_LARGE, ASize.Bytes);
end;

procedure TSession.TFTPProperty.SetAcceptTimeout(ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_ACCEPTTIMEOUT_MS,
    Longint(ATime.Milliseconds));
end;

procedure TSession.TFTPProperty.SetWildcardMatch(AMatch: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_WILDCARDMATCH, Longint(AMatch));
end;

constructor TSession.TFTPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TFTPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TIMAPProperty }

procedure TSession.TIMAPProperty.SetLoginOptions(AOptions: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOGIN_OPTIONS, PChar(AOptions));
end;

procedure TSession.TIMAPProperty.SetSASLAuthzid(AAuthzid: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SASL_AUTHZID, PChar(AAuthzid));
end;

procedure TSession.TIMAPProperty.SetSASLIR(ASend: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_SASL_IR, Longint(ASend));
end;

procedure TSession.TIMAPProperty.SetXOAuth2Bearer(AToken: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_XOAUTH2_BEARER, PChar(AToken));
end;

procedure TSession.TIMAPProperty.SetCustomRequest(ARequest: string);
begin
  if ARequest <> '' then
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, PChar(ARequest))
  else
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, 0);
end;

procedure TSession.TIMAPProperty.SetConnectOnly(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_CONNECT_ONLY, Longint(AEnable));
end;

constructor TSession.TIMAPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TIMAPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TProtocolProperty }

procedure TSession.TProtocolProperty.SetAllowedProtocols
  (AProtocols: TProtocols);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if PROTOCOL_DICT in AProtocols then
    bitmask := bitmask or CURLPROTO_DICT;
  if PROTOCOL_FILE in AProtocols then
    bitmask := bitmask or CURLPROTO_FILE;
  if PROTOCOL_FTP in AProtocols then
    bitmask := bitmask or CURLPROTO_FTP;
  if PROTOCOL_FTPS in AProtocols then
    bitmask := bitmask or CURLPROTO_FTPS;
  if PROTOCOL_GOPHER in AProtocols then
    bitmask := bitmask or CURLPROTO_GOPHER;
  if PROTOCOL_HTTP in AProtocols then
    bitmask := bitmask or CURLPROTO_HTTP;
  if PROTOCOL_HTTPS in AProtocols then
    bitmask := bitmask or CURLPROTO_HTTPS;
  if PROTOCOL_IMAP in AProtocols then
    bitmask := bitmask or CURLPROTO_IMAP;
  if PROTOCOL_IMAPS in AProtocols then
    bitmask := bitmask or CURLPROTO_IMAPS;
  if PROTOCOL_LDAP in AProtocols then
    bitmask := bitmask or CURLPROTO_LDAP;
  if PROTOCOL_LDAPS in AProtocols then
    bitmask := bitmask or CURLPROTO_LDAPS;
  if PROTOCOL_POP3 in AProtocols then
    bitmask := bitmask or CURLPROTO_POP3;
  if PROTOCOL_POP3S in AProtocols then
    bitmask := bitmask or CURLPROTO_POP3S;
  if PROTOCOL_RTMP in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMP;
  if PROTOCOL_RTMPE in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPE;
  if PROTOCOL_RTMPS in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPS;
  if PROTOCOL_RTMPT in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPT;
  if PROTOCOL_RTMPTE in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPTE;
  if PROTOCOL_RTMPTS in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPTS;
  if PROTOCOL_RTSP in AProtocols then
    bitmask := bitmask or CURLPROTO_RTSP;
  if PROTOCOL_SCP in AProtocols then
    bitmask := bitmask or CURLPROTO_SCP;
  if PROTOCOL_SFTP in AProtocols then
    bitmask := bitmask or CURLPROTO_SFTP;
  if PROTOCOL_SMB in AProtocols then
    bitmask := bitmask or CURLPROTO_SMB;
  if PROTOCOL_SMBS in AProtocols then
    bitmask := bitmask or CURLPROTO_SMBS;
  if PROTOCOL_SMTP in AProtocols then
    bitmask := bitmask or CURLPROTO_SMTP;
  if PROTOCOL_SMTPS in AProtocols then
    bitmask := bitmask or CURLPROTO_SMTPS;
  if PROTOCOL_TELNET in AProtocols then
    bitmask := bitmask or CURLPROTO_TELNET;
  if PROTOCOL_TFTP in AProtocols then
    bitmask := bitmask or CURLPROTO_TFTP;

  curl_easy_setopt(FHandle, CURLOPT_PROTOCOLS, bitmask);
end;

procedure TSession.TProtocolProperty.SetAllowedRedirectProtocols
  (AProtocols: TProtocols);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if PROTOCOL_DICT in AProtocols then
    bitmask := bitmask or CURLPROTO_DICT;
  if PROTOCOL_FILE in AProtocols then
    bitmask := bitmask or CURLPROTO_FILE;
  if PROTOCOL_FTP in AProtocols then
    bitmask := bitmask or CURLPROTO_FTP;
  if PROTOCOL_FTPS in AProtocols then
    bitmask := bitmask or CURLPROTO_FTPS;
  if PROTOCOL_GOPHER in AProtocols then
    bitmask := bitmask or CURLPROTO_GOPHER;
  if PROTOCOL_HTTP in AProtocols then
    bitmask := bitmask or CURLPROTO_HTTP;
  if PROTOCOL_HTTPS in AProtocols then
    bitmask := bitmask or CURLPROTO_HTTPS;
  if PROTOCOL_IMAP in AProtocols then
    bitmask := bitmask or CURLPROTO_IMAP;
  if PROTOCOL_IMAPS in AProtocols then
    bitmask := bitmask or CURLPROTO_IMAPS;
  if PROTOCOL_LDAP in AProtocols then
    bitmask := bitmask or CURLPROTO_LDAP;
  if PROTOCOL_LDAPS in AProtocols then
    bitmask := bitmask or CURLPROTO_LDAPS;
  if PROTOCOL_POP3 in AProtocols then
    bitmask := bitmask or CURLPROTO_POP3;
  if PROTOCOL_POP3S in AProtocols then
    bitmask := bitmask or CURLPROTO_POP3S;
  if PROTOCOL_RTMP in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMP;
  if PROTOCOL_RTMPE in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPE;
  if PROTOCOL_RTMPS in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPS;
  if PROTOCOL_RTMPT in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPT;
  if PROTOCOL_RTMPTE in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPTE;
  if PROTOCOL_RTMPTS in AProtocols then
    bitmask := bitmask or CURLPROTO_RTMPTS;
  if PROTOCOL_RTSP in AProtocols then
    bitmask := bitmask or CURLPROTO_RTSP;
  if PROTOCOL_SCP in AProtocols then
    bitmask := bitmask or CURLPROTO_SCP;
  if PROTOCOL_SFTP in AProtocols then
    bitmask := bitmask or CURLPROTO_SFTP;
  if PROTOCOL_SMB in AProtocols then
    bitmask := bitmask or CURLPROTO_SMB;
  if PROTOCOL_SMBS in AProtocols then
    bitmask := bitmask or CURLPROTO_SMBS;
  if PROTOCOL_SMTP in AProtocols then
    bitmask := bitmask or CURLPROTO_SMTP;
  if PROTOCOL_SMTPS in AProtocols then
    bitmask := bitmask or CURLPROTO_SMTPS;
  if PROTOCOL_TELNET in AProtocols then
    bitmask := bitmask or CURLPROTO_TELNET;
  if PROTOCOL_TFTP in AProtocols then
    bitmask := bitmask or CURLPROTO_TFTP;

  curl_easy_setopt(FHandle, CURLOPT_REDIR_PROTOCOLS, bitmask);
end;

procedure TSession.TProtocolProperty.SetDefaultProtocol(AProtocol: TProtocol);
var
  protocol : string;
begin
  protocol := GetEnumName(TypeInfo(TProtocol), Ord(AProtocol));
  protocol := LowerCase(Copy(protocol, Length('PROTOCOL_') + 1,
    Length(protocol) - Length('PROTOCOL_') + 1));
  curl_easy_setopt(FHandle, CURLOPT_DEFAULT_PROTOCOL, PChar(protocol));
end;

procedure TSession.TProtocolProperty.SetFollowRedirect(AFollow: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FOLLOWLOCATION, Longint(AFollow));
end;

procedure TSession.TProtocolProperty.SetMaxRedirects(AAmount: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAXREDIRS, AAmount);
end;

procedure TSession.TProtocolProperty.SetNoBody(ANoBody: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_NOBODY, Longint(ANoBody));
end;

procedure TSession.TProtocolProperty.SetVerbose(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_VERBOSE, Longint(AEnable));
end;

procedure TSession.TProtocolProperty.SetIncludeHeader(AIncludeHeader: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HEADER, Longint(AIncludeHeader));
end;

procedure TSession.TProtocolProperty.SetIgnoreContentLength(
  AIgnoreLength: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_IGNORE_CONTENT_LENGTH,
    Longint(AIgnoreLength));
end;

procedure TSession.TProtocolProperty.SetTransferEncoding(AEncoding: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TRANSFER_ENCODING, Longint(AEncoding));
end;

procedure TSession.TProtocolProperty.SetRemotePort(APort: Word);
begin
  curl_easy_setopt(FHandle, CURLOPT_PORT, Longint(APort));
end;

procedure TSession.TProtocolProperty.SetUpload(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_UPLOAD, Longint(AEnable));
end;

constructor TSession.TProtocolProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TProtocolProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.THTTPProperty }

procedure TSession.THTTPProperty.SetUserAgent(AAgent: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_USERAGENT, PChar(AAgent));
end;

procedure TSession.THTTPProperty.SetAutoReferer(AUpdateHeaders: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_AUTOREFERER, Longint(AUpdateHeaders));
end;

procedure TSession.THTTPProperty.SetHTTPAuth (
  AMethod: TSecurityProperty.TAuthMethods);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if AUTH_BASIC in AMethod then
    bitmask := bitmask or CURLAUTH_BASIC;
  if AUTH_DIGEST in AMethod then
    bitmask := bitmask or CURLAUTH_DIGEST;
  if AUTH_NEGOTIATE in AMethod then
    bitmask := bitmask or CURLAUTH_NEGOTIATE;
  if AUTH_GSSAPI in AMethod then
    bitmask := bitmask or CURLAUTH_GSSAPI;
  if AUTH_NTLM in AMethod then
    bitmask := bitmask or CURLAUTH_NTLM;
  if AUTH_DIGEST_IE in AMethod then
    bitmask := bitmask or CURLAUTH_DIGEST_IE;
  if AUTH_NTLM_WB in AMethod then
    bitmask := bitmask or CURLAUTH_NTLM_WB;
  if AUTH_BEARER in AMethod then
    bitmask := bitmask or CURLAUTH_BEARER;
  if AUTH_ANY in AMethod then
    bitmask := CURLAUTH_BASIC or CURLAUTH_DIGEST or CURLAUTH_NEGOTIATE or
      CURLAUTH_NTLM or CURLAUTH_DIGEST_IE or CURLAUTH_NTLM_WB or
      CURLAUTH_BEARER;
  if AUTH_ANYSAFE in AMethod then
    bitmask := bitmask or CURLAUTH_DIGEST or CURLAUTH_NEGOTIATE or
      CURLAUTH_NTLM or CURLAUTH_NTLM_WB or CURLAUTH_BEARER;

  curl_easy_setopt(FHandle, CURLOPT_HTTPAUTH, bitmask);
end;

procedure TSession.THTTPProperty.SetUnrestrictedAuth(ASend: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_UNRESTRICTED_AUTH, Longint(ASend));
end;

procedure TSession.THTTPProperty.SetPostRedirect(ARedirect: TPostRedirects);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if REDIRECT_POST_301 in ARedirect then
    bitmask := bitmask or CURL_REDIR_POST_301;
  if REDIRECT_POST_302 in ARedirect then
    bitmask := bitmask or CURL_REDIR_POST_302;
  if REDIRECT_POST_303 in ARedirect then
    bitmask := bitmask or CURL_REDIR_POST_303;
  if REDIRECT_POST_ALL in ARedirect then
    bitmask := CURL_REDIR_POST_ALL;

  curl_easy_setopt(FHandle, CURLOPT_POSTREDIR, bitmask);
end;

procedure TSession.THTTPProperty.SetPutMethod(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_PUT, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetPostMethod(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_POST, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetPostFields(AData: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_POSTFIELDS, PChar(AData));
end;

procedure TSession.THTTPProperty.SetPostFieldsSize(ASize: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_POSTFIELDSIZE_LARGE, ASize);
end;

procedure TSession.THTTPProperty.SetAcceptEncoding(AEncodings: TEncodings);

  function GetEncodingName (AEncoding : TEncoding) : string;
  var
    alg : string;
  begin
    alg := GetEnumName(TypeInfo(TEncodings), Ord(AEncoding));
    Result := LowerCase(Copy(alg, Length('ENCODE_') + 1, Length(alg) -
      Length('ENCODE_') + 1));
  end;

  procedure UpdateEncodeString (var encode : string);
  begin
    if encode <> '' then
      encode := encode + ', ';
  end;

var
  enc : string = '';
begin
  if AEncodings = [ENCODE_NONE] then
  begin
    curl_easy_setopt(FHandle, CURLOPT_ACCEPT_ENCODING, 0);
  end else
  begin
    if ENCODE_DEFLATE in AEncodings then
    begin
      UpdateEncodeString(enc);
      enc := enc + GetEncodingName(ENCODE_DEFLATE);
    end;
    if ENCODE_GZIP in AEncodings then
    begin
      UpdateEncodeString(enc);
      enc := enc + GetEncodingName(ENCODE_GZIP);
    end;
    if ENCODE_BR in AEncodings then
    begin
      UpdateEncodeString(enc);
      enc := enc + GetEncodingName(ENCODE_BR);
    end;

    curl_easy_setopt(FHandle, CURLOPT_ACCEPT_ENCODING, PChar(enc));
  end;
end;

procedure TSession.THTTPProperty.SetTransferEncoding(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TRANSFER_ENCODING, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetReferer(AWhere: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_REFERER, PChar(AWhere));
end;

procedure TSession.THTTPProperty.SetAltSvcCacheFile(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_ALTSVC, PChar(AFile));
end;

procedure TSession.THTTPProperty.SetAltSvcCtrl(AAltSvc: TAltSvcs);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if [ALTSVC_DISABLE] = AAltSvc then
  begin
    curl_easy_setopt(FHandle, CURLOPT_ALTSVC_CTRL, 0);
  end else
  begin
    if ALTSVC_IMMEDIATELY in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_IMMEDIATELY;
    end;
    if ALTSVC_READONLYFILE in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_READONLYFILE;
    end;
    if ALTSVC_H1 in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_H1;
    end;
    if ALTSVC_H2 in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_H2;
    end;
    if ALTSVC_H3 in AAltSvc then
    begin
      bitmask := bitmask or CURLALTSVC_H3;
    end;
    curl_easy_setopt(FHandle, CURLOPT_ALTSVC_CTRL, bitmask);
  end;
end;

procedure TSession.THTTPProperty.SetGetMethod(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HTTPGET, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetRequestTarget(ATarget: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_REQUEST_TARGET, PChar(ATarget));
end;

procedure TSession.THTTPProperty.SetHttpVersion(AVersion: THTTPVersion);
begin
  curl_easy_setopt(FHandle, CURLOPT_HTTP_VERSION, Longint(AVersion));
end;

procedure TSession.THTTPProperty.SetHttp09Allowed(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HTTP09_ALLOWED, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetIgnoreContentLength(AIgnore: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_IGNORE_CONTENT_LENGTH, Longint(AIgnore));
end;

procedure TSession.THTTPProperty.SetHttpContentDecoding(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HTTP_CONTENT_DECODING, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetHttpTransferDecoding(AEnable: Boolean);
begin
  curl_easy_setopt(Fhandle, CURLOPT_HTTP_TRANSFER_DECODING, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetExpect100Timeout(ATimeout: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_EXPECT_100_TIMEOUT_MS,
    Longint(ATimeout.Milliseconds));
end;

procedure TSession.THTTPProperty.SetPipeWait(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_PIPEWAIT, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetStreamDepends(ADependHandle: CURL);
begin
  curl_easy_setopt(FHandle, CURLOPT_STREAM_DEPENDS, ADependHandle);
end;

procedure TSession.THTTPProperty.SetStreamDependsExclusive(ADependHandle: CURL);
begin
  curl_easy_setopt(FHandle, CURLOPT_STREAM_DEPENDS_E, ADependHandle);
end;

procedure TSession.THTTPProperty.SetStreamWeight(AWeight: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_STREAM_WEIGHT, AWeight);
end;

procedure TSession.THTTPProperty.SetRange(ARange: string);
begin
  if ARange <> '' then
    curl_easy_setopt(FHandle, CURLOPT_RANGE, PChar(ARange))
  else
    curl_easy_setopt(FHandle, CURLOPT_RANGE, 0);
end;

procedure TSession.THTTPProperty.SetStartTransferFrom(AFrom: curl_off_t);
begin
  curl_easy_setopt(FHandle, CURLOPT_RESUME_FROM_LARGE, AFrom);
end;

procedure TSession.THTTPProperty.SetCustomRequest(ARequest: string);
begin
  if ARequest <> '' then
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, PChar(ARequest))
  else
    curl_easy_setopt(FHandle, CURLOPT_CUSTOMREQUEST, 0);
end;

procedure TSession.THTTPProperty.SetTimeModification(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FILETIME, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetMaxDownloadFileSize(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAXFILESIZE_LARGE, ASize.Bytes);
end;

procedure TSession.THTTPProperty.SetConnectOnly(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_CONNECT_ONLY, Longint(AEnable));
end;

procedure TSession.THTTPProperty.SetKeepSendingOnError(AKeepSending: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_KEEP_SENDING_ON_ERROR,
    Longint(AKeepSending));
end;

constructor TSession.THTTPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
  FCookie := THTTPCookie.Create(AHandle);
end;

destructor TSession.THTTPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TSecurityProperty }

procedure TSession.TSecurityProperty.SetUserPassword(AUserpwd: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_USERPWD, PChar(AUserpwd));
end;

procedure TSession.TSecurityProperty.SetUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_USERNAME, PChar(AName));
end;

procedure TSession.TSecurityProperty.SetPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PASSWORD, PChar(APassword));
end;

procedure TSession.TSecurityProperty.SetTLSUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_TLSAUTH_USERNAME, PChar(AName));
end;

procedure TSession.TSecurityProperty.SetTLSPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_TLSAUTH_PASSWORD, PChar(APassword));
end;

procedure TSession.TSecurityProperty.SetTLSAuth(AMethod: TTLSAuthMethod);
begin
  curl_easy_setopt(FHandle, CURLOPT_TLSAUTH_TYPE,
    PChar(GetEnumName(TypeInfo(TTLSAuthMethod), ord(AMethod))));
end;

procedure TSession.TSecurityProperty.SetAllowUsernameInURL(AAllow: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_DISALLOW_USERNAME_IN_URL,
    Longint(Boolean(not AAllow)));
end;

procedure TSession.TSecurityProperty.SetAuthServiceName(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SERVICE_NAME, PChar(AName));
end;

procedure TSession.TSecurityProperty.SetNetrc(AOption: TNETRCOption);
begin
  curl_easy_setopt(FHandle, CURLOPT_NETRC, Longint(AOption));
end;

procedure TSession.TSecurityProperty.SetNetrcFile(AFile: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_NETRC_FILE, PChar(AFile));
end;

procedure TSession.TSecurityProperty.SetUnrestrictedAuth(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_UNRESTRICTED_AUTH, Longint(AEnable));
end;

procedure TSession.TSecurityProperty.SetSSLCertificate(ACertificate: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SSLCERT, PChar(ACertificate));
end;

procedure TSession.TSecurityProperty.SetSSLCertificateType(AType: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SSLCERTTYPE, PChar(AType));
end;

procedure TSession.TSecurityProperty.SetSSLKey(AKey: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SSLKEY, PChar(AKey));
end;

constructor TSession.TSecurityProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TSecurityProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TDNSProperty }

procedure TSession.TDNSProperty.SetDNSCacheTimeout(ATimeout: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_DNS_CACHE_TIMEOUT,
    Longint(ATimeout.Seconds));
end;

procedure TSession.TDNSProperty.SetDNSGlobalCache(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_DNS_USE_GLOBAL_CACHE, Longint(AEnable));
end;

procedure TSession.TDNSProperty.SetDNSoverHTTPS(AUrl: string);
begin
  if AUrl <> '' then
  begin
    curl_easy_setopt(FHandle, CURLOPT_DOH_URL, PChar(AUrl));
  end else
  begin
    curl_easy_setopt(FHandle, CURLOPT_DOH_URL, 0);
  end;
end;

procedure TSession.TDNSProperty.SetDNSInterface(AInterface: string);
begin
  if AInterface <> '' then
    curl_easy_setopt(FHandle, CURLOPT_DNS_INTERFACE, PChar(AInterface))
  else
    curl_easy_setopt(FHandle, CURLOPT_DNS_INTERFACE, 0);
end;

procedure TSession.TDNSProperty.SetDNSLocalIP4(AAddress: string);
begin
  if AAddress <> '' then
    curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP4, PChar(AAddress))
  else
    curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP4, 0);
end;

procedure TSession.TDNSProperty.SetDNSLocalIP6(AAddress: string);
begin
  if AAddress <> '' then
    curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP6, PChar(AAddress))
  else
    curl_easy_setopt(FHandle, CURLOPT_DNS_LOCAL_IP6, 0);
end;

procedure TSession.TDNSProperty.SetDNSServers(AServers: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_DNS_SERVERS, PChar(AServers));
end;

procedure TSession.TDNSProperty.SetDNSShuffleAddresses(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_DNS_SHUFFLE_ADDRESSES, Longint(AEnable));
end;

constructor TSession.TDNSProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TDNSProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TSOCKS5Property }

procedure TSession.TSOCKS5Property.SetSOCKS5Auth(
  AMethod: TSecurityProperty.TAuthMethods);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if AUTH_BASIC in AMethod then
    bitmask := bitmask or CURLAUTH_BASIC;
  if AUTH_GSSAPI in AMethod then
    bitmask := bitmask or CURLAUTH_GSSAPI;

  curl_easy_setopt(FHandle, CURLOPT_SOCKS5_AUTH, bitmask);
end;

procedure TSession.TSOCKS5Property.SetSOCKS5GSSAPIServiceName(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_SOCKS5_GSSAPI_SERVICE, PChar(AName));
end;

procedure TSession.TSOCKS5Property.SetSOCKS5GSSAPINegotiation(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_SOCKS5_GSSAPI_NEC, Longint(AEnable));
end;

constructor TSession.TSOCKS5Property.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TSOCKS5Property.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TProxyProperty }

procedure TSession.TProxyProperty.SetPreProxy(APreProxy: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PRE_PROXY, PChar(APreProxy));
end;

procedure TSession.TProxyProperty.SetProxy(AProxy: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY, PChar(AProxy));
end;

procedure TSession.TProxyProperty.SetPort(APort: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYPORT, APort);
end;

procedure TSession.TProxyProperty.SetProxyType(AType: TProxyType);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYTYPE, Longint(AType));
end;

procedure TSession.TProxyProperty.SetProxyServiceName(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_SERVICE_NAME, PChar(AName));
end;

procedure TSession.TProxyProperty.SetNoProxyHosts(AHosts: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_NOPROXY, PChar(AHosts));
end;

procedure TSession.TProxyProperty.SetHttpProxyTunnel(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HTTPPROXYTUNNEL, Longint(AEnable));
end;

procedure TSession.TProxyProperty.SetProxyUserPassword(AUserpwd: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYUSERPWD, PChar(AUserpwd));
end;

procedure TSession.TProxyProperty.SetProxyUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYUSERNAME, PChar(AName));
end;

procedure TSession.TProxyProperty.SetProxyPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXYPASSWORD, PChar(APassword));
end;

procedure TSession.TProxyProperty.SetProxyTLSUsername(AName: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_TLSAUTH_USERNAME, PChar(AName));
end;

procedure TSession.TProxyProperty.SetProxyTLSPassword(APassword: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_TLSAUTH_PASSWORD, PChar(APassword));
end;

procedure TSession.TProxyProperty.SetProxyTLSAuth(
  AMethod: TSecurityProperty.TTLSAuthMethod);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_TLSAUTH_TYPE,
    PChar(GetEnumName(TypeInfo(TSecurityProperty.TTLSAuthMethod),
    ord(AMethod))));
end;

procedure TSession.TProxyProperty.SetProxyHTTPAuth(
  AMethod: TSecurityProperty.TAuthMethods);
var
  bitmask : Longint;
begin
  bitmask := 0;
  if AUTH_BASIC in AMethod then
    bitmask := bitmask or CURLAUTH_BASIC;
  if AUTH_DIGEST in AMethod then
    bitmask := bitmask or CURLAUTH_DIGEST;
  if AUTH_NEGOTIATE in AMethod then
    bitmask := bitmask or CURLAUTH_NEGOTIATE;
  if AUTH_GSSAPI in AMethod then
    bitmask := bitmask or CURLAUTH_GSSAPI;
  if AUTH_NTLM in AMethod then
    bitmask := bitmask or CURLAUTH_NTLM;
  if AUTH_DIGEST_IE in AMethod then
    bitmask := bitmask or CURLAUTH_DIGEST_IE;
  if AUTH_NTLM_WB in AMethod then
    bitmask := bitmask or CURLAUTH_NTLM_WB;
  if AUTH_BEARER in AMethod then
    bitmask := bitmask or CURLAUTH_BEARER;
  if AUTH_ANY in AMethod then
    bitmask := CURLAUTH_BASIC or CURLAUTH_DIGEST or CURLAUTH_NEGOTIATE or
      CURLAUTH_NTLM or CURLAUTH_DIGEST_IE or CURLAUTH_NTLM_WB or
      CURLAUTH_BEARER;
  if AUTH_ANYSAFE in AMethod then
    bitmask := bitmask or CURLAUTH_DIGEST or CURLAUTH_NEGOTIATE or
      CURLAUTH_NTLM or CURLAUTH_NTLM_WB or CURLAUTH_BEARER;

  curl_easy_setopt(FHandle, CURLOPT_PROXYAUTH, bitmask);
end;

procedure TSession.TProxyProperty.SetHAProxyHeader(ASend: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_HAPROXYPROTOCOL, Longint(ASend));
end;

procedure TSession.TProxyProperty.SetProxySSLCertificate(ACertificate: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_SSLCERT, PChar(ACertificate));
end;

procedure TSession.TProxyProperty.SetProxySSLCertificateType(AType: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_SSLCERTTYPE, PChar(AType));
end;

procedure TSession.TProxyProperty.SetProxySSLKey(AKey: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_PROXY_SSLKEY, PChar(AKey));
end;

constructor TSession.TProxyProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
  FSOCKS5 := TSOCKS5Property.Create(AHandle);
end;

destructor TSession.TProxyProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TTCPProperty }

procedure TSession.TTCPProperty.SetTCPFastOpen(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_FASTOPEN, Longint(AEnable));
end;

procedure TSession.TTCPProperty.SetTCPNoDelay(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_NODELAY, Longint(AEnable));
end;

procedure TSession.TTCPProperty.SetTCPKeepalive(ASendProbe: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_KEEPALIVE, Longint(ASendProbe));
end;

procedure TSession.TTCPProperty.SetTCPKeepIdle(ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_KEEPIDLE,
    Longint(ATime.Seconds));
end;

procedure TSession.TTCPProperty.SetTCPKeepInterval(ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_TCP_KEEPINTVL,
    Longint(ATime.Seconds));
end;

constructor TSession.TTCPProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TTCPProperty.Destroy;
begin
  inherited Destroy;
end;

{ TSession.TOptionsProperty }

procedure TSession.TOptionsProperty.SetNoSignal(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_NOSIGNAL, Longint(AEnable));
end;

procedure TSession.TOptionsProperty.SetAddressScope(AScope: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_ADDRESS_SCOPE, AScope);
end;

procedure TSession.TOptionsProperty.SetInterface(AInterface: string);
begin
  curl_easy_setopt(FHandle, CURLOPT_INTERFACE, PChar(AInterface));
end;

procedure TSession.TOptionsProperty.SetUnixSocketPath(APath: string);
begin
  if APath = '' then
  begin
    curl_easy_setopt(FHandle, CURLOPT_UNIX_SOCKET_PATH, 0);
  end else
  begin
    curl_easy_setopt(FHandle, CURLOPT_UNIX_SOCKET_PATH, PChar(APath));
  end;
end;

procedure TSession.TOptionsProperty.SetAbstractUnixSocketPath(APath: string);
begin
  if APath = '' then
  begin
    curl_easy_setopt(FHandle, CURLOPT_ABSTRACT_UNIX_SOCKET, 0);
  end else
  begin
    curl_easy_setopt(FHandle, CURLOPT_ABSTRACT_UNIX_SOCKET, PChar(APath));
  end;
end;

procedure TSession.TOptionsProperty.SetBufferSize(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_BUFFERSIZE, Longint(ASize.Bytes));
end;

procedure TSession.TOptionsProperty.SetFailOnError(AFailOnError: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FAILONERROR, Longint(AFailOnError));
end;

procedure TSession.TOptionsProperty.SetPathAsIs(ALeaveIt: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_PATH_AS_IS, Longint(ALeaveIt));
end;

procedure TSession.TOptionsProperty.SetConvertCRLF(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_CRLF, Longint(AEnable));
end;

procedure TSession.TOptionsProperty.SetUploadFileSize(ASize: curl_off_t);
begin
  curl_easy_setopt(FHandle, CURLOPT_INFILESIZE_LARGE, ASize);
end;

procedure TSession.TOptionsProperty.SetUploadBufferSize(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_UPLOAD_BUFFERSIZE, ASize.Bytes);
end;

procedure TSession.TOptionsProperty.SetTimeout(ATime: TTimeInterval);
begin
  if ATime.Seconds >= 1 then
    curl_easy_setopt(FHandle, CURLOPT_TIMEOUT, Longint(ATime.Seconds))
  else
    curl_easy_setopt(FHandle, CURLOPT_TIMEOUT_MS,
      Longint(ATime.Milliseconds));
end;

procedure TSession.TOptionsProperty.SetLowSpeedLimit(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOW_SPEED_LIMIT, Longint(ASize.Bytes));
end;

procedure TSession.TOptionsProperty.SetLowSpeedTime(ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOW_SPEED_TIME,
    Longint(ATime.Seconds));
end;

procedure TSession.TOptionsProperty.SetMaxUploadSpeed(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAX_SEND_SPEED_LARGE, ASize.Bytes);
end;

procedure TSession.TOptionsProperty.SetMaxDownloadSpeed(ASize: TDataSize);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAX_RECV_SPEED_LARGE, ASize.Bytes);
end;

procedure TSession.TOptionsProperty.SetMaxConnections(AConn: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAXCONNECTS, AConn);
end;

procedure TSession.TOptionsProperty.SetForceReuseConnection(AEnable: Boolean);
begin
  curl_easy_setopt(FHandle, CURLOPT_FRESH_CONNECT, Longint(not AEnable));
end;

procedure TSession.TOptionsProperty.SetCloseConnectionAfterUse(AEnable: Boolean
  );
begin
  curl_easy_setopt(FHandle, CURLOPT_FORBID_REUSE, Longint(AEnable));
end;

procedure TSession.TOptionsProperty.SetMaxReuseConnectionTime(
  ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_MAXAGE_CONN, Longint(ATime.Seconds));
end;

procedure TSession.TOptionsProperty.SetConnectionTimeout(ATime: TTimeInterval);
begin
  if ATime.Seconds >= 1 then
    curl_easy_setopt(FHandle, CURLOPT_CONNECTTIMEOUT,
      Longint(ATime.Seconds))
  else
    curl_easy_setopt(FHandle, CURLOPT_CONNECTTIMEOUT_MS,
      Longint(ATime.Milliseconds));
end;

procedure TSession.TOptionsProperty.SetIPResolve(AResolve: TIPResolve);
begin
  curl_easy_setopt(FHandle, CURLOPT_IPRESOLVE, Longint(AResolve));
end;

procedure TSession.TOptionsProperty.SetHappyEyeballsTimeout(ATime: TTimeInterval
  );
begin
  curl_easy_setopt(FHandle, CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS,
    Longint(ATime.Milliseconds));
end;

procedure TSession.TOptionsProperty.SetPrivateData(AData: Pointer);
begin
  curl_easy_setopt(FHandle, CURLOPT_PRIVATE, AData);
end;

procedure TSession.TOptionsProperty.SetUpkeepInterval(ATime: TTimeInterval);
begin
  curl_easy_setopt(FHandle, CURLOPT_UPKEEP_INTERVAL_MS,
    Longint(ATime.Milliseconds));
end;

constructor TSession.TOptionsProperty.Create(AHandle: CURL);
begin
  FHandle := AHandle;
end;

destructor TSession.TOptionsProperty.Destroy;
begin
  inherited Destroy;
end;

{ TResponse }

function TResponse.IsOpened: Boolean;
begin
  Result := (session.Opened and hasInfo);
end;

function TResponse.CheckErrors: Boolean;
begin
  Result := not Opened;
end;

function TResponse.GetErrorMessage: string;
begin
  if HasErrors then
  begin
    Result := errorBuffer;
  end;
end;

function TResponse.GetEffectiveUrl: string;
var
  url : PChar;
begin
  if Opened then
  begin
    New(url);
    url := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_EFFECTIVE_URL, @url);
    Result := url;
  end;
end;

function TResponse.GetRedirectUrl: string;
var
  url : PChar;
begin
  if Opened then
  begin
    New(url);
    url := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_REDIRECT_URL, @url);
    Result := url;
  end;
end;

function TResponse.GetContentType: string;
var
  content_type : PChar;
begin
  if Opened then
  begin
    New(content_type);
    content_type := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_CONTENT_TYPE, @content_type);
    Result := content_type;
  end;
end;

function TResponse.GetPrimaryIP: string;
var
  ip : PChar;
begin
  if Opened then
  begin
    New(ip);
    ip := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_PRIMARY_IP, @ip);
    Result := ip;
  end;
end;

function TResponse.GetLocalIP: string;
var
  ip : PChar;
begin
  if Opened then
  begin
    New(ip);
    ip := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_LOCAL_IP, @ip);
    Result := ip;
  end;
end;

function TResponse.GetResponseCode: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_RESPONSE_CODE, @Result);
  end;
end;

function TResponse.GetContent: string;
var
  ContentLength : Longint = 0;
begin
  if Opened then
  begin
    ContentLength := Length(string(PChar(session.FBuffer.Memory)));
    Result := '';
    UniqueString(Result);
    SetLength(Result, ContentLength);
    Move(PChar(session.FBuffer.Memory^), PChar(Result)[0], ContentLength);
  end;
end;

function TResponse.GetVerifySSLResult: boolean;
var
  verify : Longint = 1;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SSL_VERIFYRESULT, @verify);
    Result := (verify = 0);
  end;
end;

function TResponse.GetVerifySSLProxyResult: boolean;
var
  verify : Longint = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_PROXY_SSL_VERIFYRESULT, @verify);
    Result := Boolean(verify);
  end;
end;

function TResponse.GetConnectResponseCode: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_HTTP_CONNECTCODE, @Result);
  end;
end;

function TResponse.GetHttpVersion: TSession.THTTPProperty.THTTPVersion;
var
  ver : Longint = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_HTTP_VERSION, @ver);
    Result := TSession.THTTPProperty.THTTPVersion(ver);
  end;
end;

function TResponse.GetRedirectCount: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_REDIRECT_COUNT, @Result);
  end;
end;

function TResponse.GetUploaded: TDataSize;
var
  bytes : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SIZE_UPLOAD_T, @bytes);
    Result := TDataSize.Create;
    Result.Bytes := bytes;
  end;
end;

function TResponse.GetDownloaded: TDataSize;
var
  bytes : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SIZE_DOWNLOAD_T, @bytes);
    Result := TDataSize.Create;
    Result.Bytes := bytes;
  end;
end;

function TResponse.GetDownloadSpeed: TDataSize;
var
  bytes : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SPEED_DOWNLOAD_T, @bytes);
    Result := TDataSize.Create;
    Result.Bytes := bytes;
  end;
end;

function TResponse.GetUploadSpeed: TDataSize;
var
  bytes : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_SPEED_UPLOAD_T, @bytes);
    Result := TDataSize.Create;
    Result.Bytes := bytes;
  end;
end;

function TResponse.GetHeaderSize: TDataSize;
var
  bytes : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_HEADER_SIZE, @bytes);
    Result := TDataSize.Create;
    Result.Bytes := bytes;
  end;
end;

function TResponse.GetRequestSize: TDataSize;
var
  bytes : Longint = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_REQUEST_SIZE, @bytes);
    Result := TDataSize.Create;
    Result.Bytes := bytes;
  end;
end;

function TResponse.GetContentLengthDownload: LongWord;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T,
      @Result);
  end;
end;

function TResponse.GetContentLengthUpload: LongWord;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_CONTENT_LENGTH_UPLOAD_T,
      @Result);
  end;
end;

function TResponse.GetNumConnects: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_NUM_CONNECTS, @Result);
  end;
end;

function TResponse.GetPrimaryPort: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_PRIMARY_PORT, @Result);
  end;
end;

function TResponse.GetLocalPort: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_LOCAL_PORT, @Result);
  end;
end;

function TResponse.GetFileTime: Int64;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_FILETIME_T, @Result);
  end;
end;

function TResponse.GetTotalTime: TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  if Opened then
  begin
    CurlResult := curl_easy_getinfo(session.FHandle, CURLINFO_TOTAL_TIME_T,
      @time);
    Result := TTimeInterval.Create;
    Result.Milliseconds := time;

    if CurlResult <> CURLE_OK then
    begin
      curl_easy_getinfo(session.FHandle, CURLINFO_TOTAL_TIME, @dtime);
      Result := TTimeInterval.Create;
      Result.Milliseconds := ceil(dtime);
    end;
  end;
end;

function TResponse.GetNameLookup: TTimeInterval;
var
  time : Longword = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  if Opened then
  begin
    CurlResult := curl_easy_getinfo(session.FHandle, CURLINFO_NAMELOOKUP_TIME_T,
      @time);
    Result := TTimeInterval.Create;
    Result.Milliseconds := time;

    if CurlResult <> CURLE_OK then
    begin
      curl_easy_getinfo(session.FHandle, CURLINFO_NAMELOOKUP_TIME, @dtime);
      Result := TTimeInterval.Create;
      Result.Milliseconds := ceil(dtime);
    end;
  end;
end;

function TResponse.GetConnectTime: TTimeInterval;
var
  time : LongWord = 0;
  dtime : Double = 0;
  CurlResult : CURLcode;
begin
  if Opened then
  begin
    CurlResult := curl_easy_getinfo(session.FHandle, CURLINFO_CONNECT_TIME_T,
      @time);
    Result := TTimeInterval.Create;
    Result.Milliseconds := time;

    if CurlResult <> CURLE_OK then
    begin
      curl_easy_getinfo(session.FHandle, CURLINFO_CONNECT_TIME, @dtime);
      Result := TTimeInterval.Create;
      Result.Milliseconds := ceil(dtime);
    end;
  end;
end;

function TResponse.GetAppConnectTime: TTimeInterval;
var
  time : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_APPCONNECT_TIME_T, @time);
    Result := TTimeInterval.Create;
    Result.Milliseconds := time;
  end;
end;

function TResponse.GetPretransferTime: TTimeInterval;
var
  time : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_PRETRANSFER_TIME_T, @time);
    Result := TTimeInterval.Create;
    Result.Milliseconds := time;
  end;
end;

function TResponse.GetStartTransferTime: TTimeInterval;
var
  time : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_STARTTRANSFER_TIME_T, @time);
    Result := TTimeInterval.Create;
    Result.Milliseconds := time;
  end;
end;

function TResponse.GetRedirectTime: TTimeInterval;
var
  time : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_REDIRECT_TIME_T, @time);
    Result := TTimeInterval.Create;
    Result.Milliseconds := time;
  end;
end;

function TResponse.GetRetryAfterDelay: TTimeInterval;
var
  delay : LongWord = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_RETRY_AFTER, @delay);
    Result := TTimeInterval.Create;
    Result.Seconds := delay;
  end;
end;

function TResponse.GetOsErrno: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_OS_ERRNO, @Result);
  end;
end;

function TResponse.GetLastSocket: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_LASTSOCKET, @Result);
  end;
end;

function TResponse.GetActiveSocket: curl_socket_t;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_ACTIVESOCKET, @Result);
  end;
end;

function TResponse.GetFTPEntryPath: string;
var
  path : PChar;
begin
  if Opened then
  begin
    New(path);
    path := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_FTP_ENTRY_PATH, @path);
    Result := path;
  end;
end;

function TResponse.GetConditionUnmet: Boolean;
var
  unmet : Longint = 0;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_CONDITION_UNMET, @unmet);
    Result := Boolean(unmet);
  end;
end;

function TResponse.GetRTSPSessionID: string;
var
  id : PChar;
begin
  if Opened then
  begin
    New(id);
    id := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_SESSION_ID, @id);
    Result := id;
  end;
end;

function TResponse.GetRTSPClientCSeq: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_CLIENT_CSEQ, @Result);
  end;
end;

function TResponse.GetRTSPServerCSeq: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_SERVER_CSEQ, @Result);
  end;
end;

function TResponse.GetRTSPReceivedCSeq: Longint;
begin
  if Opened then
  begin
    Result := 0;
    curl_easy_getinfo(session.FHandle, CURLINFO_RTSP_CSEQ_RECV, @Result);
  end;
end;

function TResponse.GetScheme: string;
var
  sc : PChar;
begin
  if Opened then
  begin
    New(sc);
    sc := '';
    curl_easy_getinfo(session.FHandle, CURLINFO_SCHEME, @sc);
    Result := sc;
  end;
end;

function TResponse.GetPrivateData: Pointer;
var
  Data : PPChar = nil;
begin
  if Opened then
  begin
    curl_easy_getinfo(session.FHandle, CURLINFO_PRIVATE, Data);
    Result := Data^;
  end;
end;

constructor TResponse.Create(s: TSession);
begin
  if s.Opened then
  begin
    Self.session := s;
    curl_easy_setopt(session.FHandle, CURLOPT_ERRORBUFFER, PChar(errorBuffer));
    hasInfo := (curl_easy_perform(session.FHandle) = CURLE_OK);
  end;
end;

{ TSession }

class function TSession.WriteFunctionCallback (ptr: PChar; size: LongWord;
  nmemb: LongWord; data: Pointer): LongWord; cdecl;
begin
  if Assigned(TSession(data).FDownloadFunction) then
  begin
    Result := TSession(data).FDownloadFunction(ptr, size);
  end else
  begin
    Result := TSession(data).Write(ptr, size, nmemb);
  end;
end;

class function TSession.ReadFunctionCallback (buf: PChar; size: LongWord;
  nitems: LongWord; data: Pointer): LongWord; cdecl;
begin
  if Assigned(TSession(data).FUploadFunction) then
  begin
    Result := TSession(data).FUploadFunction(buf, size);
  end else
  begin
    Result := TSession(data).Read(buf, size, nitems);
  end;
end;

function TSession.Write(ptr: PChar; size: LongWord; nmemb: LongWord): LongWord;
begin
  Result := FBuffer.Write(ptr^, size * nmemb);
end;

function TSession.Read(buf: PChar; size: LongWord; nitems: LongWord): LongWord;
var
  dataSize : Int64 = 0;
begin
  Result := 0;
  if FBuffer.Size > 0 then
  begin
    dataSize := size * nitems;
    if (dataSize > FBuffer.Size) then
      dataSize := FBuffer.Size;
    Move(PChar(FBuffer.Memory)[FUploadOffset], buf[0], dataSize);
    FUploadOffset := FUploadOffset + dataSize;
    Result := dataSize;
  end;
end;

constructor TSession.Create;
begin
  inherited Create;

  FHandle := curl_easy_init;
  FBuffer := TMemoryStream.Create;
  FUploadOffset := 0;

  FOptions := TOptionsProperty.Create(FHandle);
  FProtocol := TProtocolProperty.Create(FHandle);
  FTCP := TTCPProperty.Create(FHandle);
  FProxy := TProxyProperty.Create(FHandle);
  FDNS := TDNSProperty.Create(FHandle);
  FSecurity := TSecurityProperty.Create(FHandle);
  FHTTP := THTTPProperty.Create(FHandle);
  FIMAP := TIMAPProperty.Create(FHandle);
  FFTP := TFTPProperty.Create(FHandle);
  FTFTP := TTFTPProperty.Create(FHandle);
  FSMTP := TSMTPProperty.Create(FHandle);
  FRTSP := TRTSPProperty.Create(FHandle);

  if Opened then
  begin
    curl_easy_setopt(FHandle, CURLOPT_WRITEDATA, Pointer(Self));
    curl_easy_setopt(FHandle, CURLOPT_WRITEFUNCTION,
      @TSession.WriteFunctionCallback);
    curl_easy_setopt(FHandle, CURLOPT_READDATA, Pointer(Self));
    curl_easy_setopt(FHandle, CURLOPT_READFUNCTION,
      @TSession.ReadFunctionCallback);
    FProtocol.FollowRedirect := True;
  end;
end;

destructor TSession.Destroy;
begin
  curl_easy_cleanup(FHandle);
  FreeAndNil(FBuffer);

  inherited Destroy;
end;

function TSession.IsOpened: Boolean;
begin
  Result := FHandle <> nil;
end;

procedure TSession.SetUrl(url: string);
begin
  if Opened then
  begin
    FBuffer.Clear;
    curl_easy_setopt(FHandle, CURLOPT_URL, PChar(url));
  end;
end;

procedure TSession.SetLocalPort(APort: Word);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOCALPORT, Longint(APort));
end;

procedure TSession.SetLocalPortRange(ARange: Longint);
begin
  curl_easy_setopt(FHandle, CURLOPT_LOCALPORTRANGE, ARange);
end;

procedure TSession.SetConnectTo(AList: TLinkedList);
begin
  curl_easy_setopt(FHandle, CURLOPT_CONNECT_TO, AList.FList);
end;

function TSession.ExtractProtocol(AUrl: string): TProtocolProperty.TProtocol;
var
  proto : TProtocolProperty.TProtocol;
  search, extract, protocolName : string;
begin
  search := LowerCase(Copy(AUrl, 1, Pos('://', AUrl) - 1));

  for proto := Low(TProtocolProperty.TProtocol) to
    High(TProtocolProperty.TProtocol) do
  begin
    protocolName := GetEnumName(TypeInfo(TProtocolProperty.TProtocol),
      Ord(proto));
    extract := LowerCase(Copy(protocolName, Length('PROTOCOL_') + 1,
      Length(protocolName) - Length('PROTOCOL_') + 1));
    if extract = search then
      Result := proto;
  end;
end;

initialization
  curl_global_init(CURL_GLOBAL_ALL);

finalization
  curl_global_cleanup;

end.

