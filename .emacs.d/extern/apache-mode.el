;;; apache-mode.el --- major mode for editing Apache configuration files

;; Keywords:	languages, faces
;; Author:	Jonathan Marten  <jonathan.marten@uk.sun.com> or
;; Last edit:	23-Oct-99        <jjm@keelhaul.demon.co.uk>

;; This file is an add-on for XEmacs or GNU Emacs (not tested with the latter).
;;
;; It is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; It is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with your copy of Emacs; see the file COPYING.  If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; There isn't really much to say.  The list of keywords was derived from
;; the documentation for Apache 1.3; there may be some errors or omissions.
;;
;; There are currently no local keybindings defined, but the hooks are
;; there in the event that anyone gets around to adding any.
;;
;; To enable automatic selection of this mode when appropriate files are
;; visited, add the following to your favourite site or personal Emacs
;; configuration file:
;;
;;   (autoload 'apache-mode "apache-mode" "autoloaded" t)
;;   (add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
;;   (add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
;;   (add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
;;   (add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))
;;

;;; Change Log:
;;
;; Version 1.0, October 1999	First public release


;;; Code:

;; Requires
(require 'regexp-opt)


;; Variables
(defvar apache-mode-map nil
  "Keymap used in Apache config mode buffers")

(defvar apache-mode-syntax-table nil
  "Apache config mode syntax table")

(defvar apache-mode-hook nil
  "*List of hook functions run by `apache-mode' (see `run-hooks')")


;; Font lock
(defconst apache-font-lock-keywords
  (purecopy
   (list
    (list "^\\s-*#.*$" 0 'font-lock-comment-face t)

    (list (concat                                       ; sections
	   "^\\s-*</?\\("
           (regexp-opt '("Directory" "Files" "IfModule"
                         "Limit" "Location" "VirtualHost"))
           "\\)\\(>\\|\\s-\\)")
	  1 'font-lock-function-name-face)

    (list (concat                                       ; keywords
	   "^\\s-*\\("
           (regexp-opt '("AccessConfig" "AccessFileName" "Action"
                         "AddAlt" "AddAltByEncoding" "AddAltByType"
                         "AddDescription" "AddEncoding" "AddHandler"
                         "AddIcon" "AddIconByEncoding" "AddIconByType"
                         "AddLanguage" "AddModule" "AddType" "AgentLog"
                         "Alias" "allow" "AllowOverride" "Anonymous"
                         "Anonymous_Authoritative" "Anonymous_LogEmail"
                         "Anonymous_MustGiveEmail" "Anonymous_NoUserID"
                         "Anonymous_VerifyEmail" "AuthAuthoritative"
                         "AuthDBAuthoritative" "AuthDBGroupFile"
                         "AuthDBMAuthoritative" "AuthDBMGroupFile"
                         "AuthDBMGroupFile" "AuthDBMUserFile"
                         "AuthDBUserFile" "AuthDigestFile" "AuthGroupFile"
                         "AuthName" "AuthType" "AuthUserFile" "BindAddress"
                         "BrowserMatch" "BrowserMatchNoCase" "CacheDefaultExpire"
                         "CacheDirLength" "CacheDirLevels" "CacheForceCompletion"
                         "CacheGcInterval" "CacheLastModifiedFactor"
                         "CacheMaxExpire" "CacheNegotiatedDocs" "CacheRoot"
                         "CacheSize" "CheckSpelling" "ClearModuleList"
                         "CookieExpires" "CookieLog" "CookieTracking" "CustomLog"
                         "DefaultIcon" "DefaultType" "deny" "DirectoryIndex"
                         "DocumentRoot" "ErrorDocument" "ErrorLog" "Example"
                         "ExpiresActive" "ExpiresByType" "ExpiresDefault"
                         "FancyIndexing" "ForceType" "Group" "Header" "HeaderName"
                         "HostNameLookups" "IdentityCheck" "ImapBase" "ImapDefault"
                         "ImapMenu" "IndexIgnore" "IndexOptions" "KeepAlive"
                         "KeepAliveTimeout" "LanguagePriority" "Listen" "LoadFile"
                         "LoadModule" "LockFile" "LogFormat" "LogLevel" "MaxClients"
                         "MaxKeepAliveRequests" "MaxRequestsPerChild" "MaxSpareServers"
                         "MetaDir" "MetaSuffix" "MimeMagicFile" "MinSpareServers"
                         "NoCache" "order" "Options" "PassEnv" "PidFile" "Port"
                         "ProxyBlock" "ProxyDomain" "ProxyPass" "ProxyRemote"
                         "ProxyRequests" "RLimitCPU" "RLimitMEM" "RLimitNPROC"
                         "ReadmeName" "Redirect" "RedirectPermanent" "RedirectTemp"
                         "RefererIgnore" "RefererLog" "require" "ResourceConfig"
                         "RewriteBase" "RewriteCond" "RewriteEngine" "RewriteLog"
                         "RewriteLogLevel" "RewriteMap" "RewriteOptions"
                         "RewriteRule" "Satisfy" "ScoreBoardFile" "Script"
                         "ScriptAlias" "ScriptLog" "ScriptLogBuffer" "ScriptLogLength"
                         "SendBufferSize" "ServerAdmin" "ServerAlias" "ServerName"
                         "ServerPath" "ServerRoot" "ServerSignature" "ServerTokens"
                         "ServerType" "SetEnv" "SetHandler" "StartServers" "TimeOut"
                         "TransferLog" "TypesConfig" "UnsetEnv" "UseCanonicalName"
                         "User" "UserDir" "XBitHack"
                         "DefaultMode" "HTTPLogFile" "HTMLDir" "PrivateDir"
                         "TopSites" "TopURLs" "LastURLs" "HeadPrefix" "HeadSuffix"
                         "DocTitle" "DocTrailer" "HideURL" "HideSys"))
           "\\)\\s-")
	  1 'font-lock-keyword-face)

    (list (concat                                       ; values
	   "\\(?:^\\|\\W\\)\\("
           (regexp-opt '("allow" "deny" "on" "valid-user" "inetd" "standalone"
                         "off" "user" "group" "any" "env" "mutual-failure" "full"
                         "email" "force-response-1.0" "downgrade-1.0" "nokeepalive"
                         "permanent" "temporary" "seeother" "gone" "All" "Options"
                         "FileInfo" "AuthConfig" "Limit" "from" "None" "Basic"
                         "Digest" "FancyIndexing" "IconsAreLinks" "ScanHTMLTitles"
                         "SuppressLastModified" "SuppressSize" "SuppressDescription"
                         "Minimal" "OS" "Full" "set" "append" "add" "unset" "none"
                         "formatted" "semi-formatted" "unformatted" "error" "nocontent"
                         "map" "referer" "URL" "inherit" "double" "GET" "PUT" "POST"
                         "DELETE" "CONNECT" "OPTIONS" "Options" "Indexes" "Includes"
                         "ExecCGI" "FollowSymLinks" "MultiViews" "IncludesNOEXEC"
                         "SymLinksIfOwnerMatch"))
           "\\)\\W")
	  1 'font-lock-type-face)))
  "Expressions to highlight in Apache config buffers.")

(put 'apache-mode 'font-lock-defaults '(apache-font-lock-keywords nil t
                                                                  ((?_ . "w")
                                                                   (?- . "w"))))
;; Syntax table
(if apache-mode-syntax-table
    nil
  (setq apache-mode-syntax-table (copy-syntax-table nil))
  (modify-syntax-entry ?_   "_"     apache-mode-syntax-table)
  (modify-syntax-entry ?-   "_"     apache-mode-syntax-table)
  (modify-syntax-entry ?\(  "(\)"   apache-mode-syntax-table)
  (modify-syntax-entry ?\)  ")\("   apache-mode-syntax-table)
  (modify-syntax-entry ?\<  "(\>"   apache-mode-syntax-table)
  (modify-syntax-entry ?\>  ")\<"   apache-mode-syntax-table)
  (modify-syntax-entry ?\"   "\""   apache-mode-syntax-table))


;;;###autoload
(defun apache-mode ()
  "Major mode for editing Apache configuration files.

\\{apache-mode-map}

\\[apache-mode] runs the hook `apache-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map apache-mode-map)
  (set-syntax-table apache-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#\\W*")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (setq mode-name "Apache")
  (setq major-mode 'apache-mode)
  (run-hooks 'apache-mode-hook))


;; Provides
(provide 'apache-mode)

;;; apache-mode.el ends here
