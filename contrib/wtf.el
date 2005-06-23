;; wtf.el --- Look up conversational and computing acronyms

;; Copyright (C) 2005  Michael Olson

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Thanks to Trent Buck for `emacs-wiki-wtf.el', which inspired the
;; creation of `wtf.el'.

;; The terms were downloaded from
;; http://cvsweb.netbsd.org/bsdweb.cgi/src/share/misc/.  No copyright
;; notice was included, but since the program that makes use of them
;; (http://cvsweb.netbsd.org/bsdweb.cgi/src/games/wtf/wtf) is in the
;; public domain, it seems reasonable to infer that the acronym files
;; are also in the public domain.

(defvar wtf-alist
  '(;; $NetBSD: acronyms,v 1.146 2005/03/10 05:45:25 soda Exp $
    ("AFAIC" . "as far as I'm concerned")
    ("AFAICR" . "as far as I can recall")
    ("AFAICT" . "as far as I can tell")
    ("AFAIK" . "as far as I know")
    ("AFAIR" . "as far as I recall")
    ("AFAIU" . "as far as I understand")
    ("AFD" . "away from desktop")
    ("AFK" . "away from keyboard")
    ("AFU" . "all fucked up")
    ("AFW" . "away from window")
    ("AIU" . "as I understand")
    ("AIUI" . "as I understand it")
    ("AKA" . "also known as")
    ("ASAIC" . "as soon as I can")
    ("ASAP" . "as soon as possible")
    ("ATM" . "at the moment")
    ("AWOL" . "absent without official leave")
    ("AYBABTU" . "all your base are belong to us")
    ("AYT" . "are you there")
    ("B/C" . "because")
    ("B/S" . "bullshit")
    ("B/W" . "between")
    ("BBIAB" . "be back in a bit")
    ("BBL" . "[I'll] be back later")
    ("BBS" . "be back soon")
    ("BBT" . "be back tomorrow")
    ("BFD" . "big fucking deal")
    ("BIAB" . "back in a bit")
    ("BIAF" . "back in a few")
    ("BIALW" . "back in a little while")
    ("BIAS" . "back in a second")
    ("BIAW" . "back in a while")
    ("BOATILAS" . "bend over and take it like a slut")
    ("BOFH" . "bastard operator from hell")
    ("BOGAHICA" . "bend over, grab ankles, here it comes again")
    ("BOHICA" . "bend over here it comes again")
    ("BRB" . "[I'll] be right back")
    ("BS" . "bullshit")
    ("BTDT" . "been there, done that")
    ("BTTH" . "boot to the head")
    ("BTW" . "by the way")
    ("CMIIW" . "correct me if I'm wrong")
    ("CNP" . "continued [in my] next post")
    ("COB" . "close of business [day]")
    ("COTS" . "commercial off-the-shelf")
    ("CYA" . "see you around")
    ("D/L" . "download")
    ("DIY" . "do it yourself")
    ("DKDC" . "don't know, don't care")
    ("DSTM" . "don't shoot the messenger")
    ("DTRT" . "do the right thing")
    ("DTWT" . "do the wrong thing")
    ("DWIM" . "do what I mean")
    ("EG" . "evil grin")
    ("EMSG" . "email message")
    ("EOB" . "end of business [day]")
    ("EOD" . "end of discussion")
    ("EOL" . "end of life")
    ("ETLA" . "extended three letter acronym")
    ("EWAG" . "experienced wild-ass guess")
    ("FAQ" . "frequently asked question")
    ("FCFS" . "first come first served")
    ("FIGJAM" . "fuck I'm good, just ask me")
    ("FIIK" . "fuck[ed] if I know")
    ("FIIR" . "fuck[ed] if I remember")
    ("FM" . "fucking magic")
    ("FOAD" . "fall over and die")
    ("FSDO" . "for some definition of")
    ("FSVO" . "for some value of")
    ("FTFM" . "fuck the fuckin' manual!")
    ("FUBAR" . "fucked up beyond all recognition")
    ("FUD" . "fear, uncertainty and doubt")
    ("FWIW" . "for what it's worth")
    ("FYI" . "for your information")
    ("G" . "grin")
    ("G/C" . "garbage collect")
    ("GAC" . "get a clue")
    ("GAL" . "get a life")
    ("GIGO" . "garbage in, garbage out")
    ("GMTA" . "great minds think alike")
    ("GTFO" . "get the fuck out")
    ("GTG" . "got to go")
    ("HAND" . "have a nice day")
    ("HHIS" . "hanging head in shame")
    ("HICA" . "here it comes again")
    ("HTH" . "hope this helps")
    ("IAC" . "in any case")
    ("IANAL" . "I am not a lawyer")
    ("IC" . "I see")
    ("ICBW" . "I could be wrong")
    ("ICCL" . "I couldn't care less")
    ("IHAFC" . "I haven't a fucking clue")
    ("IHBW" . "I have been wrong")
    ("IHNFC" . "I have no fucking clue")
    ("IIANM" . "if I am not mistaken")
    ("IIRC" . "if I recall correctly")
    ("IIUC" . "if I understand correctly")
    ("IMAO" . "in my arrogant opinion")
    ("IMCO" . "in my considered opinion")
    ("IMHO" . "in my humble opinion")
    ("IMNSHO" . "in my not so humble opinion")
    ("IMO" . "in my opinion")
    ("IOW" . "in other words")
    ("IRL" . "in real life")
    ("ISAGN" . "I see a great need")
    ("ISTM" . "it seems to me")
    ("ISTR" . "I seem to recall")
    ("ITYM" . "I think you mean")
    ("IWBNI" . "it would be nice if")
    ("IYSS" . "if you say so")
    ("J/K" . "just kidding")
    ("JHD" . "just hit ``delete''")
    ("JIC" . "just in case")
    ("JK" . "just kidding")
    ("JMO" . "just my opinion")
    ("JSYK" . "just so you know")
    ("JTLYK" . "just to let you know")
    ("KISS" . "keep it simple, stupid")
    ("KITA" . "kick in the ass")
    ("KNF" . "kernel normal form")
    ("L8R" . "later")
    ("LART" . "luser attitude readjustment tool (ie, hammer)")
    ("LBNL" . "last but not least")
    ("LJBF" . "let's just be friends")
    ("LMAO" . "laughing my ass off")
    ("LMSO" . "laughing my socks off")
    ("LOL" . "laughing out loud")
    ("LTNS" . "long time no see")
    ("MIA" . "missing in action")
    ("MOTAS" . "member of the appropriate sex")
    ("MOTOS" . "member of the opposite sex")
    ("MOTSS" . "member of the same sex")
    ("MTF" . "more to follow")
    ("MYOB" . "mind your own business")
    ("N/M" . "never mind")
    ("NBD" . "no big deal")
    ("NFC" . "no fucking clue")
    ("NFI" . "no fucking idea")
    ("NFW" . "no fucking way")
    ("NIH" . "not invented here")
    ("NMF" . "not my fault")
    ("NMP" . "not my problem")
    ("NOYB" . "none of your business")
    ("NOYFB" . "none of your fucking business")
    ("NP" . "no problem")
    ("NRFPT" . "not ready for prime time")
    ("NRN" . "no reply necessary")
    ("OIC" . "oh, I see")
    ("OMG" . "oh, my god")
    ("OT" . "off topic")
    ("OTL" . "out to lunch")
    ("OTOH" . "on the other hand")
    ("OTT" . "over the top")
    ("OTTOMH" . "off the top of my head")
    ("PEBKAC" . "problem exists between keyboard and chair")
    ("PFO" . "please fuck off")
    ("PFY" . "pimply faced youth")
    ("PITA" . "pain in the ass")
    ("PKSP" . "pound keys and spew profanity")
    ("PNG" . "persona non grata")
    ("PNP" . "plug and pray")
    ("POC" . "point of contact")
    ("POLA" . "principle of least astonishment")
    ("POLS" . "principle of least surprise")
    ("POS" . "piece of shit")
    ("PPL" . "pretty please")
    ("PTV" . "parental tunnel vision")
    ("QED" . "quod erat demonstrandum")
    ("RFC" . "request for comments")
    ("RIP" . "rest in peace")
    ("RL" . "real life")
    ("RLC" . "rod length check")
    ("ROFL" . "rolling on floor laughing")
    ("ROFLMAO" . "rolling on floor laughing my ass off")
    ("ROTFL" . "rolling on the floor laughing")
    ("RP" . "responsible person")
    ("RSN" . "real soon now")
    ("RTFB" . "read the fine/fucking book")
    ("RTFC" . "read the fine/fucking code")
    ("RTFD" . "read the fine/fucking documentation")
    ("RTFM" . "read the fine/fucking manual")
    ("RTFMP" . "read the fine/fucking man page")
    ("RTFS" . "read the fine/fucking source")
    ("SCNR" . "sorry, could not resist")
    ("SEP" . "someone else's problem")
    ("SFA" . "sweet fuck all")
    ("SHID" . "slaps head in disgust")
    ("SIMCA" . "sitting in my chair amused")
    ("SMLSFB" . "so many losers, so few bullets")
    ("SMOP" . "simple matter of programming")
    ("SNAFU" . "situation normal, all fucked up")
    ("SNERT" . "snot-nosed egotistical rude teenager")
    ("SNMP" . "sorry, not my problem")
    ("SNR" . "signal to noise ratio")
    ("SO" . "significant other")
    ("SOB" . "son of [a] bitch")
    ("SOL" . "shit out [of] luck")
    ("SOP" . "standard operating procedure")
    ("SSIA" . "subject says it all")
    ("STFA" . "search the fucking archives")
    ("STFU" . "shut the fuck up")
    ("STFW" . "search the fucking web")
    ("SUS" . "stupid user syndrome")
    ("SWAG" . "silly, wild-assed guess")
    ("SWAHBI" . "silly, wild-assed hare-brained idea")
    ("SWMBO" . "she who must be obeyed")
    ("TANSTAAFL" . "there ain't no such thing as a free lunch")
    ("TBC" . "to be continued")
    ("TBD" . "to be {decided,determined,done}")
    ("TBOMK" . "the best of my knowledge")
    ("THNX" . "thanks")
    ("THX" . "thanks")
    ("TIA" . "thanks in advance")
    ("TINC" . "there is no cabal")
    ("TLA" . "three letter acronym")
    ("TLB" . "translation lookaside buffer")
    ("TMA" . "too many abbreviations")
    ("TMI" . "too much information")
    ("TNF" . "The NetBSD Foundation")
    ("TOEFL" . "test of english as a foreign language")
    ("TPTB" . "the powers that be")
    ("TRT" . "the right thing")
    ("TTBOMK" . "to the best of my knowledge")
    ("TTFN" . "ta ta for now")
    ("TTYL" . "talk to you later")
    ("TWIAVBP" . "the world is a very big place")
    ("TY" . "thank you")
    ("TYVM" . "thank you very much")
    ("U/L" . "upload")
    ("UTSL" . "use the source, Luke")
    ("VEG" . "very evil grin")
    ("W/" . "with")
    ("W/O" . "without")
    ("WAG" . "wild-ass guess")
    ("WB" . "welcome back")
    ("WFM" . "works for me")
    ("WIBNI" . "wouldn't it be nice if")
    ("WIP" . "work in progress")
    ("WOFTAM" . "waste of fucking time and money")
    ("WOMBAT" . "waste of money, brain, and time")
    ("WRT" . "with respect to")
    ("WTF" . "{what,where,who,why} the fuck")
    ("WTH" . "{what,where,who,why} the hell")
    ("WYSIWYG" . "what you see is what you get")
    ("YALIMO" . "you are lame, in my opinion")
    ("YHBT" . "you have been trolled")
    ("YHL" . "you have lost")
    ("YKWIM" . "you know what I mean")
    ("YMA" . "yo momma's ass")
    ("YMMV" . "your mileage may vary")
    ("YW" . "you're welcome")
    ;; $NetBSD: acronyms.comp,v 1.61 2005/03/28 15:07:16 jschauma Exp $
    ("3WHS" . "three-way handshake")
    ("ABI" . "application binary interface")
    ("ACL" . "access control list")
    ("ACPI" . "advanced configuration and power interface")
    ("ADC" . "analog [to] digital converter")
    ("ADPCM" . "adaptive differential pulse code modulation")
    ("ADSL" . "asymmetric digital subscriber line")
    ("AGP" . "accelerated graphics port")
    ("AM" . "amplitude modulation")
    ("AMI" . "alternate mark inversion")
    ("ANSI" . "american national standards institute")
    ("AP" . "access point")
    ("API" . "application programming interface")
    ("APIC" . "advanced programmable interrupt controller")
    ("ARP" . "address resolution protocol")
    ("ARQ" . "automatic repeat request")
    ("AS" . "autonomous system")
    ("ASN" . "autonomous system number")
    ("ASCII" . "american standard code for information interchange")
    ("AT" . "advanced technology")
    ("ATA" . "advanced technology attachment")
    ("ATAPI" . "advanced technology attachment packet interface")
    ("ATM" . "asynchronous transfer mode")
    ("ATX" . "advanced technology extended")
    ("BEDO" . "burst extended data output")
    ("BER" . "basic encoding rules")
    ("BER" . "bit error rate")
    ("BGP" . "border gateway protocol")
    ("BIOS" . "basic input/output system")
    ("BLOB" . "binary large object")
    ("BPS" . "bits per second")
    ("BSD" . "berkeley software distribution")
    ("CAD" . "computer-aided design")
    ("CAV" . "constant angular velocity (as opposed to CLV)")
    ("CCD" . "charge coupled device")
    ("CD" . "compact disc")
    ("CDDA" . "compact disc digital audio")
    ("CDRAM" . "cache dynamic random access memory")
    ("CER" . "canonical encoding rules")
    ("CGA" . "color graphics array")
    ("CGI" . "common gateway interface")
    ("CHS" . "cylinder/head/sector")
    ("CIDR" . "classless inter-domain routing")
    ("CIS" . "contact image sensor")
    ("CLI" . "command line interface")
    ("CLUT" . "color look-up table")
    ("CLV" . "constant linear velocity (as opposed to CAV)")
    ("CMYK" . "cyan magenta yellow black")
    ("COFF" . "common object file format")
    ("COW" . "copy-on-write")
    ("CPU" . "central processing unit")
    ("CRLF" . "carriage return line feed")
    ("CRT" . "cathode ray tube")
    ("CSMA" . "carrier sense multiple access")
    ("CSMA/CA" . "carrier sense multiple access with collision avoidance")
    ("CSMA/CD" . "carrier sense multiple access with collision detection")
    ("CSS" . "cascading style sheets")
    ("CTS" . "clear to send")
    ("CVS" . "concurrent versions system")
    ("DAC" . "digital [to] analog converter")
    ("DCE" . "data control equipment")
    ("DCE" . "distributed computing environment")
    ("DCT" . "discrete cosine transform")
    ("DDC" . "display data channel")
    ("DDR" . "double data rate")
    ("DDWG" . "digital display working group")
    ("DER" . "distinguished encoding rules")
    ("DFT" . "discrete fourier transform")
    ("DHCP" . "dynamic host configuration protocol")
    ("DIFS" . "distributed inter-frame space")
    ("DLE" . "data link escape")
    ("DMA" . "direct memory access")
    ("DNS" . "domain name system")
    ("DOS" . "denial of service")
    ("DPCM" . "differential pulse code modulation")
    ("DPI" . "dots per inch")
    ("DRAM" . "dynamic random access memory")
    ("DSL" . "digital subscriber line")
    ("DSSS" . "direct sequence spread spectrum")
    ("DTD" . "document type definition")
    ("DTE" . "data terminal equipment")
    ("DTE" . "dumb terminal emulator")
    ("DVD" . "digital versatile disc")
    ("DVI" . "digital visual interface")
    ("ECP" . "enhanced capability port")
    ("EDID" . "extended display identification data")
    ("EDO" . "extended data out")
    ("EEPROM" . "electrically erasable programmable read only memory")
    ("EFM" . "eight to fourteen modulation")
    ("EGA" . "enhanced graphics array")
    ("EGP" . "exterior gateway protocol")
    ("EISA" . "extended industry standard architecture")
    ("ELF" . "executable and linking format")
    ("EPP" . "enhanced parallel port")
    ("EPRML" . "extended partial response, maximum likelihood")
    ("EPROM" . "erasable programmable read only memory")
    ("ESDRAM" . "enhanced synchronous dynamic random access memory")
    ("E-XER" . "extended XML encoding rules")
    ("FAT" . "file allocation table")
    ("FBRAM" . "frame buffer random access memory")
    ("FCS" . "frame check sequence")
    ("FDDI" . "fiber distributed data interface")
    ("FFS" . "fast file system")
    ("FHSS" . "frequency hop spread spectrum")
    ("FIR" . "fast infrared")
    ("FLOPS" . "floating [point] operations per second")
    ("FM" . "frequency modulation")
    ("FPM" . "fast page mode")
    ("FQDN" . "fully qualified domain name")
    ("FTP" . "file transfer protocol")
    ("GC" . "garbage collector")
    ("GCR" . "group-coded recording")
    ("GIF" . "graphics interchange format")
    ("GNU" . "gnu's not unix")
    ("GPL" . "gnu/general public license")
    ("GPU" . "graphics processing unit")
    ("GRE" . "generic routing encapsulation")
    ("GUI" . "graphics user interface")
    ("HDCP" . "high-bandwidth digital content protection")
    ("HTML" . "hyper-text markup language")
    ("HTTP" . "hyper-text transfer protocol")
    ("I2O" . "intelligent input/output")
    ("IANA" . "internet assigned number authority")
    ("IC" . "integrated circuit")
    ("ICB" . "internet citizen's band")
    ("ICMP" . "internet control message protocol")
    ("IDE" . "integrated drive electronics")
    ("IDRP" . "inter-domain routing protocol")
    ("IEC" . "international electrotechnical commission")
    ("IEEE" . "institute [of] electrical [and] electronics engineers")
    ("IESG" . "internet engineering steering group")
    ("IETF" . "internet engineering task force")
    ("IGP" . "interior gateway protocol")
    ("IKE" . "internet key exchange")
    ("IMAP" . "internet mail access protocol")
    ("INCITS" . "international committee on information technology standards")
    ("IPC" . "interprocess communication")
    ("IO" . "input/output")
    ("IOCTL" . "input/output control")
    ("IP" . "internet protocol")
    ("IPNG" . "internet protocol, next generation")
    ("IPSEC" . "internet protocol security")
    ("IRC" . "internet relay chat")
    ("IRQ" . "interrupt request")
    ("IRTF" . "internet research task force")
    ("ISA" . "industry standard architecture")
    ("ISDN" . "integrated services digital network")
    ("ISI" . "inter-symbol interference")
    ("ISM" . "industrial, scientific and medical")
    ("ISN" . "initial serial number")
    ("ISO" . "international standards organization")
    ("ISOC" . "internet society")
    ("ISP" . "internet service provider")
    ("JPEG" . "joint photographic experts group")
    ("KVA" . "kernel virtual address")
    ("LAN" . "local area network")
    ("LBA" . "logical block addressing")
    ("LCD" . "liquid crystal display")
    ("LCP" . "link control protocol")
    ("LDAP" . "lightweight directory access protocol")
    ("LED" . "light emitting diode")
    ("LIR" . "local internet registry")
    ("LLC" . "logical link control")
    ("LRC" . "longitudinal redundancy check")
    ("LSB" . "least significant bit [or: byte]")
    ("LUN" . "logical unit number")
    ("LZW" . "Lempel Ziv Welch")
    ("MAC" . "medium access control")
    ("MBR" . "master boot record")
    ("MDRAM" . "multibank dynamic random access memory")
    ("MFM" . "modified frequency modulation")
    ("MIDI" . "musical instrument digital interface")
    ("MIME" . "multipurpose internet mail extensions")
    ("MIPS" . "million instructions per second")
    ("MMU" . "memory management unit")
    ("MPEG" . "moving picture experts group")
    ("MSB" . "most significant bit [or: byte]")
    ("MSF" . "minutes seconds frames")
    ("MSS" . "maximum segment size")
    ("MTA" . "mail transfer agent")
    ("MTU" . "maximum transmission unit")
    ("MUA" . "mail user agent")
    ("MWE" . "module width encoding")
    ("NAT" . "network address translation")
    ("NAV" . "network allocation vector")
    ("NCP" . "network control protocol")
    ("NFS" . "network file system")
    ("NIC" . "network interface card")
    ("NIS" . "network information service")
    ("NRZ" . "non-return to zero")
    ("NUMA" . "non uniform memory access")
    ("OCL" . "object constraint language")
    ("OCR" . "optical character recognition")
    ("OEM" . "original equipment manufacturer")
    ("OFDM" . "orthogonal frequency division multiplexing")
    ("OSF" . "open software foundation")
    ("OSI" . "open systems interconnection")
    ("OTP" . "one time password")
    ("PAM" . "pluggable authentication modules")
    ("PAM" . "pulse amplitude modulation")
    ("PAT" . "port address translation")
    ("PAX" . "portable archive exchange")
    ("PC" . "personal computer")
    ("PCI" . "peripheral component interconnect")
    ("PCM" . "pulse code modulation")
    ("PCMCIA" . "personal computer memory card international association")
    ("PDU" . "protocol data unit")
    ("PDP" . "page descriptor page")
    ("PER" . "packed encoding rules")
    ("PERL" . "practical extraction [and] report language")
    ("PGP" . "pretty good privacy")
    ("PIC" . "programmable interrupt controller")
    ("PID" . "process id")
    ("PIN" . "personal identification number")
    ("PIO" . "programmed input/output")
    ("PLL" . "phase locked loop")
    ("PMT" . "photo-multiplier tube")
    ("PNG" . "portable network graphics")
    ("POP" . "post office protocol")
    ("POSIX" . "portable operating system interface [for] unix")
    ("POST" . "power on self test")
    ("POTS" . "plain old telephone system")
    ("PPP" . "point-to-point protocol")
    ("PPPOA" . "point-to-point protocol over ATM")
    ("PPPOE" . "point-to-point protocol over ethernet")
    ("PRML" . "partial response, maximum likelihood")
    ("PROM" . "programmable read only memory")
    ("PTE" . "page table entry")
    ("PTLA" . "pseudo top level aggregator")
    ("PTP" . "page table page")
    ("PSTN" . "public switched telephone network")
    ("PWM" . "pulse width modulation")
    ("QOS" . "quality of service")
    ("RAID" . "redundant array of inexpensive disks")
    ("RAM" . "random access memory")
    ("RCS" . "revision control system")
    ("RFC" . "request for comments")
    ("RGB" . "red green blue")
    ("RIFF" . "Resource Interchange File Format")
    ("RIP" . "routing information protocol")
    ("RIR" . "regional internet registry")
    ("RISC" . "reduced instruction set computing")
    ("RLE" . "run length encoding")
    ("RLL" . "run length limited")
    ("ROM" . "read only memory")
    ("RPM" . "revolutions per minute")
    ("RTF" . "rich text format")
    ("RTS" . "request to send")
    ("RTT" . "round time trip")
    ("S/PDIF" . "sony/phillips digital interface")
    ("SACD" . "super audio compact disc")
    ("SAM" . "serial access memory")
    ("SASI" . "shugart associates system interface (predecessor to SCSI)")
    ("SATA" . "serial advanced technology attachment")
    ("SCSI" . "small computer system interface")
    ("SDRAM" . "synchronous dynamic random access memory")
    ("SGRAM" . "synchronous graphics random access memory")
    ("SIFS" . "short inter-frame space")
    ("SIP" . "session initiation protocol")
    ("SIR" . "slow infrared")
    ("SLDRAM" . "synchronous-link dynamic random access memory")
    ("SMART" . "self-monitoring analysis and reporting technology")
    ("SMP" . "symmetric multiprocessing")
    ("SMTP" . "simple mail transfer protocol")
    ("SNMP" . "simple network management protocol")
    ("SPD" . "serial presence detect")
    ("SRAM" . "static random access memory")
    ("SSFDC" . "solid state floppy disc card")
    ("SSH" . "secure shell")
    ("SSL" . "secure sockets layer")
    ("STP" . "shielded twisted pair")
    ("SVGA" . "super video graphics array")
    ("TCL" . "tool command language")
    ("TCP" . "transmission control protocol")
    ("TDD" . "test driven development")
    ("TFT" . "thin film transistor")
    ("TIFF" . "tagged image file format")
    ("TLA" . "top level aggregator")
    ("TLB" . "transition lookaside buffer")
    ("TLD" . "top level domain")
    ("TLS" . "transport layer security")
    ("TMDS" . "transition minimized differential signaling")
    ("TR" . "token ring")
    ("TTL" . "time to live")
    ("TTY" . "teletype")
    ("TZ" . "time zone")
    ("UART" . "universal asynchronous receiver/transmitter")
    ("UC" . "uncacheable")
    ("UDO" . "ultra density optical (storage)")
    ("UDP" . "user datagram protocol")
    ("UFS" . "unix file system")
    ("UML" . "unified modeling language")
    ("UPS" . "uninterruptible power supply")
    ("URI" . "uniform resource identifier")
    ("URL" . "uniform resource locator")
    ("USART" . "universal synchronous/asynchronous receiver/transmitter")
    ("USB" . "universal serial bus")
    ("USWC" . "uncacheable speculative write combining")
    ("UTP" . "unshielded twisted pair")
    ("UUCP" . "unix-to-unix copy protocol")
    ("VAX" . "virtual address extension")
    ("VCM" . "virtual channel memory")
    ("VESA" . "video electronics standards association")
    ("VGA" . "video graphics array")
    ("VLAN" . "virtual local area network")
    ("VLSM" . "variable length subnetting mask")
    ("VM" . "virtual memory")
    ("VPN" . "virtual private network")
    ("VRAM" . "video random access memory")
    ("WAN" . "wide area network")
    ("WAP" . "wireless application protocol")
    ("WLAN" . "wireless local area network")
    ("WRAM" . "window random access memory")
    ("WWW" . "world wide web")
    ("XER" . "XML encoding rules")
    ("XGA" . "extended graphics array")
    ("XML" . "extensible markup language")
    ("XSL" . "extensible stylesheet language")
    ("XT" . "extended technology")
    ("ZFOD" . "zero-filled on demand")
    ;; Additional terms go here
    ("NIFOC" . "naked in front of computer")
    ("PITB" . "pain in the butt")
    ("ROTFLMAO" . "rolling on the floor laughing my ass off")
    ("WTB" . "where's the beef")
    )
  "Mapping of acronyms to expansions.")

(defun wtf-is (term)
  "Provide the definition for TERM.
When called interactively, print the message \"TERM is DEF\".
Otherwise, return DEF.

DEF refers to the definition associated with TERM in `wtf-alist'."
  (interactive
   (list (completing-read "Term: "
                          (mapcar #'(lambda (term)
                                      (downcase (car term)))
                                  wtf-alist))))
  (when (stringp term)
    (let ((def (upcase-initials (cdr (assoc (upcase term)
                                            wtf-alist)))))
      (if (interactive-p)
          (message (concat term " is " def))
        def))))

(provide 'wtf)

;;; wtf.el ends here
