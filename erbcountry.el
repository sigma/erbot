;; 2003-02-13 T13:36:31-0500 (Thursday)    D. Goel
;; countries list is copied from http://www.iana.org/cctld/cctld-whois.htm
;;; Real Code:

(defvar erbcountry-list)
(defvar erbcountry-string)


;; This is an incomplete, old  list.   We don't want to spend time to
;; create it again, so we will simply dump the contents into
;; erbcountry-string and use a routine to alistify that. 
(unless (boundp 'erbcountry-list)
  (setq erbcountry-list
	'(
	  (".ac"    "Ascension Island")
	  (".ad"    "Andorra")
	  (".ae"    "United Arab Emirates")
	  (".af"    "Afghanistan")
	  (".ag"    "Antigua and Barbuda")
	  (".ai"    "Anguilla")
	  (".al"    "Albania")
	  (".am"    "Armenia")
	  (".an"    "Netherlands Antilles")
	  (".ao"    "Angola")
	  (".aq"    "Antarctica")
	  (".ar"    "Argentina")
	  (".as"    "American Samoa")
	  (".at"    "Austria")
	  (".au"    "Australia")
	  (".aw"    "Aruba")
	  (".az"    "Azerbaijan")
	  (".ba"    "Bosnia and Herzegovina")
	  (".bb"    "Barbados")
	  (".bd"    "Bangladesh")
	  (".be"    "Belgium")
	  (".bf"    "Burkina Faso")
	  (".bg"    "Bulgaria")
	  (".bh"    "Bahrain")
	  (".bi"    "Burundi")
	  (".bj"    "Benin")
	  (".bm"    "Bermuda")
	  (".bn"    "Brunei Darussalam")
	  (".bo"    "Bolivia")
	  (".br"    "Brazil")
	  (".bs"    "Bahamas")
	  (".bt"    "Bhutan")
	  (".bv"    "Bouvet Island")
	  (".bw"    "Botswana")
	  (".by"    "Belarus")
	  (".bz"    "Belize")
	  (".ca"    "Canada")
	  (".cc"    "Cocos (Keeling) Islands")
	  (".cd"    "Congo, Democratic Republic of the")
	  (".cf"    "Central African Republic")
	  (".cg"    "Congo, Republic of")
	  (".ch"    "Switzerland")
	  (".ci"    "Cote d'Ivoire")
	  (".ck"    "Cook Islands")
	  (".cl"    "Chile")
	  (".cm"    "Cameroon")
	  (".cn"    "China")
	  (".co"    "Colombia")
	  (".cr"    "Costa Rica")
	  (".cu"    "Cuba")
	  (".cv"    "Cap Verde")
	  (".cx"    "Christmas Island")
	  (".cy"    "Cyprus")
	  (".cz"    "Czech Republic")
	  (".de"    "Germany")
	  (".dj"    "Djibouti")
	  (".dk"    "Denmark")
	  (".dm"    "Dominica")
	  (".do"    "Dominican Republic")
	  (".dz"    "Algeria")
	  (".ec"    "Ecuador")
	  (".ee"    "Estonia")
	  (".eg"    "Egypt")
	  (".eh"    "Western Sahara")
	  (".er"    "Eritrea")
	  (".es"    "Spain")
	  (".et"    "Ethiopia")
	  (".fi"    "Finland")
	  (".fj"    "Fiji")
	  (".fk"    "Falkland Islands (Malvina)")
	  (".fm"    "Micronesia, Federal State of")
	  (".fo"    "Faroe Islands")
	  (".fr"    "France")
	  (".ga"    "Gabon")
	  (".gd"    "Grenada")
	  (".ge"    "Georgia")
	  (".gf"    "French Guiana")
	  (".gg"    "Guernsey")
	  (".gh"    "Ghana")
	  (".gi"    "Gibraltar")
	  (".gl"    "Greenland")
	  (".gm"    "Gambia")
	  (".gn"    "Guinea")
	  (".gp"    "Guadeloupe")
	  (".gq"    "Equatorial Guinea")
	  (".gr"    "Greece")
	  (".gs"    "South Georgia and the South Sandwich Islands")
	  (".gt"    "Guatemala")
	  (".gu"    "Guam")
	  (".gw"    "Guinea-Bissau")
	  (".gy"    "Guyana")
	  (".hk"    "Hong Kong")
	  (".hm"    "Heard and McDonald Islands")
	  (".hn"    "Honduras")
	  (".hr"    "Croatia/Hrvatska")
	  (".ht"    "Haiti")
	  (".hu"    "Hungary")
	  (".id"    "Indonesia")
	  (".ie"    "Ireland")
	  (".il"    "Israel")
	  (".im"    "Isle of Man")
	  (".in"    "India")
	  (".io"    "British Indian Ocean Territory")
	  (".iq"    "Iraq")
	  (".ir"    "Iran (Islamic Republic of)")
	  (".is"    "Iceland")
	  (".it"    "Italy")
	  (".je"    "Jersey")
	  (".jm"    "Jamaica")
	  (".jo"    "Jordan")
	  (".jp"    "Japan")
	  (".ke"    "Kenya")
	  (".kg"    "Kyrgyzstan")
	  (".kh"    "Cambodia")
	  (".ki"    "Kiribati")
	  (".km"    "Comoros")
	  (".kn"    "Saint Kitts and Nevis")
	  (".kp"    "Korea, Democratic People's Republic")
	  (".kr"    "Korea, Republic of")
	  (".kw"    "Kuwait")
	  (".ky"    "Cayman Islands")
	  (".kz"    "Kazakhstan")
	  (".la"    "Lao People's Democratic Republic")
	  (".lb"    "Lebanon")
	  (".lc"    "Saint Lucia")
	  (".li"    "Liechtenstein")
	  (".lk"    "Sri Lanka")
	  (".lr"    "Liberia")
	  (".ls"    "Lesotho")
	  (".lt"    "Lithuania")
	  (".lu"    "Luxembourg")
	  (".lv"    "Latvia")
	  (".ly"    "Libyan Arab Jamahiriya")
	  (".ma"    "Morocco")
	  (".mc"    "Monaco")
	  (".md"    "Moldova, Republic of")
	  (".mg"    "Madagascar")
	  (".mh"    "Marshall Islands")
	  (".mk"    "Macedonia, Former Yugoslav Republic")
	  (".ml"    "Mali")
	  (".mm"    "Myanmar")
	  (".mn"    "Mongolia")
	  (".mo"    "Macau")
	  (".mp"    "Northern Mariana Islands")
	  (".mq"    "Martinique")
	  (".mr"    "Mauritania")
	  (".ms"    "Montserrat")
	  (".mt"    "Malta")
	  (".mu"    "Mauritius")
	  (".mv"    "Maldives")
	  (".mw"    "Malawi")
	  (".mx"    "Mexico")
	  (".my"    "Malaysia")
	  (".mz"    "Mozambique")
	  (".na"    "Namibia")
	  (".nc"    "New Caledonia")
	  (".ne"    "Niger")
	  (".nf"    "Norfolk Island")
	  (".ng"    "Nigeria")
	  (".ni"    "Nicaragua")
	  (".nl"    "Netherlands")
	  (".no"    "Norway")
	  (".np"    "Nepal")
	  (".nr"    "Nauru")
	  (".nu"    "Niue")
	  (".nz"    "New Zealand")
	  (".om"    "Oman")
	  (".pa"    "Panama")
	  (".pe"    "Peru")
	  (".pf"    "French Polynesia")
	  (".pg"    "Papua New Guinea")
	  (".ph"    "Philippines")
	  (".pk"    "Pakistan")
	  (".pl"    "Poland")
	  (".pm"    "St. Pierre and Miquelon")
	  (".pn"    "Pitcairn Island")
	  (".pr"    "Puerto Rico")
	  (".ps"    "Palestinian Territories")
	  (".pt"    "Portugal")
	  (".pw"    "Palau")
	  (".py"    "Paraguay")
	  (".qa"    "Qatar")
	  (".re"    "Reunion Island")
	  (".ro"    "Romania")
	  (".ru"    "Russian Federation")
	  (".rw"    "Rwanda")
	  (".sa"    "Saudi Arabia")
	  (".sb"    "Solomon Islands")
	  (".sc"    "Seychelles")
	  (".sd"    "Sudan")
	  (".se"    "Sweden")
	  (".sg"    "Singapore")
	  (".sh"    "St. Helena")
	  (".si"    "Slovenia")
	  (".sj"    "Svalbard and Jan Mayen Islands")
	  (".sk"    "Slovak Republic")
	  (".sl"    "Sierra Leone")
	  (".sm"    "San Marino")
	  (".sn"    "Senegal")
	  (".so"    "Somalia")
	  (".sr"    "Suriname")
	  (".st"    "Sao Tome and Principe")
	  (".sv"    "El Salvador")
	  (".sy"    "Syrian Arab Republic")
	  (".sz"    "Swaziland")
	  (".tc"    "Turks and Caicos Islands")
	  (".td"    "Chad")
	  (".tf"    "French Southern Territories")
	  (".tg"    "Togo")
	  (".th"    "Thaila")
	  (".us"    "USA")


	  )))



(unless (boundp 'erbcountry-string)
  (setq erbcountry-string
	".ac    Ascension Island
.ad    Andorra
.ae    United Arab Emirates
.af    Afghanistan
.ag    Antigua and Barbuda
.ai    Anguilla
.al    Albania
.am    Armenia
.an    Netherlands Antilles
.ao    Angola
.aq    Antarctica
.ar    Argentina
.as    American Samoa
.at    Austria
.au    Australia
.aw    Aruba
.ax    Aland Islands
.az    Azerbaijan
.ba    Bosnia and Herzegovina
.bb    Barbados
.bd    Bangladesh
.be    Belgium
.bf    Burkina Faso
.bg    Bulgaria
.bh    Bahrain
.bi    Burundi
.bj    Benin
.bm    Bermuda
.bn    Brunei Darussalam
.bo    Bolivia
.br    Brazil
.bs    Bahamas
.bt    Bhutan
.bv    Bouvet Island
.bw    Botswana
.by    Belarus
.bz    Belize
.ca    Canada
.cc    Cocos (Keeling) Islands
.cd    Congo, The Democratic Republic of the
.cf    Central African Republic
.cg    Congo, Republic of
.ch    Switzerland
.ci    Cote d'Ivoire
.ck    Cook Islands
.cl    Chile
.cm    Cameroon
.cn    China
.co    Colombia
.cr    Costa Rica
.cs    Serbia and Montenegro
.cu    Cuba
.cv    Cape Verde
.cx    Christmas Island
.cy    Cyprus
.cz    Czech Republic
.de    Germany
.dj    Djibouti
.dk    Denmark
.dm    Dominica
.do    Dominican Republic
.dz    Algeria
.ec    Ecuador
.ee    Estonia
.eg    Egypt
.eh    Western Sahara
.er    Eritrea
.es    Spain
.et    Ethiopia
.fi    Finland
.fj    Fiji
.fk    Falkland Islands (Malvinas)
.fm    Micronesia, Federal State of
.fo    Faroe Islands
.fr    France
.ga    Gabon
.gb    United Kingdom
.gd    Grenada
.ge    Georgia
.gf    French Guiana
.gg    Guernsey
.gh    Ghana
.gi    Gibraltar
.gl    Greenland
.gm    Gambia
.gn    Guinea
.gp    Guadeloupe
.gq    Equatorial Guinea
.gr    Greece
.gs    South Georgia and the South Sandwich Islands
.gt    Guatemala
.gu    Guam
.gw    Guinea-Bissau
.gy    Guyana
.hk    Hong Kong
.hm    Heard and McDonald Islands
.hn    Honduras
.hr    Croatia/Hrvatska
.ht    Haiti
.hu    Hungary
.id    Indonesia
.ie    Ireland
.il    Israel
.im    Isle of Man
.in    India
.io    British Indian Ocean Territory
.iq    Iraq
.ir    Iran, Islamic Republic of
.is    Iceland
.it    Italy
.je    Jersey
.jm    Jamaica
.jo    Jordan
.jp    Japan
.ke    Kenya
.kg    Kyrgyzstan
.kh    Cambodia
.ki    Kiribati
.km    Comoros
.kn    Saint Kitts and Nevis
.kp    Korea, Democratic People's Republic
.kr    Korea, Republic of
.kw    Kuwait
.ky    Cayman Islands
.kz    Kazakhstan
.la    Lao People's Democratic Republic
.lb    Lebanon
.lc    Saint Lucia
.li    Liechtenstein
.lk    Sri Lanka
.lr    Liberia
.ls    Lesotho
.lt    Lithuania
.lu    Luxembourg
.lv    Latvia
.ly    Libyan Arab Jamahiriya
.ma    Morocco
.mc    Monaco
.md    Moldova, Republic of
.mg    Madagascar
.mh    Marshall Islands
.mk    Macedonia, The Former Yugoslav Republic of
.ml    Mali
.mm    Myanmar
.mn    Mongolia
.mo    Macau
.mp    Northern Mariana Islands
.mq    Martinique
.mr    Mauritania
.ms    Montserrat
.mt    Malta
.mu    Mauritius
.mv    Maldives
.mw    Malawi
.mx    Mexico
.my    Malaysia
.mz    Mozambique
.na    Namibia
.nc    New Caledonia
.ne    Niger
.nf    Norfolk Island
.ng    Nigeria
.ni    Nicaragua
.nl    Netherlands
.no    Norway
.np    Nepal
.nr    Nauru
.nu    Niue
.nz    New Zealand
.om    Oman
.pa    Panama
.pe    Peru
.pf    French Polynesia
.pg    Papua New Guinea
.ph    Philippines
.pk    Pakistan
.pl    Poland
.pm    Saint Pierre and Miquelon
.pn    Pitcairn Island
.pr    Puerto Rico
.ps    Palestinian Territory, Occupied
.pt    Portugal
.pw    Palau
.py    Paraguay
.qa    Qatar
.re    Reunion Island
.ro    Romania
.ru    Russian Federation
.rw    Rwanda
.sa    Saudi Arabia
.sb    Solomon Islands
.sc    Seychelles
.sd    Sudan
.se    Sweden
.sg    Singapore
.sh    Saint Helena
.si    Slovenia
.sj    Svalbard and Jan Mayen Islands
.sk    Slovak Republic
.sl    Sierra Leone
.sm    San Marino
.sn    Senegal
.so    Somalia
.sr    Suriname
.st    Sao Tome and Principe
.sv    El Salvador
.sy    Syrian Arab Republic
.sz    Swaziland
.tc    Turks and Caicos Islands
.td    Chad
.tf    French Southern Territories
.tg    Togo
.th    Thailand
.tj    Tajikistan
.tk    Tokelau
.tl    Timor-Leste
.tm  Turkmenistan
.tn    Tunisia
.to    Tonga
.tp    East Timor
.tr    Turkey
.tt    Trinidad and Tobago
.tv    Tuvalu
.tw    Taiwan
.tz    Tanzania
.ua    Ukraine
.ug    Uganda
.uk    United Kingdom
.um    United States Minor Outlying Islands
.us    United States
.uy    Uruguay
.uz    Uzbekistan
.va    Holy See (Vatican City State)
.vc    Saint Vincent and the Grenadines
.ve    Venezuela
.vg    Virgin Islands, British
.vi    Virgin Islands, U.S.
.vn    Vietnam
.vu    Vanuatu
.wf    Wallis and Futuna Islands
.ws    Western Samoa
.ye    Yemen
.yt    Mayotte
.yu    Yugoslavia
.za    South Africa
.zm    Zambia
.zw    Zimbabwe"))



(defun erbcountry-create-list ()
  "Creates erbcountry-list from erbcountry-string. "
  (let ((strlist (split-string erbcountry-string "[\n]+")) splits dom name)
    (dolist (str strlist)
      (setq splits (split-string str "[ \t\n]+"))
      (setq dom (first splits))
      (setq name (mapconcat 'identity (cdr splits) " "))
      (add-to-list 'erbcountry-list  (list dom name)))))

(erbcountry-create-list)



(defun erbcountry-search (name)
  (with-temp-buffer
    (insert erbcountry-string)
    (goto-char (point-min))
    (if (search-forward name nil t)
	(buffer-substring-no-properties (line-beginning-position)
					(line-end-position))
      (error "No match. "))))

  
(defun erbcountry-lookup (ct)
  ;;(unless (stringp ct) (setq ct (format "%s" ct)))
  (second (assoc ct erbcountry-list)))

(defalias 'erbcountry 'erbcountry-lookup)
      
(provide 'erbcountry)




;;; erbcountry.el ends here
