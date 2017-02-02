;;;; copyright 2016 rsiddharth <s@ricketyspace.net>
;;;; under gnu general public license version 3 or higher.

(define-module (taocp one two eight fib)
  #:use-module (srfi srfi-1)
  #:export (fib-fast
            fib-0-1000))

(define phi
  (/ (+ 1 (sqrt 5.0)) 2.0))

(define phi-dash
  (/ (- 1 (sqrt 5.0)) 2.0))

(define (fib-fast n)
  "Returns Fibonacci of N."
  (/ (-  (expt phi n) (expt phi-dash n))
     (sqrt 5.0)))

(define (fib-fast-range start end)
  "Return Fibonacci series between START and END."
  (let ((count (1+ (- end start))))
    (reverse (fold (lambda (n prev)
                     (cons (fib-fast n) prev))
                   '()
                   (iota count start)))))

(define (fib-0-1000)
  "Return Fibonacci series between 0 and 1000."
  (fib-fast-range 0 1000))

;; scheme@(guile-user)> (fib-0-1000)
;; $5 = (0.0 1.0 1.0 2.0 3.0 5.0 8.0 13.0 21.0 34.0 54.99999999999999
;; 89.0 143.99999999999997 232.99999999999994 377.00000000000006 610.0
;; 986.9999999999998 1596.9999999999998 2584.0 4181.0
;; 6764.999999999999 10945.999999999998 17710.999999999996
;; 28656.999999999996 46367.99999999999 75025.0 121392.99999999999
;; 196418.0 317811.0 514228.99999999994 832039.9999999999
;; 1346268.9999999998 2178309.0 3524577.9999999995 5702886.999999999
;; 9227465.0 14930351.999999998 24157816.999999996 39088168.99999999
;; 63245985.99999999 102334154.99999999 165580140.99999997
;; 267914295.99999994 433494437.0 701408732.9999999 1134903169.9999998
;; 1836311903.0 2971215072.9999995 4807526975.999999 7778742048.999998
;; 12586269024.999998 20365011074.0 32951280098.999992
;; 53316291172.999985 86267571271.99998 139583862444.99997
;; 225851433716.99997 365435296161.99994 591286729878.9999
;; 956722026040.9999 1548008755919.9998 2504730781960.9995
;; 4052739537880.999 6557470319841.999 10610209857722.998
;; 17167680177564.996 27777890035287.996 44945570212852.99
;; 72723460248140.98 117669030460993.98 190392490709134.97
;; 308061521170129.0 498454011879263.9 806515533049392.9
;; 1304969544928656.8 2111485077978049.5 3416454622906706.0
;; 5527939700884755.0 8944394323791463.0 14472334024676218.0
;; 23416728348467676.0 37889062373143896.0 61305790721611580.0
;; 99194853094755490.0 160500643816367040.0 259695496911122530.0
;; 420196140727489600.0 679891637638612100.0 1100087778366101600.0
;; 1779979416004713700.0 2880067194370815500.0 4660046610375529000.0
;; 7540113804746344000.0 12200160415121873000.0 19740274219868217000.0
;; 3.194043463499009e19 5.16807088548583e19 8.362114348984841e19
;; 1.353018523447067e20 2.1892299583455514e20 3.542248481792618e20
;; 5.731478440138169e20 9.273726921930788e20 1.5005205362068957e21
;; 2.4278932283999744e21 3.9284137646068696e21 6.356306993006844e21
;; 1.0284720757613714e22 1.664102775062056e22 2.692574850823427e22
;; 4.356677625885483e22 7.04925247670891e22 1.1405930102594392e23
;; 1.84551825793033e23 2.9861112681897695e23 4.831629526120101e23
;; 7.81774079430987e23 1.2649370320429967e24 2.0467111114739838e24
;; 3.3116481435169805e24 5.358359254990965e24 8.670007398507945e24
;; 1.402836665349891e25 2.2698374052006855e25 3.6726740705505764e25
;; 5.942511475751263e25 9.615185546301839e25 1.55576970220531e26
;; 2.517288256835494e26 4.0730579590408044e26 6.590346215876297e26
;; 1.0663404174917102e27 1.72537503907934e27 2.79171545657105e27
;; 4.5170904956503905e27 7.308805952221441e27 1.1825896447871828e28
;; 1.9134702400093271e28 3.09605988479651e28 5.009530124805838e28
;; 8.105590009602348e28 1.3115120134408184e29 2.122071014401053e29
;; 3.433583027841871e29 5.555654042242924e29 8.989237070084795e29
;; 1.4544891112327722e30 2.353412818241252e30 3.8079019294740236e30
;; 6.161314747715274e30 9.969216677189299e30 1.6130531424904576e31
;; 2.6099748102093874e31 4.223027952699845e31 6.833002762909232e31
;; 1.1056030715609078e32 1.7889033478518307e32 2.8945064194127384e32
;; 4.68340976726457e32 7.577916186677309e32 1.2261325953941875e33
;; 1.9839242140619186e33 3.210056809456106e33 5.193981023518025e33
;; 8.404037832974131e33 1.3598018856492156e34 2.200205668946629e34
;; 3.5600075545958444e34 5.760213223542472e34 9.320220778138315e34
;; 1.5080434001680788e35 2.440065477981911e35 3.948108878149989e35
;; 6.388174356131899e35 1.033628323428189e36 1.672445759041379e36
;; 2.7060740824695677e36 4.3785198415109465e36 7.084593923980515e36
;; 1.1463113765491462e37 1.8547707689471975e37 3.0010821454963435e37
;; 4.8558529144435405e37 7.856935059939884e37 1.2712787974383427e38
;; 2.0569723034323313e38 3.3282511008706736e38 5.385223404303005e38
;; 8.71347450517368e38 1.4098697909476682e39 2.2812172414650363e39
;; 3.6910870324127045e39 5.972304273877741e39 9.663391306290444e39
;; 1.5635695580168187e40 2.529908688645863e40 4.0934782466626817e40
;; 6.623386935308545e40 1.0716865181971228e41 1.7340252117279775e41
;; 2.8057117299250997e41 4.5397369416530766e41 7.345448671578176e41
;; 1.1885185613231252e42 1.923063428480943e42 3.111581989804068e42
;; 5.0346454182850115e42 8.14622740808908e42 1.318087282637409e43
;; 2.132710023446317e43 3.4507973060837266e43 5.583507329530044e43
;; 9.034304635613769e43 1.4617811965143813e44 2.3652116600757582e44
;; 3.826992856590139e44 6.192204516665898e44 1.0019197373256037e45
;; 1.6211401889921933e45 2.6230599263177972e45 4.244200115309991e45
;; 6.867260041627788e45 1.1111460156937779e46 1.7978720198565564e46
;; 2.909018035550334e46 4.706890055406891e46 7.615908090957224e46
;; 1.2322798146364118e47 1.9938706237321343e47 3.2261504383685457e47
;; 5.22002106210068e47 8.446171500469226e47 1.3666192562569906e48
;; 2.211236406303913e48 3.5778556625609034e48 5.7890920688648176e48
;; 9.36694773142572e48 1.5156039800290536e49 2.452298753171626e49
;; 3.967902733200679e49 6.420201486372305e49 1.0388104219572982e50
;; 1.6808305705945286e50 2.719640992551828e50 4.400471563146356e50
;; 7.120112555698183e50 1.152058411884454e51 1.864069667454272e51
;; 3.016128079338726e51 4.880197746792998e51 7.896325826131725e51
;; 1.2776523572924722e52 2.0672849399056447e52 3.3449372971981175e52
;; 5.412222237103762e52 8.75715953430188e52 1.416938177140564e53
;; 2.292654130570752e53 3.709592307711316e53 6.002246438282068e53
;; 9.711838745993384e53 1.5714085184275453e54 2.5425923930268837e54
;; 4.114000911454429e54 6.656593304481313e54 1.077059421593574e55
;; 1.7427187520417054e55 2.8197781736352797e55 4.562496925676984e55
;; 7.382275099312264e55 1.194477202498925e56 1.9327047124301512e56
;; 3.127181914929076e56 5.059886627359227e56 8.187068542288305e56
;; 1.3246955169647531e57 2.1434023711935836e57 3.4680978881583364e57
;; 5.61150025935192e57 9.079598147510256e57 1.4691098406862176e58
;; 2.377069655437243e58 3.846179496123461e58 6.223249151560704e58
;; 1.0069428647684166e59 1.6292677799244869e59 2.6362106446929037e59
;; 4.26547842461739e59 6.901689069310294e59 1.1167167493927684e60
;; 1.8068856563237978e60 2.9236024057165662e60 4.730488062040364e60
;; 7.65409046775693e60 1.2384578529797293e61 2.0038668997554224e61
;; 3.242324752735152e61 5.246191652490574e61 8.488516405225724e61
;; 1.3734708057716302e62 2.222322446294202e62 3.595793252065832e62
;; 5.818115698360036e62 9.413908950425866e62 1.5232024648785902e63
;; 2.4645933599211766e63 3.987795824799767e63 6.452389184720945e63
;; 1.0440185009520712e64 1.6892574194241652e64 2.733275920376237e64
;; 4.422533339800402e64 7.155809260176639e64 1.1578342599977042e65
;; 1.8734151860153678e65 3.031249446013072e65 4.90466463202844e65
;; 7.935914078041513e65 1.2840578710069952e66 2.0776492788111464e66
;; 3.3617071498181414e66 5.4393564286292874e66 8.80106357844743e66
;; 1.424042000707672e67 2.3041483585524146e67 3.7281903592600866e67
;; 6.032338717812501e67 9.76052907707259e67 1.5792867794885088e68
;; 2.5553396871957675e68 4.134626466684276e68 6.689966153880044e68
;; 1.082459262056432e69 1.7514558774444365e69 2.8339151395008686e69
;; 4.585371016945305e69 7.419286156446173e69 1.200465717339148e70
;; 1.9423943329837654e70 3.142860050322913e70 5.085254383306678e70
;; 8.228114433629591e70 1.3313368816936269e71 2.1541483250565862e71
;; 3.485485206750213e71 5.639633531806798e71 9.12511873855701e71
;; 1.4764752270363813e72 2.388987100892082e72 3.8654623279284634e72
;; 6.2544494288205456e72 1.0119911756749008e73 1.6374361185569552e73
;; 2.649427294231857e73 4.286863412788811e73 6.936290707020668e73
;; 1.1223154119809478e74 1.8159444826830145e74 2.9382598946639633e74
;; 4.754204377346978e74 7.692464272010941e74 1.2446668649357916e75
;; 2.013913292136886e75 3.2585801570726773e75 5.272493449209563e75
;; 8.531073606282239e75 1.3803567055491806e76 2.2334640661774045e76
;; 3.6138207717265844e76 5.847284837903988e76 9.461105609630573e76
;; 1.5308390447534564e77 2.4769496057165133e77 4.0077886504699695e77
;; 6.484738256186484e77 1.0492526906656453e78 1.697726516284294e78
;; 2.746979206949939e78 4.444705723234233e78 7.191684930184172e78
;; 1.1636390653418405e79 1.8828075583602576e79 3.046446623702098e79
;; 4.929254182062356e79 7.975700805764454e79 1.290495498782681e80
;; 2.0880655793591263e80 3.378561078141807e80 5.466626657500933e80
;; 8.84518773564274e80 1.4311814393143676e81 2.315700212878642e81
;; 3.746881652193009e81 6.0625818650716504e81 9.809463517264659e81
;; 1.587204538233631e82 2.5681508899600968e82 4.1553554281937275e82
;; 6.723506318153825e82 1.0878861746347553e83 1.7602368064501377e83
;; 2.8481229810848926e83 4.6083597875350306e83 7.456482768619924e83
;; 1.2064842556154953e84 1.9521325324774874e84 3.158616788092983e84
;; 5.110749320570471e84 8.269366108663453e84 1.3380115429233924e85
;; 2.1649481537897377e85 3.5029596967131306e85 5.667907850502868e85
;; 9.170867547215998e85 1.4838775397718867e86 2.4009642944934864e86
;; 3.8848418342653726e86 6.285806128758859e86 1.0170647963024232e87
;; 1.6456454091783093e87 2.6627102054807328e87 4.308355614659042e87
;; 6.971065820139775e87 1.1279421434798816e88 1.8250487254938588e88
;; 2.95299086897374e88 4.778039594467599e88 7.731030463441341e88
;; 1.2509070057908937e89 2.0240100521350277e89 3.274917057925922e89
;; 5.298927110060949e89 8.573844167986872e89 1.387277127804782e90
;; 2.2446615446034697e90 3.6319386724082516e90 5.87660021701172e90
;; 9.508538889419971e90 1.5385139106431692e91 2.4893677995851663e91
;; 4.027881710228336e91 6.517249509813502e91 1.0545131220041838e92
;; 1.706238072985534e92 2.7607511949897175e92 4.466989267975251e92
;; 7.227740462964969e92 1.169472973094022e93 1.892247019390519e93
;; 3.061719992484541e93 4.95396701187506e93 8.015687004359601e93
;; 1.2969654016234662e94 2.0985341020594262e94 3.395499503682892e94
;; 5.49403360574232e94 8.88953310942521e94 1.438356671516753e95
;; 2.3273099824592739e95 3.765666653976027e95 6.092976636435301e95
;; 9.858643290411329e95 1.595161992684663e96 2.5810263217257956e96
;; 4.176188314410459e96 6.757214636136254e96 1.0933402950546714e97
;; 1.769061758668297e97 2.862402053722968e97 4.631463812391265e97
;; 7.493865866114232e97 1.2125329678505497e98 1.9619195544619732e98
;; 3.1744525223125226e98 5.1363720767744955e98 8.310824599087019e98
;; 1.3447196675861514e99 2.1758021274948534e99 3.520521795081005e99
;; 5.696323922575858e99 9.216845717656862e99 1.4913169640232722e100
;; 2.413001535788958e100 3.904318499812231e100 6.3173200356011884e100
;; 1.0221638535413418e101 1.6538958571014608e101 2.676059710642803e101
;; 4.3299555677442634e101 7.006015278387065e101 1.1335970846131328e102
;; 1.8341986124518397e102 2.967795697064972e102 4.801994309516812e102
;; 7.769790006581783e102 1.2571784316098597e103 2.0341574322680376e103
;; 3.2913358638778973e103 5.325493296145935e103 8.616829160023835e103
;; 1.3942322456169767e104 2.25591516161936e104 3.650147407236337e104
;; 5.906062568855697e104 9.556209976092033e104 1.546227254494773e105
;; 2.5018482521039765e105 4.0480755065987495e105 6.549923758702726e105
;; 1.0597999265301475e106 1.7147923024004198e106
;; 2.7745922289305678e106 4.489384531330988e106 7.263976760261555e106
;; 1.1753361291592543e107 1.90173380518541e107 3.077069934344664e107
;; 4.978803739530074e107 8.055873673874738e107 1.3034677413404814e108
;; 2.1090551087279548e108 3.412522850068436e108 5.52157795879639e108
;; 8.934100808864828e108 1.445567876766122e109 2.3389779576526047e109
;; 3.7845458344187266e109 6.123523792071331e109 9.908069626490057e109
;; 1.603159341856139e110 2.5939663045051448e110 4.197125646361284e110
;; 6.791091950866427e110 1.098821759722771e111 1.7779309548094139e111
;; 2.876752714532185e111 4.6546836693415994e111 7.531436383873783e111
;; 1.2186120053215383e112 1.9717556437089167e112
;; 3.1903676490304547e112 5.162123292739372e112 8.352490941769827e112
;; 1.3514614234509197e113 2.1867105176279024e113
;; 3.5381719410788226e113 5.724882458706724e113 9.263054399785546e113
;; 1.4987936858492272e114 2.425099125827782e114 3.923892811677009e114
;; 6.348991937504791e114 1.02728847491818e115 1.662187668668659e115
;; 2.6894761435868387e115 4.351663812255499e115 7.0411399558423364e115
;; 1.1392803768097834e116 1.8433943723940173e116
;; 2.9826747492038004e116 4.8260691215978174e116 7.808743870801618e116
;; 1.2634812992399436e117 2.0443556863201056e117
;; 3.3078369855600484e117 5.352192671880153e117 8.660029657440204e117
;; 1.4012222329320356e118 2.267225198676056e118 3.6684474316080913e118
;; 5.9356726302841475e118 9.60412006189224e118 1.5539792692176386e119
;; 2.5143912754068627e119 4.0683705446245015e119 6.582761820031365e119
;; 1.0651132364655865e120 1.7233894184687228e120
;; 2.7885026549343096e120 4.511892073403033e120 7.300394728337342e120
;; 1.1812286801740375e121 1.9112681530077717e121
;; 3.0924968331818097e121 5.00376498618958e121 8.096261819371391e121
;; 1.310002680556097e122 2.119628862493236e122 3.429631543049333e122
;; 5.549260405542569e122 8.978891948591903e122 1.452815235413447e123
;; 2.350704430272637e123 3.803519665686085e123 6.154224095958722e123
;; 9.957743761644806e123 1.611196785760353e124 2.6069711619248333e124
;; 4.218167947685186e124 6.82513910961002e124 1.1043307057295206e125
;; 1.7868446166905226e125 2.891175322420043e125 4.678019939110566e125
;; 7.569195261530608e125 1.2247215200641173e126 1.981641046217178e126
;; 3.2063625662812954e126 5.1880036124984726e126 8.39436617877977e126
;; 1.358236979127824e127 2.1976735970058013e127 3.5559105761336255e127
;; 5.753584173139427e127 9.309494749273052e127 1.5063078922412482e128
;; 2.4372573671685534e128 3.9435652594098005e128
;; 6.3808226265783535e128 1.0324387885988155e129
;; 1.6705210512566508e129 2.702959839855466e129 4.3734808911121175e129
;; 7.076440730967584e129 1.14499216220797e130 1.8526362353047282e130
;; 2.997628397512698e130 4.850264632817428e130 7.847893030330125e130
;; 1.269815766314755e131 2.054605069347768e131 3.324420835662523e131
;; 5.379025905010291e131 8.703446740672814e131 1.4082472645683102e132
;; 2.2785919386355918e132 3.686839203203902e132 5.965431141839495e132
;; 9.652270345043395e132 1.5617701486882891e133 2.5269971831926287e133
;; 4.088767331880918e133 6.615764515073545e133 1.0704531846954464e134
;; 1.732029636202801e134 2.8024828208982473e134 4.534512457101048e134
;; 7.336995277999296e134 1.1871507735100343e135 1.9208503013099638e135
;; 3.1080010748199986e135 5.028851376129963e135 8.13685245094996e135
;; 1.316570382707992e136 2.1302556278029885e136 3.4468260105109804e136
;; 5.577081638313968e136 9.023907648824948e136 1.4600989287138919e137
;; 2.362489693596387e137 3.8225886223102786e137 6.185078315906665e137
;; 1.0007666938216944e138 1.619274525412361e138 2.6200412192340552e138
;; 4.239315744646416e138 6.859356963880472e138 1.1098672708526889e139
;; 1.795802967240736e139 2.905670238093425e139 4.7014732053341606e139
;; 7.607143443427585e139 1.2308616648761745e140 1.991576009218933e140
;; 3.222437674095107e140 5.214013683314041e140 8.436451357409148e140
;; 1.3650465040723188e141 2.208691639813234e141 3.573738143885553e141
;; 5.782429783698786e141 9.356167927584338e141 1.5138597711283123e142
;; 2.449476563886747e142 3.9633363350150586e142 6.412812898901804e142
;; 1.0376149233916865e143 1.678896213281867e143 2.716511136673553e143
;; 4.395407349955419e143 7.111918486628973e143 1.1507325836584395e144
;; 1.8619244323213365e144 3.0126570159797754e144 4.874581448301112e144
;; 7.887238464280887e144 1.2761819912582e145 2.064905837686289e145
;; 3.341087828944489e145 5.405993666630778e145 8.747081495575267e145
;; 1.4153075162206045e146 2.290015665778131e146 3.7053231819987362e146
;; 5.995338847776867e146 9.700662029775601e146 1.5696000877552472e147
;; 2.539666290732807e147 4.1092663784880546e147 6.648932669220861e147
;; 1.0758199047708915e148 1.740713171692978e148 2.816533076463869e148
;; 4.557246248156847e148 7.373779324620716e148 1.1931025572777562e149
;; 1.9304804897398278e149 3.123583047017584e149 5.054063536757412e149
;; 8.177646583774997e149 1.3231710120532408e150 2.1409356704307403e150
;; 3.464106682483982e150 5.605042352914722e150 9.069149035398702e150
;; 1.4674191388313424e151 2.3743340423712125e151 3.841753181202555e151
;; 6.216087223573768e151 1.0057840404776323e152 1.6273927628350091e152
;; 2.6331768033126416e152 4.260569566147651e152 6.893746369460292e152
;; 1.1154315935607943e153 1.8048062305068232e153
;; 2.9202378240676177e153 4.725044054574441e153 7.645281878642057e153
;; 1.23703259332165e154 2.001560781185856e154 3.2385933745075055e154
;; 5.240154155693362e154 8.478747530200868e154 1.371890168589423e155
;; 2.2197649216095093e155 3.591655090198932e155 5.811420011808443e155
;; 9.403075102007376e155 1.5214495113815815e156 2.4617570215823187e156
;; 3.9832065329639005e156 6.4449635545462196e156
;; 1.0428170087510119e157 1.6873133642056336e157
;; 2.7301303729566468e157 4.4174437371622795e157 7.147574110118925e157
;; 1.1565017847281205e158 1.8712591957400128e158 3.027760980468134e158
;; 4.899020176208146e158 7.92678115667628e158 1.2825801332884426e159
;; 2.075258248956071e159 3.3578383822445138e159 5.433096631200584e159
;; 8.790935013445098e159 1.4224031644645682e160 2.301496665809078e160
;; 3.723899830273646e160 6.025396496082725e160 9.74929632635637e160
;; 1.5774692822439095e161 2.552398914879546e161 4.129868197123456e161
;; 6.682267112003002e161 1.0812135309126458e162 1.7494402421129458e162
;; 2.830653773025592e162 4.580094015138538e162 7.41074778816413e162
;; 1.1990841803302669e163 1.9401589591466796e163 3.139243139476946e163
;; 5.079402098623626e163 8.218645238100573e163 1.32980473367242e164
;; 2.151669257482477e164 3.481473991154897e164 5.633143248637374e164
;; 9.11461723979227e164 1.4747760488429645e165 2.3862377728221914e165
;; 3.861013821665157e165 6.247251594487348e165 1.0108265416152503e166
;; 1.635551701063985e166 2.6463782426792358e166 4.28192994374322e166
;; 6.928308186422456e166 1.1210238130165677e167 1.8138546316588132e167
;; 2.934878444675381e167 4.748733076334195e167 7.683611521009576e167
;; 1.243234459734377e168 2.0115956118353344e168 3.254830071569711e168
;; 5.266425683405046e168 8.521255754974755e168 1.3787681438379806e169
;; 2.2308937193354555e169 3.609661863173436e169 5.8405555825088935e169
;; 9.450217445682327e169 1.529077302819122e170 2.4740990473873544e170
;; 4.003176350206477e170 6.477275397593833e170 1.0480451747800309e171
;; 1.6957727145394138e171 2.7438178893194447e171
;; 4.4395906038588585e171 7.183408493178303e171 1.1622999097037164e172
;; 1.8806407590215465e172 3.0429406687252632e172 4.923581427746809e172
;; 7.966522096472073e172 1.289010352421888e173 2.0856625620690953e173
;; 3.374672914490983e173 5.460335476560078e173 8.835008391051063e173
;; 1.4295343867611142e174 2.31303522586622e174 3.742569612627335e174
;; 6.055604838493555e174 9.798174451120892e174 1.5853779289614443e175
;; 2.5651953740735335e175 4.150573303034977e175 6.715768677108512e175
;; 1.0866341980143488e176 1.7582110657252e176 2.844845263739549e176
;; 4.6030563294647495e176 7.447901593204297e176 1.2050957922669046e177
;; 1.9498859515873345e177 3.1549817438542393e177 5.104867695441573e177
;; 8.259849439295813e177 1.3364717134737384e178 2.1624566574033203e178
;; 3.498928370877058e178 5.6613850282803775e178 9.160313399157436e178
;; 1.4821698427437814e179 2.3982011826595251e179 3.880371025403307e179
;; 6.278572208062831e179 1.0158943233466138e180 1.6437515441528969e180
;; 2.659645867499511e180 4.303397411652407e180 6.963043279151918e180
;; 1.1266440690804325e181 1.8229483969956245e181
;; 2.9495924660760573e181 4.772540863071682e181 7.722133329147739e181
;; 1.2494674192219417e182 2.021680752136716e182 3.271148171358658e182
;; 5.292828923495374e182 8.56397709485403e182 1.3856806018349407e183
;; 2.242078311320344e183 3.627758913155283e183 5.8698372244756265e183
;; 9.49759613763091e183 1.536743336210654e184 2.486502949973745e184
;; 4.023246286184398e184 6.509749236158144e184 1.0532995522342541e185
;; 1.704274475850069e185 2.757574028084323e185 4.461848503934392e185
;; 7.219422532018715e185 1.1681271035953107e186 1.8900693567971822e186
;; 3.0581964603924926e186 4.948265817189675e186 8.006462277582167e186
;; 1.2954728094771843e187 2.0961190372354008e187 3.391591846712585e187
;; 5.487710883947986e187 8.879302730660571e187 1.436701361460856e188
;; 2.3246316345269132e188 3.7613329959877685e188 6.085964630514681e188
;; 9.84729762650245e188 1.5933262257017133e189 2.578055988351958e189
;; 4.1713822140536703e189 6.749438202405629e189 1.09208204164593e190
;; 1.767025861886493e190 2.8591079035324227e190 4.626133765418916e190
;; 7.48524166895134e190 1.2111375434370255e191 1.959661710332159e191
;; 3.1707992537691844e191 5.1304609641013445e191 8.301260217870528e191
;; 1.343172118197187e192 2.17329813998424e192 3.516470258181428e192
;; 5.689768398165667e192 9.206238656347096e192 1.4896007054512763e193
;; 2.410224571085986e193 3.899825276537262e193 6.3100498476232484e193
;; 1.020987512416051e194 1.651992497178376e194 2.672980009594427e194
;; 4.324972506772803e194 6.99795251636723e194 1.1322925023140033e195
;; 1.832087753950726e195 2.964380256264729e195 4.796468010215455e195
;; 7.760848266480186e195 1.2557316276695637e196 2.0318164543175818e196
;; 3.2875480819871463e196 5.319364536304729e196 8.606912618291876e196
;; 1.39262771545966e197 2.253318977288848e197 3.6459466927485085e197
;; 5.8992656700373556e197 9.545212362785863e197 1.544447803282322e198
;; 2.498969039560908e198 4.043416842843231e198 6.54238588240414e198
;; 1.058580272524737e199 1.7128188607651506e199 2.7713991332898876e199
;; 4.484217994055038e199 7.255617127344926e199 1.1739835121399964e200
;; 1.899545224874489e200 3.0735287370144856e200 4.973073961888975e200
;; 8.04660269890346e200 1.3019676660792435e201 2.1066279359695897e201
;; 3.408595602048833e201 5.515223538018424e201 8.923819140067255e201
;; 1.4439042678085678e202 2.336286181815293e202 3.7801904496238607e202
;; 6.116476631439154e202 9.896667081063015e202 1.6013143712502167e203
;; 2.5909810793565184e203 4.192295450606735e203 6.783276529963254e203
;; 1.097557198056999e204 1.7758848510533244e204 2.8734420491103234e204
;; 4.649326900163648e204 7.52276894927397e204 1.2172095849437618e205
;; 1.969486479871159e205 3.186696064814921e205 5.156182544686079e205
;; 8.342878609501001e205 1.3499061154187079e206 2.184193976368808e206
;; 3.534100091787516e206 5.718294068156324e206 9.252394159943839e206
;; 1.4970688228100163e207 2.4223082388044002e207
;; 3.9193770616144174e207 6.341685300418816e207 1.0261062362033233e208
;; 1.660274766245205e208 2.6863810024485288e208 4.346655768693734e208)
