{-# LANGUAGE OverloadedStrings          #-}

module ENCOINS.Core.V1.OnChain.Aiken.UPLC where

import           Data.Text                                 (Text)

encoinsPolicyCheck :: Text
encoinsPolicyCheck = "590f85010000323232323232323232323232322322232323232323232323232323232323232323232323232323232323232323232323232323232533302e3232323232323232323232323232323232323232323232323232323232323232323232323232323232323232533305a533305a533305a533305a533305a011101014a0201c294040185280a99982d0028a51100414a0200229414ccc164cdc4a400066e00cdc000d00c0010a5113303d3758660aa60ae05690021299982d19baf330563058001480000784c0d8cc0b4dd59982b182c000a40046605a0306605a02e0042940c0f8cdc1000a410112f4a6660ae002202629000199191919129998300008a51132533305c33712600a607200290020980198320010a50375660c4002600200244a6660be00229444c94ccc16ccdc39802181c000a400826600600660c60042940dd59830800980080091299982e8008a400026466e0120023300300300130600010043375e6e98008dd319814002998140131981400980819baf374c0026e98cc09c010048ccc0fc0040992f5bded8c06608000246eaccc13cc14400520023304637586609a609e046900212999829299982919baf3304e3050001480000604c0b8cc094dd5998271828000a4004608401829404cdd7998271828000a400866e952004330583305349930103d87980004bd700a5033303c0010234bd6f7b6301981e80091bab3304c304e00148008cc10c00494ccc13ccdd7998259826800a400002a26056660446eaccc12cc1340052002303f00914a0660766eb0cc124c12c07d200023304a304c00148008cc0c000494ccc134cdd7998249825800a400002626052660406eaccc124c12c0052002303d01b14a0660726eb0cc11cc1240752002233048304a00148008ccdca80b99191919191b9237666e9ccc154dd38029982a9ba73305530520033305530530034bd701982a9ba90014bd701bae3055001305500232323376060aa00460aa00260aa0026eb0c14c004c14c108dd61828820808a99982499baf374c0046e980044cdd79ba73323223002001300100122533305000114bd700991919191982a9ba900133006006003375c60a20066eacc144008c150008c148004068dd3998279ba90184bd700a5033323222330030020013001001222533305000214bd6f7b630099191919199981c9998038038018028008010029bae3051003375a60a200460a800660a4004010062666464446464a66609a66e1d200200114bd6f7b6300991bab3055001304b002304b001330030020013001001222533304f00214c0103d87a8000132323232533304e3371e00a002266e95200033054374c00497ae01333007007003005375c60a00066eacc140008c14c00cc144008060058c0b1208092f401302b33704002904044bd19b8248010cccc8c00400488894ccc13400c40044c8c8cc010008cccc01801800401000cc144010dd698278019999180080091129998260010a5eb804c8c8cc13cdd418018011998028028008019828001982700100211824800b8048000c0a4cdc10012410112f4605066e0800d2080897a5333041337100069000099b814800000c400cdd598220051bad3042009375a608a002608a0046086002608600e608200c6eb8c100004c100004c0fc008c8c8cdd8181f801181f800981f8009bac303d001303d02c375860760566eb8c0dc0b4dd5981a8161bae3038001302e3253330313370e9000181800088008a99819a4812a4578706563746564206f6e20696e636f727265637420636f6e7374727563746f722076617269616e742e00163302c302e02848008dd5998159816800a401066054605804c90000a4c2c44666600e00400244464a666066a66607000229445280a60103d87a800013374a90001981c9ba60014bd70199980300100091119299981b19b87001480005300103d87a800013374a90001981e1ba80014bd7019b8000200101b01830010012222253330350041003132323232333330090090033333300a007001002006005006005375c606c0066eb4c0d8008c0e4014c0dc010c0040048888894ccc0d00144cc0d4cdd81ba9004375000697adef6c6013232323253330333375e6600a01000298103d8798000133039337606ea4020dd40038048a99981999b8f0080011323253330353370e9000000899191981e99bb037520186ea000401cdd6981e80098198010802981980099980300400380109981c99bb037520026ea0008cccccc02802800c02001c018014dd7181a8019bad30350023038006303600530010012222253330310041003132323232333330090090033333300a007001002006005006005375c60640066eacc0c8008c0d4014c0cc010c0040048888894ccc0c00144cc0c4cdd81ba9004374c00697adef6c60132323232533302f3375e6600a01000298103d8798000133035337606ea4020dd30038048a99981799b8f0080011323253330313370e9000000899191981c99bb037520186e9800401cdd5981c80098178010802981780099980300400380109981a99bb037520026e98008cccccc02802800c02001c018014dd718188019bab303100230340063032005233007300200123232323371290000009bad302f001302f001302e0012333004001222333005002222533302a3370e004900008008998181ba733030375200c660606ea400ccc0c0dd400125eb800040052f5c060020024444a666054006200226464646466600c002004666601001000600c00a6eb8c0ac00cdd6981580118170021816001980080091112999814001880089919191919980300080119998040040018030029bae302900337566052004605800860540066002002444a66604a00429444c8c94ccc088c00c0084ccc01401400400c52818148019bac30270023001001222533302300214a026464a666040600600429444ccc01401400400cc09c00cc09400894ccc06ccdc3800a4000297adef6c6013233330034bd6f7b63024410000100533330054bd6f7b630245000010043001001222225333022004133023337606ea400cdd300125eb7bdb1804c8c8c8c94ccc084cdd799802803800a60103d8798000133027337606ea401cdd30030040a99981099b8f007001133027337606ea401cdd300300189981399bb037520026e98008ccccc02402400c01c018014dd718118019bab30230023026005302400422533301933720004002298103d8798000153330193371e0040022980103d87a800014c103d87b8000300100122222533301f004133020337606ea400cdd400125eb7bdb1804c8c8c8c94ccc078cdd799802803800a60103d8798000133024337606ea401cdd40030040a99980f19b8f007001133024337606ea401cdd400300189981219bb037520026ea0008ccccc02402400c01c018014dd718100019bad30200023023005302100430010012222533301c00310011323233004002333300600600100400330200043756603c0066002002444a666032004297ae0132323301c374c600600466600a00a002006603a006603600446600800244660080024466e0520000013001001222533301600214bd6f7b6300991919191980d99bb037520026ea0cc014004008ccc01c01c00c014dd7180b8019bad3017002301a00330180023001001222533301400214bd6f7b6300991919191980c99bb037520026e98cc014004008ccc01c01c00c014dd7180a8019bab3015002301800330160023001001222533301200214bd70099192999807980180109980a8011998028028008018999802802800801980b001980a0011919191919191919299980b180c80109919191919299980d980f00109919191919191900a1999180080091129998120010a4c26466600800860500060046002604c00400246464931bad3022002375c60400026eacc07c03cdd6980e80718010031800803919299980c99b87480000044c8c8c8c94ccc088c0940084c8c92632533301f3370e9000000899192999813181480109924c64a66604466e1d2000001132325333029302c002132498c03800454cc099241334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016302a0013020002153330223370e90010008991919191919299981698180010a4c2a660549201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a605c002605c0046eb4c0b0004c0b0008dd6981500098100010a9981224812b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016302000115330234901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163027001301d0031533301f3370e90010008a999812180e8018a4c2a6604292011d4578706563746564206e6f206669656c647320666f7220436f6e737472001615330214912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016301d0023007003153301f491334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e20657870656374656400163023001302300230210013017002153301b4912b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016301700123253330183370e900000089919299980f98110010a4c2a66038921334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c6040002602c0042a66603066e1d200200113232533301f3022002149854cc0712401334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c6040002602c0042a660349212b436f6e73747220696e64657820646964206e6f74206d6174636820616e7920747970652076617269616e740016301600115330184901334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375a6038002603800460340026034010603000e2a660269201334c6973742f5475706c652f436f6e73747220636f6e7461696e73206d6f7265206974656d73207468616e2065787065637465640016375c602e002602e0046eb8c054004c054008c8c8cdd8180a801180a800980a8009bac30130013013002375860220026eb0008c8c8cdd81807801180780098078009bac00133001001480008888cccc01ccdc38008018071199980280299b8000448008c0400040080088c01cdd5000918029baa0015734ae6d5ce2ab9d5573caae7d5d02ba157441"

ledgerValidatorCheck :: Text
ledgerValidatorCheck = "58c10100003232323232323232323232222253330093232323233323222330030020013001001222533301300214a0264646464a66602866ebc0140045288999803803801802980a001980a001180b801980a80100100099ba548000cc040dd4801a5eb80dd5998039804800a40246600c601000490001bae330053007004480005261633001001480008888cccc01ccdc38008018061199980280299b8000448008c0380040080088c014dd5000918019baa0015734aae7555cf2ab9f5740ae855d11"