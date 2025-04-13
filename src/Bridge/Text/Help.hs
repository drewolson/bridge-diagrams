module Bridge.Text.Help
  ( helpText,
  )
where

import Data.Text (Text)
import Text.RawString.QQ (r)

helpText :: Text
helpText =
  [r|Given a plaintext representation of a hand (or many hands), a diagram will be
generated.  Hands are given as lists of suits, separated by a space. Honors can
be upper or lowercase. The character 'x' or 'X' can be provided instead of a rank.
Void suits can be represented using the '-' character or the strings "void" or
"Void".

Examples:

akxxx qtx jxx xx
akqxxxxx - - kjxxx
AKQxxxxx Void void KJxxx

You can also optionally provide a seat, vulnerability, opening lead, and
scoring separated by commas.

Examples:

1st; akxxx qxx jtx xx; jx jx akxxx qxxx, r/r, d4, imps


Several types of layouts are supported.



Single hand:

akxxx kqx txx xx

♠AKxxx
♥KQx
♦Txx
♣xx



Single dummy, two hands separated by ';'

akxxx qxx jtx xx; jx jx akxxx qxxx

♠AKxxx
♥Qxx
♦JTx
♣xx

♠Jx
♥Jx
♦AKxxx
♣Qxxx

Or with an opening lead, vul, and scoring:

akxxx qxx jtx xx; jx jx akxxx qxxx, d4, r/r, imps

Vul: R/R   ♠AKxxx
IMPs       ♥Qxx
           ♦JTx
           ♣xx
Lead: ♦4
           ♠Jx
           ♥Jx
           ♦AKxxx
           ♣Qxxx



Double dummy, four hands separated by ';'. If only 3 hands are provided,
a fourth will be derived from the provided hands.

akxxx qxx jtx xx; qxx akxxx xxx kx; jx jx akxxx qxxx; xxx xxx qx axxxx

           ♠AKxxx
           ♥Qxx
           ♦JTx
           ♣xx
♠xxx        -----     ♠Qxx
♥xxx       |  N  |    ♥AKxxx
♦Qx        |W   E|    ♦xxx
♣Axxxx     |  S  |    ♣Kx
            -----
           ♠Jx
           ♥Jx
           ♦AKxxx
           ♣Qxxx



Defense, west perspective, two hands separated by '<'

xxx xxx qx axxxx < akxxx qxx jtx xx

           ♠AKxxx
           ♥Qxx
           ♦JTx
           ♣xx
♠xxx        -----
♥xxx       |  N  |
♦Qx        |W    |
♣Axxxx     |     |
            -----



Defense, east perspective, two hands separated by '>'

akxxx qxx jtx xx > qxx akxxx xxx kx

♠AKxxx
♥Qxx
♦JTx
♣xx
 -----     ♠Qxx
|  N  |    ♥AKxxx
|    E|    ♦xxx
|     |    ♣Kx
 -----
|]
