module Board
where

{-
message to react to
board:
 |0|1|2|
-+-+-+-+x
0|O| | |
-+-+-+-+
1| |X| |
-+-+-+-+
2|#| | |
-+-+-+-+
 y

moves:
(0, 2) = X by "pVF"
(0, 0) = O by "xDtqHaVauiAtOpoBezmIn"
(1, 1) = X by "pVF"
(0, 2) = O by "xDtqHaVauiAtOpoBezmIn"
-}
message :: String
message = "d1:cd1:0i0e1:1i2ee2:id3:pVF4:prevd1:cd1:0i0e1:1i0ee2:id21:xDtqHaVauiAtOpoBezmIn4:prevd1:cd1:0i1e1:1i1ee2:id3:pVF4:prevd1:cd1:0i0e1:1i2ee2:id21:xDtqHaVauiAtOpoBezmIn1:v1:oe1:v1:xe1:v1:oe1:v1:xe"
