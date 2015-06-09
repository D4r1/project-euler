import System.Environment

import qualified Euler.E1
import qualified Euler.E2
import qualified Euler.E3
import qualified Euler.E4
import qualified Euler.E5
import qualified Euler.E6
import qualified Euler.E7
import qualified Euler.E8
import qualified Euler.E9
import qualified Euler.E10
import qualified Euler.E11
import qualified Euler.E12
import qualified Euler.E13
import qualified Euler.E14
import qualified Euler.E15
import qualified Euler.E16
import qualified Euler.E17
import qualified Euler.E18
import qualified Euler.E19
import qualified Euler.E20
import qualified Euler.E21
import qualified Euler.E22
import qualified Euler.E23
import qualified Euler.E24
import qualified Euler.E25
import qualified Euler.E26
import qualified Euler.E27
import qualified Euler.E28
import qualified Euler.E29
import qualified Euler.E30
import qualified Euler.E31
import qualified Euler.E32
import qualified Euler.E33
import qualified Euler.E34
import qualified Euler.E35
import qualified Euler.E36
import qualified Euler.E37
import qualified Euler.E38
import qualified Euler.E39
import qualified Euler.E40
import qualified Euler.E41
import qualified Euler.E42
import qualified Euler.E43
import qualified Euler.E44
import qualified Euler.E45
import qualified Euler.E46
import qualified Euler.E47
import qualified Euler.E48
import qualified Euler.E49
import qualified Euler.E50
import qualified Euler.E51
import qualified Euler.E52
import qualified Euler.E53
import qualified Euler.E54
import qualified Euler.E55
import qualified Euler.E56
import qualified Euler.E57
import qualified Euler.E58
import qualified Euler.E59
import qualified Euler.E60
import qualified Euler.E61
import qualified Euler.E62
import qualified Euler.E63
import qualified Euler.E64
import qualified Euler.E65
import qualified Euler.E66
import qualified Euler.E67
import qualified Euler.E68
import qualified Euler.E69
import qualified Euler.E70

mains :: [IO ()]
mains =
	[ Euler.E1.main
	, Euler.E2.main
	, Euler.E3.main
	, Euler.E4.main
	, Euler.E5.main
	, Euler.E6.main
	, Euler.E7.main
	, Euler.E8.main
	, Euler.E9.main
	, Euler.E10.main
	, Euler.E11.main
	, Euler.E12.main
	, Euler.E13.main
	, Euler.E14.main
	, Euler.E15.main
	, Euler.E16.main
	, Euler.E17.main
	, Euler.E18.main
	, Euler.E19.main
	, Euler.E20.main
	, Euler.E21.main
	, Euler.E22.main
	, Euler.E23.main
	, Euler.E24.main
	, Euler.E25.main
	, Euler.E26.main
	, Euler.E27.main
	, Euler.E28.main
	, Euler.E29.main
	, Euler.E30.main
	, Euler.E31.main
	, Euler.E32.main
	, Euler.E33.main
	, Euler.E34.main
	, Euler.E35.main
	, Euler.E36.main
	, Euler.E37.main
	, Euler.E38.main
	, Euler.E39.main
	, Euler.E40.main
	, Euler.E41.main
	, Euler.E42.main
	, Euler.E43.main
	, Euler.E44.main
	, Euler.E45.main
	, Euler.E46.main
	, Euler.E47.main
	, Euler.E48.main
	, Euler.E49.main
	, Euler.E50.main
	, Euler.E51.main
	, Euler.E52.main
	, Euler.E53.main
	, Euler.E54.main
	, Euler.E55.main
	, Euler.E56.main
	, Euler.E57.main
	, Euler.E58.main
	, Euler.E59.main
	, Euler.E60.main
	, Euler.E61.main
	, Euler.E62.main
	, Euler.E63.main
	, Euler.E64.main
	, Euler.E65.main
	, Euler.E66.main
	, Euler.E67.main
	, Euler.E68.main
	, Euler.E69.main
	, Euler.E70.main
	]

main :: IO ()
main = do
	as <- getArgs
	let a = read $ as !! 0
	mains!!(a - 1)
