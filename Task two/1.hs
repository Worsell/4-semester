
function a = function' a [] where
		function' [] res = res
		function' inp out = function' (tail inp) (head inp : out)