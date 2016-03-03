module Triangle

enum Triangle { Equilateral, Isoceles, Scalene, Invalid }

pred validTriangle[x, y, z : Int] {
	x < add[y, z] and y < add[x, z] and z < add[x, y]
}

pred validCase [x, y, z : Int, class : Triangle] {
	validTriangle[x, y, z]
	#(x+y+z) = 1 <=> class = Equilateral
	#(x+y+z) = 2 <=> class = Isoceles
	#(x+y+z) = 3 <=> class = Scalene
}

pred invalidCase [x, y, z : Int, class : Triangle] {
	not validTriangle[x, y, z]
	class = Invalid
}

pred Tri [x, y, z : Int, class : Triangle] {
	validCase[x, y, z, class] or invalidCase [x, y, z, class]
}

pred CE_EQU [x, y, z : Int, class : Triangle] {
	validCase [x, y, z, class]
	x = y and y = z
}

pred TT_PERM_SCA_xyz [x, y, z : Int] {
	validTriangle [x, y, z]
	#(x+y+z) = 3
	x < z and z < y
}

pred PERM_SCA_xyz [x, y, z : Int, class : Triangle] {
	Tri [x, y, z, class]
	TT_PERM_SCA_xyz [x, y, z]
}
run PERM_SCA_xyz for 6 Int

pred TB_INV_1_2 [x, y, z : Int] {
	not validTriangle [x, y, z]
	x >= add[y, z] and y >= add[x, z] and z >= add[x, y]
	x = 0 and y = 0 and z != 0
}
run TB_INV_1_2 for 6 Int expect 0

run CE_EQU for 6 Int
